#' Split two regional or national rasters into gridded tiles
#'
#'@param rasterpath file paths of input raster files
#'@param rasterID text strings to identify output tiles (e.g. CDL for NASS Cropland Data Layer)
#'@param regionalextent optional, vector of state names to crop national raster to region of interest
#'@param div division factor specifying the number of tiles in x and y dimensions
#'@param buffercells number of cells overlapping between adjacent tiles
#'@param NAvalues No data or background values of input rasters
#'@param writetiles logical, write tiles as directory of .tif files?
#'@param tiledir path to directory where output tiles should be saved

#'@return A list of raster tiles. Each element of the list is a pair of raster tiles (one tile for each of the two input raster).
#'@keywords bees landscape ecology spatial
#'@details
#'NAvalue parameter is used to identify raster tiles that are all background (e.g. areas of open water). These background tiles are excluded from list of output tiles and tile directory.
#'@export
#'@import logger
#'@import future
#'@examples
#'example forthcoming.

grid_rasters <- function(rasterpath, rasterID,
                        regionalextent=NA,
                        div, buffercells=c(0,0),
                        NAvalues,
                        writetiles = T, tiledir) {

  ######################################################################################################
  ##### Part 1: Setup and load data

  # load libraries
  library(future); library(foreach)

  #separate file paths to CDL and vegetation rasters.
  cdl_path <- rasterpath[1]
  veg_path <- rasterpath[2]

  #set up logger to write status updates
  library(logger)
  logger::log_threshold(DEBUG)

  # create directories for output files if they don't already exist
  if (!dir.exists(tiledir)) {
    dir.create(tiledir)
  }

  #create CDL and NVC tile folders if they don't already exist
  if (!dir.exists(paste0(tiledir, "/", rasterID[1]))) {
    dir.create(paste0(tiledir, "/", rasterID[1]))
    dir.create(paste0(tiledir, "/", rasterID[2]))
  }

  # load raster1
  # We will use raster object to re-project state polygons
  cdl <- raster::raster(cdl_path)

  # if necessary, download polygon layer of state boundaries
  if (!any(is.na(regionalextent)) & is.character(regionalextent)) {
    logger::log_info('Re-projecting shapefile to match CDL raster.')

    # download shapefile of US states
    region <- tigris::states() %>% dplyr::filter(NAME %in% regionalextent) %>%
      sf::st_transform(crs = sf::st_crs(cdl)) %>% # re-project polygon layer to match raster1
      terra::vect()

  } else if ('sf' %in% class(regionalextent)) {
    region <- sf::st_transform(regionalextent, crs = sf::st_crs(cdl)) %>%
      terra::vect()
  }

  ######################################################################################################
  ##### Part 2: Crop national rasters to regional extent

  # read input raster and crop to extent of provided shapefile
  # use the terra package because it is faster than raster.
  if (!any(is.na(regionalextent))) {

    tictoc::tic()
    logger::log_info('Cropping national raster(s) to shapefile extent (if regionalextent is provided).')

    region_cdl <- terra::rast(cdl_path) %>%
      terra::crop(y=region) %>%
      raster::raster()   # convert to a raster object so the SpaDES package works


    if (!is.na(veg_path)) {
      region_nvc <- terra::rast(nvc_path) %>%
        terra::crop(y=region) %>%
        raster::raster()   # convert to a raster object so the SpaDES package works

    }

      tictoc::toc()
  }

  ######################################################################################################
  ##### Part 3: Split raster1 into tiles

  logger::log_info('Splitting rasters into specified number of tiles (n = xdiv * ydiv).')
  logger::log_info('Splitting first raster.')

  cdl_tiles <- tryCatch({
    # set up parallel processing cluster (will be used by splitRaster function)
    cl <- parallel::makeCluster(parallel::detectCores())  # use all cores

    tictoc::tic()

    # split raster1 into tiles using a regular grid
    cdl_tiles <- SpaDES.tools::splitRaster(r=region_cdl, nx=div[1], ny=div[2],
                                           buffer=buffercells, cl=cl)

  }, error= function(err){ # if the parallel execution fails, try running with only one thread

    logger::log_info(paste("Split first raster w/ single thread. Parallel processing error = ",err))
    tictoc::tic()

    # split raster1 into tiles using a regular grid
    cdl_tiles <- SpaDES.tools::splitRaster(r=region_cdl, nx=div[1], ny=div[2],
                                           buffer=buffercells)
    return(cdl_tiles)
  })

  ######################################################################################################
  ##### Part 4: If raster2 file path is provided, split raster2 into tiles
  logger::log_info('Splitting second raster.')

  nvc_tiles <- tryCatch({
    # set up parallel processing cluster (will be used by splitRaster function)
    cl <- parallel::makeCluster(parallel::detectCores())  # use all cores

    tictoc::tic()

    nvc_tiles <- SpaDES.tools::splitRaster(r=region_nvc, nx=div[1], ny=div[2],
                                         buffer=buffercells, cl=cl)
  }, error= function(err){ # if the parallel execution fails, try running with only one thread

    logger::log_info(paste("Split second raster w/ a single thread. Parallel processing error = ",err))
    tictoc::tic()

    # split raster1 into tiles using a regular grid
    nvc_tiles <- SpaDES.tools::splitRaster(r=region_nvc, nx=div[1], ny=div[2],
                                           buffer=buffercells)
    return(nvc_tiles)
  })


  ######################################################################################################
  ##### Part 5: Handle background tiles that are all NA

  # save lists of which raster tiles are all NA values (cdl == 0 & nvc == -9999)
  # We don't need to process raster tiles that are completely NA values (background)
  # I use the purrr map function because it nicely applies a function over a list, which is the format returned by splitRaster

  logger::log_info('Identifying raster tiles that are all NA values.')

  #turn on parallel processing for furrr package
  future::plan(multisession)

  tictoc::tic()
  # list of NA tiles for raster1
  todiscard_cdl <- furrr::future_map(.x=cdl_tiles, .f = function(x) {
    raster::cellStats(x, stat=max) == NAvalues[1] },
    .options = furrr::furrr_options(seed = TRUE)) %>% unlist()

  # list of NA tiles raster2
  todiscard_nvc <- furrr::future_map(.x=nvc_tiles, .f = function(x) {
      raster::cellStats(x, stat=max) == NAvalues[2] },
      .options = furrr::furrr_options(seed = TRUE)) %>% unlist()

  tictoc::toc()

    # if NA tiles from raster1 and raster2 do NOT match, make folder of mis-matched tiles
    # (no match = one layer is all background, but the other has values other than NA)

  if (any(todiscard_cdl != todiscard_nvc)) {
      warning('Raster1 and Raster2 background tiles (100% NA values) do not match.
           The boundaries (e.g. land/water) of these rasters are probably different.
           Look at the tiles in the mismatched tiles folder.')

    # create directory for mismatched tiles
    if (!dir.exists(paste0(tiledir, "/MismatchedBorderTiles/"))) {
      dir.create(paste0(tiledir, "/MismatchedBorderTiles/"))
    }

    # write mis-matched rasters as .tif files
    for (i in which(todiscard_cdl != todiscard_nvc)) {

      raster::writeRaster(cdl_tiles[[i]], paste0(tiledir, "/MismatchedBorderTiles/", rasterID[1],
                                                   "Tile", i, ".tif"), overwrite=T)
      if (!is.na(veg_path)) {
        raster::writeRaster(nvc_tiles[[i]], paste0(tiledir, "/MismatchedBorderTiles/", rasterID[2],
                                                   "Tile", i, ".tif"), overwrite=T)
      }
    }
  }

  # make list object of raster tiles to return (non-NA values in one or more raster layers)
  tile_list <- furrr::future_map2(.x= purrr::discard(cdl_tiles, todiscard_nvc&todiscard_cdl),
                               .y= purrr::discard(nvc_tiles, todiscard_nvc&todiscard_cdl), .f=c)

  ######################################################################################################
  ##### Part 6: Write tiles as individual .tif files

  if (writetiles == T) {
    logger::log_info('Writing output tiles.')

    # set up parallel processing cluster
    cl <- parallel::makeCluster(parallel::detectCores())  # use all but 2 cores
    parallel::clusterExport(cl=cl, envir=environment(),
                      varlist=c('cdl_tiles', 'nvc_tiles', 'tiledir', 'rasterID'))

    doParallel::registerDoParallel(cl)  # register the parallel backend


    # exclude tiles that are all NA values for BOTH layers
    foreach::foreach(i= which(!(todiscard_nvc&todiscard_cdl))) %dopar% {

      raster::writeRaster(cdl_tiles[[i]], paste0(tiledir, "/", rasterID[1], "/", rasterID[1], "_Tile", i, ".tif"), overwrite=T)
      raster::writeRaster(nvc_tiles[[i]], paste0(tiledir, "/", rasterID[2], "/", rasterID[2], "_Tile", i, ".tif"), overwrite=T)
    }
  }

  #turn off parallel environments
  parallel::stopCluster(cl); future::plan(sequential)
  logger::log_info('Gridding function complete, returning pairs of raster tiles as a list.')

  return(tile_list)
}
