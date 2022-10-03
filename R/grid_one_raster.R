#' Split regional or national raster into gridded tiles
#'
#'@param rasterpath file path of input raster file
#'@param rasterID text string to identify output tiles (e.g. CDL for NASS Cropland Data Layer)
#'@param regionalextent optional, vector of state names to crop national raster to region of interest
#'@param div division factor specifying the number of tiles in x and y dimensions
#'@param buffercells number of cells overlapping between adjacent tiles
#'@param NAvalue No data or background value of input raster
#'@param writetiles logical, write tiles as directory of .tif files?
#'@param tiledir path to directory where output tiles should be saved

#'@return A list of raster tiles.
#'@keywords bees landscape ecology spatial
#'@details
#'NAvalue parameter is used to identify raster tiles that are all background (e.g. areas of open water). These background tiles are excluded from list of output tiles and tile directory.
#'@export
#'@import logger
#'@import future


grid_one_raster <- function(rasterpath, rasterID,
                            regionalextent=NA,
                            div,
                            buffercells=c(0,0),
                            NAvalue,
                            writetiles = T,
                            tiledir) {

  ######################################################################################################
  ##### Part 1: Setup and load data

  # load libraries
  library(future); library(foreach)

  #set up logger to write status updates
  library(logger)
  logger::log_threshold(DEBUG)

  # create directories for output files if they don't already exist
  if (!dir.exists(tiledir)) {
    dir.create(tiledir, recursive = T)
  }

  #create CDL and NVC tile folders if they don't already exist
  if (!dir.exists(paste0(tiledir, "/", rasterID[1]))) {
    dir.create(paste0(tiledir, "/", rasterID[1]))
  }

  # load raster1
  # We will use raster object to re-project state polygons
  input_raster <- raster::raster(rasterpath)

  # if necessary, download polygon layer of state boundaries
  if (!any(is.na(regionalextent)) & is.character(regionalextent)) {
    logger::log_info('Re-projecting shapefile to match CDL raster.')

    # download shapefile of US states
    region <- tigris::states() %>% dplyr::filter(NAME %in% regionalextent) %>%
      sf::st_transform(crs = sf::st_crs(input_raster)) # re-project polygon layer to match raster1

  } else if ('sf' %in% class(regionalextent)) {
    region <- sf::st_transform(regionalextent, crs = sf::st_crs(input_raster))
  }

  ######################################################################################################
  ##### Part 2: Crop national raster to regional extent

  # read input raster and crop to extent of provided shapefile
  if (!any(is.na(regionalextent))) {

    logger::log_info('Cropping national raster to shapefile extent (if regionalextent is provided).')

    region_raster <- raster::raster(rasterpath)

    # if buffercells is specified, add buffer around state boundary to make sure edge tiles have sufficient overlap
    if (buffercells[1] > 0) {
      region <- sf::st_buffer(region, dist=(res(region_raster)*buffercells[1]))
    }

    region_raster <- raster::crop(region_raster, y=region)
  }

  ######################################################################################################
  ##### Part 3: Split raster into tiles

  logger::log_info('Splitting regional raster into specified number of tiles (n = xdiv * ydiv).')


  raster_tiles <- tryCatch({
    # set up parallel processing cluster (will be used by splitRaster function)
    cl <- parallel::makeCluster(parallel::detectCores()-2)  # use all but two cores

    # split raster into tiles using a regular grid
    raster_tiles <- SpaDES.tools::splitRaster(r=region_raster, nx=div[1], ny=div[2],
                                              buffer=buffercells, cl=cl)

  }, error= function(err){ # if the parallel execution fails, try running with only one thread

    logger::log_info(paste("Split raster w/ single thread. Parallel processing error = ",err))

    # split raster into tiles using a regular grid
    raster_tiles <- SpaDES.tools::splitRaster(r=region_raster, nx=div[1], ny=div[2],
                                              buffer=buffercells)
    return(raster_tiles)
  })

  ######################################################################################################
  ##### Part 5: Handle background tiles that are all NA

  # save lists of which raster tiles are all NA values (cdl == 0 )
  # We don't need to process raster tiles that are completely NA values (background)
  # I use the purrr map function because it nicely applies a function over a list, which is the format returned by splitRaster

  #turn on parallel processing for furrr package
  future::plan(multisession)

  # make list of NA tiles
  todiscard_tiles <- furrr::future_map(.x=raster_tiles, .f = function(x) {
    raster::cellStats(x, stat=max) == NAvalue | raster::cellStats(x, stat=max) == -Inf },
    .options = furrr::furrr_options(seed = TRUE)) %>% unlist()

  ######################################################################################################
  ##### Part 6: Write tiles as individual .tif files

  if (writetiles == T) {
    logger::log_info('Writing output tiles.')

    # set up parallel processing cluster
    cl <- parallel::makeCluster(parallel::detectCores()-2)  # use all cores
    parallel::clusterExport(cl=cl, envir=environment(),
                            varlist=c('raster_tiles', 'tiledir', 'rasterID'))
    doParallel::registerDoParallel(cl)  # register the parallel backend

    # exclude tiles that are all NA values
    foreach::foreach(i= which(!(todiscard_tiles))) %dopar% {

      raster::writeRaster(raster_tiles[[i]], paste0(tiledir, "/", rasterID, "/", rasterID,"_Tile", i, ".tif"), overwrite=T)
    }
  }

  #turn off parallel environments
  parallel::stopCluster(cl); future::plan(sequential)
  logger::log_info('Gridding function complete, returning raster tiles as a list.')

  tile_list <- raster_tiles[which(!(todiscard_tiles))]

  return(tile_list)
}
