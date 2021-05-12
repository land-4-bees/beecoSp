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

#'@examples
#'example forthcoming.

grid_one_raster <- function(rasterpath, rasterID, regionalextent=NA,
                         div, buffercells=c(0,0),
                         NAvalue, writetiles = T, tiledir...) {

  ######################################################################################################
  ##### Part 1: Setup and load data

  #set up logger to write status updates
  logger::log_threshold(DEBUG)

  # load raster. We will use raster object to re-project state polygons
  temp_raster <- raster::raster(rasterpath)

  # if necessary, download polygon layer of state boundaries
  if (!any(is.na(regionalextent)) & is.character(regionalextent)) {
    logger::log_info('Re-projecting regional shapefile to match raster.')

    # download shapefile of US states
    region <- tigris::states() %>% dplyr::filter(NAME %in% regionalextent) %>%
      sf::st_transform(crs = sf::st_crs(temp_raster)) %>% # re-project polygon layer to match raster1
      terra::vect(

      )
  } else if ('sf' %in% class(regionalextent)) {
    region <- sf::st_transform(regionalextent, crs = sf::st_crs(temp_raster)) %>%
      terra::vect()
  }

  ######################################################################################################
  ##### Part 2: Crop national raster(s) to regional extent

  # read input raster and crop to extent of provided shapefile
  # use the terra package because it is faster than raster.
  if (!is.na(regionalextent)) {

    logger::log_info('Cropping national raster to shapefile extent (if regionalextent is provided).')

    region_raster <- terra::rast(rasterpath) %>%
      terra::crop(y=region) %>%
      raster::raster()   # convert to a raster object so the SpaDES package works
  }

  ######################################################################################################
  ##### Part 3: Split raster into tiles

  logger::log_info('Splitting regional raster into specified number of tiles (n = xdiv * ydiv).')

  # set up parallel processing cluster (will be used by splitRaster function)
  cl <- parallel::makeCluster(parallel::detectCores() - 2)  # use all but 2 cores

  # split raster1 into tiles using a regular grid
  raster_tiles <- SpaDES.tools::splitRaster(r=region_raster, nx=div[1], ny=div[2],
                                         buffer=buffercells, cl=cl)

  ######################################################################################################
  ##### Part 5: Handle background tiles that are all NA

  # save list of raster tiles are all NA values (raster == NA_value)
  # don't process raster tiles that are completely NA values (background)
  # use the 'map' function because it applies a function over a list, which is the format returned by splitRaster

  #turn on parallel processing for furrr package
  future::plan(multisession)

  # make list of NA tiles
  todiscard_tiles <- furrr::future_map(.x=raster_tiles, .f = function(x) {
    raster::cellStats(x, stat=max) == NAvalue },
    .options = furrr::furrr_options(seed = TRUE)) %>% unlist()

  ######################################################################################################
  ##### Part 6: Write tiles as individual .tif files

  if (writetiles == T) {
    logger::log_info('Writing output tiles.')

    # create directory for output files if it doesn't already exist
    if (!dir.exists(tiledir)) {
      dir.create(tiledir)
    }

    #create folder for tile output if it doesn't already exist
    if (!dir.exists(paste0(tiledir, "/", rasterID))) {
      dir.create(paste0(tiledir, "/", rasterID))
    }

    # set up parallel processing cluster
    cl <- parallel::makeCluster(parallel::detectCores() - 2)  # use all but 2 cores
    parallel::clusterExport(cl=cl, envir=environment(), varlist=c('region_raster'))
    doParallel::registerDoParallel(cl)  # register the parallel backend

    # exclude tiles that are all NA values for BOTH layers
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
