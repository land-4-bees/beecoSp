# function to split CDL and LANDFIRE regional or national rasters into gridded tiles

grid_rasters <- function(rpath, rasterID, tile_dir,
                        regionalextent=NA, 
                        div, buffercells=c(0,0),
                        NAvalues, 
                        writetiles = T) {
  
  ######################################################################################################
  ##### Part 1: Setup and load data
  
  # lOad libraries
  library(future); library(foreach)
  
  #separate file paths to CDL and vegetation rasters. 
  cdl_path <- rpath[1]
  veg_path <- rpath[2]

  #set up logger to write status updates
  library(logger)
  logger::log_threshold(DEBUG)
  
  # load raster1
  # We will use raster object to re-project state polygons
  cdl <- raster::raster(cdl_path)
  
  # if necessary, download polygon layer of state boundaries
  if (!any(is.na(regionalextent)) & is.character(regionalextent)) {
    logger::log_info('Re-projecting regional shapefile to match CDL raster.')
    
    # download shapefile of US states
    region <- tigris::states() %>% dplyr::filter(NAME %in% regionalextent) %>%
      sf::st_transform(crs = sf::st_crs(cdl)) # re-project polygon layer to match raster1
    
  } else if ('sf' %in% class(regionalextent)) {
    region <- sf::st_transform(regionalextent, crs = sf::st_crs(cdl))
  }
  
  ######################################################################################################
  ##### Part 2: Crop national rasters to regional extent
  
  # create directory for output files if it doesn't already exist
  if (!dir.exists(tile_dir)) {
    dir.create(tile_dir)
  }
  
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
  
  logger::log_info('Splitting regional rasters into specified number of tiles (n = xdiv * ydiv).')
  
  
  # set up parallel processing cluster (will be used by splitRaster function)
  cl <- parallel::makeCluster(parallel::detectCores() - 2)  # use all but 2 cores
  
  tictoc::tic()
  
  # split raster1 into tiles using a regular grid
  cdl_tiles <- SpaDES.tools::splitRaster(r=region_cdl, nx=div[1], ny=div[2], 
                                         buffer=buffercells, cl=cl)
  
  ######################################################################################################
  ##### Part 4: If raster2 file path is provided, split raster2 into tiles
  
  nvc_tiles <- SpaDES.tools::splitRaster(r=region_nvc, nx=div[1], ny=div[2], 
                                         buffer=buffercells, cl=cl)

  ######################################################################################################
  ##### Part 5: Handle background tiles that are all NA

  # save lists of which raster tiles are all NA values (cdl == 0 & nvc == -9999)
  # We don't need to process raster tiles that are completely NA values (background)
  # I use the purrr map function because it nicely applies a function over a list, which is the format returned by splitRaster

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
    if (!dir.exists(paste0(tile_dir, "/MismatchedBorderTiles/"))) {
      dir.create(paste0(tile_dir, "/MismatchedBorderTiles/"))
    }
    
    # write mis-matched rasters as .tif files
    for (i in which(todiscard_cdl != todiscard_nvc)) {
      
      raster::writeRaster(cdl_tiles[[i]], paste0(tile_dir, "/MismatchedBorderTiles/", rasterID[1],
                                                   "Tile", i, ".tif"), overwrite=T)
      if (!is.na(veg_path)) {
        raster::writeRaster(nvc_tiles[[i]], paste0(tile_dir, "/MismatchedBorderTiles/", rasterID[2], 
                                                   "Tile", i, ".tif"), overwrite=T)
      }
    }
  }
    
  # make list object of raster tiles to return (non-NA values in one or more raster layers)
  tile_list <- furrr::future_map2(.x= purrr::discard(cdl_tiles, todiscard_nvc&todiscard_cdl), 
                               .y= purrr::discard(nvc_tiles, todiscard_nvc&todiscard_cdl), .f=c)
  
  ######################################################################################################
  ##### Part 5: Write tiles as individual .tif files

  if (writetiles == T) {
    logger::log_info('Writing output tiles.')
    
    # set up parallel processing cluster
    cl <- parallel::makeCluster(parallel::detectCores() - 2)  # use all but 2 cores
    parallel::clusterExport(cl=cl, envir=environment(), varlist=c('region_cdl', 'region_nvc'))
    doParallel::registerDoParallel(cl)  # register the parallel backend

    
    # exclude tiles that are all NA values for BOTH layers
    foreach::foreach(i= which(!(todiscard_nvc&todiscard_cdl))) %dopar% {
      
      #create CDL and NVC tile folders if they don't already exist
      if (!dir.exists(paste0(tile_dir, "/", rasterID[1]))) {
        dir.create(paste0(tile_dir, "/", rasterID[1]))
        dir.create(paste0(tile_dir, "/", rasterID[2]))
      }
      
      raster::writeRaster(cdl_tiles[[i]], paste0(tile_dir, "/", rasterID[1], "/", rasterID[1], "_Tile", i, ".tif"), overwrite=T)
      raster::writeRaster(nvc_tiles[[i]], paste0(tile_dir, "/", rasterID[2], "/", rasterID[2], "_Tile", i, ".tif"), overwrite=T)
    }
  }
  
  #turn off parallel environments
  parallel::stopCluster(cl); future::plan(sequential)
  logger::log_info('Gridding function complete, returning pairs of raster tiles as a list.')

  return(tile_list)
}