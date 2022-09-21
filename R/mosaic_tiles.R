#'Mosaic together raster tiles to form state-level rasters
#'
#'@param tiledir file path to directory that contains raster tiles in .tif format
#'@param chunksize1 numeric indicating how many tiles should be in each cluster
#'@param ID variable to identify groups of rasters to process together (e.g. CDLYear). Will be included in file name for output raster.
#'@param outdir file path to directory where state rasters should be saved
#'@param season if applicable, vector of seasons to process. E.g. spring, summer, fall
#'@param compress logical, should state rasters be compressed?
#'@param verbose logical, print many status messages
#'@export
mosaic_tiles <- function(tiledir, chunksize1, ID, outdir, season=NA, compress=T, verbose=F) {

  library(terra)

  # save some strings to use later
  if (is.na(season)) {
    compress_filename <- paste0(outdir, '/', ID, '_FinalRasterCompress.tif')
    rawsize_filename <- paste0(outdir, '/', ID, '_FinalRaster.tif')

  } else {
    compress_filename <- paste0(outdir, '/', ID, '_', season, '_FinalRasterCompress.tif')
    rawsize_filename <- paste0(outdir, '/', ID, '_', season, '_FinalRaster.tif')
  }
  # make list of files in tiledir
  tile_paths <- list.files(tiledir, full.names=T)
  logger::log_info('Make mega: Identified ', length(tile_paths), ' raster files before filtering.')

  # exclude any extra files
  tile_paths <- tile_paths[!grepl(tile_paths, pattern= ".tif.aux")]
  tile_paths <- tile_paths[!grepl(tile_paths, pattern= "MegaTile")]
  tile_paths <- tile_paths[!grepl(tile_paths, pattern= "Final")]

  # filter to correct season
  if (!is.na(season)) {
    tile_paths <- tile_paths[grepl(tile_paths, pattern=season)]
  }

  # filter to correct year of CDL (or other ID variable, as necessary)
  # this ID variable will also be included in filename of final output raster
  if (!is.na(ID)) {
    tile_paths <- tile_paths[grepl(tile_paths, pattern=ID)]
  }

  logger::log_info('Make mega: Trying to load ', length(tile_paths), ' raster files after filtering.')


  # if a state has only one tile, write single tile as final raster
  if (length(tile_paths) == 1) {
    onetile <- terra::rast(tile_paths[[1]])

    if (compress == T) {
      terra::writeRaster(onetile, filename=compress_filename, overwrite=T,
                       wopt= list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3")))
    } else {
      terra::writeRaster(onetile, filename=rawsize_filename, overwrite=T)
    }
  }

  # if a state has multiple tiles, execute hierarchical mosaic to stitch all tiles into a single raster
  if (length(tile_paths) > 1) {

    # sort tile list so mega tiles will be adjacent (based on distance between tile centroids)
    tile_list <- vector("list", length(tile_paths))

    # load tile rasers into R list
    for (i in 1:length(tile_paths)) {
      tile_list[[i]] <- terra::rast(tile_paths[i])
    }

    end <- length(tile_list)

    # assign tiles to clusters based on lat/long
    clusters <- beecoSp::calc_tile_clusters(tile_list=tile_list, chunksize=chunksize1, plot_clusters=F)
    ngroups <- length(unique(clusters))

    logger::log_info('Make mega: starting mosaic-ing.')

    ##### create mega tiles by executing mosaic respecting cluster membership
    for (i in 1:ngroups) {
      assign(x=paste0('args', i), value=tile_list[clusters == i])

      # save file name for mega-tile
      if (!is.na(season)) {
        megatile_filename <- paste0(tiledir, '/', ID, '_', season, '_MegaTile', i, "_", chunksize1, '.tif')
      } else if (is.na(season)) {
        megatile_filename <- paste0(tiledir, '/', ID, '_MegaTile', i, '_', chunksize1, '.tif')
      }
      # execute mosaic to create a mega-tile
      assign(x=paste0('MT', i), value= base::eval(rlang::call2("mosaic", !!!get(paste0('args', i)), .ns="terra", fun='mean',
                                                               filename=megatile_filename,
                                                               overwrite=T)))
      if (verbose == T) {
        logger::log_info(paste0('Mega tile ', i, " is finished."))
      }
    }

    logger::log_info('Make mega: Finished creating mega tiles.')


    # remove some large objects from memory
    rm(tile_list); rm(tile_paths)
    rm(list=ls(pattern="args"))

    ######## Mega-tiles to final raster!

    mega_paths <- list.files(tiledir, full.names=T)
    logger::log_info('Make final: Identified ', length(mega_paths), ' raster files before filtering.')


    # exclude any extra files
    mega_paths <- mega_paths[!grepl(mega_paths, pattern= ".tif.aux")]
    mega_paths <- mega_paths[grepl(mega_paths, pattern= "MegaTile")]
    mega_paths <- mega_paths[!grepl(mega_paths, pattern= "MegaMega")]
    mega_paths <- mega_paths[grepl(mega_paths, pattern= paste0("_", chunksize1, ".tif"))] #filter to mega-tiles that were created with correct chunksize

    if (!is.na(ID)) {
      mega_paths <- mega_paths[grepl(mega_paths, pattern=ID)]
    }

    # filter to correct season
    if (!is.na(season)) {
      mega_paths <- mega_paths[grepl(mega_paths, pattern=season)]    
    }

    logger::log_info('Make final: Trying to load ', length(mega_paths), ' raster files after filtering.')

    # load mega-tiles into list to mosaic
    mega_list <- vector("list", length(mega_paths))

    for (i in 1:length(mega_paths)) {
      mega_list[[i]] <- terra::rast(mega_paths[i])
    }
    rsrc <- terra::src(mega_list) # convert list of rasters to terra SpatRaster Collection

    logger::log_info('Loaded ', length(mega_list), ' raster files.')

    a <- Sys.time()

    # if a state has only one mega-tile, write single mega-tile as final raster
    if (length(mega_paths) == 1) {
      logger::log_info('This state only has one mega-tile. Writing this raster as final output.')
      onetile <- terra::rast(mega_paths[[1]])

      if (compress == T) {
        terra::writeRaster(onetile, filename=compress_filename, overwrite=T,
                           wopt= list(gdal=c("COMPRESS=DEFLATE", "PREDICTOR=3")))
      } else {
        terra::writeRaster(onetile, filename=rawsize_filename, overwrite=T)
      }
    } else if (length(mega_paths) > 1) {

      logger::log_info('Make final: Attempting mosaic.')

      if (compress == T) {
        file1 <- compress_filename

        gdalUtils::mosaic_rasters(gdalfile=mega_paths,
                                  dst_dataset=compress_filename,
                                  overwrite=T,
                                  ot='Int16',
                                  co=c("COMPRESS=DEFLATE", "BIGTIFF=YES"))

      } else if (compress == F) {
        file1 <- rawsize_filename
        base::eval(rlang::call2("mosaic", rsrc, .ns="terra", fun='mean',
                                filename=rawsize_filename, overwrite=T))
      }

      b <- Sys.time() # save end time
      logger::log_info(paste0("Make final: Final raster exists? ", file.exists(file1)))
      logger::log_info(paste0("Make final: ", difftime(b,a, units="mins"), ' minutes  to execute mosaic.'))
    }

  }
}
