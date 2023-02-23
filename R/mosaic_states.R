#'Mosaic together state rasters to form a national raster
#'
#'@param statedir file path to directory that contains state rasters in .tif format
#'@param tier numeric indicating round of hierarchical processing (usually 1-3 rounds)
#'@param ID variable to identify groups of rasters to process together (e.g. CDLYear). Will be included in file name for output raster.
#'@param outdir file path to directory where national rasters should be saved
#'@param season if applicable, vector of seasons to process. E.g. spring, summer, fall
#'@param verbose logical, print many status messages
#'@details Due to large files sizes, output rasters from this function are always compressed. This is different from 'mosaic_tiles' function.
#'@export
mosaic_states <- function(statedir, tier, ID, outdir, season=NA, usepackage='gdal') {

  library(terra)

  # save file name for final, national raster
  if (!is.na(season)) {
    national_filename <- paste0(outdir, '/', ID, "_", season, "_NationalRaster.tif")
  } else if (is.na(season)) {
    national_filename <- paste0(outdir, '/', ID,"_", "NationalRaster.tif")
  }

  if (1 %in% tier) {
    # make list of files in statedir
    state_paths <- list.files(statedir, full.names=T)
    logger::log_info('Tier 1: Identified ', length(state_paths), ' raster files before filtering.')

    # exclude any extra files
    state_paths <- state_paths[!grepl(state_paths, pattern= ".tif.aux")]
    state_paths <- state_paths[!grepl(state_paths, pattern= "MegaTile")]
    state_paths <- state_paths[!grepl(state_paths, pattern= "NationalRaster")]

    # filter to correct season
    if (!is.na(season)) {
      state_paths <- state_paths[grepl(state_paths, pattern=season)]
    }

    # filter to correct year of CDL (or other ID variable, as necessary)
    # this ID variable will also be included in filename of final output raster
    if (!is.na(ID)) {
      state_paths <- state_paths[grepl(state_paths, pattern=ID)]
    }

    logger::log_info('Tier 1: Trying to load ', length(state_paths), ' raster files after filtering.')


    # if a CDL year has only one tile, write single tile as final raster
    if (length(state_paths) == 1) {
      stop('There is only one state raster for the year specified. Check input files.')
    }


      # sort tile list so mega tiles will be adjacent (based on distance between tile centroids)
      state_list <- vector("list", length(state_paths))

      # load tile rasers into R list
      for (i in 1:length(state_paths)) {
        state_list[[i]] <- terra::rast(state_paths[i])
      }

      # assign tiles to clusters based on lat/long
      clusters <- beecoSp::calc_state_clusters(state_list=state_list, tier=1, plot_clusters=F, mult=0.6)
      ngroups <- length(unique(clusters))

      logger::log_info('Tier 1: starting mosaic-ing state rasters using ', ngroups, " clusters.")

      ##### create mega tiles by executing mosaic respecting cluster membership
      for (i in 1:ngroups) {

        # save file name for mega-tile
        if (!is.na(season)) {
          megatile_filename_t1 <- paste0(statedir, '/', ID, "_", season, "_NationalMegaTile", i, '_Tier1.tif')
        } else if (is.na(season)) {
          megatile_filename_t1 <- paste0(statedir, '/', ID,"_NationalMegaTile", i, '_Tier1.tif')
        }

        if (usepackage == 'terra') {

          assign(x=paste0('args', i), value=state_list[clusters == i])

          # execute mosaic to create a mega-tile
          base::eval(rlang::call2("mosaic", !!!get(paste0('args', i)), .ns="terra", fun='mean',
                                                                   filename=megatile_filename_t1,
                                                                   overwrite=T))
        } else if (usepackage == 'gdal') {

          gdalUtils::mosaic_rasters(gdalfile=state_paths[clusters == i],
                                  dst_dataset=megatile_filename_t1,
                                  overwrite=T)

        }

        logger::log_info(paste0('Tier 1: Mega tile ', i, " is finished."))
      }

      logger::log_info('Tier 1: Finished creating mega tiles.')

  }

  if (2 %in% tier) {

    ######## Tier 2

    mega_paths <- list.files(statedir, full.names=T)
    logger::log_info('Tier 2: Identified ', length(mega_paths), ' raster files before filtering.')


    # exclude any extra files
    mega_paths <- mega_paths[!grepl(mega_paths, pattern= ".tif.aux")]
    mega_paths <- mega_paths[!grepl(mega_paths, pattern= "MegaMega")]
    mega_paths <- mega_paths[!grepl(mega_paths, pattern= "NationalRaster")]
    if (1 %in% tier) {
      mega_paths <- mega_paths[grepl(mega_paths, pattern= "MegaTile")]

      mega_paths <- mega_paths[grepl(mega_paths, pattern= paste0("_Tier1.tif"))|
                               grepl(mega_paths, pattern= paste0("_Tier1.tif"))]
    }#filter to mega-tiles that were created in previous round

    if (!is.na(ID)) {
      mega_paths <- mega_paths[grepl(mega_paths, pattern=ID)]
    }

    # filter to correct season
    if (!is.na(season)) {
      mega_paths <- mega_paths[grepl(mega_paths, pattern=season)]
    }

    logger::log_info('Tier 2: Trying to load ', length(mega_paths), ' raster files after filtering.')

    # load mega-tiles into list to mosaic
    mega_list <- vector("list", length(mega_paths))

    for (i in 1:length(mega_paths)) {
      mega_list[[i]] <- terra::rast(mega_paths[i])
    }

    # if a state has only one mega-tile, write single mega-tile as final raster
    if (length(mega_paths) == 1) {
      logger::log_info('This region only has one mega-tile. Writing this raster as final output.')

      # copy final raster to folder with cleaner file names
      file.copy(from=mega_paths[[1]], to=national_filename)

      # remove tier three to skip the next section of code
      tier <- gsub(tier, pattern='3', replacement="")

    } else {

      # assign tiles to clusters based on lat/long
      clusters2 <- beecoSp::calc_state_clusters(state_list=mega_list, tier=2, plot_clusters=F, mult=0.8)
      ngroups2 <- length(unique(clusters2))

      logger::log_info('Tier 2: starting mosaic-ing mega tiles using ', ngroups2, " clusters.")

      ##### create mega tiles by executing mosaic respecting cluster membership
      for (i in 1:ngroups2) {

        # save file name for mega-tile
        if (!is.na(season)) {
          megatile_filename_t2 <- paste0(statedir, '/', ID, "_", season, "_NationalMegaTile", i, '_Tier2.tif')
        } else if (is.na(season)) {
          megatile_filename_t2 <- paste0(statedir, '/', ID,"_NationalMegaTile", i, '_Tier2.tif')
        }

        if (usepackage == 'terra') {

          assign(x=paste0('args2', i), value=mega_list[clusters2 == i])

          base::eval(rlang::call2("mosaic", !!!get(paste0('args2', i)), .ns="terra", fun='mean',
                                  filename=megatile_filename_t2,
                                  overwrite=T))

        } else if (usepackage == 'gdal') {

          gdalUtils::mosaic_rasters(gdalfile=mega_paths[clusters2 == i],
                                dst_dataset=megatile_filename_t2,
                                overwrite=T)
        }

        logger::log_info(paste0('Tier 2: Mega tile ', i, " is finished."))
        rm(list=ls(pattern='args2'))
      }
    }
    logger::log_info('Tier 2: Finished creating mega tiles.')
  }

  if (3 %in% tier) {
    ######## Tier 3

    mega_paths2 <- list.files(statedir, full.names=T)
    logger::log_info('Tier 3: Identified ', length(mega_paths2), ' raster files before filtering.')

    # exclude any extra files
    mega_paths2 <- mega_paths2[!grepl(mega_paths2, pattern= ".tif.aux")]
    mega_paths2 <- mega_paths2[!grepl(mega_paths2, pattern= "MegaMega")]

    if (2 %in% tier) {
      mega_paths2 <- mega_paths2[grepl(mega_paths2, pattern= "MegaTile")]
      mega_paths2 <- mega_paths2[grepl(mega_paths2, pattern= paste0("_Tier2.tif"))|
                                 grepl(mega_paths2, pattern= paste0("_Tier2_gdal.tif"))]
      } #filter to mega-tiles that were created in previous round

    if (!is.na(ID)) {
      mega_paths2 <- mega_paths2[grepl(mega_paths2, pattern=ID)]
    }

    if (!is.na(season)) {
      mega_paths2 <- mega_paths2[grepl(mega_paths2, pattern=season)]
    }

    logger::log_info('Tier 3: Trying to load ', length(mega_paths2), ' raster files after filtering.')

    # load mega-tiles into list to mosaic
    mega_list2 <- vector("list", length(mega_paths2))

    for (i in 1:length(mega_paths2)) {
      mega_list2[[i]] <- terra::rast(mega_paths2[i])
    }

    # if a state has only one mega-tile, write single mega-tile as final raster
    if (length(mega_paths2) == 1) {
      logger::log_info('This region only has one mega-tile. Writing this raster as final output.')

      # copy final raster to folder with cleaner file names
      file.copy(from=mega_paths2[[1]], to=national_filename)

    } else {
      # assign tiles to clusters based on lat/long
      clusters3 <- beecoSp::calc_state_clusters(state_list=mega_list2, tier=3, plot_clusters=F, mult=10)
      ngroups3 <- length(unique(clusters3))

      logger::log_info('Tier 3: starting mosaic-ing mega tiles using ', ngroups3, ' clusters.')

      ##### create mega tiles by executing mosaic respecting cluster membership
      for (i in 1:ngroups3) {

        # save file name for mega-tile
        if (!is.na(season)) {
          megatile_filename_t3 <- paste0(statedir, '/', ID, "_", season, "_NationalMegaTile", i, '_Tier3.tif')
        } else if (is.na(season)) {
          megatile_filename_t3 <- paste0(statedir, '/', ID,"_NationalMegaTile", i, '_Tier3.tif')
        }

        if (usepackage == 'terra') {

          assign(x=paste0('args3', i), value=mega_list2[clusters3 == i])

          base::eval(rlang::call2("mosaic", !!!get(paste0('args3', i)), .ns="terra", fun='mean',
                                  filename=megatile_filename_t3,
                                  overwrite=T))

        } else if (usepackage == 'gdal') {

          gdalUtils::mosaic_rasters(gdalfile=mega_paths2[clusters3 == i],
                                    dst_dataset=megatile_filename_t3,
                                    overwrite=T,
                                    co=c("COMPRESS=DEFLATE", "BIGTIFF=YES"))

        }
        rm(list=ls(pattern='args3'))

      }

      if (ngroups3 == 1) {
        # copy final raster to one folder with cleaner file names
        file.remove(national_filename)
        file.copy(from=megatile_filename_t3, to=national_filename, overwrite=T)
      }
    }
    logger::log_info('Tier 3: Finished national raster.')
  }
  logger::log_info(paste0("Make final: Final raster exists? ", file.exists(national_filename)))

  # # if final raster exists, remove megatiles
  # if (file.exists(national_filename)) {
  #   state_paths <- list.files(statedir, full.names=T)
  #   megatile_paths <- state_paths[grepl(state_paths, pattern= "MegaTile")]
  #
  #   file.remove(megatile_paths)
  # }

}
