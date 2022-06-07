#'Mosaic together state rasters to form a national raster
#'
#'@param statedir file path to directory that contains state rasters in .tif format
#'@param tier numeric indicating round of hierarchical processing (usually 1-3 rounds)
#'@param ID variable to identify groups of rasters to process together (e.g. CDLYear). Will be included in file name for output raster.
#'@param outdir file path to directory where natinoal rasters should be saved
#'@param season if applicable, vector of seasons to process. E.g. spring, summer, fall
#'@param verbose logical, print many status messages
#'@details Due to large files sizes, output rasters from this function are always compressed. This is different from 'mosaic_tiles' function.
#'@export
mosaic_states <- function(statedir, tier, ID, outdir, season=NA, usepackage='gdal') {

  library(terra)
  source('./code/functions/calc_state_clusters.R')
  
  # save some strings to use later
  compress_filename <- paste0(outdir, '/', ID, '_NationalRasterCompress.tif')
  rawsize_filename <- paste0(outdir, '/', ID, '_NationalRaster.tif')
  
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
      tile_paths <- tile_paths[grepl(tile_paths, pattern=season)]
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
      clusters <- calc_state_clusters(state_list=state_list, tier=1, plot_clusters=T, mult=0.6)
      ngroups <- length(unique(clusters))
      
      logger::log_info('Tier 1: starting mosaic-ing state rasters using ', ngroups, " clusters.")
      
      ##### create mega tiles by executing mosaic respecting cluster membership
      for (i in 1:ngroups) {
        
        if (usepackage == 'terra') {
          
          assign(x=paste0('args', i), value=state_list[clusters == i]) 
          # execute mosaic to create a mega-tile
          base::eval(rlang::call2("mosaic", !!!get(paste0('args', i)), .ns="terra", fun='mean',
                                                                   filename=paste0(statedir, '/', ID,"_NationalMegaTile", i, '_Tier1.tif'),
                                                                   overwrite=T))
        } else if (usepackage == 'gdal') {
          gdalUtils::mosaic_rasters(gdalfile=state_paths[clusters == i], 
                                  dst_dataset=paste0(statedir, '/', ID,"_NationalMegaTile", i, '_Tier1.tif'),
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
    mega_paths <- mega_paths[grepl(mega_paths, pattern= "MegaTile")]
    mega_paths <- mega_paths[!grepl(mega_paths, pattern= "MegaMega")]
    mega_paths <- mega_paths[!grepl(mega_paths, pattern= "NationalRaster")]
    mega_paths <- mega_paths[grepl(mega_paths, pattern= paste0("_Tier1.tif"))|
                               grepl(mega_paths, pattern= paste0("_Tier1.tif"))] #filter to mega-tiles that were created in previous round
    
    if (!is.na(ID)) {
      mega_paths <- mega_paths[grepl(mega_paths, pattern=ID)]
    }
    
    logger::log_info('Tier 2: Trying to load ', length(mega_paths), ' raster files after filtering.')
    
    # load mega-tiles into list to mosaic
    mega_list <- vector("list", length(mega_paths))
    
    for (i in 1:length(mega_paths)) {
      mega_list[[i]] <- terra::rast(mega_paths[i])
    }
    
    # assign tiles to clusters based on lat/long
    clusters2 <- calc_state_clusters(state_list=mega_list, tier=2, plot_clusters=F, mult=0.8)
    ngroups2 <- length(unique(clusters2))
    
    logger::log_info('Tier 2: starting mosaic-ing mega tiles using ', ngroups2, " clusters.")
    
    ##### create mega tiles by executing mosaic respecting cluster membership
    for (i in 1:ngroups2) {
      
      if (usepackage == 'terra') {
        
        assign(x=paste0('args2', i), value=mega_list[clusters2 == i]) 
        base::eval(rlang::call2("mosaic", !!!get(paste0('args2', i)), .ns="terra", fun='mean',
                                filename=paste0(statedir, '/', ID,"_NationalMegaTile", i, '_Tier2.tif'),
                                overwrite=T))

      } else if (usepackage == 'gdal') {

        gdalUtils::mosaic_rasters(gdalfile=mega_paths[clusters2 == i], 
                              dst_dataset=paste0(statedir, '/', ID,"_NationalMegaTile", i, '_Tier2.tif'),
                              overwrite=T)
      }
      
      logger::log_info(paste0('Tier 2: Mega tile ', i, " is finished."))
      rm(list=ls(pattern='args2'))
    }
    
    logger::log_info('Tier 2: Finished creating mega tiles.')
    
  }
  
  if (3 %in% tier) {
    ######## Tier 3
    
    mega_paths2 <- list.files(statedir, full.names=T)
    logger::log_info('Tier 3: Identified ', length(mega_paths2), ' raster files before filtering.')
    
    
    # exclude any extra files
    mega_paths2 <- mega_paths2[!grepl(mega_paths2, pattern= ".tif.aux")]
    mega_paths2 <- mega_paths2[grepl(mega_paths2, pattern= "MegaTile")]
    mega_paths2 <- mega_paths2[!grepl(mega_paths2, pattern= "MegaMega")]
    mega_paths2 <- mega_paths2[grepl(mega_paths2, pattern= paste0("_Tier2.tif"))|
                                 grepl(mega_paths2, pattern= paste0("_Tier2_gdal.tif"))] #filter to mega-tiles that were created in previous round
    
    if (!is.na(ID)) {
      mega_paths2 <- mega_paths2[grepl(mega_paths2, pattern=ID)]
    }
    
    logger::log_info('Tier 3: Trying to load ', length(mega_paths2), ' raster files after filtering.')
    
    # load mega-tiles into list to mosaic
    mega_list2 <- vector("list", length(mega_paths2))
    
    for (i in 1:length(mega_paths2)) {
      mega_list2[[i]] <- terra::rast(mega_paths2[i])
    }
    
    # assign tiles to clusters based on lat/long
    clusters3 <- calc_state_clusters(state_list=mega_list2, tier=3, plot_clusters=F, mult=10)
    ngroups3 <- length(unique(clusters3))
    
    logger::log_info('Tier 3: starting mosaic-ing mega tiles using ', ngroups3, ' clusters.')
    
    ##### create mega tiles by executing mosaic respecting cluster membership
    for (i in 1:ngroups3) {
      
      if (usepackage == 'terra') {
        
        assign(x=paste0('args3', i), value=mega_list2[clusters3 == i]) 
        base::eval(rlang::call2("mosaic", !!!get(paste0('args3', i)), .ns="terra", fun='mean',
                                filename=paste0(statedir, '/', ID,"_NationalMegaTile", i, '_Tier3.tif'),
                                overwrite=T))

      } else if (usepackage == 'gdal') {

        gdalUtils::mosaic_rasters(gdalfile=mega_paths2[clusters3 == i], 
                                  dst_dataset=paste0(statedir, '/', ID,"_NationalRaster_Tier3.tif"),
                                  overwrite=T, 
                                  co=c("COMPRESS=DEFLATE", "BIGTIFF=YES"))
        
      }
      rm(list=ls(pattern='args3'))
      
    }
    
    logger::log_info('Tier 3: Finished national raster.')
  }
}