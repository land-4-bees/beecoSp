#'Calculate distance weighted land cover
#'
#'Calculate distance weighted sum of raster classes, assuming middle pixel is focal cell.
#'@param land_raster File path to land cover raster (centered on sampling location)
#'@param forage_range Mean foraging range of bee community of interest (determines shape of distance weighting curve)
#'@details Distance weighting is an exponential decline function with a maximum distance of 'forage_range' *2.
#' See Lonsdorf et al (2009) for details.
#' 'land_raster' must have a radius greater than 'forage_range'*2, but can be much larger (whole county). The raster used for distance weighting is cropped to radius of 'forage_range' times 2.
#'@export
#'@examples
#' Usage example coming soon.

apply_distweight <- function(landdir=T, landfiles, forage_range, attr_path=NA, attr_value) {

  raster::rasterOptions(tmptime=2)

  #make list of raw landscape .tif files
  if (landdir==T) {
    lands <- list.files(landfiles, pattern = "\\.tif$", full.names=T)
  }
  if (landdir==F) {lands <- landfiles}

  # Register workers for parallelization
  cl <- parallel::makeCluster(parallel::detectCores())
  doSNOW::registerDoSNOW(cl)

  # Create a progress bar for the parallelization loop
  pb <- txtProgressBar(min=1, max=length(lands), style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)


  #loop over all other landscapes and merge composition files with first one
  if (length(lands) > 1) {
    temp <- foreach::foreach(i=c(1:length(lands)), .options.snow=opts, .packages = c('raster', 'rgdal', 'beecoSp')) %dopar%  {
      land <- raster::raster(lands[i])
      dwt <- beecoSp::distweight_lulc(land_raster=land, forage_range=forage_range)
      dwt$Landscape <- gsub(basename(lands[i]), pattern='.tif', replacement="")
      return(dwt)
    }

  # stop clusters for parallelization
  parallel::stopCluster(cl)

  #convert data frames in list
  all <- plyr::rbind.fill(temp)

  if (!is.na(attr_path)) {
    #import NASS attribute table
    NASS_attribute <- read.csv(attr_path)

    #add class names to data frame
    all <- merge(all, NASS_attribute, by.x="landcover_class", by.y=attr_value, all.x=T)
    all$VALUE <- all$VALUE[drop=T]
  }

  return(all)
  }
}
