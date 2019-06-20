

apply_distweight <- function(landdir=T, landfiles, forage_range, attr_path, attr_value) {

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
    temp <- foreach::foreach(i=1:length(lands), .options.snow=opts, .export=c('forage_range'),
                             .packages = c('raster', 'rgdal')) %dopar%  {
      land <- raster::raster(lands[i])
      dwt <- distweight_lulc(land_raster=land, forage_range=forage_range)
      return(dwt)
    }

  # stop clusters for parallelization
  parallel::stopCluster(cl)

  #convert data frames in list
  all <- plyr::rbind.fill(temp)

  #import NASS attribute table
  NASS_attribute <- read.csv(attr_path)

  #add class names to data frame
  all <- merge(all, NASS_attribute, by.x="VALUE", by.y=attr_value, all.x=T)
  all$VALUE <- all$VALUE[drop=T]



}
