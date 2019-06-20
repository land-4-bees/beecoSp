#'Calculate landscape percent land use, or any categorical raster
#'
#'@param landdir Logical specifying if 'landfiles' is a directory
#'@param landfiles Landscape raster files to process (path to directory or list of files)
#'@param writeoutput Logical specifying if results should be written to .csv file
#'@param outfile If writeoutput=T, path to .csv file to store results (including .csv filename)
#'@param attr_path Path to .csv file of raster attribute table
#'@param attr_value Column name in attribute table that specifies raster values
#'@param background Logical, specify background value?
#'@param bgvalues or NoData values to be excluded from total landscape area
#'@keywords bees landscape ecology spatial
#'@import foreach
#'@import dplyr
#'@export
#'@examples
#' Usage example coming soon.


#can specify a folder of landscapes (use landdir=T), or a list of landscape files
#outputs a .csv output file of landscape composition values of all input landscapes

landcomp <- function(landdir=T, landfiles, writeoutput=T, outfile, attr_path=NA, attr_value, background=F, bgvalues=-1000) {

raster::rasterOptions(tmptime=2)


#make list of raw landscape .tif files
if (landdir==T) {
  lands <- list.files(landfiles, pattern = "\\.tif$", full.names=T)
}
if (landdir==F) {lands <- landfiles}

#calculate landscape composition for first landscape
land <- raster::raster(lands[1])

df1 <- data.frame(table(values(land)))
if (length(df1) != 2) { stop('Something is wrong with input raster. Could not tabulate unique values') }


# Go parallel !!!

# Register workers for parallelization
cl <- parallel::makeCluster(parallel::detectCores())
doSNOW::registerDoSNOW(cl)

# Create a progress bar for the parallelization loop
pb <- txtProgressBar(min=1, max=length(lands), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)


#loop over all other landscapes and merge composition files with first one
if (length(lands) > 1) {
  temp <- foreach::foreach(i=1:length(lands), .options.snow=opts, .packages = c('raster', 'rgdal')) %dopar%  {
    land <- raster::raster(lands[i])
    dfn <- data.frame(table(values(land), useNA= 'ifany'))
    if (length(dfn) != 2) { stop('Something is wrong with input raster. Could not tabulate unique values') }
    names(dfn) <- c("VALUE", "Cell_Num")

    if (background == T) {
      dfn <- dfn[!dfn$VALUE %in% bgvalues,]
      #if (length(dfn$VALUE == 0)) { stop (paste0('Landscape', ' land cover contains only background values. Check raster files.'))}
    }
    dfn$Pct_Land <- (dfn$Cell_Num/sum(dfn$Cell_Num))*100
    dfn$Landscape <- gsub(basename(lands[i]), pattern='.tif', replacement="")
    return(dfn)
  }
}

# stop clusters for parallelization
parallel::stopCluster(cl)

#convert data frames in list
all <- plyr::rbind.fill(temp)

if (!is.na(attr_path)) {
  #import NASS attribute table
  NASS_attribute <- read.csv(attr_path)

  #add class names to data frame
  all <- merge(all, NASS_attribute, by.x="VALUE", by.y=attr_value, all.x=T)
  all$VALUE <- all$VALUE[drop=T]
}

if (writeoutput==T) {
  write.csv(all, file=outfile)
}


return(all)
}

