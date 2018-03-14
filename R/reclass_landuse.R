#'Reclassify land use raster
#'
#'Reclassify land use (e.g. Crop Data Layer) raster by supplied matrix with option to calculate mean of new raster
#'@param rasterpath file path of land use raster file
#'@param reclasstable R data frame of reclass table
#'@param from column in reclass table with land use raster values (e.g. CDL values from 1 to 254)
#'@param to column in reclass table with new values (e.g. pesticide index of each land use)
#'@param writerast logical, should write file of reclassified raster?
#'@param outpath if writerast is TRUE, path name where reclass raster should be written
#'@param meanreclass logical, return mean value of reclassified raster
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#'CDL_reclass' example forthcoming.


CDL_reclass <- function(rasterpath, reclasstable, from, to, writerast=F, outpath, meanreclass) {
  cdlrast <- raster::raster(rasterpath)
  reclassmatrix <- matrix(reclasstable[,names(reclasstable) == from])
  reclassmatrix <- cbind(reclassmatrix, reclasstable[,names(reclasstable) == to])
  colnames(reclassmatrix) <- c('is', 'becomes')
  
  reclassrast <- raster::reclassify(cdlrast, rcl=reclassmatrix) 
  
  if (writerast == T) {
    outputrast <- paste(gsub(basename(rasterpath), pattern=".tif", replacement=""), to, sep=".")
    #create output directory if it doesn't already exist
    if (!dir.exists(outpath)) {
      dir.create(outpath)
    }
    raster::writeRaster(reclassrast, paste(outpath,"/", outputrast, ".tif", sep=""), overwrite=T)
  }
  if (meanreclass == T) {
    df <- data.frame(outputrast, raster::cellStats(reclassrast, stat="mean", na.rm=T))
    names(df) <- c('RastID', 'MeanValue')
    return(df)
  }
}