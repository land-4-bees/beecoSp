#'Clip landscape raster by polygon or buffered point
#'
#'Internal function that clip landscapes within 'execute_landclip', not to be called directly
#'@param polygonID id of specific polygon used for clip and mask
#'@param land landscape raster, passed from 'execute_landclip'
#'@param polygons SpatialPolygonDataFrame of polygons to clip, passed from 'execute_landclip'
#'@param idvar Identifying variable name within feature shapefile, passed from 'execute_landclip'
#'@param outdir Directory where .tif landscape clips are to be stored, passed from 'execute_landclip'
#'@param overrast Logical, should existing rasters with same filename be overwritten?
#'@param na_value Numeric, to assign NA values in raster
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#'see 'execute_landclip' for usage example.

#clip and mask landscape raster from polygon, export as .tif file
clipmask <- function(land, polygonID, polygons, outdir, idvar, overrast, na_value=NA) {
  #subset 'polygons' layer to just polygon that matches ID of specific row
  onepoly <- polygons[polygons[[idvar]] == polygonID,]
  clip <- raster::crop(land, onepoly)
  mask <- raster::mask(clip, onepoly)

  if (!is.na(na_value)) {
    raster::NAvalue(mask) <- na_value
  }
  #store id variable for specific landscape raster
  nameraster <- as.character(onepoly[[idvar]])

  #add id variable to output directory file path
  rasterpath <- paste(outdir, nameraster, sep="/")

  #write raster file as .tif to output directory
  if (!is.na(na_value)) {
    raster::writeRaster(mask, filename=rasterpath, format='GTiff', overwrite=overrast, NAflag=na_value)
  } else {
    raster::writeRaster(mask, filename=rasterpath, format='GTiff', overwrite=overrast)
  }
  return(data.frame(LandID=nameraster, WriteComplete=T))
}





