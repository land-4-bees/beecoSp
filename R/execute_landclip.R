#'Execute landscape clip and mask
#'
#'Execute landscape clip and mask
#'@param polygons SpatialPolygonDataFrame of polygons to use to clip raster
#'@param rasterpath Full path name for landscape raster file
#'@param outdir Directory where .tif landscape clips are to be stored, do not include final backslash
#'@param idvar Identifying variable name within feature shapefile to use for naming output rasters
#'@param overrast Logical, should existing rasters with same filename be overwritten?
#'@param parallel execute landscape clips in parallel? (if yes requires set up of parallel environment)
#'@param na_value Numeric, to assign NA values in raster
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#' execute_landclip()
#'
#'
execute_landclip <- function(polygons, rasterpath, idvar, outdir, overrast, na_value) {
  #check that output directory is valid
  if (!file.exists(outdir)){
    #create folder if the directory doesn't exist
    dir.create(outdir)
  }

  land <- raster::raster(rasterpath)

  #check that polygons are in same projection as landscape raster
  #if projections are not the same, project polygon layer
  if (sp::proj4string(land) != sp::proj4string(polygons)) {
    polygons <- sp::spTransform(polygons, CRS=sp::proj4string(land))
  }

  #store list of names of landscapes to be processed
  polygon_ids <- as.list(polygons[[idvar]])


  plyr::ldply(polygon_ids, .fun=clipmask, land=land, polygons=polygons, outdir=outdir, idvar=idvar,
              overrast=overrast, na_value=na_value)

}
