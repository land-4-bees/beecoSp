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
execute_landclip <- function(polygons, rasterpath, idvar, outdir, overrast, na_value=NA, datatype) {
  #check that output directory is valid
  if (!dir.exists(outdir)){
    #create folder if the directory doesn't exist
    dir.create(outdir)
  }

  land <- terra::rast(rasterpath)

  #check that polygons are in same projection as landscape raster
  #if projections are not the same, project polygon layer
  if (sf::st_crs(land) != sf::st_crs(polygons)) {
    polygons <- sf::st_transform(polygons, crs=sf::st_crs(land))
  }

  #store list of names of landscapes to be processed
  polygon_ids <- as.list(polygons[[idvar]])
  plyr::ldply(polygon_ids, .fun=clipmask, land=land, polygons=polygons, outdir=outdir, idvar=idvar,
              overrast=overrast, na_value=na_value, datatype=datatype)

}
