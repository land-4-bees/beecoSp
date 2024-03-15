#'Buffer and re-project input points or polygons
#'
#'Clip landscape raster by polygon or buffered point features
#'@param rasterpath Full path name for landscape raster file
#'@param featurepath Full path name for point shapefile
#'@param bufferdist Radius of desired landscape buffer in meters
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#' bufferproject()

bufferproject <- function(rasterpath, featurepath, bufferdist=NA){

  land <- terra::rast(rasterpath)
  #check that raster projection is in meters
  proj <- sf::st_crs(land)
  if (!any(grepl(proj, pattern='LENGTHUNIT\\["metre"'))) {
    stop("The projection used for your landscape raster must be defined in meters.")
  }

  #import landscape centroid GPS coordinates from points shapefile
  landcenter <- sf::st_read(featurepath)

  #check if points are in same projection as raster layer
  #if different projection, reproject points to same projection as raster layer
  if (sf::st_crs(land) != sf::st_crs(landcenter)) {
    landcenter <- sf::st_transform(landcenter, crs=sf::st_crs(land))
  }

  polygons <- sf::st_buffer(landcenter, dist=bufferdist)

  return(polygons)
}
