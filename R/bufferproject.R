#'Buffer and reproject input points or polygons
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

  land <- raster::raster(rasterpath)
  #check that raster projection is in meters
  proj <- sp::proj4string(land)
  if (grep(proj, pattern="+units=m") != 1) {
    stop("The projection used for your landscape raster must be defined in meters.")
  }

  #import landscape centroid GPS coordinates from points shapefile
  landcenter <- rgdal::readOGR(featurepath)

  #check if points are in same projection as raster layer
  #if different projection, reproject points to same projection as raster layer
  if (sp::proj4string(land) != sp::proj4string(landcenter)) {
    landcenter <- sp::spTransform(landcenter, CRS=sp::proj4string(land))
  }

  polygons <-rgeos::gBuffer(landcenter, byid=T, width=bufferdist)

  return(polygons)
}
