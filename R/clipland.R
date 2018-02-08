#'Clip landscape raster by polygon or buffered point
#'
#'Clip landscape raster by polygon or buffered point features
#'@param polygonID id of specific polygon used for clip and mask
#'@param land landscape raster, passed from 'clipland' function
#'@param polygons SpatialPolygonDataFrame of polygons to clip, passed from'clipland' function
#'@param idvar Variable name within 'polygons' that defines id variable, passed from 'clipland' function
#'@param outdir Directory where .tif landscape clips are to be stored, passed from 'clipland'
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#' clipmask()

#clip and mask landscape raster from polygon, export as .tif file
clipmask <- function(land, polygonID, polygons, outdir, idvar){
  #subset 'polygons' layer to just polygon that matches ID of specific row
  onepoly <- polygons[polygons[[idvar]] == polygonID,]
  clip <- raster::crop(land, onepoly)
  mask <- raster::mask(clip, onepoly)
  #store id variable for specific landscape raster
  nameraster <- as.character(onepoly[[idvar]])

  #add id variable to output directory file path
  rasterpath <- paste(outdir, nameraster, sep="/")

  #write raster file as .tif to output directory
  raster::writeRaster(mask, filename=rasterpath, format='GTiff')
  return(data.frame(LandID=nameraster, WriteComplete=T))
}

#'Clip landscape raster by polygon or buffered point
#'
#'Clip landscape raster by polygon or buffered point features
#'@param rasterpath Full path name for landscape raster file
#'@param featurepath Full path name for polygon or point shapefile
#'@param usepoints Start with points shapefile of landscape centroids
#'@param bufferdist Radius of desired landscape buffer in km
#'@param outdir Directory where .tif landscape clips are to be stored, do not include final backslash
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#' clipland()

clipland <- function(rasterpath, featurepath, points=F, buffer=NA){
  #check that output directory is valid
  if (!file.exists(outdir)){
    #create folder if the directory doesn't exist
    dir.create(outdir)
  }

  land <- raster::raster(rasterpath)
  if (usepoints == T) {
    #import landscape centroid GPS coordinates from shapefile
    landcenter <- rgdal::readOGR(featurepath)

    #check if points are in same projection as raster layer
    #if different projection, reproject points to same projection as raster layer
    if (sp::proj4string(land) != sp::proj4string(landcenter)) {
      landcenter <- sp::spTransform(landcenter, CRS=sp::proj4string(land))
      proj <- sp::proj4string(land)
      if (grep(proj, pattern="+units=m") != 1) {
        stop("The projection used for your landscape raster must be defined in meters.")
      }

    }

    polygons <-rgeos::gBuffer(landcenter, byid=T, width=bufferdist)
  } else {
    #if using polygons, import them directly
    polygons <- rgdal::readOGR(featurepath)
    #check that polygons are in same projection as landscape raster
    #if projections are not the same, project polygon layer
    if (sp::proj4string(land) != sp::proj4string(polygons)) {
      polygons <- sp::spTransform(polygons, CRS=sp::proj4string(land))
    }
  }

polygon_ids <- as.list(polygons[[idvar]])

#ames(polygon_ids) <- 'LandName'
#polygon_ids$LandName <- as.character(polygon_ids$LandName) %>%
#                        as.list(polygon_ids)

polygonID <- polygon_ids[[1]]

plyr::ldply(polygon_ids, .fun=clipmask, land=land, polygons=polygons, outdir=outdir, idvar=idvar)

}



