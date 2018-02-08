#'Clip landscape raster by polygon or buffered point
#'
#'Internal function that clip landscapes within 'execute_landmask', not to be called directly
#'@param polygonID id of specific polygon used for clip and mask
#'@param land landscape raster, passed from 'execute_landmask'
#'@param polygons SpatialPolygonDataFrame of polygons to clip, passed from 'execute_landmask'
#'@param idvar Identifying variable name within feature shapefile, passed from 'execute_landmask'
#'@param outdir Directory where .tif landscape clips are to be stored, passed from ''execute_landmask'
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

#'Buffer and reproject input points or polygons
#'
#'Clip landscape raster by polygon or buffered point features
#'@param rasterpath Full path name for landscape raster file
#'@param featurepath Full path name for point or polygon shapefile
#'@param usepoints Shapefile is points, default is FALSE
#'@param bufferdist Radius of desired landscape buffer in km
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#' bufferproject()

bufferproject<- function(rasterpath, featurepath, usepoints=F, buffer=NA){

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
  }

return(polygons)
}

#'Execute landscape clip and mask
#'
#'Execute landscape clip and mask
#'@param polygons SpatialPolygonDataFrame of polygons to use to clip raster
#'@param rasterpath Full path name for landscape raster file
#'@param outdir Directory where .tif landscape clips are to be stored, do not include final backslash
#'@param idvar Identifying variable name within feature shapefile to use for naming output rasters
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#' execute_landclip()
#'
#'
execute_landclip <- function(polygons, rasterpath, idvar, outdir) {
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


  plyr::ldply(polygon_ids, .fun=clipmask, land=land, polygons=polygons, outdir=outdir, idvar=idvar)

}



