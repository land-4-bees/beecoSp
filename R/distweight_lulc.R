#'Calculate distance weighted land cover
#'
#'Calculate distance weighted sum of raster classes, assuming middle pixel is focal cell.
#'@param land_raster File path to land cover raster (centered on sampling location)
#'@param forage_range Mean foraging range of bee community of interest (determines shape of distance weighting curve)
#'@details Distance weighting is an exponential decline function with a maximum distance of 'forage_range' *2.
#' See Lonsdorf et al (2009) for details.
#' 'land_raster' must have a radius greater than 'forage_range'*2, but can be much larger (whole county). The raster used for distance weighting is cropped to radius of 'forage_range' times 2.
#'@export
#'@examples
#' Usage example coming soon.

distweight_lulc <- function(land_raster, forage_range) {

  #rename foraging range to mesh with existing code
  mean_FR <- forage_range

  #read in land cover raster
  hab.r <- raster::raster(land_raster)

  #set a maximum foraging distance to be twice the foraging range
  #will be used later to determine size of moving window
  maxforage.dist <- mean_FR*2

  ###set up moving window specifications to be used for distance weighting later
  #store cell size of raster
  c.size <- raster::res(hab.r)[1]

  #matrix boundary for moving window (in number of pixels)
  radius <- round(maxforage.dist/c.size)

  # size of moving window is set to twice the radius plus one
  # create matrices for moving window distance matrix
  dist.m <- matrix(data=1, nrow= radius*2+1, ncol= radius*2+1)
  focal.c <- median(1:nrow(dist.m)) #row number of focal cell in the distance matrix

  # calculating distance all cells from the focal cell in the nested matrix
  # loops through unique combination of all row and column numbers (cells), uses pythag theorum to calculate distance from cell center to focal cell center
  for(i in 1:nrow(dist.m)) {
    for (j in 1:ncol(dist.m)) {

      dist.m[i,j] <- sqrt( ((i-0.5)*c.size - (focal.c - 0.5)*c.size)^2 +
                             ((j-0.5)*c.size - (focal.c - 0.5)*c.size)^2 )
    }
  }

  effdist.v <- exp(-dist.m / mean_FR) # calculate effective distance (moving window weights)

  # set cell value to 0, when effdist > (2xforaging distance)
  effdist.v[which(dist.m > maxforage.dist)] <- 0

  #create spatial point for center of land cover raster
  centroid <- sp::SpatialPoints(cbind( (((xmax(hab.r)+xmin(hab.r))/2)), ((ymax(hab.r)+ymin(hab.r))/2)),
                                proj4string=crs(hab.r))

  #adjust coordinate of center pixel so the dimensions of cropped raster = distance weighting matrix
  x <- (xmin(centroid)-xmin(hab.r))/c.size; xd <- x- floor(x)
  if (xd == 0) {
    newx <- (xmax(hab.r)+xmin(hab.r)+0.0001)/2
  }
  y <- (ymin(centroid)-ymin(hab.r))/c.size; yd <- y- floor(y)
  if (yd == 0) {
    newy <- (ymax(hab.r)+ymin(hab.r)+0.0001)/2
  }

  if (exists('newx') & exists('newy')) {
    centroid <- sp::SpatialPoints(cbind(newx, newy), proj4string=crs(hab.r))
    warning('Centroid of raster was moved slightly to create raster with odd dimensions. Check that centroid point is in acceptable location for site it represents.')
  } else if (exists('newx') & !exists('newy')) {
    centroid <- sp::SpatialPoints(cbind(newx, ymin(centroid)), proj4string=crs(hab.r))
    warning('Centroid of raster was moved slightly to create raster with odd dimensions. Check that centroid point is in acceptable location for site it represents.')
  } else if (!exists('newx') & exists('newy')) {
    centroid <- sp::SpatialPoints(cbind( xmin(centroid), newy), proj4string=crs(hab.r))
    warning('Centroid of raster was moved slightly to create raster with odd dimensions. Check that centroid point is in acceptable location for site it represents.')
  }

  basename(landraster)
  #change centroid to sf package format so can write to shapefile
  #writeOGR doesn't seem to work without attribute information
  centroid <- sf::st_as_sf(centroid)
  land_name <- gsub(basename(land_raster), pattern='.tif', replacement = "")
  # write centroid to a shapefile
  sf::st_write(centroid, paste0("centroid_point_",land_name, ".shp"), driver="ESRI Shapefile")

  #create buffer around center point, with radius equal to half of the moving window dimensions
  #add a small constant to radius of moving window to create odd number of pixels (one middle 'focal' cell)
  landpoly <- raster::buffer(centroid, width=(radius+1)*c.size) %>%
              sf::st_as_sf()

  #crop land cover raster to 2x foraging distance (plus one extra middle focal cell)
  hab_crop <- crop(hab.r, landpoly, snap='in')

  #convert polygon to raster, assigning distance weighting values to new raster
  dist_rast <- raster::raster(effdist.v, template=hab_crop)

  #if dimensions of land cover raster do not match distance weighting raster, stop because something is wrong
  check <- dim(dist_rast) == dim(hab_crop)
  if (any(check == F)) {
    stop('Dimensions of cropped land cover raster and distance weighting raster do not match. Please input a land cover raster with radius >= ((forage_distance*2)/cell size)+1.')
  }

  mat <- data.frame(raster::zonal(dist_rast, z=hab_crop, fun='sum'))

  mat <- dplyr::rename(mat, landcover_class=zone, sum_distweight=sum) %>%
         dplyr::mutate(normalized_distweight=sum_distweight/sum(sum_distweight))

return(mat)
}
