#'Clip raster tiles to smaller size
#'
#'Internal function that clips raster tiles by a specified buffer. Useful when tiles were created with overlap to handle edge effects.
#'@param rasterpath file path to raster file
#'@param buffercells vector indicating the number of buffer in ncells in x and y dimension.
#'@export

cliprasterbuffer <- function(rasterpath, buffercells) {
# read terra tile
index_tile <- terra::rast(rasterpath)

##### Crop merged tiles, removing overlap

# create extent object that removes the buffer cells 
delta_x <- terra::res(index_tile)[1]*buffercells[1]
delta_y <- terra::res(index_tile)[2]*buffercells[2]

# subtract buffer distance from tile extent
original_extent <- terra::ext(c(
  terra::ext(index_tile)$xmin + delta_x,
  terra::ext(index_tile)$xmax - delta_x,
  terra::ext(index_tile)$ymin + delta_y,
  terra::ext(index_tile)$ymax - delta_y
))

if (!dir.exists(paste0(dirname(rasterpath), "/ClippedTiles/"))) {
  dir.create(paste0(dirname(rasterpath), "/ClippedTiles/"))
}

cropped_tile <- terra::crop(index_tile, original_extent, 
  filename=paste0(dirname(rasterpath), "/ClippedTiles/", basename(rasterpath)), overwrite=T)
}