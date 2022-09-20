#'Identify clusters of adjacent tiles to mosaic to larger raster
#'
#'Internal function that identifies groups for 'mosaic_tiles', not to be called directly
#'@param tile_list list of all raster tiles
#'@param chunksize numeric indicating how many tiles should be in each cluster
#'@param plot_clusters logical, make map indicating cluster membership?
#'@export
#'@noRd
#'@keywords Internal
#'
calc_tile_clusters <- function(tile_list, chunksize, plot_clusters=F) {

  # convert tiles to polygons
  for (i in 1:length(tile_list)) {
    # sort tile list so mega-tiles will be adjacent
    a <- terra::as.polygons(terra::ext(tile_list[[i]]))

    if (i == 1) {
      polys <- a
    } else if (i > 1) {
      polys <- rbind(polys, a)
    }
  }

  # make point object of polygon centroids
  tile_centroids <- terra::centroids(polys)

  # calculate distance between centroids

  # using chunksize 1, figure out how many groups there should be
  ngroups <- ceiling(length(tile_list)/chunksize)

  # k-means clustering to assign polygons to groups based on proximity
  xy <- data.frame(geom(tile_centroids))


  if (length(tile_list) > chunksize) {
    set.seed(42)
    km <- kmeans(cbind(xy$x, xy$y), centers=ngroups)
  } else {
    km <- data.frame(cluster=rep_len(1, length.out=length(tile_list)))
  }
  palette(c("#88e99a", "#183629", "#54d7eb", "#312a55", "#ec9fe7", "#5b0891", "#cddb9b", "#66050d",
            "#b3e61c", "#167b2b", "#a3c9fe", "#0f767a", "#24e53d", "#eb1241", "#df8a1d", "#ad486c",
            "#f7b8a2", "#986d5e", "#f3d426", "#5064be"))

  if (plot_clusters==T) {plot(polys, col=km$cluster, pch=20, main="warning: if > 20 clusters, colors will recycle")}

  # make vector indicating group membership
  clusters <- km$cluster
  return(clusters)
}
