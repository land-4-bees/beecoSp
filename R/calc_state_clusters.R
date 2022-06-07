#'Identify clusters of adjacent states to mosaic to national raster
#'
#'Internal function that identifies groups for 'mosaic_states', not to be called directly.
#'@param state_list list of all state rasters
#'@param tier numeric indicating round of hierarchical processing (usually 1-3 rounds)
#'@param plot_clusters logical, make map indicating cluster membership?
#'@param mult numeric, multiplier indicating increase in search distance compared with the previous tier
#'@details This function is very similar to calc_tile_clusters, but has some different defaults assuming that input rasters will be whole states, not sub-state tiles.

calc_state_clusters <- function(state_list, tier=1, plot_clusters=F, mult=0.8) {
  
  library(dplyr)
  # define max cluster distances based on 3 tier hierarchy
  # hopefully, with three rounds of hierarchical mosaic, we can get to entire country raster
  firstdist <- 1000000
  
  if (tier == 1) {
    maxdist <- firstdist
  } else {
    maxdist <- firstdist * (tier*mult)
  }
  
  # convert tiles to polygons
  for (i in 1:length(state_list)) {
    # sort tile list so mega-tiles will be adjacent
    a <- terra::as.polygons(terra::ext(state_list[[i]]))

    if (i == 1) {
      polys <- a
    } else if (i > 1) {
      polys <- rbind(polys, a)
    }
  }

  # set crs of polygon layer
  terra::crs(polys) <- terra::crs(state_list[[1]])
  
  # make point object of polygon centroids
  tile_centroids <- terra::centroids(polys) %>% terra::project("epsg:4326")
  
  # calculate distance between tile centroids
  dist <- terra::distance(tile_centroids)

  # cluster all points using a hierarchical clustering approach
  set.seed(42)
  hc <- hclust(dist, method="complete")
  
  # define clusters based on a tree "height" cutoff distance
  clusters <- cutree(hc, h=maxdist) # define clusters w/ max distance of 10000km 
  
  if (plot_clusters == T) {
    palette(c("#88e99a", "#183629", "#54d7eb", "#312a55", "#ec9fe7", "#5b0891", "#cddb9b", "#66050d", 
              "#b3e61c", "#167b2b", "#a3c9fe", "#0f767a", "#24e53d", "#eb1241", "#df8a1d", "#ad486c", 
              "#f7b8a2", "#986d5e", "#f3d426", "#5064be"))
    
    plot(polys, col=clusters, pch=20, main="warning: if > 20 clusters, colors will recycle")
  }
  
  return(clusters)
}
