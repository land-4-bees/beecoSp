#'Filter sites to random subset based on proximity
#'
#'Select sites that are separated by some minimum distance to ensure independence of bee communities
#'@param points Spatial points object of sites (sampling locations)
#'@param cutoff Distance cutoff (in meters) for independent samples (typically average foraging range of bee(s))
#'@details Spatial points must lat/long (not projected). See 'distGeo' function (geosphere package) for details of distance calculation.
#'@export
#'@examples
#' Usage example coming soon.


subsetplots <- function(points, cutoff) {
  repeat {
    d <- geosphere::distm(points)
    diag(d) <- -1
    d
    #list of points that are less than cutoff apart
    closepoints <- data.frame(which(d >= 0 & d < cutoff, arr.ind=T))
    if (length(rownames(closepoints)) == 0) break
    else {temp <- data.frame(table(closepoints[2]))
    #points that have the maximum number of neighbors that are closer than cutoff (usually two neighbors)
    removept <- sample(as.numeric(as.character(temp$Var1[temp$Freq == max(temp$Freq)])), 1)

    #subset points removing one of the worst points (with max neighbors closer than cutoff)
    points <- points[-removept,]
    }
  }
  return(points)
}
