


subsetplots <- function(points, cutoff) {
  repeat {
    d <- distm(points)
    diag(d) <- -1
    d
    #list of whittaker plots that are less than cutoff apart
    closepoints <- data.frame(which(d >= 0 & d < cutoff, arr.ind=T))
    if (length(rownames(closepoints)) == 0) break
    else {temp <- data.frame(table(closepoints[2]))
    #whittaker plots that have the maximum number of neighbors that are closer than cutoff (usually two neighbors)
    removept <- sample(as.numeric(as.character(temp$Var1[temp$Freq == max(temp$Freq)])), 1)
    
    #whittaker plots subset which first one of very close plots removed
    points <- points[-removept,]
    }
  }
  return(points)  
}