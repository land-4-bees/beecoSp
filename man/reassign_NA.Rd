% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reassign_NA.R
\name{reassign_NA}
\alias{reassign_NA}
\title{Reassign raster NA values}
\usage{
reassign_NA(map, window_size, replace_any = F)
}
\arguments{
\item{map}{input raster file}

\item{window_size}{diameter of moving window, including the focal cell}

\item{replace_any}{logical, can NA values be filled with any other raster class?}
}
\description{
Internal functions to reassign raster NA values based on the surrounding cells
}
\details{
window_size of parameter includes the focal cell. For example, a window size of 5 would include 2 cells in each direction from the focal cell.
If replace_any is false, the global environment must contain a vector called 'allow_classes' which specifies raster classes that can fill NA values. This is a sub-optimal solution because it requires configuring the global env correctly, but the focal functions does not accept more than two arguments, so I don't see any other way to do this...
}
\keyword{bees}
\keyword{ecology}
\keyword{landscape}
\keyword{spatial}
