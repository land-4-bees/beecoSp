% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/execute_landclip.R
\name{execute_landclip}
\alias{execute_landclip}
\title{Execute landscape clip and mask}
\usage{
execute_landclip(polygons, rasterpath, idvar, outdir, overrast, na_value = NA)
}
\arguments{
\item{polygons}{SpatialPolygonDataFrame of polygons to use to clip raster}

\item{rasterpath}{Full path name for landscape raster file}

\item{idvar}{Identifying variable name within feature shapefile to use for naming output rasters}

\item{outdir}{Directory where .tif landscape clips are to be stored, do not include final backslash}

\item{overrast}{Logical, should existing rasters with same filename be overwritten?}

\item{na_value}{Numeric, to assign NA values in raster}

\item{parallel}{execute landscape clips in parallel? (if yes requires set up of parallel environment)}
}
\description{
Execute landscape clip and mask
}
\examples{
execute_landclip()


}
\keyword{bees}
\keyword{ecology}
\keyword{landscape}
\keyword{spatial}
