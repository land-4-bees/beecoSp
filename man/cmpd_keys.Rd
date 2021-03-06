% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cmpd_keys.R
\name{cmpd_keys}
\alias{cmpd_keys}
\title{Generate reclass tables to convert land use (e.g. Crop Data Layer) into application of particular pesticide compounds}
\usage{
cmpd_keys(pestpath, cmpd, states, yrs, landuse, outpath)
}
\arguments{
\item{pestpath}{= file path for master pesticide use dataset}

\item{cmpd}{= name of the compound of interest (character, all caps, must match USGS name)}

\item{states}{= vector of states (characters) to include}

\item{yrs}{= vector of years (integers) to include}

\item{landuse}{= input file with CDL values and crop names}

\item{outpath}{= file path to write reclass keys}
}
\description{
Generate reclass tables to convert land use (e.g. Crop Data Layer) into application of particular pesticide compounds
}
