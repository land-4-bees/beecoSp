#translate spatial functions into correct format for R package
#this allows it to be loaded directly from Github

#adapted from this blog post: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

#install.packages("devtools")
library("devtools")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)

#check and make sure working directory is set correctly
getwd()

#setwd("D:/Documents/Land4Bees/beecoSp")
#create and name R package (do not run again)
#create("beecoSp")

#add package dependencies
devtools::use_package("raster")
devtools::use_package("rgdal")
devtools::use_package("rgeos")
devtools::use_package("plyr")
#updated 'description file'


#create documentation
devtools::load_all()
devtools::document()

#add sample data
devtools::use_data(ny_landuse, ny_samplesites)


#install package locally
setwd("D:/Documents/Land4Bees")
devtools::install("beecoSp")


#try adding from Github
devtools::install_github("land-4-bees/beecoSp")



