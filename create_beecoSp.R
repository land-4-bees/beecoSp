#translate spatial functions into correct format for R package
#this allows it to be loaded directly from Github

#adapted from this blog post: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

#install.packages("devtools")
library(devtools)
#devtools::install_github("klutometis/roxygen")
library(roxygen2)

#check and make sure working directory is set correctly
getwd()

setwd("Z:/SoftwareDevelopment/beecoSp")
#create and name R package (do not run again)
#create("beecoSp")

#add package dependencies
usethis::use_package("raster")
usethis::use_package("terra")
usethis::use_package("rgdal")
usethis::use_package("rgeos")
usethis::use_package("plyr")
usethis::use_package("future")
usethis::use_package("foreach")
usethis::use_package("logger")
#updated 'description file'


#create documentation
devtools::load_all()
devtools::document()

# #add sample data
# devtools::use_data(ny_landuse, ny_samplesites)


#install package locally
setwd("Z:/SoftwareDevelopment")
devtools::install("beecoSp")


#try adding from Github
devtools::install_github("land-4-bees/beecoSp")



