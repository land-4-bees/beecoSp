#download and attach 'beecoSp' package
#see beecoSp help documentation for information on each function

library(devtools)
install_github("land-4-bees/beecoSp")
library(beecoSp); library(dplyr)

##### Part 1: Make buffers around each site, clip land cover raster to buffer, calculate the area of each class (pct of total landscape)
regional_landcover_path <-'./landmodel_inputs/midAtlantic_CDL'

#generate landscape buffers around each sampling location
landbuffers <- bufferproject(rasterpath=regional_landcover_path, featurepath= './spatial_points/site_sampling_locations.shp',
                             bufferdist=500)

#clips regional land cover raster by landscape buffers
land_ids <- execute_landclip(landbuffers, rasterpath=regional_landcover_path, idvar='SiteID_Year', 
                             outdir='./site_clips/', overrast=T, na_value=255, parallel=F)

#rerun landscape composition (pct of landscape in each class), not counting background values when present in landscapes
lcomp <- landcomp(landdir=T, landfiles=clipdir, writeoutput=F, attr_value='VALUE', attr_path='D:/SpatialData/NASS_CDL/NASS_classes.csv', background=T, bgvalues=c(NA, 255, 0))


##### Part 2: Group land cover classes into broader categories (agriculture, natural, and developed), calculate area of broader categories

#read attribute table with columns defining which classes are agriculture and natural
NASS_classes <- read.csv('D:/SpatialData/NASS_CDL/NASS_classes.csv')
#define vector of CDL classes defined as agricultural land
crops <- as.character(NASS_classes$CLASS_NAME[NASS_classes$GROUP == 'A'])
#define vector of CDL classes defined as natural land
natural <- as.character(NASS_classes$CLASS_NAME[NASS_classes$GROUP == 'N'])

#reshape landscape composition dataset and add columns for percent natural, developed, and agriculture
comp_wide <- dplyr::select(lcomp, Pct_Land, Landscape, CLASS_NAME) %>%
  tidyr::spread(key=CLASS_NAME, value=Pct_Land, fill=0) %>%
  dplyr::mutate(developed= rowSums(dplyr::select(., contains('Developed')))) %>% #add sum of all developed classes
  dplyr::mutate(arable= rowSums(dplyr::select(., one_of(crops))), natural=rowSums(dplyr::select(., one_of(natural)))) #add sum of arable and natural classes


