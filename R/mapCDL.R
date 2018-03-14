#plot landscape with NASS CDL colors

#import NASS attribute table
NASS_attribute <- read.delim('E:/Melanie/Dell_herb_land/NASS_classes.txt')

#Remove values with no description
NASS_attribute <- NASS_attribute[NASS_attribute$CLASS_NAME != " ",]

#Create color variable to use for raster symbology
NASS_attribute$Color <- rgb(red=NASS_attribute$RED, green=NASS_attribute$GREEN, blue=NASS_attribute$BLUE, alpha=NASS_attribute$OPACITY)
NASS_attribute <- NASS_attribute[order(NASS_attribute$VALUE),]


r <- ratify(land)
rat <- levels(r)[[1]]
rat$classes <- NASS_attribute$CLASS[NASS_attribute$VALUE %in% rat$ID]
cols <- NASS_attribute$Color[NASS_attribute$VALUE %in% rat$ID]
levels(r) <- rat

#plot original raster
levelplot(r, col.regions=cols)
