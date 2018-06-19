#'Calculate landscape percent land use, or any categorical raster
#'
#'@param landdir Logical specifying if 'landfiles' path is a directory
#'@param landfiles Path to landscape raster file (or files) to process
#'@param outfile Filename and path to .csv file to store results
#'@param idvar Identifying variable name within feature shapefile, passed from 'execute_landclip'
#'@param outdir Directory where .tif landscape clips are to be stored, passed from 'execute_landclip'
#'@param overrast Logical, should existing rasters with same filename be overwritten?
#'@keywords bees landscape ecology spatial
#'@export
#'@examples
#'see 'execute_landclip' for usage example.


#can specify a folder of landscapes (use landdir=T), or a list of landscape files
#outputs a .csv output file of landscape composition values of all input landscapes

landcomp <- function(landdir=T, landfiles, outfile, writeoutput=T, attr_path) {

library('raster'); rasterOptions(tmptime=2)


#make list of raw landscape .tif files
if (landdir==T) {
  lands <- list.files(landfiles, pattern = "\\.tif$", full.names=T)
}
if (landdir==F) {lands <- landfiles}
#calculate landscape composition for first landscape
land <- raster(lands[1])

df1 <- data.frame(table(values(land)))

if (length(df1) == 2) {
names(df1) <- c("VALUE", "Cell_Num")
df1 <- df1[!df1$VALUE %in% c(0,-999),]
df1$Pct_Land <- (df1$Cell_Num/sum(df1$Cell_Num))*100
df1$Landscape <- basename(lands[1])

all <- df1

#loop over all other landscapes and merge composition files with first one
if (length(lands) > 1) {
  for (i in 2:length(lands)) {
    land <- raster(lands[i])
    dfn <- data.frame(table(values(land)))
    names(dfn) <- c("VALUE", "Cell_Num")
    dfn <- dfn[!dfn$VALUE %in% c(-999, 0),]
    dfn$Pct_Land <- (dfn$Cell_Num/sum(dfn$Cell_Num))*100
    dfn$Landscape <- basename(lands[i])

    all <- rbind(dfn, all)
  }
}


#import NASS attribute table
NASS_attribute <- read.delim(attr_path)

#add class names to data frame
all <- merge(all, NASS_attribute[, c("VALUE", "CLASS_NAME")])
all$VALUE <- all$VALUE[drop=T]


if (writeoutput==T) {
  write.csv(all, file=outfile) }
}

  if (length(df1) == 1){
  all <- data.frame(VALUE=NA, Cell_Num=NA, Pct_Land=NA, Landscape=basename(lands), CLASS_NAME=NA)
  }

return(all)
}

