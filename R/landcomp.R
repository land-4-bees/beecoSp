#'Calculate landscape composition (pct of each land cover type)
#'
#'@param landdir Logical specifying if 'landfiles' is a directory
#'@param landfiles Landscape raster files to process (path to directory or list of files)
#'@param writeoutput Logical specifying if results should be written to .csv file
#'@param outfile If writeoutput=T, path to .csv file to store results (including .csv filename)
#'@param attr_path Path to .csv file of raster attribute table
#'@param attr_value Column name in attribute table that specifies raster values
#'@param background Logical, specify background value?
#'@param bgvalues or NoData values to be excluded from total landscape area
#'@keywords bees landscape ecology spatial
#'@details This function can be used to calculate the number of cells and pct area covered for any categorical raster.
#'@import foreach
#'@import dplyr
#'@export
#'@examples
#' Usage example coming soon.


#can specify a folder of landscapes (use landdir=T), or a list of landscape files
#outputs a .csv output file of landscape composition values of all input landscapes

landcomp <- function(landdir=T, landfiles, writeoutput=T, outfile, attr_path=NA, attr_value, background=F, bgvalues=-1000) {


#make list of raw landscape .tif files
if (landdir==T) {
  lands <- list.files(landfiles, pattern = "\\.tif$", full.names=T)
}
if (landdir==F) {lands <- landfiles}

# function to calculate landscape composition
comp_funct <- function(y, background=background) {
  land <- terra::rast(y)
  dfn <- terra::freq(land) %>%
    select(-layer)

  if (length(dfn) != 2) { stop('Something is wrong with input raster. Could not tabulate unique values') }
  names(dfn) <- c("VALUE", "Cell_Num")

  if (background == T) {
    dfn <- dfn[!dfn$VALUE %in% bgvalues,]
  }

  dfn$Pct_Land <- (dfn$Cell_Num/sum(dfn$Cell_Num))*100
  dfn$Landscape <- gsub(basename(y), pattern='.tif', replacement="")

  dfn <- dfn %>% dplyr::select(Landscape, dplyr::everything())

  return(dfn)
}

all <- purrr::map(lands, comp_funct, background=background) %>%
  purrr::list_rbind()


if (!is.na(attr_path)) {
  #import NASS attribute table
  NASS_attribute <- read.csv(attr_path)

  #add class names to data frame
  all <- merge(all, NASS_attribute, by.x="VALUE", by.y=attr_value, all.x=T)
  all$VALUE <- all$VALUE[drop=T]
}

if (writeoutput==T) {
  write.csv(all, file=outfile)
}

return(all)
}

