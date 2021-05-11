#'Reclassify land use raster into pesticide use intensity
#'
#'creates a state map of pesticide use for a particular compound by attributing pesticide use intensity (kg/ha) to land use/land cover in the Cropland Data Layer
#'@param state name of state you want to reclassify (e.g. 'MD')
#'@param year year of data you want to reclassify (e.g. 2012)
#'@param cmpd compound name from the USGS pesticide dataset (e.g. 'glyphosate')
#'@param from column in reclass table with land use raster values (e.g. CDL values from 1 to 254)
#'@param to column in reclass table with new values (e.g. main current option is application, 'kg_ha' - kg/hectare)
#'@param writerast logical, should write file of reclassified raster?
#'@param outpath if writerast is TRUE, path name where reclass raster should be written
#'@param meanreclass logical, return mean value of reclassified raster
#'@details This function is a wrapper for beecoSp's 'CDL_reclass' specific to reclassifying state maps into pesticide use intensity.
#'

CDL_reclass_cmpd <- function(state, year, cmpd, from, to, writerast=F, meanreclass) {

  rasterpath <- paste("./Land_use_data/CDL/",year,"_cdls/",
                      toupper(state),"/cdl_30m_r_",
                      tolower(state),"_", year,"_albers.tif", sep="")

  reclasspath <- paste("./reclass_keys/", tolower(cmpd), "/", toupper(state), "_", year, "_cdl_key.csv", sep = "")

  reclasstable <- read.csv(reclasspath)

  CDL_reclass(rasterpath=rasterpath, reclasstable=reclasstable, from=from, to=to, writerast=writerast,
              outpath=outpath, meanreclass=meanreclass)

}
