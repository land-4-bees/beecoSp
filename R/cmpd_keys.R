## this function generates reclass tables to translate the Cropland Data Layer into application rates (kg/ha) for a particular compound - the output is a different file for each state and year
#'Generate reclass tables to convert land use (e.g. Crop Data Layer) into application of particular pesticide compounds
#'@param pestpath = file path for master pesticide use dataset
#'@param cmpd = name of the compound of interest (character, all caps, must match USGS name)
#'@param states = vector of states (characters) to include
#'@param yrs = vector of years (integers) to include
#'@param landuse = input file with CDL values and crop names
#'@param outpath = file path to write reclass keys
#'@examples
#'See Insert link to Maggie's github site here!

cmpd_keys <- function(pestpath, cmpd, states, yrs, landuse, outpath){
  landuse <- read.csv(landuse) %>%
    dplyr::mutate(crop = as.character(crop))
  data <- read.csv(pestpath) %>%
    dplyr::mutate(kg_ha = kg_app/(acres*0.404686),
                  STATE_ALPHA = as.character(STATE_ALPHA)) %>%
    dplyr::filter(cmpd_usgs == cmpd,
                  STATE_ALPHA %in% states,
                  Year %in% yrs) %>%
    dplyr::select(state_fips = State_FIPS_code,
                  state_alpha = STATE_ALPHA,
                  year = Year,
                  cat,
                  cmpd_usgs,
                  crop,
                  kg_ha) %>%
    dplyr::mutate(crop = as.character(crop))
  y <- split(data, list(data$state_alpha, data$year))
  cdl_list <- lapply(y, FUN=full_join, y=landuse, by="crop") %>%
    lapply(FUN = mutate, unsurveyed = ifelse(crop=="Not_surveyed",1,0),
           noncrop = ifelse(crop=="Non_crop",1,0)) %>%
    lapply(FUN = rename,
           crop_usgs = crop) %>%
    lapply(FUN = dplyr::select,
           value, Categories, crop_usgs, kg_ha, unsurveyed, noncrop) %>%
    lapply(FUN = arrange, value)
  names <- names(cdl_list) %>%
    gsub(pattern='[.]', replacement = '_') %>%
    paste(tolower(cmpd), "cdl_key.csv", sep="_")
  setwd(outpath)
  for(i in 1:length(cdl_list)) {
    r <- write.csv(cdl_list[[i]], names[i], row.names=FALSE)
  }
}
