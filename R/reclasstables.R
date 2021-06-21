#'Create reclass tables for a particular pesticide, state, and year
#'
#'Function returns individual dataframes for each state-year reclass table and includes option to write reclass tables to file in an indicated folder.
#'@param filepath file path for master pesticide use reclass table
#'@param states vector of states (characters) to include
#'@param years vector of years (integers) to include
#'@param write_reclass logical, write_reclass = T writes reclass tables to file
#'@param outfolder folder path to write reclass keys

reclasstables <- function(filepath, states, years, write_reclass, outfolder) {

  tables <- read_csv(filepath) %>%  #split master file into desired reclass tables
    dplyr::filter(state_alpha %in% states,
                  year %in% years) %>%
    dplyr::group_by(state_alpha, year) %>%
    dplyr::group_split()

  names <- read_csv(filepath) %>% #store names for each reclass table
    dplyr::filter(state_alpha %in% states,
                  year %in% years) %>%
    dplyr::group_by(state_alpha, year) %>%
    dplyr::group_keys() %>%
    mutate(stateyr = paste(state_alpha, year, sep="_"))

  names(tables) <- names$stateyr #set names of dataframes in list

  if(write_reclass == T){
    if(!dir.exists(outfolder)){
      dir.create(outfolder)
    }
    for(i in 1:length(tables)){
      outpath = paste0(outfolder,"/",names$stateyr[[i]],".csv")
      write_csv(tables[[i]], outpath, append=F)
    }
  }

  return(tables) # return list of tibbles
  #list2env(tables, globalenv()) #return tibbles to the global environment
}
