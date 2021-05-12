#'Reassign raster NA values
#'
#'Internal functions to reassign raster NA values based on the surrounding cells
#'@param map input raster file
#'@param window_size diameter of moving window, including the focal cell
#'@param replace_any logical, can NA values be filled with any other raster class?
#'@export
#'@keywords bees landscape ecology spatial
#'@details
#' window_size of parameter includes the focal cell. For example, a window size of 5 would include 2 cells in each direction from the focal cell.
#' If replace_any is false, the global environment must contain a vector called 'allow_classes' which specifies raster classes that can fill NA values. This is a sub-optimal solution because it requires configuring the global env correctly, but the focal functions does not accept more than two arguments, so I don't see any other way to do this...

reassign_NA <- function(map, window_size, replace_any=F) {

  # specify allow_classes is a global variable (necessary for futures package to work)
  allow_classes

    if (replace_any == T) {

      #use custom function, but specify which classes can be returned as the mode.
      pooey <- terra::focal(smaller_test, na.only=T, w=window_size, fun='modal',
                            na.rm=T)

    } else if (replace_any == F) {

      #use custom function, but specify which classes can be returned as the mode.
      #If the allowed classes don't exist, return -1001
      pooey <- terra::focal(smaller_test, na.only=T, w=window_size, fun='custom_modal',
                            na.rm=T)
    }

  return(pooey)
}

# function to identify the most common raster class with a set of allowed classes (e.g. most common agricultural class)
custom_modal <- function(x, na.rm, ...) {

  # retrieve list of allowable classes from the global environment

  allow_classes <- get('allow_classes', pryr::where('allow_classes'))

  if (any(x %in% allow_classes)) {

    #reassign dis-allowed classes to NA
    x[!x %in% allow_classes] <- NA

    #calculate mode of remaining (not NA) values
    mo <- terra::modal(x, na.rm=T, ties='random')
    return(mo)

  } else {

    return(-1001)

  }
}



