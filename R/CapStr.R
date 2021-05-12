#' Helper function to capitalize a string
#'@param x input string
#'@return capitalized output string
#'@keywords bees landscape ecology spatial
#'@export

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
