#' NA vector fill function

#'

#' \code{seq_fill_na} fills NA values within a vector with linearly interpolated numeric values.

#' 

#'

#' @param vector numeric vector

#'

#' @return numeric vector without NA values.

#'

#' @importFrom zoo na.fill

#' 

#' @export



seq_fill_na <- function(vector){
  
  
  
  # here you start coding, for example, start with sum checks:
  
  
  
  if(!inherits(vector, "numeric")) cat("Argument vector should be a 'numeric' object.")
  
  if (length(which(!is.na(vector)))<2) { # interpolation needs at least 2 numeric values
    return(vector)
    } else {
    vector <- na.fill(vector,"extend")  # fill NA values
  }
  
  # when finished, return the result
  
  return(vector)
  
}