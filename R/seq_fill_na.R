#' NA vector fill function
#'
#' \code{seq_fill_na} fills NA values within a vector with linearly interpolated numeric values.
#'
#' @param vector numeric vector
#'
#' @return numeric vector without NA values.
#'
#' @importFrom zoo na.fill
#' 
#' @export

seq_fill_na <- function(vector){
  # Check if input is numeric
  if(!inherits(vector, "numeric")){
    print("Invalid Input. Vector has to be a 'numeric' object.")
    return(vector)
  }
  # Interpolation needs at least 2 numeric values
  if (length(which(!is.na(vector)))<2) {
    return(vector)
    } else {
  # Staring interpolation    
    vector <- na.fill(vector,"extend")  # fill NA values
  }
  # When finished, return the result
  return(vector)
}