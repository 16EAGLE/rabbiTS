#' Time Series: Fill NA values
#'
#' \code{TS_raster_fill_na} is a function,
#' to interpolate missing NA values in stack or brick time series raster,
#' where the different layers represent the dimension of time. 
#' The interpolation will be done in the 3rd dimension, which means that a vector of values per
#' georaphic position is given to the interpolation function.
#'
#' @param stack_obj numeric stack or brick raster
#'
#' @return Brick raster, which containing NA values were interpolated in the dimension of time.
#'
#' @importFrom raster getValues
#' @importFrom future plan multiprocess
#' @importFrom future.apply future_apply
#' @importFrom rabbiTS seq_fill_na
#'
#' @export

TS_raster_fill_na <- function(stack_obj){
  
  # Check object type of input:
  
  if((inherits(stack_obj, "RasterStack") | (inherits(stack_obj, "RasterBrick")))){

    # Get all values of the input and save them in matrix
    temp_matrix <- getValues(stack_obj)
  
    # Check if there are NA values to replace
    if (any(which(is.na(temp_matrix)))) {
    # Start processing  
      plan(multiprocess) #Setup for Multi core processing 
    # Start of interpolation to temporary matrix  
      result <- future_apply(temp_matrix, 2, FUN = seq_fill_na)
    # Writing results into original Stack or Brick Structure  
      for (i in 1:ncol(result)) {
        stack_obj[[i]] <- result[,i]
      }
    } else {
    # Return original input if the input does not contain any NA values
      print("No NA values in TimeSeries Object. Returned original input")
      return(stack_obj)
    }
    # When finished, return the result
    return(stack_obj)
  } else {
    # Return input if is not stack or brick object
    print("Input is not a RasterStack or RasterBrick")
    return(stack_obj)
  }
}