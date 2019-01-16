#' Dummy function
#'
#' A short description of your function, for example: \code{dummy} is a dummy function,
#' serving as a template for you  to create new functions. Just copy this file into the
#' R directory of this package, rename it to the name of your function and start changing 
#' it, including this text.
#'
#' @param x numeric, a number of your choice.
#' @param y numeric, a number to be multiplied-
#' @param additive logical, if \code{TRUE}, x and y are added, else they are substracted..
#'
#' @return Describe, what is returned by your function, for example: a numeric value \code{z}.
#'
#' @importFrom raster raster extract projectRaster
#' @importFrom rasterVis levelplot
#' @importFrom ggplot2 ggplot aes geom_line geom_area theme_bw theme scale_y_continuous scale_x_continuous scale_color_manual labs
#'
#' @export

dummy <- function(x, y, additive){
  
  # here you start coding, for example, start with sum checks:
  
  if(!inherits(x, "numeric")) cat("Argument x should be a 'numeric' object.")
  if(!inherits(y, "numeric")) cat("Argument y should be a 'numeric' object.")
  if(!inherits(additive, "logical")) cat("Argument additive should be a 'logical' object.")
  
  # after some user input checks, you could continue, e.g.:
  if(isTRUE(additive)) z <- x+y else z <- x-y
  
  # when finished, return the result
  return(z)
}