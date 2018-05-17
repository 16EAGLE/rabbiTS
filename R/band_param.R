#' Calculate a band paramater
#'
#' \code{scene_param} applies a function to all cell values per band to retrieve a band parameter.
#'
#' @param r raster or stack of rasters, each representing a different acquisition time.
#' @param fun function to be applied.
#'
#' @return Two-column data.frame with "stat" containing the band parameter(s) and "time" indicating consecutive order of the parameter in time
#'
#' @importFrom raster unstack cellStats
#'
#' @export

band_param <- function(r, fun){
  r <- unstack(r)
  v <- sapply(r, function(x, f = fun) cellStats(x, f), USE.NAMES = F)
  df <- cbind.data.frame(v, seq(1:length(v)))

  colnames(df) <- c("stat", "time")
  return(df)
}
