#' Animate a time series of rasters
#'
#' \code{animate_series} animates a series of rasters in consecutive order.
#'
#' @param r raster or stack of rasters, each representing a different acquisition time.
#' @param dates character vector of length \code{nlayers(r)}, representing corresponding times of \code{r}.
#' @param breaks numeric vector, value range to be displayed, e.g. \code{seq(1, 180, by = 1)}
#' @param param data.frame or \code{NULL}, optional parameter data.frame derived using \link{bands_param} to show the development of a paramater over time.
#' @param param.name character, name of the defined paramater. Default is "Parameter".
#' @param file.name character, path to the output file Default is a temporary file.
#' @param ... arguments passed to \link{saveGIF}, e.g. interval=0.2, ani.width = 500, ani.height = 700 etc.
#'
#' @return List of plots that are used as frames for the animation. An animation file will be written to file.name.
#'
#' @importFrom animation saveGIF
#' @importFrom raster nlayers
#' @importFrom rasterVis levelplot
#' @importFrom ggplot2 ggplot aes geom_line geom_area theme_bw theme scale_y_continuous scale_x_continuous scale_color_manual labs
#' @importFrom gridExtra grid.arrange
#'
#' @export

animate_series <- function(r, dates, breaks, param = NULL, param.name = "Parameter", file.name = tempfile(fileext = ".gif"), ...){

  saveGIF({
    for(i in c(1:nlayers(r))){
      p.level <- levelplot(r[[i]], at = breaks, margin = F, col.regions=terrain.colors(max(breaks)))
      if(!is.null(param)){
        p.time <- ggplot(data = param, aes(x = time, y = stat)) +
          geom_line(color = "grey", size = 1) +
          theme_bw() + theme(aspect.ratio=0.5) +
          scale_y_continuous(expand = c(0,0),limits = c(0, round(max(param$stat)+(max(param$stat)*0.1)))) +
          scale_x_continuous(expand = c(0,0), labels = dates) +
          scale_color_manual(name= "",values = cols) +
          labs(x = "Time", y=param.name, label=c("123","456"))
        if(i > 1){
          p.time <- p.time +
            geom_line(data = param[1:i,], color = "black", size = 1) +
            geom_area(data = param[1:i,], color = "black", fill = "black", alpha = 0.5, size = 1)
        }
        grid.arrange(grobs = list(p.level, p.time), layout_matrix = rbind(c(1), c(1), c(2)))
      } else{ plot(p.level)}
    }
  }, movie.name=file.name, ...)
}
