#' Get extent of xy-dataframe
#'
#' @param x A dataframe containing x and y
#'
#' @return an "extent" vector
#' @export
#'
rb_calc_xyz_extent <- function(x) {
  c(xmin = floor(min(x$x)),
    xmax = ceiling(max(x$x)),
    ymin = floor(min(x$y)),
    ymax = ceiling(max(x$y)))
}

#' Rasterize xyz data
#'
#' @param xyz A tiblle
#' @param r0 A base raster
#' @param agg Level of aggregation (2, 4, ...)
#' @param fun Normally mean or count
#' @param no_trim No trimming
#'
#' @return A terra raster
#' @export
#'
rb_rasterize_xyz <- function(xyz, r0, agg = 1, fun = "mean", no_trim = TRUE) {
  
  e <-  rb_calc_xyz_extent(xyz)
  r0 <- terra::crop(r0, e)
  if(agg > 1) r0 <- terra::aggregate(r0, fact = agg)
  
  t <- 
    terra::rasterize(x = as.matrix(xyz[, 1:2]),
                     y = r0,
                     values = xyz$z,
                     fun = fun,
                     crs = terra::crs(r0)) 
  if(!no_trim) t <- t |> terra::trim()
  
  return(t)
}
