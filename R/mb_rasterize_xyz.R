#' Rasterize xyz data
#'
#' @param xyz A tiblle
#' @param r0 A base raster
#' @param agg Level of aggregation (2, 4, ...)
#' @param fun Normally mean or count
#'
#' @return A {terra} raster
#' @export
#'
mb_rasterize_xyz <- function(xyz, r0, agg = 1, fun = "mean") {

  my_extent <- function(x) {
    c(xmin = min(x$x), xmax = max(x$x), ymin = min(x$y), ymax = max(x$y))
  }
  e <-  my_extent(xyz)
  r0 <- terra::crop(r0, e)
  if(agg > 1) r0 <- terra::aggregate(r0, fact = agg)

  terra::rasterize(x = as.matrix(xyz[, 1:2]),
                   y = r0,
                   values = xyz$z,
                   fun = fun,
                   crs = terra::crs(r0)) |>
    terra::trim()
}
