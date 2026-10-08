#' A raster template
#'
#' Create a skeleton covering eez with no associated cell values.
#' Make it divisable by 8, 16,  ..., 2048

#' @return a terra raster
#' @export
#'
rb_create_base_raster <- function() {
  .rb_need("terra")

  #
  #
  CRS <- 5325

  # The bounding box of the Icelandic EEZ in EPSG:5325, rounded out to multiples of 2048 m.
  bb <- c(1107968, -260096, 2314240, 827392)

  r <-
    terra::rast(xmin = bb[1],
                ymin = bb[2],
                xmax = bb[3],
                ymax = bb[4],
                nlyrs = 1,
                resolution = c(8, 8),
                crs = paste0("epsg:", CRS))
  return(r)
}
