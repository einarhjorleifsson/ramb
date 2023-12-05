#' A raster template
#'
#' Create a skeleton covering eez with no associated cell values.
#' Make it divisable by 8, 16,  ..., 2048

#' @return a {terra} raster
#' @export
#'
mb_base_raster <- function() {

  #
  #
  CRS <- 5325

  if(FALSE) {
    BASE <- 2048
    eez <-
      gisland::read_eez() |>
      sf::st_transform(crs = CRS)
    bb <-
      eez |>
      sf::st_bbox()
    bb <- as.integer( BASE * round( bb / BASE )) + c(0, 0, BASE, BASE)
  }
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
