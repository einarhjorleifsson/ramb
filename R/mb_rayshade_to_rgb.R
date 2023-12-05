#' rayshade_to_rgb
#'
#' Turns a matrix into a 3 band RGB raster. See also hillshader::matrix_to_raster
#'
#' @param shaded The input matrix, typically the output of a rayshader operation
#' @param original The original raster from which matrix is derived
#'
#' @return A terra-raster layer
#' @export
#'
mb_rayshade_to_rgb <- function(shaded, original) {
  rb <-
    terra::rast(shaded,
              extent = terra::ext(original),
              crs = terra::crs(original))
  terra::values(rb[[1]]) <- scales::rescale(terra::values(rb[[1]]), from = c(0, 1), to = c(0, 255))
  terra::values(rb[[2]]) <- scales::rescale(terra::values(rb[[2]]), from = c(0, 1), to = c(0, 255))
  terra::values(rb[[3]]) <- scales::rescale(terra::values(rb[[3]]), from = c(0, 1), to = c(0, 255))
  j <- is.na(terra::values(original))
  terra::values(rb[[1]])[j] <- NA
  terra::values(rb[[2]])[j] <- NA
  terra::values(rb[[3]])[j] <- NA
  return(rb)
}
