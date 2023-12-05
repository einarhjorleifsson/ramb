#' Rayshade raster
#'
#' An opinionated rayshading of rasterized bathymetry depth data.
#'
#' The returned object can be viewed via rayshader::plot_maps
#'
#' @param r A single banded raster ({raster} or {terra}).
#'
#' @return A RGB array
#' @export
#'
mb_rayshade_raster <- function(r, zscale = 4) {
  rm <-
    r |>
    rayshader::raster_to_matrix()
  rs <-
    rm |>
    rayshader::height_shade() |>
    rayshader::add_overlay(rayshader::sphere_shade(rm,
                                                   texture = "desert",
                                                   zscale = zscale, colorintensity = 5), alphalayer=0.5) |>
    rayshader::add_shadow(rayshader::lamb_shade(rm, zscale = zscale), 0)
  return(rs)
}
