#' Rayshade raster
#'
#' An opinionated rayshading of rasterized bathymetry depth data.
#'
#' The returned object can be viewed via rayshader::plot_maps
#' 
#' Generally the lower the zscale the more exaggeration. Suggest using a
#' value 1 for 8x8 meter, value 2 for 16x6 meters, value 3 for 32x32 meters, ...
#'
#' @param r A single banded raster (raster or terra).
#' @param zscale A value (default 1) dictating how much ... 
#'
#' @return A RGB array
#' @export
#'
mb_rayshade_raster <- function(r, zscale = 1) {
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
