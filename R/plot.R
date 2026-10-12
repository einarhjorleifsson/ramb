#' Plot trails
#'
#' @param d a tibble containing trails
#' @param col the variable that controls colour
#' @param tooltip the variable to show as tooltip
#' @param no_lines plot track as lines (default = TRUE)
#' @param radius the radius of the points (default 400 m)
#' @param highlight_colour colour of track (default black)
#' @param stroke_colour stroke colour of track (default cyan)
#'
#'
#' @return a mapdeck map
#' @export
#'
rb_plot_trail <-
  function(
    d,
    col = "speed",
    tooltip = "speed",
    path = FALSE,
    radius = 400,
    highlight_colour = "black",
    stroke_colour = "cyan"
  ) {
    .rb_need("mapdeck")

    col2hex <- function(cname) {
      colMat <- grDevices::col2rgb(cname)
      grDevices::rgb(
        red = colMat[1, ] / 255,
        green = colMat[2, ] / 255,
        blue = colMat[3, ] / 255
      )
    }

    if (!"sf" %in% class(d)) {
      d <-
        d |>
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
    }

    if (path) {
      track <-
        d |>
        dplyr::summarise(do_union = FALSE) |>
        sf::st_cast("LINESTRING")
    }

    m <- mapdeck::mapdeck()

    if (path) {
      m <-
        m |>
        mapdeck::add_path(
          data = track,
          layer_id = "track",
          stroke_width = 300,
          width_min_pixels = 1,
          width_max_pixels = 5,
          #tooltip = "vessel",
          auto_highlight = TRUE,
          highlight_colour = paste0(col2hex(highlight_colour), "80"),
          update_view = FALSE,
          stroke_colour = paste0(col2hex(stroke_colour), "80")
        )
    }

    m |>
      mapdeck::add_scatterplot(
        data = d,
        fill_colour = col,
        radius = radius,
        tooltip = tooltip,
        layer_id = "points",
        palette = "inferno",
        legend = TRUE
      )
  }
#' rb_plot_trip
#'
#' @param data A tibble containing variable names lon and lat (crs 4326)
#' @param tid Trip id variable name, default tid
#' @param radius Radius (in meters) of the point size (default 10)
#' @param col What variable to be used for colour scale (default speed)
#' @param trip add trip path, default FALSE
#'
#' @return xxx
#'
#' @export
#'
rb_plot_trip <- function(data, tid, radius = 10, col = "speed", trip = TRUE) {
  .rb_need("mapdeck")

  if (any(!class(data) %in% "sf")) {
    data <-
      data |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  }

  m <- mapdeck::mapdeck()

  if (trip) {
    m <-
      m |>
      mapdeck::add_path(
        data = data |>
          dplyr::group_by({{ tid }}) |>
          dplyr::summarise(do_union = FALSE) |>
          sf::st_cast("LINESTRING"),
        layer_id = "track",
        stroke_width = 1000,
        width_min_pixels = 5,
        width_max_pixels = 10,
        # NEED TO MAKE THIS DYNAMIC
        tooltip = "tid",
        auto_highlight = TRUE,
        highlight_colour = "#FF000095",
        update_view = FALSE,
        stroke_colour = "#E0FFFF80"
      )
  }

  m <-
    m |>
    mapdeck::add_scatterplot(
      data = data,
      fill_colour = "speed",
      radius = radius,
      palette = "inferno"
    )
  return(m)
}
