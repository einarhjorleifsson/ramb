#' Plot a vessel track on an interactive map
#'
#' The pings as points coloured by a variable, optionally with the track as
#' a path underneath.
#'
#' @param d A track tibble with `lon` and `lat` (WGS 84), or an sf object.
#' @param col The column that controls the point colour, default `"speed"`.
#' @param tooltip The column shown as the tooltip, default `"speed"`.
#' @param path Logical; if `TRUE`, the track is drawn as a path under the
#'   points (default `FALSE`).
#' @param radius The point radius in metres, default 400.
#' @param highlight_colour,stroke_colour The colours of the path.
#'
#' @return A [mapdeck::mapdeck()] map.
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
#' Plot a vessel's trips on an interactive map
#'
#' The pings as points (coloured by `speed`), with a path per trip
#' underneath.
#'
#' @param data A track tibble with `lon` and `lat` (WGS 84) and `speed`, or
#'   an sf object.
#' @param tid The trip id column.
#' @param radius The point radius in metres, default 10.
#' @param col Reserved; the points are coloured by `speed`.
#' @param trip Logical; if `TRUE` (default), a path per trip is drawn.
#'
#' @return A [mapdeck::mapdeck()] map.
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
