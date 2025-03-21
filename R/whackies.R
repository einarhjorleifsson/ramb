# todo If time step is very very small one may get high derived speedn




#' A wrapper around approx
#'
#' @param x A vector, nomally time
#' @param y The value to interpolate
#'
#' @return A vector with orgiginal and interpolated values, if any
#' @export
#'
rb_interpolate <- function(x, y) {
  stats::approx(x, y, x, method = "linear", rule = 1, f = 0, ties = mean)$y
}


#' Yet another speed filter
#' 
#' This function work like a pacman, chopping sequentially off the 
#' first encountered whack.
#' 
#' To use this on multiple trips, use a grouped data frame with 
#' tidyverse code like 
#' data |>  group_by(id) |> mutate(speed = track_speed(lon, lat, time))
#' 
#' The function uses traipse::track_speed, which by convention the first 
#' value is set to NA missing value, because the difference applies to 
#' each sequential pair of locations. Here, these missing values are
#' replaced by zero.
#' 
#' @param lon longitude in decimal degrees
#' @param lat latitude in decimal degrees
#' @param time date-time in POSIXct
#' @param kn_max speed threshold in knots
#'
#' @return a boolean vector, TRUE if point classified as whacky
#' @export
#'
rb_whacky_speed <- function(lon, lat, time, kn_max = 25) {
  
  if(length(lon) != length(lat) | length(lon) != length(time)) {
    stop("Length of coordinates and time must be the same")
  }
  
  ms_max <- rb_kn2ms(kn_max)
  .rid_original <- 1:length(lon)
  
  d <- 
    tibble::tibble(time = time,
                   x = lon, 
                   y = lat) |> 
    dplyr::mutate(.rid = 1:dplyr::n(),
                  speed = traipse::track_speed(x, y, time),
                  speed = tidyr::replace_na(speed, 0))
  
  while(any(d$speed > ms_max, na.rm = TRUE)) {
    a_whack <-
      d |> 
      dplyr::filter(speed > ms_max) |> 
      dplyr::slice(1) |> 
      dplyr::pull(.rid)
    d <- 
      d |> 
      dplyr::filter(.rid != a_whack) |>  
      dplyr::mutate(speed = traipse::track_speed(x, y, time),
                    speed = tidyr::replace_na(speed, 0))
  }
  
  x <- ifelse(.rid_original %in% d$.rid, FALSE, TRUE)
  return(x)
  
}


#' Title
#'
#' @param d An sf-dataframe contain lon, lat and time
#' @param filter Drop whacky points
#' @param max_speed Criteria for drops, units in knots
#'
#' @return a dataframe with addition variable ".whacky"
#' @export
#'
rb_whacky_speed_trip <- function(d, filter = TRUE, max_speed = 20) {
  tr <- methods::as(d |> dplyr::mutate(.idtrip = 1), "Spatial")
  tr <- suppressWarnings( trip::trip(tr, c("time", ".idtrip")) )
  d$.whacky <- !trip::speedfilter(tr, max.speed = ramb::rb_kn2ms(max_speed) / 1000 * 60 * 60)
  if(filter) {
    d |> 
      dplyr::filter(!.whacky) |> 
      dplyr::select(-.whacky)
  }
  # some extra precaution - would be of interest why not capured above
  if(filter) {
    d <-
      d |> 
      dplyr::mutate(.whacky = ramb::rb_whacky_speed(lon, lat, time)) |> 
      dplyr::filter(!.whacky) |> 
      dplyr::select(-.whacky)
  }
  
  return(d)
}
