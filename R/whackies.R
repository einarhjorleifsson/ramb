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
  approx(x, y, x, method = "linear", rule = 1, f = 0, ties = mean)$y
}
# fix this in ramb - expect that the tibble is sf


#' rb_whacky_speed
#'
#' Marks derived speed (unit knots) above a certain criterion as whacky (TRUE,
#' rest will be FALSE).
#' 
#' To use this on multiple vessels or trips, use a grouped data frame with 
#' tidyverse code 
#' like ...
#'
#' @param lon longitude
#' @param lat latitude
#' @param time date-time in POSIXct
#' @param criteria speed in nautical miles per hour
#'
#' @return A boolean vector
#' @export
#'
rb_whacky_speed <- function(lon, lat, time, criteria = 20) {
  n <- length(lon)
  if(n >= 2) {
    x1 <- rb_speed(lon, lat, time)
    # first point has undefined speed
    x1[1] <- x1[2]
    x2 <- dplyr::lead(x1)
    x2[length(x2)] <- x2[length(x2) - 1]
    x <- ifelse(x1 > criteria & x2 > criteria, TRUE, FALSE)
  } else {
    x <- rep(NA, n)
  }
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
  tr <- as(d |> mutate(.idtrip = 1), "Spatial")
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
