# todo If time step is very very small one may get high derived speed

#' rb_whacky_speed
#'
#' Marks derived speed (units in m per s) above a certain criterion as whacky (TRUE,
#' rest will be FALSE).
#' 
#' To use this on multiple vessels or trips, use a grouped data frame with 
#' tidyverse code 
#' like ...
#'
#' @param lon longitude
#' @param lat latitude
#' @param time date-time in POSIXct
#' @param criteria speed in meters per second
#'
#' @return A boolean vector
#' @export
#'
rb_whacky_speed <- function(lon, lat, time, criteria = rb_kn2ms(25)) {
  x1 <- traipse::track_speed(lon, lat, time)
  x2 <- dplyr::lead(x1)
  dplyr::if_else(x1 > criteria & (x2 > criteria | is.na(x2)), TRUE, FALSE, TRUE)
}



