# Compare track_time vs rb_time (rb_calc_step_time)

#' Time between steps, forcing users to explicitly specify the unit
#'
#' @param time time
#' @param units units of return value
#'
#' @return A numeric vector
#' @export
#'
#'
rb_calc_step_time <- function(time, units = "secs")  {
  difftime(time, dplyr::lag(time), units = units) |> 
    as.numeric()
}

#' Calculate step acceleration
#' 
#' To use this on multiple vessels or trips, use a grouped data frame with tidyverse code like ... 
#'
#' @param lon longitude
#' @param lat latitude
#' @param time time in POSIXct
#'
#' @return A numerical vector
#' @export
#'
rb_calc_step_acceleration <- function(lon, lat, time) {
  .rb_track_speed(lon, lat, time) / .rb_track_time(time)
}
