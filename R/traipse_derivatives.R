#' Track speed
#' 
#' @description Calculates speed in nautical miles per hour. Based on 
#' traipse::track_speed but returns knots not meters per second and sets the 
#' first value as the second value. If the length of the vector is one, 
#' na values are automatically returned (rather than an error returned 
#' by traipse::track_speed)
#' 
#'
#' @param lon longitude
#' @param lat latitude
#' @param time datetime (POSIXct)
#'
#' @return numerical vector in nautical miles per hour
#' @export
#'
rb_speed <- function(lon, lat, time) {
  n <- length(lon)
  if(n >= 2) {
    x <- traipse::track_speed(lon, lat, time) |> rb_ms2kn()
    x[1] <- x[2]
  } else {
    x <- rep(NA_real_, n)
  }
  return(x)
}
