# todo If time step is very very small one may get high derived speed

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
#' @param d A dataframe contain lon, lat and time
#'
#' @return a dataframe with addition variable ".whacky"
#' @export
#'
rb_whacky_speed_trip <- function(d) {
  tr <- d |> dplyr::mutate(id = 1) |> dplyr::select(lon, lat, time, id, dplyr::everything())
  sp::coordinates(tr) <- ~lon+lat
  sp::proj4string(tr) <- sp::CRS("+proj=longlat +datum=WGS84", doCheckCRSArgs = FALSE)
  suppressWarnings(tr <- trip::trip(tr, c("time", "id")))
  d$.whacky <- !trip::speedfilter(tr, max.speed = ramb::rb_kn2ms(20) / 1000 * 60 * 60)
  return(d)
}
