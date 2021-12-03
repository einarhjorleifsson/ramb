#' Distance between steps
#' 
#' @description Calculate distance from previous point or to next point. 
#' Insired by traipse::track_distance but simplified and allows to pass
#' additional arguement to geodist::geodist.
#' 
#'
#' @param x longitude
#' @param y latitude
#' @param ... additional arguements to geodist
#'
#' @return A distance vector in meters.
#' @export
#' 
rb_sd <- function (x, y, ...) 
{
  x <- 
    geodist::geodist(cbind(x, y), sequential = TRUE, pad = TRUE, ...)
  return(x)
}

#' Time between steps, forcing users to explicitly specify the unit
#'
#' @param time time
#' @param units units of return value
#'
#' @return A numeric vector
#' @export
#'
#'
rb_st <- function(time, units = "secs")  {
  difftime(time, dplyr::lag(time), units = units) %>% 
    as.numeric()
}