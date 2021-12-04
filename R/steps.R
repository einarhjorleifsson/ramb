# Compare track_time vs rb_time (rb_st)

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