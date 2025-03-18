#' Track time duration
#' 
#' Calculate time duration based on sequential difference of date-time input. 
#' The unit of time duration is seconds.
#' 
#' Function is a modification of traipse::track_time allowing for calculation
#' of time from previous points rather than by convention the time to next
#' point. It also allows for puting some weighing a.la. the datacall::intvTacsat
#' function.
#' 
#' @param date date-time in POSIXct
#' @param weight A numerical vector of length 2 weight to apply to calculation
#' of mean interval rate towards and away from ping
#' @param fill Boolean (default TRUE)
#'
#' @return A vector of duration in seconds
#' @export
#'
rb_track_time <- function (date, weight = c(0, 1), fill = TRUE) {
  
  # tests
  cls <- class(date)
  if (!inherits(date, "POSIXct")) {
    date <- try(as.POSIXct(date), silent = TRUE)
    if (inherits(date, "try-error")) {
      stop(sprintf("Cannot convert 'date' of class '%s' to POSIXct", 
                   paste(cls, collapse = ",")))
    }
  }
  # Check if 'weight' is a length 2 numeric vector
  if (length(weight) != 2) 
    stop("weight must be specified as a length 2 numeric vector")
  
  # Normalize 'weight' to sum to 1
  weight <- weight/sum(weight, na.rm = TRUE)
  
  if (any(weight == 0)) {
    if (weight[1] == 0) {
      int <- c(NA_real_, diff(unclass(date)))
      if(fill) int[1] <- int[2]
    }
    if (weight[2] == 0) {
      int <- c(diff(unclass(date)), NA_real_)
      if(fill) {
        l <- length(int)
        int[l] <- int[l-1]
      }
    }
  } else {
    int <- rowSums(cbind(c(NA_real_, diff(unclass(date))) * weight[1], c(diff(unclass(date)), NA_real_) * weight[2]))
    if(fill) {
      l <- length(int)
      int[1] <- int[2]
      int[l] <- int[l-1]
    }
  }
  
  return(int)
}
