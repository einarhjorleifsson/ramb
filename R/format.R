#' Create timestamp from date and time character columns
#'
#' @param date Name of the date variable
#' @param time Name of the time variable
#' @param tz Time zone (default GMT)
#' @param format time output format (default
#'
#' @return A vector of same length as date and time
#' @export
#'
rb_create_timestamp <- function(date, time, tz = "GMT", format = "%d/%m/%Y  %H:%M:%S") {
  as.POSIXct(paste(date, time, sep = " "),
             tz = tz,
             format = format)
}

rb_format_logbooks <- function() {

}

rb_format_vms <- function() {

}
