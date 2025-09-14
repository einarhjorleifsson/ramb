#' Create timestamp from date and time character columns
#'
#' This function combines separate date and time character columns into a POSIXct timestamp.
#' It allows specifying the time zone and the expected date-time format. The function checks for leap day (29/02) validity.
#'
#' @param date Character vector or column containing dates (e.g. "2022-12-31" or "31/12/2022").
#' @param time Character vector or column containing times (e.g. "23:59:59").
#' @param tz Time zone to use for the output (default "UTC").
#' @param format Expected date-time format (default "%d/%m/%Y %H:%M:%S").
#'
#' @return A POSIXct vector of the same length as `date` and `time`, or NA where conversion fails.
#' @examples
#' # Using default format "%d/%m/%Y %H:%M:%S"
#' rb_create_timestamp("14/09/2025", "15:30:00")
#' rb_create_timestamp(c("01/01/2020", "31/12/2021"), c("00:00:00", "23:59:59"))
#'
#' # Example using ISO 8601 (UTC) date and time format
#' rb_create_timestamp("2025-09-14", "12:01:57", format = "%Y-%m-%d %H:%M:%S")
#'
#' # Example with different time zone
#' rb_create_timestamp("14/09/2025", "15:30:00", tz = "Europe/Reykjavik")
#'
#' # Failing example (wrong date-time format)
#' rb_create_timestamp("2025-09-14", "12:01:57")
#' # Returns NA with a warning
#'
#' # Failing example (mismatched vector lengths)
#' rb_create_timestamp(c("14/09/2025", "15/09/2025"), "15:30:00")
#'
#' # Failing leap day example
#' rb_create_timestamp("29/02/2023", "12:00:00")
#' # Returns NA with a warning about invalid leap day
#'
#' @export
rb_create_timestamp <- function(date, time, tz = "UTC", format = "%d/%m/%Y %H:%M:%S") {
  # Input checks
  if (missing(date) || missing(time)) stop("Both 'date' and 'time' must be provided.")
  if (!is.character(date)) stop("'date' must be a character vector.")
  if (!is.character(time)) stop("'time' must be a character vector.")
  if (length(date) != length(time)) stop("'date' and 'time' must be the same length.")
  if (!is.character(tz) || length(tz) != 1) stop("'tz' must be a single character string.")
  if (!is.character(format) || length(format) != 1) stop("'format' must be a single character string.")
  
  x <- paste(date, time)
  ts <- as.POSIXct(x, tz = tz, format = format)
  
  # Leap day check (for format "%d/%m/%Y" or similar)
  leap_day_issues <- rep(FALSE, length(date))
  # Try to extract the date part, robust for different formats
  if (grepl("%d/%m/%Y", format, fixed = TRUE)) {
    day_month_year <- regmatches(date, regexec("^(\\d{2})/(\\d{2})/(\\d{4})$", date))
    for (i in seq_along(day_month_year)) {
      dmy <- day_month_year[[i]]
      if (length(dmy) == 4) {
        dd <- as.integer(dmy[2])
        mm <- as.integer(dmy[3])
        yyyy <- as.integer(dmy[4])
        # Check for 29/02 in non-leap year
        if (dd == 29 && mm == 2) {
          is_leap <- (yyyy %% 4 == 0 & yyyy %% 100 != 0) | (yyyy %% 400 == 0)
          if (!is_leap) {
            leap_day_issues[i] <- TRUE
            ts[i] <- NA
          }
        }
      }
    }
    if (any(leap_day_issues)) {
      warning("Some dates had 29/02 on non-leap years and were set to NA.")
    }
  }
  
  if (any(is.na(ts))) {
    warning("Some date-time combinations could not be converted. Check your format: ", format)
  }
  ts
}