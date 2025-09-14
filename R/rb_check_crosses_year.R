#' Check if Any Datetime Pairs Cross Year Boundary
#'
#' This function checks, for each pair of datetimes in two vectors, if the interval between them crosses a calendar year boundary.
#'
#' @param datetime1 A vector of POSIXct or Date objects. The start times.
#' @param datetime2 A vector of POSIXct or Date objects. The end times.
#'
#' @return A logical (boolean) vector of the same length as `datetime1` and `datetime2`, indicating for each pair if the interval crosses a year boundary.
#'
#' @details
#' - The function checks that both vectors are of the same class and have the same timezone (for POSIXct).
#' - It returns `TRUE` for a given pair if `datetime1` and `datetime2` are in different calendar years.
#' - If the two vectors are not the same class or timezone, an error is thrown.
#'
#' @examples
#' # Example 1: POSIXct, different years
#' dt1 <- as.POSIXct(c("2024-12-31 23:59:59", "2024-01-01 00:00:00"), tz = "UTC")
#' dt2 <- as.POSIXct(c("2025-01-01 00:00:01", "2024-12-31 23:00:00"), tz = "UTC")
#' rb_check_crosses_year(dt1, dt2)
#' # Returns: TRUE FALSE
#'
#' # Example 2: Date class, different years
#' dt1 <- as.Date(c("2024-12-31", "2025-01-01"))
#' dt2 <- as.Date(c("2025-01-01", "2025-01-01"))
#' rb_check_crosses_year(dt1, dt2)
#' # Returns: TRUE FALSE
#'
#' # Example 3: POSIXct, same year
#' dt1 <- as.POSIXct("2025-06-01 12:00:00", tz = "UTC")
#' dt2 <- as.POSIXct("2025-12-31 23:59:59", tz = "UTC")
#' rb_check_crosses_year(dt1, dt2)
#' # Returns: FALSE
#'
#' # Example 4: Error if timezones differ
#' \dontrun{
#' dt1 <- as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
#' dt2 <- as.POSIXct("2025-01-01 00:00:01", tz = "Europe/Oslo")
#' rb_check_crosses_year(dt1, dt2)
#' }
#'
#' # Example 5: Error if types differ
#' \dontrun{
#' dt1 <- as.Date("2024-12-31")
#' dt2 <- as.POSIXct("2025-01-01 00:00:01", tz = "UTC")
#' rb_check_crosses_year(dt1, dt2)
#' }
#'
#' @export
rb_check_crosses_year <- function(datetime1, datetime2) {
  # Check same class
  if (!inherits(datetime1, class(datetime2))) {
    stop("datetime1 and datetime2 must be of the same class.")
  }
  # For POSIXct, check timezones
  if (inherits(datetime1, "POSIXt")) {
    tz1 <- attr(datetime1, "tzone")
    tz2 <- attr(datetime2, "tzone")
    # If tzone not set, default to ""
    if (is.null(tz1)) tz1 <- ""
    if (is.null(tz2)) tz2 <- ""
    if (!identical(tz1, tz2)) {
      stop("datetime1 and datetime2 must have the same timezone.")
    }
  }
  # Check length
  if (length(datetime1) != length(datetime2)) {
    stop("datetime1 and datetime2 must be of the same length.")
  }
  # Extract years
  year1 <- as.integer(format(datetime1, "%Y"))
  year2 <- as.integer(format(datetime2, "%Y"))
  # Return logical vector
  return(year1 != year2)
}