#' Check for fishing trips that cross calendar years
#'
#' @description
#' Given an eflalo data.frame (with date/time columns for departure and landing), warns if any trips start in one year and end in another.
#' Adds combined POSIXct timestamp columns if not already present, using `rb_create_timestamp`.
#' Uses `rb_check_crosses_year` to detect cross-year trips.
#'
#' @param eflalo A data.frame containing at least FT_DDAT, FT_DTIME, FT_LDAT, FT_LTIME columns (date/time for departure/landing).
#'
#' @return Invisibly returns the eflalo data.frame (with possible new columns).
#' @details
#' Trips that start in one year and end in another can cause problems in downstream functions (e.g., splitAmongPings).
#' This function warns the user and suggests splitting such trips.
#'
#' @examples
#' eflalo <- data.frame(
#'   FT_DDAT = c("31/12/2022", "01/01/2023"),
#'   FT_DTIME = c("23:50:00", "10:00:00"),
#'   FT_LDAT = c("01/01/2023", "01/01/2023"),
#'   FT_LTIME = c("00:30:00", "12:00:00")
#' )
#' dc_check_eflalo_crosses_year(eflalo)
#'
#' # Example with a trip crossing year
#' eflalo2 <- data.frame(
#'   FT_DDAT = "31/12/2024",
#'   FT_DTIME = "23:59:00",
#'   FT_LDAT = "01/01/2025",
#'   FT_LTIME = "00:30:00"
#' )
#' dc_check_eflalo_crosses_year(eflalo2)
#'
#' # Example using UTC and ISO format
#' eflalo3 <- data.frame(
#'   FT_DDAT = "2024-12-31",
#'   FT_DTIME = "23:59:59",
#'   FT_LDAT = "2025-01-01",
#'   FT_LTIME = "00:00:01"
#' )
#' dc_check_eflalo_crosses_year(eflalo3)
#'
#' @export
dc_check_eflalo_crosses_year <- function(eflalo) {
  # Input checks
  if (!is.data.frame(eflalo)) stop("'eflalo' must be a data.frame.")
  required_cols <- c("FT_DDAT", "FT_DTIME", "FT_LDAT", "FT_LTIME")
  missing_cols <- setdiff(required_cols, colnames(eflalo))
  if (length(missing_cols) > 0)
    stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
  
  # Add (if necessary) combined POSIXct columns using rb_create_timestamp
  if (!"FT_DDATIM" %in% colnames(eflalo)) {
    eflalo$FT_DDATIM <- rb_create_timestamp(eflalo$FT_DDAT, eflalo$FT_DTIME)
  }
  if (!"FT_LDATIM" %in% colnames(eflalo)) {
    eflalo$FT_LDATIM <- rb_create_timestamp(eflalo$FT_LDAT, eflalo$FT_LTIME)
  }
  
  # Check for trips that cross the year using rb_check_crosses_year
  crosses_year <- rb_check_crosses_year(eflalo$FT_DDATIM, eflalo$FT_LDATIM)
  if (any(crosses_year, na.rm = TRUE)) {
    warning("There are trips that cross the year. This is not accounted for in splitAmongPings. Consider splitting those trips into half")
  }
  invisible(eflalo)
}