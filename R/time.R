#' Add a timestamp from a date and a time column
#'
#' Combines a column of dates and a column of times, both character, into one `POSIXct` column, warning where the
#' conversion fails; an impossible leap day (29 February in a non-leap year, with a `%d/%m/%Y` date format) is set
#' to `NA`.
#'
#' @param data A data frame.
#' @param date,time The columns of `data` that hold the dates and the times, as character.
#' @param name The name of the new column, default `"time"`. An existing column is never overwritten.
#' @param tz The time zone of the result, default `"GMT"`.
#' @param format The date-time format, default `"%d/%m/%Y %H:%M:%S"`.
#'
#' @return `data` with the column `name` added, `NA` where the conversion fails.
#'
#' @examples
#' d <- data.frame(date = c("14/09/2025", "29/02/2023"), clock = c("15:30:00", "12:00:00"))
#' rb_add_timestamp(d, date, clock)
#' rb_add_timestamp(d, date, clock, name = "departure", tz = "Europe/Reykjavik")
#' iso <- data.frame(date = "2025-09-14", clock = "12:01:57")
#' rb_add_timestamp(iso, date, clock, format = "%Y-%m-%d %H:%M:%S")
#' try(rb_add_timestamp(iso, date, clock))
#'
#' @export
rb_add_timestamp <- function(
  data,
  date,
  time,
  name = "time",
  tz = "GMT",
  format = "%d/%m/%Y %H:%M:%S"
) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame; collect a lazy table first.",
      call. = FALSE
    )
  }
  if (missing(date) || missing(time)) {
    stop("Both `date` and `time` must be given.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1) {
    stop("`name` must be a single character string.", call. = FALSE)
  }
  if (name %in% names(data)) {
    stop(
      "column `",
      name,
      "` already exists; choose another `name`",
      call. = FALSE
    )
  }
  data[[name]] <- .rb_timestamp(
    dplyr::pull(data, {{ date }}),
    dplyr::pull(data, {{ time }}),
    tz = tz,
    format = format
  )
  data
}

# The vector body of rb_add_timestamp(), and of the alias rb_create_timestamp().
.rb_timestamp <- function(
  date,
  time,
  tz = "GMT",
  format = "%d/%m/%Y %H:%M:%S"
) {
  # Input checks
  if (!is.character(date)) {
    stop("'date' must be a character vector.", call. = FALSE)
  }
  if (!is.character(time)) {
    stop("'time' must be a character vector.", call. = FALSE)
  }
  if (length(date) != length(time)) {
    stop("'date' and 'time' must be the same length.", call. = FALSE)
  }
  if (!is.character(tz) || length(tz) != 1) {
    stop("'tz' must be a single character string.", call. = FALSE)
  }
  if (!is.character(format) || length(format) != 1) {
    stop("'format' must be a single character string.", call. = FALSE)
  }

  x <- paste(date, time)
  ts <- as.POSIXct(x, tz = tz, format = format)

  # Leap day check (for format "%d/%m/%Y" or similar)
  leap_day_issues <- rep(FALSE, length(date))
  if (grepl("%d/%m/%Y", format, fixed = TRUE)) {
    day_month_year <- regmatches(
      date,
      regexec("^(\\d{2})/(\\d{2})/(\\d{4})$", date)
    )
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
      warning(
        "Some dates had 29/02 on non-leap years and were set to NA.",
        call. = FALSE
      )
    }
  }

  if (any(is.na(ts))) {
    warning(
      "Some date-time combinations could not be converted. Check your format: ",
      format,
      call. = FALSE
    )
  }
  ts
}
#' Duration between two datetimes
#'
#' Computes the elapsed time between two datetime columns
#' (or vectors), returned as a plain numeric vector in the requested `unit`.
#' It works identically on three kinds of input:
#'
#' - **A lazy table** (e.g. a `dbplyr`/DuckDB table): a column is added in the
#'   database, translated to SQL `(epoch(to) - epoch(from)) * scale`, so the
#'   calculation never leaves the backend.
#' - **A data frame or tibble**: a column is added locally via
#'   [base::difftime()].
#' - **A `POSIXt` vector** (e.g. inside `dplyr::mutate()` on a local data
#'   frame): the duration vector is returned directly. In this form the second
#'   positional argument is the end time.
#'
#' For table inputs the function refuses to overwrite an existing column:
#' if `name` is already present, it aborts. Choose another `name` in that
#' case. The unit is carried by the value, not by a column-name prefix or a
#' `difftime` attribute, so the result is always unitless numeric.
#'
#' Note that the vector form cannot be used inside `dplyr::mutate()` on a
#' *lazy* table (dbplyr would pass the unknown call through to SQL); use the
#' table form there.
#'
#' @param x A data frame, tibble, lazy table (`tbl_lazy`), or a `POSIXt`
#'   vector of start times.
#' @param from Unquoted name of the column holding the start time (table
#'   form only).
#' @param to Unquoted name of the column holding the end time (table form),
#'   or a `POSIXt` vector of end times (vector form).
#' @param unit A single string: one of `"secs"`, `"mins"`, `"hours"` or
#'   `"days"`. Default: `"hours"`.
#' @param name A single string naming the column to create (table form only).
#'   Default: `"duration"`.
#'
#' @return For table inputs, the input with one column added. For vector
#'   input, a numeric vector of durations in `unit`.
#'
#' @examples
#' # Vector form (also usable inside dplyr::mutate on a local data frame)
#' t1 <- as.POSIXct("2007-06-04 14:03:36", tz = "UTC")
#' t2 <- as.POSIXct("2007-06-07 05:25:45", tz = "UTC")
#' rb_calc_duration(t1, t2)                  # 63.37 hours
#' rb_calc_duration(t1, t2, unit = "mins")   # 3802.15 minutes
#'
#' # Table form
#' d <- tibble::tibble(T1 = t1, T2 = t2)
#' rb_calc_duration(d, T1, T2)
#' rb_calc_duration(d, T1, T2, unit = "days", name = "duration_days")
#'
#' # Refuses to overwrite an existing column
#' try(rb_calc_duration(d, T1, T2, name = "T1"))
#'
#' @export
rb_calc_duration <- function(x, from, to, unit = "hours", name = "duration") {
  if (inherits(x, "POSIXt")) {
    # vector form: second positional argument is the end time
    end <- if (missing(to)) from else to
    return(as.numeric(difftime(time1 = end, time2 = x, units = unit)))
  }
  # table form: never overwrite an existing column
  if (name %in% colnames(x)) {
    stop("column `", name, "` already exists; choose another `name`")
  }
  if (inherits(x, "tbl_lazy")) {
    scale <- switch(
      unit,
      secs = 1,
      mins = 1 / 60,
      hours = 1 / 3600,
      days = 1 / 86400,
      stop("`unit` must be one of 'secs', 'mins', 'hours', 'days'")
    )
    dplyr::mutate(x, !!name := (epoch({{ to }}) - epoch({{ from }})) * scale)
  } else {
    dplyr::mutate(
      x,
      !!name := rb_calc_duration({{ from }}, to = {{ to }}, unit = unit)
    )
  }
}
#' Detect intervals that cross a year boundary
#'
#' For each pair of datetimes, whether the interval between them crosses a
#' calendar year boundary.
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
#' rb_detect_crosses_year(dt1, dt2)
#' # Returns: TRUE FALSE
#'
#' # Example 2: Date class, different years
#' dt1 <- as.Date(c("2024-12-31", "2025-01-01"))
#' dt2 <- as.Date(c("2025-01-01", "2025-01-01"))
#' rb_detect_crosses_year(dt1, dt2)
#' # Returns: TRUE FALSE
#'
#' # Example 3: POSIXct, same year
#' dt1 <- as.POSIXct("2025-06-01 12:00:00", tz = "UTC")
#' dt2 <- as.POSIXct("2025-12-31 23:59:59", tz = "UTC")
#' rb_detect_crosses_year(dt1, dt2)
#' # Returns: FALSE
#'
#' # Example 4: Error if timezones differ
#' \dontrun{
#' dt1 <- as.POSIXct("2024-12-31 23:59:59", tz = "UTC")
#' dt2 <- as.POSIXct("2025-01-01 00:00:01", tz = "Europe/Oslo")
#' rb_detect_crosses_year(dt1, dt2)
#' }
#'
#' # Example 5: Error if types differ
#' \dontrun{
#' dt1 <- as.Date("2024-12-31")
#' dt2 <- as.POSIXct("2025-01-01 00:00:01", tz = "UTC")
#' rb_detect_crosses_year(dt1, dt2)
#' }
#'
#' @export
rb_detect_crosses_year <- function(datetime1, datetime2) {
  # Check same class
  if (!inherits(datetime1, class(datetime2))) {
    stop("datetime1 and datetime2 must be of the same class.")
  }
  # For POSIXct, check timezones
  if (inherits(datetime1, "POSIXt")) {
    tz1 <- attr(datetime1, "tzone")
    tz2 <- attr(datetime2, "tzone")
    # If tzone not set, default to ""
    if (is.null(tz1)) {
      tz1 <- ""
    }
    if (is.null(tz2)) {
      tz2 <- ""
    }
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
#' Check for fishing trips that cross calendar years
#'
#' Warns if any trip in an eflalo table starts in one year and lands in
#' another: `vmstools::splitAmongPings()` does not handle such trips, so they
#' should be split first. Departure and landing timestamps are added where
#' missing, as [rb_add_timestamp()] adds them; the check is
#' [rb_detect_crosses_year()].
#'
#' @param eflalo A data frame with `FT_DDAT`, `FT_DTIME`, `FT_LDAT` and
#'   `FT_LTIME` (departure and landing date and time), or the combined
#'   `FT_DDATIM` and `FT_LDATIM`.
#'
#' @return `eflalo`, invisibly, with the timestamp columns added where they
#'   were missing.
#'
#' @examples
#' eflalo <- data.frame(
#'   FT_DDAT = c("31/12/2022", "01/01/2023"),
#'   FT_DTIME = c("23:50:00", "10:00:00"),
#'   FT_LDAT = c("01/01/2023", "01/01/2023"),
#'   FT_LTIME = c("00:30:00", "12:00:00")
#' )
#' rb_check_trip_crosses_year(eflalo)
#'
#' # Example with a trip crossing year
#' eflalo2 <- data.frame(
#'   FT_DDAT = "31/12/2024",
#'   FT_DTIME = "23:59:00",
#'   FT_LDAT = "01/01/2025",
#'   FT_LTIME = "00:30:00"
#' )
#' rb_check_trip_crosses_year(eflalo2)
#'
#' # Example using UTC and ISO format
#' eflalo3 <- data.frame(
#'   FT_DDAT = "2024-12-31",
#'   FT_DTIME = "23:59:59",
#'   FT_LDAT = "2025-01-01",
#'   FT_LTIME = "00:00:01"
#' )
#' rb_check_trip_crosses_year(eflalo3)
#'
#' @export
rb_check_trip_crosses_year <- function(eflalo) {
  # Input checks
  if (!is.data.frame(eflalo)) {
    stop("'eflalo' must be a data.frame.")
  }
  required_cols <- c("FT_DDAT", "FT_DTIME", "FT_LDAT", "FT_LTIME")
  missing_cols <- setdiff(required_cols, colnames(eflalo))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Add (if necessary) combined POSIXct columns, as rb_add_timestamp() does
  if (!"FT_DDATIM" %in% colnames(eflalo)) {
    eflalo$FT_DDATIM <- .rb_timestamp(eflalo$FT_DDAT, eflalo$FT_DTIME)
  }
  if (!"FT_LDATIM" %in% colnames(eflalo)) {
    eflalo$FT_LDATIM <- .rb_timestamp(eflalo$FT_LDAT, eflalo$FT_LTIME)
  }

  # Check for trips that cross the year using rb_detect_crosses_year
  crosses_year <- rb_detect_crosses_year(eflalo$FT_DDATIM, eflalo$FT_LDATIM)
  if (any(crosses_year, na.rm = TRUE)) {
    warning(
      "There are trips that cross the year. This is not accounted for in splitAmongPings. Consider splitting those trips into half"
    )
  }
  invisible(eflalo)
}
