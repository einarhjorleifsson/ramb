#' Calculate the Duration Between Two Datetimes
#'
#' `rb_calc_duration()` computes the elapsed time between two datetime columns
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
