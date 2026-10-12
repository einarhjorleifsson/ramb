#' Share Catch and Value from Events to Trails Using Hierarchical Allocation
#'
#' Allocates event weights (`LE_KG`) and values (`LE_EURO`) from the `events` data frame
#' to corresponding fishing trails in the `trails` data frame using a three-step hierarchical
#' matching and allocation procedure (by day, ICES rectangle, and trip). This ensures
#' that catches and values are distributed as precisely as possible, with unmatched allocations
#' handled at progressively broader levels.
#'
#' @section Hierarchical Allocation Procedure:
#' \enumerate{
#'   \item \strong{Date Level}:
#'     \itemize{
#'       \item Events with defined ICES rectangle and date are spread among matching pings (`state == 1`).
#'       \item Unmatched pings proceed to the next level.
#'     }
#'   \item \strong{ICES Rectangle Level}:
#'     \itemize{
#'       \item Remaining events with defined ICES rectangle (but missing catch date) are spread among remaining pings, if available.
#'       \item Unmatched pings proceed to the next level.
#'     }
#'   \item \strong{Trip Level}:
#'     \itemize{
#'       \item Remaining events with only a trip id are spread among any remaining pings.
#'       \item Pings already allocated are not reallocated.
#'     }
#' }
#'
#' \strong{Note:} Two further allocation steps are planned but not yet implemented
#' (see `raise_unallocated` and `raise_unmatched`).
#'
#' @param trails A data frame of fishing trail records. Must contain:
#'   \describe{
#'     \item{.pid}{integer, unique trail identifier}
#'     \item{VE_COU}{character, vessel country}
#'     \item{VE_REF}{character, vessel reference}
#'     \item{time}{POSIXct, UTC timestamp}
#'     \item{FT_REF}{character, fishing trip reference}
#'     \item{ir}{character, ICES rectangle}
#'     \item{state}{integer, should be 0 or 1}
#'   }
#' @param events A data frame of landing event records. Must contain:
#'   \describe{
#'     \item{.eid}{integer, unique event identifier}
#'     \item{VE_COU}{character, vessel country}
#'     \item{VE_REF}{character, vessel reference}
#'     \item{FT_REF}{character, fishing trip reference}
#'     \item{date}{Date, event date}
#'     \item{LE_RECT}{character, ICES rectangle}
#'     \item{LE_KG}{numeric, landed weight in kg}
#'     \item{LE_EURO}{numeric, landed value in euro}
#'   }
#' @param remove_diagnostic Logical. If TRUE (default), diagnostic columns are removed from output.
#' @param raise_unallocated Logical (default FALSE). Reserved: raise trips whose events were
#'   only partly allocated, so the full event total is shared (not yet implemented).
#' @param raise_unmatched Logical (default FALSE). Reserved: share events whose trip has no
#'   match in `trails` at a broader level, e.g. vessel by month (not yet implemented).
#'
#' @details
#' The function spreads catch and value information through three hierarchical steps.
#' At each step, allocations are only made to pings not already allocated at a finer level.
#' Only `state == 1` pings are eligible for allocation.
#'
#' @return
#' A data frame matching the number of rows in `trails`, with added columns:
#'   \describe{
#'     \item{LE_KG}{numeric, allocated landed weight in kg}
#'     \item{LE_EURO}{numeric, allocated landed value in euro}
#'     \item{.how}{character, allocation step: "trip_rectangle_day", "trip_rectangle", or "trip"}
#'   }
#' Additional columns are included if `remove_diagnostic = FALSE`.
#'
#' @export
rb_share_catch <- function(
  pings,
  events,
  remove_diagnostic = TRUE,
  raise_unallocated = FALSE,
  raise_unmatched = FALSE
) {
  # Input checks ----
  # Check for required columns and correct data types in 'pings' and 'events' data frames
  required_pings <- c(
    ".pid",
    "VE_COU",
    "VE_REF",
    "time",
    "FT_REF",
    "ir",
    "state"
  )
  required_events <- c(
    ".eid",
    "VE_COU",
    "VE_REF",
    "FT_REF",
    "date",
    "LE_RECT",
    "LE_KG",
    "LE_EURO"
  )

  # Check for columns
  missing_pings <- setdiff(required_pings, colnames(pings))
  if (length(missing_pings) > 0) {
    stop(
      "Missing required columns in pings: ",
      paste(missing_pings, collapse = ", ")
    )
  }
  missing_events <- setdiff(required_events, colnames(events))
  if (length(missing_events) > 0) {
    stop(
      "Missing required columns in events: ",
      paste(missing_events, collapse = ", ")
    )
  }

  # Check classes for pings
  if (!is.integer(pings$.pid)) {
    stop(".pid in pings must be integer")
  }
  if (!is.character(pings$VE_COU)) {
    stop("VE_COU in pings must be character")
  }
  if (!is.character(pings$VE_REF)) {
    stop("VE_REF in pings must be character")
  }
  if (!inherits(pings$time, "POSIXct")) {
    stop("time in pings must be POSIXct/datetime")
  }
  if (!is.character(pings$FT_REF)) {
    stop("FT_REF in pings must be character")
  }
  if (!is.character(pings$ir)) {
    stop("ir in pings must be character")
  }
  if (!is.integer(pings$state)) {
    stop("state in pings must be integer")
  }
  if (!all(pings$state %in% c(0, 1))) {
    stop("state in pings must be 0 or 1")
  }

  # Check classes for events
  if (!is.integer(events$.eid)) {
    stop(".eid in events must be integer")
  }
  if (!is.character(events$VE_COU)) {
    stop("VE_COU in events must be character")
  }
  if (!is.character(events$VE_REF)) {
    stop("VE_REF in events must be character")
  }
  if (!is.character(events$FT_REF)) {
    stop("FT_REF in events must be character")
  }
  if (!inherits(events$date, "Date")) {
    stop("date in events must be Date")
  }
  if (!is.character(events$LE_RECT)) {
    stop("LE_RECT in events must be character")
  }
  if (!is.numeric(events$LE_KG)) {
    stop("LE_KG in events must be numeric")
  }
  if (!is.numeric(events$LE_EURO)) {
    stop("LE_EURO in events must be numeric")
  }

  # Prepare extra columns ----
  pings <- pings |> dplyr::mutate(date = lubridate::as_date(time))
  events <-
    events |>
    dplyr::mutate(state = 1L)

  # Mapping step ----
  # Determine at which allocation step each event should be joined
  # Create step_plan to assign events to allocation steps based on available keys
  step_plan <-
    events |>
    dplyr::select(.eid, VE_COU, VE_REF, FT_REF, date, LE_RECT) |>
    dplyr::left_join(
      pings |>
        dplyr::filter(state == 1) |>
        dplyr::select(VE_COU, VE_REF, FT_REF, date, LE_RECT = ir) |>
        dplyr::distinct() |>
        dplyr::mutate(vms = "yes"),
      by = dplyr::join_by(VE_COU, VE_REF, FT_REF),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      .step = dplyr::case_when(
        date.x == date.y & LE_RECT.x == LE_RECT.y & vms == "yes" ~ 1L,
        is.na(date.x) & LE_RECT.x == LE_RECT.y & vms == "yes" ~ 2L,
        vms == "yes" ~ 3L,
        .default = 9999L
      )
    ) |>
    dplyr::arrange(.step) |>
    dplyr::select(.eid, .step) |>
    dplyr::distinct(.eid, .keep_all = TRUE)

  events <-
    events |>
    dplyr::left_join(step_plan, by = dplyr::join_by(.eid))

  # Keep track of .pid that are not mapped after each step
  PID_left <- pings$.pid

  # Step 1: Join by trip + rectangle + day -------------------------------------
  # Allocate events to pings at the most specific (date & rectangle) level
  events1 <-
    events |>
    dplyr::filter(.step == 1) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, date, LE_RECT, state) |>
    dplyr::summarise(
      LE_KG = sum(LE_KG, na.rm = TRUE),
      LE_EURO = sum(LE_EURO, na.rm = TRUE),
      .groups = "drop"
    )

  pings1 <- pings |>
    dplyr::inner_join(
      events1,
      by = c("VE_COU", "VE_REF", "FT_REF", "ir" = "LE_RECT", "date", "state")
    ) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, ir, date) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = "1 trip_rectangle_day"
    ) |>
    dplyr::ungroup()

  PID_left <- PID_left[!PID_left %in% pings1$.pid]

  # Step 2: Join by trip + rectangle -------------------------------------------
  # Allocate remaining events to pings at the rectangle level

  pings_left <-
    pings |>
    dplyr::filter(.pid %in% PID_left)

  events2 <-
    events |>
    dplyr::filter(.step == 2L) |>
    # avoid many-to-many among other things
    dplyr::group_by(VE_COU, VE_REF, FT_REF, LE_RECT, state) |>
    dplyr::summarise(
      LE_KG = sum(LE_KG, na.rm = TRUE),
      LE_EURO = sum(LE_EURO, na.rm = TRUE),
      .groups = "drop"
    )

  pings2 <-
    pings_left |>
    dplyr::select(-date) |>
    dplyr::inner_join(
      events2,
      by = c("VE_COU", "VE_REF", "FT_REF", "ir" = "LE_RECT", "state")
    ) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, ir, state) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = "2 trip_rectangle"
    ) |>
    dplyr::ungroup()

  PID_left <- PID_left[!PID_left %in% pings2$.pid]

  # Step 3: Join by trip -------------------------------------------------------
  # Allocate remaining events to pings at the trip level

  pings_left <-
    pings |>
    dplyr::filter(.pid %in% PID_left)
  events3 <-
    events |>
    dplyr::filter(.step == 3L) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, state) |>
    dplyr::summarise(
      LE_KG = sum(LE_KG, na.rm = TRUE),
      LE_EURO = sum(LE_EURO, na.rm = TRUE),
      .groups = "drop"
    )
  pings3 <-
    pings_left |>
    dplyr::left_join(events3, by = c("VE_COU", "VE_REF", "FT_REF", "state")) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, state) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = ifelse(state == 1, "3 trip", NA)
    ) |>
    dplyr::ungroup()

  # Combine allocation results from all steps into a single output data frame ----
  out <-
    dplyr::bind_rows(
      pings1 |> dplyr::mutate(.how = as.character(.how)),
      pings2 |> dplyr::mutate(.how = as.character(.how)),
      pings3 |> dplyr::mutate(.how = as.character(.how))
    ) |>
    dplyr::arrange(.pid)

  # Reserved: raise trips whose events were only partly allocated (not implemented)
  if (raise_unallocated) {
    # Under consideration
    # Will likely be used to raise all catches within a trip with
    #  those that were not allocated
  }

  # Reserved: share events whose trip has no match in trails (not implemented)
  if (raise_unmatched) {
    # Under consideration
    # Will likely be used to raise any event trip catches that are not matched
    # with trips in the trails data - likely first by vessel then by month, ...
  }

  # Remove diagnostic columns if requested
  if (remove_diagnostic) {
    out <-
      out |>
      dplyr::select(-c(date, .wt))
  }

  # Output checks --------------------------------------------------------------
  # Validate that output matches input in row count and total LE_KG allocation
  if (nrow(pings) != nrow(out)) {
    stop("Number of rows in output does not match input pings - File an issue")
  }

  kg_out <- sum(out$LE_KG, na.rm = TRUE)
  kg_in <- sum(events$LE_KG, na.rm = TRUE)

  if (abs(kg_out - kg_in) > 1e-6) {
    warning(paste("kg in: ", round(kg_in), " kg out: ", round(kg_out)))
    warning("Sum of LE_KG in output does not match events - File an issue")
  }

  return(out)
}
#' Sum columns across by prefix in Data Frame
#'
#' Summarizes (row-wise) all numeric columns in a data frame whose names start with a specified prefix,
#' returning their sum as a new column. Optionally removes the original columns used in the sum.
#'
#' Useful for eflalo data where multiple columns represent catches of different species,
#' and you want to produce a total catch column and at the same time remove the original columns.
#'
#' @param d A data frame (typically an eflalo data frame).
#' @param start_name A string (default "LE_KG"). Prefix of column names to sum across. All columns starting with this string will be included in the sum.
#' @param remove Logical (default TRUE). If TRUE, removes all original columns whose names start with \code{start_name} after summing.
#'
#' @return
#' The input data frame with a new column named \code{start_name} containing the row sums. If \code{remove = TRUE}, the original columns are removed.
#'
#' @details
#' The function will error if no columns match \code{start_name}, or if any matching columns are not numeric.
#'
#' @examples
#' # Example: Summing all columns starting with 'catch_'
#' df <- data.frame(
#'   id = 1:3,
#'   LE_KG_COD = c(1, 2, 3),
#'   LE_KG_HAD = c(0, 1, 0),
#'   LE_KG_HER = c(2, 1, 2)
#' )
#' rb_sum_across(df, prefix = "LE_KG")
#'
#' # Keep original columns
#' rb_sum_across(df, prefix = "LE_KG", remove = FALSE)
#'
#' @export
rb_sum_across <- function(d, prefix = "LE_KG", remove = TRUE) {
  # Input checks
  if (is.null(prefix) || !is.character(prefix) || length(prefix) != 1) {
    stop("prefix must be a single character string.")
  }
  cols_to_sum <- grep(paste0("^", prefix), names(d), value = TRUE)
  if (length(cols_to_sum) == 0) {
    stop("No columns found starting with '", prefix, "'.")
  }
  if (!all(sapply(d[cols_to_sum], is.numeric))) {
    stop("All columns to sum must be numeric.")
  }

  d <- dplyr::mutate(
    d,
    !!prefix := rowSums(dplyr::across(dplyr::starts_with(prefix)))
  )

  if (remove) {
    # Remove only the original columns, not the new sum column
    cols_to_remove <- setdiff(cols_to_sum, prefix)
    d <- dplyr::select(d, -dplyr::all_of(cols_to_remove))
  }
  return(d)
}
