#' Spread Catch and Value from Events to Trails Using Hierarchical Allocation
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
#' \strong{Note:} Additional allocation steps ("Einstein" and "go nuclear") are not yet implemented.
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
#' @param ignore_Einstein Logical (default TRUE). Whether to ignore unallocated events ("Einstein step" not implemented).
#' @param go_nuclear Character (default "absolutely not"). Any other value triggers alternative allocation (not implemented).
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
#' @note
#' `ignore_Einstein`: Further discussion is required for implementing allocation of remaining catch.
#' 
#' `go_nuclear`: Further discussion is required for implementing allocation of trips in the events 
#' data not in the trails data. 
#'
#' @author Einar Hjörleifsson
#' 
#' @export
dc_spread_cash_and_catch <- function(trails, events, remove_diagnostic = TRUE,
                                     ignore_Einstein = TRUE,
                                     go_nuclear = "absolutely not") {
  
  
  # Input checks ----
  # Check for required columns and correct data types in 'trails' and 'events' data frames
  required_trails <- c(".pid", "VE_COU", "VE_REF", "time", "FT_REF", "ir", "state")
  required_events <- c(".eid", "VE_COU", "VE_REF", "FT_REF", "date", "LE_RECT", "LE_KG", "LE_EURO")
  
  
  
  
  # Check for columns
    missing_trails <- setdiff(required_trails, colnames(trails))
  if (length(missing_trails) > 0)
    stop("Missing required columns in trails: ", paste(missing_trails, collapse = ", "))
  missing_events <- setdiff(required_events, colnames(events))
  if (length(missing_events) > 0)
    stop("Missing required columns in events: ", paste(missing_events, collapse = ", "))
  
  # Check classes for trails
  if (!is.integer(trails$.pid)) stop(".pid in trails must be integer")
  if (!is.character(trails$VE_COU)) stop("VE_COU in trails must be character")
  if (!is.character(trails$VE_REF)) stop("VE_REF in trails must be character")
  if (!inherits(trails$time, "POSIXct")) stop("time in trails must be POSIXct/datetime")
  if (!is.character(trails$FT_REF)) stop("FT_REF in trails must be character")
  if (!is.character(trails$ir)) stop("ir in trails must be character")
  if (!is.integer(trails$state)) stop("state in trails must be integer")
  if (!all(trails$state %in% c(0,1))) stop("state in trails must be 0 or 1")
  
  # Check classes for events
  if (!is.integer(events$.eid)) stop(".eid in events must be integer")
  if (!is.character(events$VE_COU)) stop("VE_COU in events must be character")
  if (!is.character(events$VE_REF)) stop("VE_REF in events must be character")
  if (!is.character(events$FT_REF)) stop("FT_REF in events must be character")
  if (!inherits(events$date, "Date")) stop("date in events must be Date")
  if (!is.character(events$LE_RECT)) stop("LE_RECT in events must be character")
  if (!is.numeric(events$LE_KG)) stop("LE_KG in events must be numeric")
  if (!is.numeric(events$LE_EURO)) stop("LE_EURO in events must be numeric")
  
  # Prepare extra columns ----
  trails <- trails |> mutate(date = as_date(time))
  events <- 
    events |>
    # fix upstream
    rename(date = date) |> 
    dplyr::mutate(state = 1L)

  # Mapping step ----
  # Determine at which allocation step each event should be joined
  # Create step_plan to assign events to allocation steps based on available keys
  step_plan <- 
    events |> 
    select(.eid, VE_COU, VE_REF, FT_REF, date, LE_RECT) |> 
    left_join(trails |> 
                filter(state == 1) |> 
                select(VE_COU, VE_REF, FT_REF, date, LE_RECT = ir) |> 
                distinct() |> 
                mutate(vms = "yes"),
              by = join_by(VE_COU, VE_REF, FT_REF),
              relationship = "many-to-many") |> 
    mutate(.step = case_when(date.x == date.y & LE_RECT.x == LE_RECT.y & vms == "yes" ~ 1L,
                             is.na(date.x) & LE_RECT.x == LE_RECT.y & vms == "yes" ~ 2L,
                             vms == "yes" ~ 3L,
                             .default = 9999L)) |> 
    arrange(.step) |> 
    select(.eid, .step) |> 
    distinct(.eid, .keep_all = TRUE)
  
  events <- 
    events |> 
    left_join(step_plan,
              by = join_by(.eid))
  

  # Keep track of .pid that are not mapped after each step
  PID_left <- trails$.pid
  
  # Step 1: Join by trip + rectangle + day -------------------------------------
  # Allocate events to trails at the most specific (date & rectangle) level
  events1 <- 
    events |> 
    filter(.step == 1) |> 
    group_by(VE_COU, VE_REF, FT_REF, date, LE_RECT, state) |> 
    summarise(LE_KG = sum(LE_KG, na.rm = TRUE),
              LE_EURO = sum(LE_EURO, na.rm = TRUE),
              .groups = "drop")

  trails1 <- trails |>
    dplyr::inner_join(events1,
                      by = c("VE_COU", "VE_REF", "FT_REF", "ir" = "LE_RECT", "date", "state")) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, ir, date) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = "1 trip_rectangle_day"
    ) |>
    dplyr::ungroup()
  
  PID_left <- PID_left[!PID_left %in% trails1$.pid]
   
  # Step 2: Join by trip + rectangle -------------------------------------------
  # Allocate remaining events to trails at the rectangle level

  trails_left <- 
    trails |> 
    filter(.pid %in% PID_left)

  events2 <- 
    events |> 
    filter(.step == 2L) |> 
    # avoid many-to-many among other things
    dplyr::group_by(VE_COU, VE_REF, FT_REF, LE_RECT, state) |> 
    dplyr::summarise(LE_KG = sum(LE_KG, na.rm = TRUE),
                     LE_EURO = sum(LE_EURO, na.rm = TRUE),
                     .groups = "drop")
  
  trails2 <- 
    trails_left |>
    dplyr::select(-date) |>
    dplyr::inner_join(events2, 
                      by = c("VE_COU", "VE_REF", "FT_REF", "ir" = "LE_RECT", "state")) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, ir, state) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = "2 trip_rectangle"
    ) |>
    dplyr::ungroup()
  
  PID_left <- PID_left[!PID_left %in% trails2$.pid]

  # Step 3: Join by trip -------------------------------------------------------
  # Allocate remaining events to trails at the trip level
  
  trails_left <- 
    trails |> 
    filter(.pid %in% PID_left)
  events3 <- 
    events |> 
    filter(.step == 3L) |> 
    dplyr::group_by(VE_COU, VE_REF, FT_REF, state) |> 
    dplyr::summarise(LE_KG = sum(LE_KG, na.rm = TRUE),
                     LE_EURO = sum(LE_EURO, na.rm = TRUE),
                     .groups = "drop")
  trails3 <- 
    trails_left |> 
    dplyr::left_join(events3, 
                     by = c("VE_COU", "VE_REF", "FT_REF", "state")) |>
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
    dplyr::bind_rows(trails1 |> mutate(.how = as.character(.how)), 
                     trails2 |> mutate(.how = as.character(.how)), 
                     trails3 |> mutate(.how = as.character(.how))) |> 
    dplyr::arrange(.pid)
  
  # Optionally handle "Einstein" step for unallocated events (not implemented)
  if(!ignore_Einstein) {
    # Under considerations
    # Will likely be used to raise all catches within a trip with
    #  those that were not allocated
  }
  
  # Optionally handle "go nuclear" step for unmatched trip catches (not implemented)
  if(go_nuclear != "absolutely not") {
    # Under considerations
    # Will likely be used to raise any event trip catches that are not matched
    # with trips in the trails data - likely first by vessel then by month, ...
  }
  
  
  # Remove diagnostic columns if requested
  if(remove_diagnostic) {
    out <- 
      out |> 
      dplyr::select(-c(date, .wt))
  }
  
  # Output checks --------------------------------------------------------------
  # Validate that output matches input in row count and total LE_KG allocation
    if (nrow(trails) != nrow(out)) {
    stop("Number of rows in output does not match input trails - File an issue")
  }
  
  kg_out <- sum(out$LE_KG, na.rm = TRUE)
  kg_in  <- sum(events$LE_KG, na.rm = TRUE)
  
  if (abs(kg_out - kg_in) > 1e-6) {
    warning(paste("kg in: ", round(kg_in), " kg out: ", round(kg_out)))
    warning("Sum of LE_KG in output does not match events - File an issue")
  }
  
  return(out)
}