#' Allocate Landings from Events to Trails
#'
#' This function allocates landing event weights (`LE_KG`) and values (`LE_EURO`)
#' from the `events` data frame to the corresponding fishing trails in the `trails`
#' data frame using a hierarchical matching and allocation strategy.
#'
#' @param trails A data frame containing fishing trail records. **Required columns:**
#'   \itemize{
#'     \item{\code{.pid}:} {integer, unique trail identifier}
#'     \item{\code{VE_COU}:} {character, vessel country}
#'     \item{\code{VE_REF}:} {character, vessel reference}
#'     \item{\code{time}:} {POSIXct/datetime, UTC timestamp, derived from SI_DATE and SI_TIME}
#'     \item{\code{FT_REF}:} {character, fishing trip reference}
#'     \item{\code{ir}:} {character, ICES rectangle, derive from SI_LONG and SI_LAT}
#'     \item{\code{state}:} {integer, state (must be 0 or 1)}
#'   }
#' @param events A data frame containing landing event records. **Required columns:**
#'   \itemize{
#'     \item{\code{.eid}:} {integer, unique event identifier}
#'     \item{\code{VE_COU}:} {character, vessel country}
#'     \item{\code{VE_REF}:} {character, vessel reference}
#'     \item{\code{FT_REF}:} {character, fishing trip reference}
#'     \item{\code{e_date}:} {Date, event date, derived from LE_CDAT which is a character vector
#'     \item{\code{LE_RECT}:} {character, ICES rectangle}
#'     \item{\code{LE_KG}:} {numeric, landed weight in kg}
#'     \item{\code{LE_EURO}:} {numeric, landed value in euro}
#'   }
#'
#' @details
#' The function spreads event cash and catches to trails using three hierarchical steps:
#' \enumerate{
#'   \item Joins by year, trip, state, rectangle and day.
#'   \item Joins remaining unmatched records by year, trip, state and rectangle.
#'   \item Joins remaining unmatched records by year, trip, and state
#' }
#' 
#' If any trip cash or catches are remaining the above are raised by trip based raising factor. 
#'
#' @return A data frame similar to \code{trails}, with updated \code{LE_KG}, \code{LE_EURO},
#'   and a column \code{.how} indicating at which step the spread took place. Additional variables 
#'   may be returned if arguement remove_diagnostic is set to FALSE.
#'   
#'
#' @export
#'
dc_spread_cash_and_catch <- function(trails, events, remove_diagnostic = TRUE) {
  # Input checks ----
  required_trails <- c(".pid", "VE_COU", "VE_REF", "time", "FT_REF", "ir", "state")
  required_events <- c("VE_COU", "VE_REF", "FT_REF", "e_date", "LE_RECT", "LE_KG", "LE_EURO")
  
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
  if (!inherits(events$e_date, "Date")) stop("e_date in events must be Date")
  if (!is.character(events$LE_RECT)) stop("LE_RECT in events must be character")
  if (!is.numeric(events$LE_KG)) stop("LE_KG in events must be numeric")
  if (!is.numeric(events$LE_EURO)) stop("LE_EURO in events must be numeric")
  
  # Prepare extra columns ----
  trails <- trails |>
    dplyr::mutate(#.year = lubridate::year(time),
                  .yday = lubridate::yday(time))
  #.pid = dplyr::row_number())
  events <- events |>
    dplyr::mutate(#.year = lubridate::year(e_date),
                  .yday = lubridate::yday(e_date),
                  # TODO: Set before passing to function
                  #.eid = dplyr::row_number(),
                  state = 1L)
  
  # upfront map at what step each event will be processed
  events <- 
    events |> 
    mutate(
      .step = 
        case_when(
          !is.na(e_date) & !is.na(LE_RECT) & !is.na(FT_REF) ~ 1L,
           is.na(e_date) & !is.na(LE_RECT) & !is.na(FT_REF) ~ 2L,
           is.na(e_date) &  is.na(LE_RECT) & !is.na(FT_REF) ~ 3L,
          .default = 0L))
  
  if(any(events$.step == 0L)) {
    warning("There are records with no trip id (LE_REF)")
  }
  
  # Keep track of .pid and .eid that are dropped at each step
  PID_left <- trails$.pid
  
  # Step 1: Join by trip+rectangle+day ----
  trails1 <- trails |>
    dplyr::inner_join(events |> filter(.step == 1),
                      by = c("VE_COU", "VE_REF", "FT_REF", "ir" = "LE_RECT", ".yday", "state")) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, ir, .yday, .eid) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = "trip_rectangle_day"
    ) |>
    dplyr::ungroup()
  
  PID_left <- PID_left[!PID_left %in% trails1$.pid]
  
  # Step 2: Join by trip+rectangle+year for unmatched ----
  print("Step2")
  trails_left <- 
    trails |> 
    filter(.pid %in% PID_left)

  events2 <- 
    events |> 
    filter(.step == 2L) |> 
    # avoid many-to-many among other things
    dplyr::group_by(VE_COU, VE_REF, FT_REF, LE_RECT, state) |> 
    dplyr::summarise(LE_KG = sum(LE_KG, na.rm = TRUE),
                     LE_EURO = sum(LE_KG, na.rm = TRUE),
                     .groups = "drop")
  
  trails2 <- 
    trails_left |>
    dplyr::select(-.yday) |>
    dplyr::inner_join(events2, 
                      by = c("VE_COU", "VE_REF", "FT_REF", "ir" = "LE_RECT", "state")) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, ir, state) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = "trip_rectangle"
    ) |>
    dplyr::ungroup()
  
  PID_left <- PID_left[!PID_left %in% trails2$.pid]

  # Step 3: Join by trip+year for unmatched, allocate only remainder ----
  #         This may fail if state remaining are all zero
  trails_left <- 
    trails |> 
    filter(.pid %in% PID_left)
  events3 <- 
    events |> 
    filter(.step == 3L) |> 
    # avoid many-to-many among other things
    dplyr::group_by(VE_COU, VE_REF, FT_REF, state) |> 
    dplyr::summarise(LE_KG = sum(LE_KG, na.rm = TRUE),
                     LE_EURO = sum(LE_KG, na.rm = TRUE),
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
      .how = ifelse(is.na(LE_KG), NA_character_, "trip")
    ) |>
    dplyr::ungroup()
  
  out <- 
    # fix for now, should deal with this upstream
    dplyr::bind_rows(trails1 |> mutate(.how = as.character(.how)), 
                     trails2 |> mutate(.how = as.character(.how)), 
                     trails3 |> mutate(.how = as.character(.how))) |> 
    dplyr::arrange(.pid)
  
  # # Output checks ----
  # if (nrow(trails) != nrow(out)) 
  #   stop("Number of rows in output does not match input trails - File an issue")
  # 
  # # Step 4: Remainder - at trip level, raise all records currently in out by a raising factor
  # raising_factor <- 
  #   events |> 
  #   group_by(VE_COU, VE_REF, FT_REF, .year) |> 
  #   summarise(.in_kg = sum(LE_KG, na.rm = TRUE),
  #             .in_eu = sum(LE_EURO, na.rm = TRUE),
  #             .groups = "drop") |> 
  #   left_join(out |> 
  #               group_by(VE_COU, VE_REF, FT_REF, .year) |> 
  #               summarise(.out_kg = sum(LE_KG, na.rm = TRUE),
  #                         .out_eu = sum(LE_EURO, na.rm = TRUE),
  #                         .groups = "drop")) |> 
  #   mutate(.r_kg = .in_kg / .out_kg,
  #          .r_eu = .in_eu / .out_eu)
  # out <- 
  #   out |> 
  #   left_join(raising_factor) |> 
  #   mutate(.how = case_when(.r_kg > 1 | .r_eu > 1 ~ paste0(.how, " raised - kg: ", round(.r_kg, 2), " euro: ", round(.r_eu,2)),
  #                           .default = .how),
  #          LE_KG = case_when(.r_kg > 1 ~ LE_KG * .r_kg,
  #                            .default = LE_KG),
  #          LE_EURO = case_when(.r_eu > 1 ~ LE_EURO * .r_eu,
  #                              .default = LE_EURO))
  
  
  if(remove_diagnostic) {
    out <- 
      out |> 
      dplyr::select(-c(.yday, e_date, .eid, .wt))
  }
  
  # Output checks ----
  if (nrow(trails) != nrow(out)) 
    stop("Number of rows in output does not match input trails - File an issue")
  if (abs(sum(out$LE_KG, na.rm = TRUE) - sum(events$LE_KG, na.rm = TRUE)) > 1e-6)
    warning("Sum of LE_KG in output does not match events - File an issue")
  
  return(out)
}