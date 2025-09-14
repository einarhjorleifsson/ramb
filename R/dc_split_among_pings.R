#' Distribute logbook catch/value among VMS pings using dplyr
#'
#' This function mimics the main logic of vmstools::splitAmongPings, but is written purely with dplyr verbs.
#' It links VMS ping data (tacsat) to logbook trip data (eflalo) and distributes the reported catch and/or value
#' across the relevant VMS pings, optionally weighted by a given column (e.g., time interval).
#'
#' The function supports splitting at different hierarchical levels: by trip, rectangle, and/or day.
#' The default is to split by all available levels. Only fishing pings (`SI_STATE == 1`) are assigned catch/value.
#'
#' @param tacsat   Data frame of VMS pings. Must include at least: SI_STATE, SI_DATE, SI_TIME, FT_REF, VE_REF, LE_RECT, (and optionally a weighting column).
#' @param eflalo   Data frame of logbook trips. Must include at least: FT_REF, VE_REF, FT_DDAT, FT_DTIME, FT_LDAT, FT_LTIME, LE_KG, LE_EURO, LE_RECT.
#' @param variable Which variable(s) to split: "all" (default), "value", or "kgs".
#' @param level    Character vector of grouping levels. Can include any of c("day", "ICESrectangle", "trip"). Default: all.
#' @param by       Optional. Name of a tacsat column to use as a weight for splitting (e.g., "INTV" for time interval). If NULL, splits equally.
#'
#' @return         Data frame of fishing pings with assigned catch/value columns (Assigned_KG, Assigned_EURO).
#'
#' @details
#' - The function performs a left join between tacsat and eflalo on the grouping columns derived from `level`.
#' - For each group (e.g., unique day, rectangle, trip), catch and/or value are distributed among pings, either equally or weighted by the `by` column.
#' - Only pings with SI_STATE == 1 (fishing) will be assigned catch/value.
#' - No "conserve" logic is implemented: only pings that can be matched to logbook trips receive catch/value.
#' - The function expects date columns in "dd/mm/yyyy" format and time columns as "HH:MM".
#' - Only dplyr, lubridate, and base R are used.
#'
#' @examples
#' # Minimal example with two rectangles and two days:
#' eflalo <- tibble::tibble(
#'   VE_REF   = "V001",
#'   FT_REF   = 101,
#'   FT_DDAT  = c("01/09/2025", "02/09/2025"),
#'   FT_DTIME = c("06:00", "06:00"),
#'   FT_LDAT  = c("01/09/2025", "02/09/2025"),
#'   FT_LTIME = c("20:00", "20:00"),
#'   LE_CDAT  = c("01/09/2025", "02/09/2025"),
#'   LE_RECT  = c("32F1", "32F2"),
#'   LE_KG    = c(300, 500),
#'   LE_EURO  = c(600, 1000)
#' )
#' tacsat <- tibble::tibble(
#'   VE_REF   = rep("V001", 8),
#'   FT_REF   = rep(101, 8),
#'   SI_DATE  = c("01/09/2025","01/09/2025","01/09/2025","01/09/2025",
#'                "02/09/2025","02/09/2025","02/09/2025","02/09/2025"),
#'   SI_TIME  = c("08:00","12:00","16:00","20:00",
#'                "08:00","12:00","16:00","20:00"),
#'   LE_RECT  = c("32F1","32F1","32F2","32F2",
#'                "32F2","32F2","32F1","32F1"),
#'   SI_STATE = rep(1, 8),
#'   INTV     = c(2, 4, 4, 2, 2, 4, 4, 2)
#' )
#' result <- dc_split_among_pings(
#'   tacsat = tacsat,
#'   eflalo = eflalo,
#'   variable = "all",
#'   level = c("day", "ICESrectangle", "trip"),
#'   by = "INTV"
#' )
#' # View with print(head(result))
#'
#' @author
#' Einar Hjörleifsson, Copilot
#'
#' @export
dc_split_among_pings <- function(tacsat, eflalo, variable = "all",
                                  level = c("day", "ICESrectangle", "trip"),
                                  by = NULL) {
  library(dplyr)
  library(lubridate)
  
  # 1. Prepare date columns (POSIXct)
  if (!"SI_DATIM" %in% names(tacsat)) {
    tacsat <- tacsat |>
      mutate(SI_DATIM = as.POSIXct(paste(SI_DATE, SI_TIME), format = "%d/%m/%Y %H:%M", tz = "UTC"))
  }
  if (!"FT_DDATIM" %in% names(eflalo)) {
    eflalo <- eflalo |>
      mutate(FT_DDATIM = as.POSIXct(paste(FT_DDAT, FT_DTIME), format = "%d/%m/%Y %H:%M", tz = "UTC"))
  }
  if (!"FT_LDATIM" %in% names(eflalo)) {
    eflalo <- eflalo |>
      mutate(FT_LDATIM = as.POSIXct(paste(FT_LDAT, FT_LTIME), format = "%d/%m/%Y %H:%M", tz = "UTC"))
  }
  
  # 2. Filter for fishing pings only
  tacsat_fish <- tacsat |> filter(SI_STATE == 1)
  
  # 3. Add grouping variables based on `level`
  tacsat_fish <- tacsat_fish |>
    mutate(SI_YEAR = year(SI_DATIM),
           SI_DAY = yday(SI_DATIM)) |>
    mutate(LE_RECT = as.character(LE_RECT)) # ensure rectangle is character
  
  eflalo <- eflalo |>
    mutate(SI_YEAR = year(FT_DDATIM),
           SI_DAY = yday(FT_DDATIM)) |>
    mutate(LE_RECT = as.character(LE_RECT))
  
  # 4. Define grouping columns
  groups <- c()
  if ("trip" %in% level) groups <- c(groups, "SI_YEAR", "VE_REF", "FT_REF")
  if ("ICESrectangle" %in% level) groups <- c(groups, "LE_RECT")
  if ("day" %in% level) groups <- c(groups, "SI_DAY")
  
  # 5. Join tacsat and eflalo on grouping variables
  joined <- tacsat_fish |>
    left_join(
      eflalo,
      by = intersect(c("SI_YEAR", "VE_REF", "FT_REF", "LE_RECT", "SI_DAY"), names(eflalo))
    )
  
  # 6. Calculate group weights
  if (!is.null(by)) {
    joined <- joined |>
      group_by(across(all_of(groups))) |>
      mutate(wt = !!sym(by) / sum(!!sym(by), na.rm = TRUE)) |>
      ungroup()
  } else {
    joined <- joined |>
      group_by(across(all_of(groups))) |>
      mutate(wt = 1 / n()) |>
      ungroup()
  }
  
  # 7. Assign catch/value by weight
  if (variable == "all") {
    # Assign all possible catch/value columns
    if ("LE_KG" %in% names(joined)) joined$Assigned_KG <- joined$LE_KG * joined$wt
    if ("LE_EURO" %in% names(joined)) joined$Assigned_EURO <- joined$LE_EURO * joined$wt
  } else if (variable == "kgs") {
    if ("LE_KG" %in% names(joined)) joined$Assigned_KG <- joined$LE_KG * joined$wt
  } else if (variable == "value") {
    if ("LE_EURO" %in% names(joined)) joined$Assigned_EURO <- joined$LE_EURO * joined$wt
  }
  
  # 8. Return tacsat with assigned values
  joined
}