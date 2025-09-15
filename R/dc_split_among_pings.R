#' Distribute logbook catch/value among vessel track pings
#'
#' This function distributes logbook-reported catch and/or value among relevant 
#' vessek track pings. It links VMS ping data (\code{tacsat}) with logbook 
#' trip data (\code{eflalo}) and distributes reported catch/value across 
#' the relevant vessel track pings, optionally weighted by a specified 
#' column (e.g., time interval).
#' 
#' It mimics the logic of \code{vmstools::splitAmongPings} 
#' but is implemented somewhat differently. For now it only works on one weight
#' (logbook LE_KG) column and/or one value (logbook LE_EURO) and it does not have
#' the conserve feature of the {vmstools} function. 
#'
#' Splitting is always hierarchical and must be one of the following (from coarsest to finest):
#'   \itemize{
#'     \item \code{level = c("trip")}
#'     \item \code{level = c("trip", "ICESrectangle")}
#'     \item \code{level = c("trip", "ICESrectangle", "day")}
#'   }
#' Only these three forms are allowed. Only fishing pings (\code{SI_STATE == 1}) receive catch/value.
#'
#' @section Required columns:
#' \describe{
#'   \item{tacsat (VMS pings):}{
#'     \itemize{
#'       \item \code{SI_STATE} (integer: 1 = fishing, 0 = not fishing; only 1 or 0 allowed)
#'       \item \code{SI_DATIM} (datetime; POSIXct)
#'       \item \code{VE_REF}, \code{FT_REF} (character; vessel/trip identifiers)
#'       \item \code{LE_RECT} (character; ICES rectangle)
#'       \item (optional: weighting column, e.g., \code{INTV})
#'     }
#'   }
#'   \item{eflalo (logbook trips):}{
#'     \itemize{
#'       \item \code{FT_REF}, \code{VE_REF} (character; must match tacsat)
#'       \item \code{LE_CDAT} (date; trip date, format: "dd/mm/yyyy" or POSIXct)
#'       \item \code{LE_RECT} (character; ICES rectangle)
#'       \item \code{LE_KG} (numeric; catch in kg)
#'       \item \code{LE_EURO} (numeric; value in euro)
#'     }
#'   }
#' }
#'
#' @section Returned value:
#' Returns a tibble/data.frame of pings (rows from \code{tacsat}), with columns:
#' \itemize{
#'   \item All columns from input \code{tacsat}
#'   \item \code{KG}: assigned catch (kg) per ping (numeric; 0 if not assigned)
#'   \item \code{EURO}: assigned value (€) per ping (numeric; 0 if not assigned)
#' }
#'
#' @details
#' \itemize{
#'   \item Performs a left join between \code{tacsat} and \code{eflalo} on grouping columns based on \code{level} (hierarchical).
#'   \item For each group (e.g., trip, rectangle, day), splits catch/value among fishing pings, either equally or weighted by the \code{by} column.
#'   \item Only pings with \code{SI_STATE == 1} will be assigned catch/value.
#'   \item \code{SI_STATE} must only contain values 0 or 1; an error is thrown otherwise.
#'   \item No "conserve" logic: only matched pings receive catch/value.
#'   \item Date columns must be "dd/mm/yyyy" or POSIXct; time columns not used directly.
#' }
#'
#' @param tacsat   Data frame of VMS pings (see Required columns).
#' @param eflalo   Data frame of logbook trips (see Required columns).
#' @param level    Character vector; hierarchical grouping for splitting, must be one of:
#'                 \code{c("trip")}, \code{c("trip", "ICESrectangle")}, or \code{c("trip", "ICESrectangle", "day")}.
#' @param by       Optional; name of a \code{tacsat} column to use as weight for splitting (e.g., "INTV"). If \code{NULL} (default), splits equally.
#' @param variable Which variable(s) to split: \code{"all"} (default), \code{"value"}, or \code{"kgs"}.
#'
#' @return  The orginal vessel track table with additional assigned catch/value columns (\code{KG}, \code{EURO}).
#'
#' @examples
#' library(dplyr); library(lubridate)
#' eflalo <- tibble(
#'   VE_COU = rep("ISL", 4),
#'   VE_REF = rep("V001", 4),
#'   FT_REF = rep("1", 4),
#'   LE_CDAT = c("01/01/2025", "01/01/2025", "02/01/2025", "02/01/2025"),
#'   LE_RECT = c("32F1", "32F2", "32F3", "32F2"),
#'   LE_KG = c(100, 500, 1200, 300),
#'   LE_EURO = c(100, 500, 1200, 300)
#' )
#' tacsat <- tibble(
#'   SI_DATIM = seq(ymd_hms("2025-01-01 06:00:00"), ymd_hms("2025-01-02 18:00:00"), by = "1 hour")
#' ) %>%
#'   mutate(
#'     VE_COU = "ISL",
#'     VE_REF = "V001",
#'     FT_REF = "1",
#'     LE_RECT = case_when(
#'       between(SI_DATIM, ymd_hms("2025-01-01 09:00:00 UTC"), ymd_hms("2025-01-01 13:00:00 UTC")) ~ "32F1",
#'       between(SI_DATIM, ymd_hms("2025-01-01 18:00:00 UTC"), ymd_hms("2025-01-01 21:00:00 UTC")) ~ "32F2",
#'       between(SI_DATIM, ymd_hms("2025-01-02 02:00:00 UTC"), ymd_hms("2025-01-02 03:00:00 UTC")) ~ "32F3",
#'       between(SI_DATIM, ymd_hms("2025-01-02 13:00:00 UTC"), ymd_hms("2025-01-02 14:00:00 UTC")) ~ "32F2",
#'       TRUE ~ "9999"
#'     ),
#'     SI_STATE = 1L,
#'     INTV = 1
#'   ) %>%
#'   mutate(.rowid = 1:n(), .before = SI_DATIM)
#' # Test hierarchical forms
#' result1 <- dc_split_among_pings(
#'   tacsat = tacsat,
#'   eflalo = eflalo,
#'   variable = "all",
#'   level = c("trip", "ICESrectangle", "day"),
#'   by = "INTV"
#' )
#' result2 <- dc_split_among_pings(
#'   tacsat = tacsat,
#'   eflalo = eflalo,
#'   variable = "all",
#'   level = c("trip", "ICESrectangle"),
#'   by = "INTV"
#' )
#' result3 <- dc_split_among_pings(
#'   tacsat = tacsat,
#'   eflalo = eflalo,
#'   variable = "all",
#'   level = c("trip"),
#'   by = "INTV"
#' )
#' # All SI_STATE must be 0 or 1:
#' stopifnot(all(result1$SI_STATE %in% c(0,1)))
#' stopifnot(all(result2$SI_STATE %in% c(0,1)))
#' stopifnot(all(result3$SI_STATE %in% c(0,1)))
#' # head(result1); head(result2); head(result3)
#'
#' @author
#' Einar Hjörleifsson
#'
#' @export
dc_split_among_pings <- function(
    tacsat,
    eflalo,
    level = c("trip", "ICESrectangle", "day"),
    by = NULL,
    variable = "all"
) {
  
  # ---- Input checks ----
  # Check required columns in tacsat
  needed_tacsat <- c("SI_STATE", "SI_DATIM", "VE_REF", "FT_REF", "LE_RECT")
  missing_tacsat <- setdiff(needed_tacsat, names(tacsat))
  if (length(missing_tacsat) > 0) {
    stop("tacsat is missing required columns: ", paste(missing_tacsat, collapse = ", "))
  }
  # Check SI_STATE is only 0 or 1, and is integer/numeric
  if (!is.numeric(tacsat$SI_STATE) && !is.integer(tacsat$SI_STATE)) {
    stop("tacsat$SI_STATE must be integer or numeric (0 or 1 only).")
  }
  if (any(!tacsat$SI_STATE %in% c(0, 1))) {
    stop("tacsat$SI_STATE must only contain 0 (not fishing) or 1 (fishing).")
  }
  # Check required columns in eflalo
  needed_eflalo <- c("VE_REF", "FT_REF", "LE_CDAT", "LE_RECT", "LE_KG", "LE_EURO")
  missing_eflalo <- setdiff(needed_eflalo, names(eflalo))
  if (length(missing_eflalo) > 0) {
    stop("eflalo is missing required columns: ", paste(missing_eflalo, collapse = ", "))
  }
  # Check SI_DATIM is POSIXct
  if (!inherits(tacsat$SI_DATIM, "POSIXct")) {
    stop("tacsat$SI_DATIM must be POSIXct (datetime).")
  }
  # If by is provided, check that column exists and numeric
  if (!is.null(by)) {
    if (!by %in% names(tacsat)) stop("by = '", by, "' is not a column in tacsat.")
    if (!is.numeric(tacsat[[by]])) stop("Weighting column '", by, "' must be numeric.")
    if (any(is.na(tacsat[[by]]))) stop("Weighting column '", by, "' contains NA values; please handle or remove.")
  }
  # Check that variable is one of allowed
  allowed_vars <- c("all", "kgs", "value")
  if (!variable %in% allowed_vars) stop("variable must be one of: ", paste(allowed_vars, collapse = ", "))
  # Check level is allowed (hierarchical only)
  allowed_levels <- list(
    c("trip"),
    c("trip", "ICESrectangle"),
    c("trip", "ICESrectangle", "day")
  )
  level_match <- any(vapply(allowed_levels, function(x) identical(level, x), logical(1)))
  if (!level_match) {
    stop(
      "level must be one of: c('trip'), c('trip', 'ICESrectangle'), or c('trip', 'ICESrectangle', 'day')"
    )
  }
  
  # ---- Prepare grouping variables ----
  # Parse eflalo date
  if (is.character(eflalo$LE_CDAT)) {
    eflalo$LE_CDAT <- lubridate::dmy(eflalo$LE_CDAT)
  } else if (!inherits(eflalo$LE_CDAT, "Date")) {
    stop("eflalo$LE_CDAT must be Date or character in 'dd/mm/yyyy' format.")
  }
  # For tacsat derive grouping day/year
  tacsat <- tacsat %>%
    mutate(.year = year(SI_DATIM), .yday = yday(SI_DATIM)) %>%
    mutate(LE_RECT = as.character(LE_RECT))
  eflalo <- eflalo %>%
    mutate(.year = year(LE_CDAT), .yday = yday(LE_CDAT)) %>%
    mutate(LE_RECT = as.character(LE_RECT))
  
  # Grouping columns (enforced hierarchy)
  if (identical(level, c("trip"))) {
    groups <- c(".year", "VE_REF", "FT_REF")
  } else if (identical(level, c("trip", "ICESrectangle"))) {
    groups <- c(".year", "VE_REF", "FT_REF", "LE_RECT")
  } else if (identical(level, c("trip", "ICESrectangle", "day"))) {
    groups <- c(".year", "VE_REF", "FT_REF", "LE_RECT", ".yday")
  }
  # Check grouping columns exist in both
  missing_in_tacsat <- setdiff(groups, names(tacsat))
  missing_in_eflalo <- setdiff(groups, names(eflalo))
  if (length(missing_in_tacsat) > 0) stop("Grouping columns missing in tacsat: ", paste(missing_in_tacsat, collapse = ", "))
  if (length(missing_in_eflalo) > 0) stop("Grouping columns missing in eflalo: ", paste(missing_in_eflalo, collapse = ", "))
  
  # ---- Join and weight calculation ----
  join_by <- intersect(groups, names(eflalo))
  joined <- tacsat %>%
    left_join(eflalo, by = join_by)
  
  # Calculate weights
  if (!is.null(by)) {
    joined <- joined %>%
      group_by(across(all_of(groups))) %>%
      mutate(wt = !!sym(by) / sum(!!sym(by), na.rm = TRUE)) %>%
      ungroup()
  } else {
    joined <- joined %>%
      group_by(across(all_of(groups))) %>%
      mutate(wt = 1 / n()) %>%
      ungroup()
  }
  
  # ---- Assign catch/value by weight (only for SI_STATE==1) ----
  joined <- joined %>%
    mutate(
      KG   = if ("LE_KG"   %in% names(.) & (variable %in% c("all", "kgs")))   LE_KG   * wt * as.integer(SI_STATE == 1) else NA_real_,
      EURO = if ("LE_EURO" %in% names(.) & (variable %in% c("all", "value"))) LE_EURO * wt * as.integer(SI_STATE == 1) else NA_real_
    )
  
  # Replace NA with 0 for KG/EURO
  joined$KG[is.na(joined$KG)] <- 0
  joined$EURO[is.na(joined$EURO)] <- 0
  
  # Result: keep all tacsat columns plus KG/EURO, drop eflalo columns and temp vars
  keep_cols <- union(names(tacsat), c("KG", "EURO"))
  out <- joined[, intersect(keep_cols, names(joined)), drop=FALSE]
  
  return(out)
}