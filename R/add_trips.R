#' Add trip id to VMS data
#'
#' Adds a trip id column to a VMS dataset by matching each VMS record to logbook entries where:
#'   - vessel IDs match, and
#'   - the VMS timestamp falls between logbook departure and arrival times.
#' Similar to \code{vmstools::mergeEflalo2Tacsat}, but here not dependent on specific variable names in the data frames.
#'
#' @param ve A data frame or tibble containing VMS data. Must have columns specified by \code{vessel_id} and \code{time}.
#' @param le A data frame or tibble containing logbook data. Must have columns specified by \code{vessel_id}, \code{trip_id}, \code{departure}, and \code{arrival}.
#' @param vessel_id Unquoted column name for vessel ID (must exist in both datasets). Default: \code{vid}.
#' @param trip_id Unquoted column name for trip ID in the logbook dataset. Default: \code{tid}.
#' @param time Unquoted column name for timestamp in the VMS dataset. Default: \code{time}.
#' @param departure Unquoted column name for trip departure time in the logbook dataset. Default: \code{T1}.
#' @param arrival Unquoted column name for trip arrival time in the logbook dataset. Default: \code{T2}.
#'
#' @return A tibble with all columns from the VMS data and the trip id column appended (if matched). The departure and arrival columns from the logbook are removed.
#' @export
#'
#' @examples
#' # rb_add_trips_id(ve, le, vessel_id = vid, trip_id = tid, time = time, departure = T1, arrival = T2)
rb_add_tripid <- function(ve, le, vessel_id = vid, trip_id = tid, time = time, departure = T1, arrival = T2) {
  
  # Turn column names into strings for checking
  vessel_id_chr <- as.character(substitute(vessel_id))
  trip_id_chr <- as.character(substitute(trip_id))
  time_chr <- as.character(substitute(time))
  departure_chr <- as.character(substitute(departure))
  arrival_chr <- as.character(substitute(arrival))
  
  # Check for required columns in VMS data
  vms_missing <- setdiff(c(vessel_id_chr, time_chr), names(ve))
  if(length(vms_missing) > 0) {
    warning("Missing the following columns in VMS data: ", paste(vms_missing, collapse = ", "))
  }
  
  # Check for required columns in logbook data
  logbook_missing <- setdiff(c(vessel_id_chr, trip_id_chr, departure_chr, arrival_chr), names(le))
  if(length(logbook_missing) > 0) {
    warning("Missing the following columns in logbook data: ", paste(logbook_missing, collapse = ", "))
  }
  
  out <- ve |>
    dplyr::left_join(le |> dplyr::distinct( {{ vessel_id }}, {{ trip_id }}, .keep_all = TRUE),
                     by = dplyr::join_by( {{ vessel_id }}, dplyr::between( {{time}}, {{ departure }}, {{ arrival }} ))) |>
    dplyr::select(-dplyr::any_of(c(departure_chr, arrival_chr)))
  
  # Warn if all trip_ids are NA after join
  if(all(is.na(out[[trip_id_chr]]))) {
    warning("No trip IDs were matched to the VMS data. Check if timestamps and vessel IDs overlap between datasets.")
  }
  
  # Warn about dropped columns
  warning("Departure and arrival columns (", departure_chr, ", ", arrival_chr, ") will be removed from the output.")
  
  return(out)
}

