#' Add trip id to VMS data
#'
#' Adds the trip id to the VMS data based on vms timestamps that are between logbook
#' departure and arrival time. Supposedly doing the same thing as vmstools::mergeEflalo2Tacsat
#'
#' @param ve VMS dataset
#' @param le Logbook dataset
#' @param vessel_id The vessel id in both dataset (default vid)
#' @param trip_id The trip id variable in the logbook dataset (default tid)
#' @param time The time variable in the VMS dataset (default time)
#' @param departure Trip departure time in the logbook dataset (default T1)
#' @param arrival Trip arrival time in the logbook dataset (default T2)
#'
#' @return A tibble
#' @export
#'
rb_add_trips <- function(ve, le, vessel_id = vid, trip_id = tid, time = time, departure = T1, arrival = T2) {
  ve |>
    dplyr::left_join(le |> dplyr::distinct( {{ vessel_id }}, {{ trip_id }} , .keep_all = TRUE),  # keep only the first row
                     by = dplyr::join_by( {{ vessel_id }}, dplyr::between( {{time}}, {{ departure }}, {{ arrival }} ))) |>
    dplyr::select(-c({{ departure }}, {{ arrival }}))
}

#' Add trip id to VMS data
#'
#' Adds the trip id to the VMS data based on vms timestamps that are between logbook
#' departure and arrival time. Supposedly doing the same thing as vmstools::mergeEflalo2Tacsat
#'
#' @param ve VMS dataset
#' @param le Logbook dataset
#' @param vessel_id The vessel id in both dataset (default VE_REF)
#' @param trip_id The trip id variable in the logbook dataset (default FT_REF)
#' @param time The time variable in the VMS dataset (default SI_DATE)
#' @param departure Trip departure time in the logbook dataset (default FT_DTIME)
#' @param arrival Trip arrival time in the logbook dataset (default FT_LTIME)
#'
#' @return A tibble
#'
#' @export
#'
rb_add_trips_vt <- function(ve, le, vessel_id = VE_REF, trip_id = FT_REF, time = SI_DATE, departure = FT_DTIME, arrival = FT_LTIME) {
  ve |>
    dplyr::left_join(le |> dplyr::distinct( {{ vessel_id }}, {{ trip_id }} , .keep_all = TRUE),  # keep only the first row
                     by = dplyr::join_by( {{ vessel_id }}, dplyr::between( {{time}}, {{ departure }}, {{ arrival }} ))) |>
    dplyr::select(-c({{ departure }}, {{ arrival }}))
}

