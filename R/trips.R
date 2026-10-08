#' Create trip
#'
#' The function 
#'
#' @param x A boolean vector
#'
#' @return An integer vector of the same length as input, providing unique trip number
#' @export
#'
rb_trip <- function(x) {
  .rb_superseded("rb_trip", 'rb_cut_trip_voyages(method = "runs")')
  tibble::tibble(x = x) |> 
    dplyr::mutate(tid = dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L),
                  tid = ifelse(x, -tid, tid))  |>  
    dplyr::group_by(x) |>  
    dplyr::mutate(tid = cumsum(tid))  |>  
    dplyr::ungroup(x) |> 
    dplyr::pull(tid)
}

#' define_trips_jepol
#'
#' Use the columns "SI_HARB" to determine when a vessel is on a trip. A trip is defined
#' from when it leaves the harbour till it returns
#'
#' @param vessel_id a vector containing vessel id
#' @param time a vector containing timestamp
#' @param in_harbour a binary vector indicating if vessel in harbour (1) or not (0)
#' @param min_dur the minimum trip length (hours)
#' @param max_dur the maximum trip length (hours)
#' @param split_trips If the trip is longer than the maximum hours, it will try to split
#' the trip into two or more trips, if there is long enough intervals between pings
#' 
#'
#' @return sequential numbers identifying trips, unique within each vessel
#' 
#' @export


rb_trip_jepol <- function(vessel_id, time, in_harbour,
                          min_dur = 0.5, max_dur = 72, split_trips = TRUE) {
  .rb_superseded("rb_trip_jepol", 'rb_cut_trip_voyages(method = "jepol")')
  .rb_jepol(vessel_id, time, in_harbour, min_dur, max_dur, split_trips)$trip
}


