#' Categories MMSI
#'
#'
#' See: https://en.wikipedia.org/wiki/Maritime_Mobile_Service_Identity
#'
#' @param mmsi MMSI
#'
#' @return A vector with mmsi categories
#' @export
#'
rb_mmsi_category <- function(mmsi) {
  
  category <-
    tibble::tibble(mmsi = mmsi) |>
    dplyr::mutate(mmsi = as.character(mmsi),
                  mmsi_candidate =
                    dplyr::case_when(
                      nchar(mmsi) == 9 & numbers_only(mmsi) ~ TRUE,
                      .default = FALSE)) |>
    dplyr::mutate(.mmsi_category =
                    dplyr::case_when(mmsi_candidate & stringr::str_sub(mmsi, 1, 1) == "0" ~ "coast station",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 1) == "1" ~ "SAR aircraft",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 1) %in% as.character(2:7) ~ "vessel",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 1) == "8" ~ "Handheld VHF transceiver",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 3) == "970" ~ "Search and Rescue Transponders",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 3) == "972" ~ "Man overboard",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 3) == "974" ~ "406 MHz EPIRBs fitted with an AIS transmitter",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 2) == "98" ~ "Child vessel",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 2) == "99" ~ "Navigational aid",
                                     mmsi_candidate & stringr::str_sub(mmsi, 1, 1) == "9" ~ "Starts with 9",
                                     .default = NA)) |>
    dplyr::pull(.mmsi_category)
  
  return(category)
  
}

#' Return flag state from MMSI
#'
#' @param mmsi MMSI
#' @param lookup A tibble
#'
#' @return A vector
#' @export
#'
rb_mmsi_flag <- function(mmsi, lookup) {
  
  # Check that it doesn't match any non-number
  numbers_only <- function(x) !grepl("\\D", x)
  
  tibble::tibble(mmsi = mmsi) |>
    dplyr::mutate(.mmsi_candidate =
                    dplyr::case_when(nchar(mmsi) == 9 & numbers_only(mmsi) ~ TRUE,
                                     .default = FALSE)) |>
    dplyr::mutate(.MID =
                    dplyr::case_when(
                      .mmsi_candidate == TRUE & stringr::str_sub(mmsi, 1, 1) %in% as.character(2:7) ~ stringr::str_sub(mmsi, 1, 3),
                      .mmsi_candidate & stringr::str_sub(mmsi, 1, 2) == "98" ~ stringr::str_sub(mmsi, 3, 5),
                      .default = NA)) |>
    dplyr::mutate(.MID = as.integer(.MID)) |>
    dplyr::left_join(lookup |> dplyr::mutate(MID = as.integer(MID)),
                     by = dplyr::join_by(.MID == MID)) |>
    dplyr::pull(flag)
}

