#' Predict gear width in meters based on Benthis
#' 
#' @param metier Metier 5
#' @param length Vessel length in meters
#' @param kw Vessel power
#'
#' @return A numeric vector in meters
#' @export
#'
rb_benthis_width <- function(metier, length, kw) {
  tibble::tibble(metier5 = {{metier}}, 
                 length = {{length}}, 
                 kw = {{kw}}) |> 
    dplyr::left_join(metier5_benthis_lookup,
                     by = dplyr::join_by(metier5)) |> 
    dplyr::left_join(benthis_parameters |> 
                       dplyr::select(benthis_metier:variable),
                     by = dplyr::join_by(benthis_metier)) |> 
    dplyr::mutate(width = 
                    dplyr::case_when(
                      model == "power" & variable == "avg_kw" ~    a * kw^b,
                      model == "power" & variable == "avg_oal" ~   a * length^b,
                      model == "linear" & variable == "avg_oal" ~  a * length + b,
                      .default = NA)) |> 
    dplyr::pull(width)
}