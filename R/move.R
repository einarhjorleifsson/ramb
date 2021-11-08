# TODO: vectorise bearing in order to get rid of row
#' @export
stk_bearing <- function(d, lon = lon, lat = lat) {
  d %>%
    dplyr::mutate(.x2 = dplyr::lead({{ lon }}), .y2 = dplyr::lead({{ lat }})) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bearing = geosphere::bearing(c({{ lon }}, {{ lat }}), c(.x2, .y2))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.x2, .y2))
}
#' @export
stk_angle <- function(d, bearing = bearing) {
  d %>%
    dplyr::mutate(angle = ({{ bearing }} - dplyr::lead({{ bearing }})) / 180 * pi)
}
#' @export
stk_distance <- function(d, lon = lon, lat = lat) {
  d %>%
    dplyr::mutate(distance = geo::arcdist({{ lat }}, {{ lon }}, dplyr::lead({{ lat }}), dplyr::lead({{ lon }}), scale = "km") * 1000)
}
