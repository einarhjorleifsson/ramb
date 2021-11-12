#' Fit Gaussian mixture model on vessel speed
#'
#' @param d A data frame with xx
#' @param cs The multiplier on the sigma value of the first Gussian mode
#'
#' @return A tibble containing lower and upper speed threshold of the first Gaussian mode
#' @export
#'
rb_gaussian <- function(d, cs = 1.96) {
  d %>%
    dplyr::group_by(id) %>%
    tidyr::nest(data = c(x, y, rowid, time, lon, lat, behaviour)) %>%
    dplyr::mutate(data = purrr::map(data, as.data.frame)) %>%
    dplyr::mutate(prepdat = purrr::map(data, function(x) moveHMM::prepData(x, type = "UTM", coordNames = c("x", "y")))) %>%
    dplyr::mutate(prepdat = purrr::map(prepdat, dplyr::filter, !is.na(step))) %>%
    dplyr::mutate(prepdat = purrr::map(prepdat, dplyr::mutate, speed = step/60 * 1.9438)) %>%
    dplyr::mutate(m2 = purrr::map(prepdat, function(x) mixtools::normalmixEM(x$speed, mu = c(1,4,8), sigma = c(1,1,1)))) %>%
    dplyr::mutate(threshold.lower = purrr::map_dbl(m2, function(x) x$mu[1] - cs * x$sigma[1])) %>%
    dplyr::mutate(threshold.upper = purrr::map_dbl(m2, function(x) x$mu[1] + cs * x$sigma[1])) %>%
    dplyr::select(id, prepdat, threshold.lower, threshold.upper) %>%
    tidyr::unnest(prepdat) %>%
    dplyr::ungroup()
}
