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
    # Note: Check if really needed
    dplyr::group_by(id) %>%
    tidyr::nest(data = c(x, y, rowid, time, lon, lat, behaviour)) %>%
    dplyr::mutate(data = purrr::map(data, as.data.frame)) %>%
    dplyr::mutate(prepdat = purrr::map(data, function(x) moveHMM::prepData(x, type = "UTM", coordNames = c("x", "y")))) %>%
    dplyr::mutate(prepdat = purrr::map(prepdat, dplyr::filter, !is.na(step))) %>%
    dplyr::mutate(prepdat = purrr::map(prepdat, dplyr::mutate, speed = step/60 * 1.9438)) %>%
    dplyr::mutate(model = purrr::map(prepdat, function(x) mixtools::normalmixEM(x$speed, mu = c(1,4,8), sigma = c(1,1,1)))) %>%
    dplyr::mutate(threshold.lower = purrr::map_dbl(model, function(x) x$mu[1] - cs * x$sigma[1])) %>%
    dplyr::mutate(threshold.upper = purrr::map_dbl(model, function(x) x$mu[1] + cs * x$sigma[1])) %>%
    dplyr::select(id, prepdat, threshold.lower, threshold.upper) %>%
    tidyr::unnest(prepdat) %>%
    dplyr::ungroup()
}

#' Title
#'
#' @param d A data frame with xx
#' @param cs The multiplier on the sigma value of the first Gussian mode (not active)
#'
#' @return A tibble
#' @export
#'
rb_gaussian_binary_clustering <- function(d, cs = 1.96) {
  d %>%
    dplyr::select(id:time, lon:behaviour) %>%
    group_by(id) %>%
    tidyr::nest(data = c(time, lon, lat, behaviour, rowid)) %>%
    # function EMbC expects a data.frame, a tibble is a no, no
    dplyr::mutate(data = purrr::map(data, as.data.frame)) %>%
    dplyr::mutate(model = purrr::map(data, EMbC::stbc, info = -1)) %>%
    dplyr::mutate(model.tbl = purrr::map(model, tidy_bin_clst_path)) %>%
    dplyr::select(id, model.tbl) %>%
    tidyr::unnest(model.tbl) %>%
    dplyr::rename(time = dTm) %>%
    dplyr::mutate(speed = speed * 1.9438)
}

#' Hidden Markov model with step only
#'
#' @param d A data frame with xx
#'
#' @return A tibble
#' @export
#'
rb_hidden_markov_step <- function(d) {

  # Note: These functions should likely not be hardwired in the function call

  par_mixEM <- function(o) {
    sigma <- c(o$sigma[1], o$sigma[2] * (1 - 1/4), o$sigma[3] + 2)
    c(o$mu, sigma)
  }

  lh_fitHMM <- function(o, par0) {
    moveHMM::fitHMM(data = o,  nbStates = 3, stepPar0 = par0,
                    verbose = 0, stepDist = "gamma", angleDist = "none")
  }

  d %>%
    dplyr::select(id, time, x, y, lon, lat, behaviour) %>%
    tidyr::nest(data = c(time, x, y, lon, lat, behaviour)) %>%
    # function prepData expects a data.frame, a tibble is a no, no
    dplyr::mutate(data = purrr::map(data, as.data.frame)) %>%
    dplyr::mutate(data2 = purrr::map(data, moveHMM::prepData, type = "UTM", coordNames = c("x", "y"))) %>%
    # get rid of NA's
    dplyr::mutate(data2 = purrr::map(data2, dplyr::filter, !is.na(step))) %>%
    # get starting values for the parameters
    dplyr::mutate(mixEM = purrr::map(data2, function(x) mixtools::normalmixEM(x$step, mu = c(20,150,300), sigma =c(20,20,20)))) %>%
    # extract the parameters - note, should possibly be just one step, all in a single function
    dplyr::mutate(par0 = purrr::map(mixEM, par_mixEM)) %>%
    dplyr::mutate(model = purrr::map2(data2, par0, lh_fitHMM)) %>%
    dplyr::mutate(ks = purrr::map(model, tidy_movehmm)) %>%
    dplyr::mutate(vit = purrr::map(model, moveHMM::viterbi)) %>%
    dplyr::select(id, data2, vit) %>%
    tidyr::unnest(c(data2, vit))

}

#' Hidden Markov model with step and turn
#'
#' @param d A data frame with xx
#'
#' @return A tibble
#' @export
#'
rb_hidden_markov_step_and_turn <- function(d) {

  # Note: These functions should likely not be hardwired in the function call

  par_mixEM <- function(o) {
    sigma <- c(o$sigma[1], o$sigma[2] * (1 - 1/4), o$sigma[3] + 2)
    c(o$mu, sigma)
  }

  lh_fitHMM_model5 <- function(d, par0) {

    angleMean0 <- c(pi, pi/2, 0) # angle mean
    kappa0 <-     c(0.1, 0.2, 0.8)   # angle concentration
    anglePar0 <- c(angleMean0, kappa0)

    moveHMM::fitHMM(data = d,  nbStates = 3, stepPar0 = par0,
                    verbose = 0, stepDist = "gamma", angleDist = "wrpcauchy",
                    anglePar0 = anglePar0)
  }


  d %>%
    dplyr::select(id, time, x, y, lon, lat, behaviour) %>%
    tidyr::nest(data = c(time, x, y, lon, lat, behaviour)) %>%
    # function prepData expects a data.frame, a tibble is a no, no
    dplyr::mutate(data = purrr::map(data, as.data.frame)) %>%
    dplyr::mutate(data2 = purrr::map(data, moveHMM::prepData, type = "UTM", coordNames = c("x", "y"))) %>%
    # get rid of NA's
    dplyr::mutate(data2 = purrr::map(data2, dplyr::filter, !is.na(step))) %>%
    # get starting values for the parameters
    dplyr::mutate(mixEM = purrr::map(data2, function(x) mixtools::normalmixEM(x$step, mu = c(20,150,300), sigma =c(20,20,20)))) %>%
    # extract the parameters - note, should possibly be just one step, all in a single function
    dplyr::mutate(par0 = purrr::map(mixEM, par_mixEM)) %>%
    dplyr::mutate(model = purrr::map2(data2, par0, lh_fitHMM_model5)) %>%
    dplyr::mutate(ks = purrr::map(model, tidy_movehmm)) %>%
    dplyr::mutate(vit = purrr::map(model, moveHMM::viterbi)) %>%
    dplyr::select(id, data2, vit) %>%
    tidyr::unnest(c(data2, vit))
}

