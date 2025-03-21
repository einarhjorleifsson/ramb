
#' A wrapper around approx
#'
#' @param x A vector, nomally time
#' @param y The value to interpolate
#'
#' @return A vector with orgiginal and interpolated values, if any
#' @export
#'
rb_interpolate <- function(x, y) {
  stats::approx(x, y, x, method = "linear", rule = 1, f = 0, ties = mean)$y
}