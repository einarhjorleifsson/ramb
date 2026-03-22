#' Bin a Numeric Vector to Bin Midpoints
#'
#' `rb_midpoint()` assigns each value in a numeric vector to the midpoint of
#' its bin, given a user-specified bin width. Bins are left-closed intervals of
#' the form \eqn{[lo, lo + dx)}, and each value is replaced by \eqn{lo + dx/2}.
#'
#' The bin boundaries are defined relative to zero, so a bin width of `0.5`
#' produces bins \eqn{[\ldots, -0.5, 0, 0.5, 1, \ldots)} with midpoints
#' \eqn{\ldots, -0.25, 0.25, 0.75, 1.25, \ldots}.
#'
#' `NA` values in `x` are silently passed through as `NA`.
#'
#' @param x Numeric vector to be binned.
#' @param dx A single positive numeric value specifying the bin width.
#'
#' @return A numeric vector of the same length as `x`, with each element
#'   replaced by the midpoint of its bin.
#'
#' @examples
#' # Bin latitudes into 1-degree bins
#' rb_midpoint(c(62.3, 62.6, 63.2, 64.9), dx = 1)
#' # Returns: 62.5 62.5 63.5 64.5
#'
#' # Bin values into bins of width 0.2
#' rb_midpoint(c(0.13, 0.22, 0.38, 0.49), dx = 0.2)
#' # Returns: 0.1 0.3 0.3 0.5
#'
#' # Negative values
#' rb_midpoint(c(-1.4, -0.9, 0.1, 1.6), dx = 1)
#' # Returns: -1.5 -0.5 0.5 1.5
#'
#' # Works correctly with non-representable bin widths (e.g. 0.1)
#' rb_midpoint(c(0.1, 0.2, 0.3), dx = 0.1)
#' # Returns: 0.15 0.25 0.35
#'
#' @export
rb_midpoint <- function(x, dx) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")
  if (!is.numeric(dx) || length(dx) != 1) stop("`dx` must be a single numeric value.")
  if (is.na(dx) || dx <= 0) stop("`dx` must be a positive number.")
  
  floor(round(x / dx, digits = 10)) * dx + dx / 2
}
