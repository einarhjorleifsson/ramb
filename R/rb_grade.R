#' Binning Data to Midpoints
#'
#' \code{rb_grade} assigns each value in a numeric vector to the midpoint 
#' of its bin, given a user-specified bin width.
#'
#' @param x Numeric vector to be binned (e.g., longitude, latitude, or any continuous variable).
#' @param dx Numeric value specifying the width of each bin.
#'
#' @return Numeric vector of the same length as \code{x}, with each element replaced by its bin midpoint.
#' @examples
#' # Example 1: Bin latitudes into 1-degree bins
#' lat <- c(62.3, 62.6, 63.2, 64.9)
#' rb_grade(lat, 1)
#' # Returns: 62.5 62.5 63.5 64.5
#'
#' # Example 2: Bin values into bins of width 0.2
#' vals <- c(0.13, 0.22, 0.38, 0.49)
#' rb_grade(vals, 0.2)
#' # Returns: 0.1 0.3 0.3 0.5
#'
#' # Example 3: Negative values
#' rb_grade(c(-1.4, -0.9, 0.1, 1.6), 1)
#' # Returns: -1.5 -0.5 0.5 1.5
#'
#' @export
rb_grade <- function(x, dx) {
  # Input validation
  stopifnot(is.numeric(x), length(dx) == 1, is.numeric(dx), dx > 0)
  x %/% dx * dx + dx/2
}