#' A poor-mans bounding box
#'
#' @param x A dataframe with x and y
#'
#' @return A vector
#' @export
#'
mb_bb <- function(x) {
  c(xmin = min(x$x), ymin = min(x$y), xmax = max(x$x), ymax = max(x$y))
}