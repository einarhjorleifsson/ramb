#' A poor-mans bounding box
#'
#' @param x A dataframe with x and y
#'
#' @return A vector
#' @export
#'
rb_create_bbox <- function(x) {
  c(xmin = min(x$x), ymin = min(x$y), xmax = max(x$x), ymax = max(x$y))
}