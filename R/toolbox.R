#' Event id
#'
#' @param x A vector, normally character or integer
#'
#' @return A numerical vector that labels each "discreet" event
#' @export
#'
rb_event <- function(x) {
  x <- dplyr::if_else(x != lag(x), 1L, 0L, 1L)
  x <- cumsum(x)
  return(x)
}
