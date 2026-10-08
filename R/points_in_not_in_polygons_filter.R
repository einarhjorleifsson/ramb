#' Points not in polygon
#'
#' @param x object containin geometry POINT of class sf, sfc or sfg
#' @param y object cotaining geometry POLYGON of class sf, sfc or sfg
#'
#' @return Normally POINT object of class sf that are within object y
#' 
#' @note No serious tesing is done
#' 
#' @export
#'
rb_st_remove <- function(x, y) {
  i <- lengths(sf::st_intersects(x, y)) == 0
  x <- x[i, ]
  return(x)
}