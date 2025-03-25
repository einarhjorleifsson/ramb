#' Points in polygon
#' 
#' Find points that are within polygon(s)
#'
#' @param x object containing geometry POINT of class sf or sfc
#' @param y object containing geometry POLYGON of class sf or sfc
#'
#' @return A boolean vector
#' 
#' @export
#'
rb_points_in_polygons <- function(x, y) {
  
  # the checks
  if(!inherits(sf::st_geometry(x), "sfc_POINT")) { 
    stop("Expected first object to be POINTs")
  }
  if(!inherits(sf::st_geometry(y), "sfc_POLYGON")) { 
    stop("Expected latter object to be POLYGONs")
  }
  
  if(sf::st_crs(x) != sf::st_crs(y)) {
    stop("The objects do not have the same projections")
  }
  
  # the meat  
  i <- sf::st_intersects(x, y) |> lengths() > 0
  return(i)
  
}


#' Points in polygon
#'
#' @param x object containing geometry POINT of class sf, sfc or sfg
#' @param y object containing geometry POLYGON of class sf, sfc or sfg
#'
#' @return Normally POINT object of class sf that are within object y
#' 
#' @note No serious testing in done
#' @export
#'
rb_st_keep <- function(x, y) {
  
  i <- rb_points_in_polygons(x, y)
  
  # the return
  if(inherits(x, "data.frame")) {
    x <- x[i, ]
    return(x)
  }
  
  if(inherits(x, "sfc")) {
    return(i)
  }
  
}


#' Points not in polygon
#'
#' @param x object containin geometry POINT of class sf, sfc or sfg
#' @param y object cotaining geometry POLYGON of class sf, sfc or sfg
#'
#' @return Normally POINT object of class sf that are within object y
#' 
#' @export
#'
rb_st_drop <- function(x, y) {
  
  i <- rb_points_in_polygons(x, y)
  i <- !i
  
  # the return
  if(inherits(x, "data.frame")) {
    x <- x[i, ]
    return(x)
  }
  
  if(inherits(x, "sfc")) {
    return(i)
  }
}