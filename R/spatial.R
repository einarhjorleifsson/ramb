#' Detect points inside polygons
#'
#' @param points POINTs, as an sf or sfc object.
#' @param polygons POLYGONs, as an sf or sfc object, in the same CRS as
#'   `points`.
#'
#' @return A logical vector, `TRUE` where the point lies in a polygon.
#'
#' @seealso [rb_keep_in_polygons()], [rb_drop_in_polygons()].
#' @export
rb_detect_in_polygons <- function(points, polygons) {
  # the checks
  if (!inherits(sf::st_geometry(points), "sfc_POINT")) {
    stop("Expected first object to be POINTs")
  }
  if (!inherits(sf::st_geometry(polygons), "sfc_POLYGON")) {
    stop("Expected latter object to be POLYGONs")
  }

  if (sf::st_crs(points) != sf::st_crs(polygons)) {
    stop("The objects do not have the same projections")
  }

  # the meat
  i <- sf::st_intersects(points, polygons) |> lengths() > 0
  return(i)
}


#' Keep the points inside polygons
#'
#' @param points POINTs: an sf data frame (its rows are filtered) or an sfc
#'   geometry.
#' @param polygons POLYGONs, as an sf or sfc object, in the same CRS as
#'   `points`.
#'
#' @return For an sf data frame, the rows inside a polygon; for an sfc, the
#'   logical vector from [rb_detect_in_polygons()].
#' @export
rb_keep_in_polygons <- function(points, polygons) {
  i <- rb_detect_in_polygons(points, polygons)

  # the return
  if (inherits(points, "data.frame")) {
    points <- points[i, ]
    return(points)
  }

  if (inherits(points, "sfc")) {
    return(i)
  }
}


#' Drop the points inside polygons
#'
#' @param points POINTs: an sf data frame (its rows are filtered) or an sfc
#'   geometry.
#' @param polygons POLYGONs, as an sf or sfc object, in the same CRS as
#'   `points`.
#'
#' @return For an sf data frame, the rows outside every polygon; for an sfc,
#'   the negated logical vector from [rb_detect_in_polygons()].
#' @export
rb_drop_in_polygons <- function(points, polygons) {
  i <- rb_detect_in_polygons(points, polygons)
  i <- !i

  # the return
  if (inherits(points, "data.frame")) {
    points <- points[i, ]
    return(points)
  }

  if (inherits(points, "sfc")) {
    return(i)
  }
}
