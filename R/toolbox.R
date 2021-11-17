#' Event id
#'
#' @param x A vector, normally character or integer
#'
#' @return A numerical vector that labels each "discreet" event
#' @export
#'
rb_event <- function(x) {
  x <- dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L)
  x <- cumsum(x)
  return(x)
}

# NOTE: Need to check if data is properly arranged
#' rb_interval_id
#'
#' The function aim is to match an event in one tibble with an interval in an
#' another tibble. Specifically designed to be used with ais/vms data (points) 
#' and fishing logbook records where start and end time of fishing activity is
#' recorded.
#'
#' @param point A tibble containing variables vid (vessel id) and time
#' @param interval A tibble containing variables vid, id.lgs (the fishing 
#' activity id), start and end
#' @param vid The vessel id variable name
#' @param time The name of the time variable in the ais data
#' @param start The name of the start time variable in the logbooks
#' @param end The name of the end time variable in the logbooks
#' @param id The name of the tow id variable in the logbooks
#'
#' @return The point tibble with additional variable id.lgs (fishing activity
#' id).
#' @export
#'
rb_interval_id <- function(point, interval, vid, time, start, end, id) {
  
  point.dt <-
    point %>%
    dplyr::select(vid = {{vid}},
                  t = {{time}}) %>% 
    #dplyr::arrange(t) %>% 
    dplyr::mutate(dummy = t) %>% 
    data.table::data.table()
  interval.dt <-
    interval %>%
    dplyr::select(vid = {{vid}},
                  t1 = {{start}},
                  t2 = {{end}},
                  id = {{id}}) %>% 
    #dplyr::arrange(start, end) %>%
    data.table::data.table()
  
  data.table::setkey(point.dt, vid, t, dummy)
  data.table::setkey(interval.dt, vid, t1, t2)
  
  x <- 
    data.table::foverlaps(point.dt, interval.dt, nomatch = NA) %>% 
    tibble::as_tibble() %>% 
    dplyr::pull( id )
  
  point %>% 
    dplyr::mutate(.id = x) %>% 
    return()
  
}


rb_interval_id2 <- function(point, interval, vid, time, start, end, cruise_id) {
  
  point.dt <-
    point %>%
    mutate(rowid = 1:n()) %>% 
    dplyr::rename(vid = {{vid}},
                  t = {{time}}) %>% 
    #dplyr::arrange(t) %>% 
    dplyr::mutate(dummy = t) %>% 
    data.table::data.table()
  interval.dt <-
    interval %>%
    dplyr::select(vid = {{vid}},
                  t1 = {{start}},
                  t2 = {{end}},
                  cruise_id = {{cruise_id}}) %>% 
    #dplyr::arrange(start, end) %>%
    data.table::data.table()
  
  data.table::setkey(point.dt, vid, t, dummy)
  data.table::setkey(interval.dt, vid, t1, t2)
  
  data.table::foverlaps(point.dt, interval.dt, nomatch = NA) %>% 
    tibble::as_tibble() %>%  
    select(-c(t1, t2, dummy)) %>% 
    return()
  
}


#' rb_whacky_points
#'
#' @param d A tibble containing lon and lat
#' @param tolerance The tolerance atomic value for marking a whacky point (TRUE), units in nautical miles
#'
#' @return A tibble with a boolean variable whacky
#' @export
#'
rb_whacky_points <- function(d, tolerance = 10) {
  d %>% 
    dplyr::mutate(.dis99 = geo::arcdist(lat, lon, dplyr::lead(lat), dplyr::lead(lon), scale = "nmi"),
                  whacky = dplyr::if_else(.dis99 > tolerance, TRUE, FALSE, TRUE)) %>% 
    dplyr::select(-.dis99)
}


#' ms2kn
#'
#' meters per second to knots
#' 
#' @param x A numerical vector of speed in meters per second
#'
#' @return A vector, speed in knots (nautical miles per hour)
#' @export
#'
ms2kn <- function(x) {
  x * 1.94384449
}

#' kn2ms
#'
#' knots to meters per second
#' 
#' @param x A numerical vector of speed in knots
#'
#' @return A vector, speed in meters per second
#' @export
#'
kn2ms <- function(x) {
  x / 1.94384449
}
