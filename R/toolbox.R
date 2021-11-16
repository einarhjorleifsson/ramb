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

#' @export
rb_whacky_points <- function(d, tolerance = 10) {
  d %>% 
    dplyr::mutate(.dis99 = round(geo::arcdist(lat, lon, dplyr::lead(lat), dplyr::lead(lon), scale = "nmi"), 2),
                  whacky = dplyr::if_else(.dis99 > tolerance, TRUE, FALSE, TRUE)) %>% 
    dplyr::select(-.dis99)
}
