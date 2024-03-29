#' Event id
#'
#' @param x A vector, normally character or integer
#'
#' @return A numerical vector that labels each discreet event
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

#' @export
rb_interval_id2 <- function(point, interval, vid, time, start, end, id) {
  
  point.dt <-
    point %>%
    #dplyr::mutate(rowid = 1:dplyr::n()) %>% 
    dplyr::rename(vid = {{vid}},
                  time = {{time}}) %>% 
    #dplyr::arrange(t) %>% 
    dplyr::mutate(dummy = time) %>% 
    data.table::data.table()
  interval.dt <-
    interval %>%
    dplyr::select(vid = {{vid}},
                  t1 = {{start}},
                  t2 = {{end}},
                  .id = {{id}}) %>% 
    #dplyr::arrange(start, end) %>%
    data.table::data.table()
  
  data.table::setkey(point.dt, vid, time, dummy)
  data.table::setkey(interval.dt, vid, t1, t2)
  
  data.table::foverlaps(point.dt, interval.dt, nomatch = NA) %>% 
    tibble::as_tibble() %>%  
    dplyr::select(-c(t1, t2, dummy)) %>% 
    return()
  
}
# stk <- 
#   expand_grid(vid = c(1000, 1001, 1002),
#               time = seq(ymd_hms("2022-09-22 00:00:00"), 
#                          ymd_hms("2022-09-22 23:59:59"), 
#                          by = "min"))
# lgs <- 
#   tibble(vid = c(1000, 1000, 1001),
#          visir = c(-1, -2, -3),
#          start_setting = c(ymd_hms("2022-09-22 10:00:00"), ymd_hms("2022-09-22 15:00:00"), ymd_hms("2022-09-22 10:00:00")),
#          start_haul = c(ymd_hms("2022-09-22 11:00:00"), ymd_hms("2022-09-22 16:00:00"), ymd_hms("2022-09-22 11:30:00")),
#          end_haul = c(ymd_hms("2022-09-22 14:00:00"), ymd_hms("2022-09-22 18:30:00"), ymd_hms("2022-09-22 12:00:00")))
# stk %>% 
#   rb_interval_id2(lgs, vid, time, start_setting, end_haul, visir) %>% 
#   ggplot(aes(time, factor(vid), fill = factor(.id))) +
#   geom_tile()


#' rb_whacky_points
#'
#' @param d A tibble containing lon and lat
#' @param tolerance The tolerance atomic value for marking a whacky point (TRUE), units in nautical miles
#'
#' @return A tibble with a boolean variable whacky
#' 
#' @author Einar Hjörleifsson, \email{einar.hjorleifsson@gmail.com}
#' 
#' @export
#'
rb_whacky_points <- function(d, tolerance = 10) {
  d %>% 
    dplyr::mutate(.dis99 = geo::arcdist(lat, lon, dplyr::lead(lat), dplyr::lead(lon), scale = "nmi"),
                  whacky = dplyr::if_else(.dis99 > tolerance, TRUE, FALSE, TRUE)) %>% 
    dplyr::select(-.dis99)
}


#' rb_ms2kn
#'
#' meters per second to knots for those of us that forget the convertion number
#' 
#' @param x A numerical vector of speed in meters per second
#'
#' @return A vector, speed in knots (nautical miles per hour)
#' @export
#' 
#' @author Einar Hjörleifsson, \email{einar.hjorleifsson@gmail.com}
#'
#' @examples
#' rb_ms2kn(0:8)
rb_ms2kn <- function(x) {
  x * 1.94384449
}

#' rb_kn2ms
#'
#' knots to meters per second for those of us that forget the convertion number
#' 
#' @param x A numerical vector of speed in knots
#'
#' @return A vector, speed in meters per second
#' @export
#' 
#' @author Einar Hjörleifsson, \email{einar.hjorleifsson@gmail.com}
#'
#' @examples
#' rb_kn2ms(seq(0, 16, by = 2))
rb_kn2ms <- function(x) {
  x / 1.94384449
}


#' rb_summary
#'
#' @param d ...
#'
#' @return A summary tibble, dependent on grouping upstream
#' @export
#'
rb_summary <- function(d) {
  d %>% 
    dplyr::summarise(pings = dplyr::n(),
                     t.min = min(time),
                     t.max = max(time),
                     s.min = min(speed),
                     s.med = median(speed),
                     s.max = max(speed),
                     di.min = min(dist, na.rm = TRUE),
                     di.med = median(dist, na.rm = TRUE),
                     di.max = max(dist, na.rm = TRUE),
                     du.min = min(dura, na.rm = TRUE),
                     du.med = median(dura, na.rm = TRUE),
                     du.max = max(dura, na.rm = TRUE),
                     n.tips = dplyr::n_distinct(tid, na.rm = TRUE),
                     n.harb = dplyr::n_distinct(hid, na.rm = TRUE))
}

#' Create trip
#'
#' @param x A vector
#'
#' @return An integer vector of the same length as input, providing unique trip number
#' @export
#'
rb_trip <- function(x) {
  tibble::tibble(x = x) %>% 
    dplyr::mutate(tid = dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L),
                  tid = ifelse(x, -tid, tid)) %>% 
    dplyr::group_by(x) %>% 
    dplyr::mutate(tid = cumsum(tid)) %>% 
    dplyr::ungroup(x) %>% 
    dplyr::pull(tid)
}

#' Pad harbour id
#' 
#' Add the harbour id to the first and the last point of an actual trip
#'
#' @param tid trip id, possibly generated by a rb_trip
#' @param hid harbour id
#'
#' @return The hid vector with added harbour id to the first and the last point
#' of a trip.
#' @export
#'
rb_pad_harbour <- function(tid, hid) {
  tibble::tibble(.tid = tid,
                 .hid = hid) %>% 
    dplyr::mutate(.hid = dplyr::case_when(.tid !=  dplyr::lag(.tid) & .tid > 0 ~  dplyr::lag(.hid),
                                          .tid != dplyr::lead(.tid) & .tid > 0 ~ dplyr::lead(.hid),
                                          TRUE ~ .hid)) %>% 
    dplyr::pull(.hid)
}


#' rb_peek
#' 
#' Experimental function, returns rows adjacent to the one fulfilling a critera
#'
#' @param d ais/vms track tibble
#' @param what the variable name to check
#' @param criteria the critera.
#'
#' @return a tibble
#' @export
#'
rb_peek <- function(d, what, criteria) {
  
  d %>% 
    dplyr::filter( {{what}} > criteria) %>% 
    dplyr::pull(.rid) ->
    rid
  d <-
    d %>% 
    dplyr::mutate(whacky = ifelse(.rid %in% rid, "whacky", ""))
  # need to vectorize:
  rids <- NULL
  for(i in 1:length(rid)) rids <- c(rids, rid[i] + c(-2, -1, 0, 1, 2))
  d %>% dplyr::filter(.rid %in% rids)
  
}
