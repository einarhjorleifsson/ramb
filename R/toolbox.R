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


rb_interval_id2 <- function(point, interval, vid, time, start, end, cruise_id) {
  
  point.dt <-
    point %>%
    dplyr::mutate(rowid = 1:n()) %>% 
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
    dplyr::summarise(pings = n(),
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

#' rb_define_trip
#' 
#' The input tibble needs a variable identifying vessel (default vid) and a variable that
#' indicates the harbour identification number (default hid) whose value indicate 
#' if in harbour.  If out of harbour the value is NA.
#'
#' @param d ais/vms tracks tibble
#' @param vid variable name containing vessel id
#' @param hid variable name conaining harbour id, is NA if out of harbour
#'
#' @return A tibble with one additional variable named tid, sequential positive
#' values indicate trip number, negative values indicate "harbour trip".
#' @export
#'
rb_define_trip <- function(d, vid = vid, hid = hid) {
  d %>% 
    dplyr::mutate(inharbour = ifelse(!is.na( {{hid}} ), TRUE, FALSE)) %>% 
    dplyr::group_by( {{vid}} ) %>% 
    dplyr::mutate(.gr0 = data.table::rleid( inharbour )) %>% 
    dplyr::group_by( {{vid}}, inharbour) %>% 
    dplyr::mutate(tid = data.table::rleid(.gr0)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(tid = ifelse(inharbour, -tid, tid)) %>% 
    dplyr::select(c(-.gr0, inharbour))
  
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
    mutate(whacky = ifelse(.rid %in% rid, "whacky", ""))
  # need to vectorize:
  rids <- NULL
  for(i in 1:length(rid)) rids <- c(rids, rid[i] + c(-2, -1, 0, 1, 2))
  d %>% dplyr::filter(.rid %in% rids)
  
}


#' md_trip
#'
#' @param data A tibble containing variable names lon and lat (crs 4326)
#' @param tid Trip id variable name, default tid
#' @param radius Radius (in meters) of the point size (default 10)
#' @param col What variable to be used for colour scale (default speed)
#' @param trip add trip path, default FALSE
#'
#' @return xxx
#' 
#' @export
#'
md_trip <- function(data, tid, radius = 10, col = "speed", trip = TRUE) {
  
  if(any(!class(data) %in% "sf")) {
    data <- 
      data %>% 
      sf::st_as_sf(coords = c("lon", "lat"),
                   crs = 4326,
                   remove = FALSE)
  }
  
  m <- mapdeck()
  
  if(trip) {
    m <- 
      m %>% 
      mapdeck::add_path(data = data %>% 
                          group_by( {{tid}} ) %>% 
                          summarise(do_union = FALSE) %>%
                          sf::st_cast("LINESTRING"),
                        layer_id = "track",
                        stroke_width = 1000,
                        width_min_pixels = 5,
                        width_max_pixels = 10,
                        # NEED TO MAKE THIS DYNAMIC
                        tooltip = "tid",
                        auto_highlight = TRUE,
                        highlight_colour = "#FF000095",
                        update_view = FALSE,
                        stroke_colour = "#E0FFFF80")
  }
  
  m <- 
    m %>% 
    mapdeck::add_scatterplot(data = data,
                             fill_colour = "speed",
                             radius = radius,
                             palette = "inferno")
  return(m)
}