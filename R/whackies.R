#' Yet another speed filter
#' 
#' This function chops sequentially off the first encountered whack and then
#' starts again from the beginning.
#' 
#' THIS FUNCTION DOES NOT WORK IF THE FIRST DATAPOINT IN THE SEQUENCE IS A
#' WHACKY POINT
#' 
#' To use this on multiple trips or vessels, use a grouped data frame with 
#' tidyverse code like 
#' data |>  group_by(id) |> mutate(whack = rb_whacky_speed(lon, lat, time))
#' 
#' The function uses traipse::track_speed, where by convention the first 
#' value is set to NA (missing value), because the difference applies to 
#' each sequential pair of locations. Here, these missing values are
#' replaced by zero.
#' 
#' @param lon longitude in decimal degrees
#' @param lat latitude in decimal degrees
#' @param time date-time in POSIXct
#' @param kn_max speed threshold in knots
#'
#' @return a boolean vector, TRUE if point classified as whacky
#' @export
#'
rb_whacky_speed <- function(lon, lat, time, kn_max = 25) {
  
  if(length(lon) != length(lat) | length(lon) != length(time)) {
    stop("Length of coordinates and time must be the same")
  }
  
  ms_max <- rb_kn2ms(kn_max)
  .rid_original <- 1:length(lon)
  
  d <- 
    tibble::tibble(time = time,
                   x = lon, 
                   y = lat) |> 
    dplyr::mutate(.rid = 1:dplyr::n(),
                  speed = traipse::track_speed(x, y, time),
                  speed = tidyr::replace_na(speed, 0))
  
  while(any(d$speed > ms_max, na.rm = TRUE)) {
    a_whack <-
      d |> 
      dplyr::filter(speed > ms_max) |> 
      dplyr::slice(1) |> 
      dplyr::pull(.rid)
    if(a_whack == 2) a_whack <- 1
    d <- 
      d |> 
      dplyr::filter(.rid != a_whack) |>  
      dplyr::mutate(speed = traipse::track_speed(x, y, time),
                    speed = tidyr::replace_na(speed, 0))
  }
  
  x <- ifelse(.rid_original %in% d$.rid, FALSE, TRUE)
  
  p <- round(sum(x) / length(x) * 100, digits = 1)
  if(p > 5) {
    message(paste0(p, "% of the data are classified as whacky points"))
  }
  return(x)
  
}



#' Yet another distance filter
#' 
#' To use this on multiple trips, use a grouped data frame with 
#' tidyverse code like 
#' data |>  group_by(id) |> mutate(whack = rb_whacky_distance(lon, lat))
#' 
#' The function uses traipse::track_distance, which by convention the first 
#' value is set to NA missing value, because the difference applies to 
#' each sequential pair of locations. Here, these missing values are
#' replaced by zero.
#' 
#' The function is overly liberal in that 
#' @param lon longitude in decimal degrees
#' @param lat latitude in decimal degrees
#' @param miles_max maximum threshold in miles
#'
#' @return a boolean vector, TRUE if point classified as whacky
#'
rb_whacky_distance <- function(lon, lat, miles_max = 6) {
  
  if(length(lon) != length(lat)) {
    stop("Length of coordinates and time must be the same")
  }
  
  meters_max <- miles_max * 1852
  
  .rid_original <- 1:length(lon)
  
  d <- 
    tibble::tibble(x = lon, 
                   y = lat) |> 
    dplyr::mutate(.rid = 1:dplyr::n(),
                  distance = traipse::track_distance(x, y),
                  distance = tidyr::replace_na(distance, 0))
  
  while(any(d$distance > meters_max, na.rm = TRUE)) {
    a_whack <-
      d |> 
      dplyr::filter(distance > meters_max) |> 
      dplyr::slice(1) |> 
      dplyr::pull(.rid)
    d <- 
      d |> 
      dplyr::filter(.rid != a_whack) |>  
      dplyr::mutate(distance = traipse::track_distance(x, y),
                    distance = tidyr::replace_na(distance, 0))
  }
  
  x <- ifelse(.rid_original %in% d$.rid, FALSE, TRUE)
  return(x)
  
}



# https://academic.oup.com/icesjms/article/81/2/390/7516127

#' Yet another speed filter
#' 
#' The input dataframe has to contain the following variables: x, y, time. device_id and seq.
#' The x and the y are coordates in units of meters.
#' 
#' This function is fast but it chops off equal amount of valid points as invalid
#' points. Function rb_whacky_speed is more conservative, albeit slower. The latter
#' also does not filter the data.
#'
#' @param df A dataframe containing variables named 
#' @param speed_filter speed in knots
#'
#' @return A filtered dataframe with a bunch of stuff
#' @export
#'
rb_whacky_speed_mendo <- function(df, speed_filter = 25) {
  
  ms2knots = 1.9438 #(m/s)/knots
  
  df <- 
    df |> 
    dplyr::group_by(device_id) |> 
    dplyr::mutate(dx = c(0,abs(diff(x))),
                  dy = c(0,abs(diff(y)))) |> 
    dplyr::mutate(dt = c(as.numeric(time - dplyr::lag(time),units="secs")),
                  dd = sqrt(dx^2 + dy^2),
                  speed = dd / dt * ms2knots) |> 
    dplyr::ungroup()
  
  repeat {
    subset <-
      df |> 
      dplyr::filter(speed > speed_filter)
    sel <- factor(subset$seq)
    nrows <- length(sel)
    if (nrows==0) {
      break
    }  else {
      df <- df[!df$seq %in% sel,]
    }
    df <-
      df |> 
      dplyr::group_by(device_id) |> 
      dplyr::mutate(dx = c(0, abs(diff(x))),
                    dy = c(0, abs(diff(y)))) |> 
      dplyr::mutate(dt = c(as.numeric(time - dplyr::lag(time), units="secs"))) |> 
      dplyr::mutate(dd = sqrt(dx^2 + dy^2)) |> 
      dplyr::mutate(speed = dd / dt * ms2knots) |> 
      # add so data returned to user is 
      dplyr::ungroup()
  }
  return(df)
}

#' Title
#'
#' @param d An sf-dataframe contain lon, lat and time
#' @param filter Drop whacky points
#' @param max_speed Criteria for drops, units in knots
#'
#' @return a dataframe with addition variable ".whacky"
#' @export
#'
rb_whacky_speed_trip <- function(d, filter = TRUE, max_speed = 20) {
  tr <- methods::as(d |> dplyr::mutate(.idtrip = 1), "Spatial")
  tr <- suppressWarnings( trip::trip(tr, c("time", ".idtrip")) )
  d$.whacky <- !trip::speedfilter(tr, max.speed = ramb::rb_kn2ms(max_speed) / 1000 * 60 * 60)
  if(filter) {
    d |> 
      dplyr::filter(!.whacky) |> 
      dplyr::select(-.whacky)
  }
  # some extra precaution - would be of interest why not captured above
  if(filter) {
    d <-
      d |> 
      dplyr::mutate(.whacky = ramb::rb_whacky_speed(lon, lat, time)) |> 
      dplyr::filter(!.whacky) |> 
      dplyr::select(-.whacky)
  }
  
  return(d)
}
