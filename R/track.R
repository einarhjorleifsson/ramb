#' Great-circle distance between two points
#'
#' Haversine distance, in metres, on a sphere of radius 6,371,000 m: the radius the
#' impossible-position filters ([rb_flag_ping_impossible()]) use, so a distance computed here
#' agrees with theirs. Vectorised, and the four arguments recycle to a common length, so it
#' works inside `mutate()` with `dplyr::lag()` and `dplyr::lead()`.
#'
#' @param lon1,lat1 Longitude and latitude of the first point, decimal degrees.
#' @param lon2,lat2 Longitude and latitude of the second point, decimal degrees.
#'
#' @return Numeric vector of distances in metres; `NA` where any coordinate is `NA`.
#'
#' @examples
#' rb_calc_distance(0, 64, 0, 65)                 # one degree of latitude, 111,195 m
#' rb_calc_distance(-21.93, 64.15, -23.93, 65.65) # Reykjavik to Talknafjordur, about 190 km
#' \dontrun{
#' pings |>
#'   group_by(vid) |>
#'   mutate(m = rb_calc_distance(dplyr::lag(lon), dplyr::lag(lat), lon, lat)) |>
#'   ungroup()
#' }
#'
#' @export
rb_calc_distance <- function(lon1, lat1, lon2, lat2) {
  k <- pi / 180
  a <- sin((lat2 - lat1) * k / 2)^2 +
    cos(lat1 * k) * cos(lat2 * k) * sin((lon2 - lon1) * k / 2)^2
  2 * 6371000 * asin(sqrt(pmin(1, a)))
}
# geodesic.R — distance on the WGS84 ellipsoid, recoded from traipse/geodist (plan 013: no copied code).
#
# Vincenty's inverse formula. It agrees with geodist's "geodesic" measure (Karney) to well under a millimetre for
# the short steps of a track; it may not converge for nearly antipodal points, which successive pings never are.

.rb_geodesic <- function(lon1, lat1, lon2, lat2) {
  a <- 6378137
  f <- 1 / 298.257223563
  b <- (1 - f) * a
  k <- pi / 180
  L <- (lon2 - lon1) * k
  U1 <- atan((1 - f) * tan(lat1 * k))
  U2 <- atan((1 - f) * tan(lat2 * k))
  sU1 <- sin(U1)
  cU1 <- cos(U1)
  sU2 <- sin(U2)
  cU2 <- cos(U2)
  lam <- L
  for (i in 1:200) {
    sl <- sin(lam)
    cl <- cos(lam)
    ss <- sqrt((cU2 * sl)^2 + (cU1 * sU2 - sU1 * cU2 * cl)^2)
    cs <- sU1 * sU2 + cU1 * cU2 * cl
    sig <- atan2(ss, cs)
    sa <- ifelse(ss == 0, 0, cU1 * cU2 * sl / ss)
    c2a <- 1 - sa^2
    c2m <- ifelse(c2a == 0, 0, cs - 2 * sU1 * sU2 / c2a)
    C <- f / 16 * c2a * (4 + f * (4 - 3 * c2a))
    lam0 <- lam
    lam <- L +
      (1 - C) * f * sa * (sig + C * ss * (c2m + C * cs * (-1 + 2 * c2m^2)))
    if (all(abs(lam - lam0) < 1e-12, na.rm = TRUE)) break
  }
  u2 <- c2a * (a^2 - b^2) / b^2
  A <- 1 + u2 / 16384 * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
  B <- u2 / 1024 * (256 + u2 * (-128 + u2 * (74 - 47 * u2)))
  ds <- B *
    ss *
    (c2m +
      B /
        4 *
        (cs *
          (-1 + 2 * c2m^2) -
          B / 6 * c2m * (-3 + 4 * ss^2) * (-3 + 4 * c2m^2)))
  b * A * (sig - ds)
}

# The step from the previous fix: NA for the first. As traipse::track_distance(), track_time(), track_speed().
.rb_track_distance <- function(x, y) {
  n <- length(x)
  if (n == 0) {
    return(numeric(0))
  }
  c(NA_real_, .rb_geodesic(x[-n], y[-n], x[-1], y[-1]))
}
.rb_track_time <- function(date) {
  if (!inherits(date, "POSIXct")) {
    date <- as.POSIXct(date)
  }
  if (length(date) == 0) {
    return(numeric(0))
  }
  c(NA_real_, diff(unclass(date)))
}
.rb_track_speed <- function(x, y, date) {
  .rb_track_distance(x, y) / .rb_track_time(date)
}
# Compare track_time vs rb_time (rb_calc_step_time)

#' Time between steps, forcing users to explicitly specify the unit
#'
#' @param time time
#' @param units units of return value
#'
#' @return A numeric vector
#' @export
#'
#'
rb_calc_step_time <- function(time, units = "secs") {
  difftime(time, dplyr::lag(time), units = units) |>
    as.numeric()
}

#' Calculate step acceleration
#'
#' To use this on multiple vessels or trips, use a grouped data frame with tidyverse code like ...
#'
#' @param lon longitude
#' @param lat latitude
#' @param time time in POSIXct
#'
#' @return A numerical vector
#' @export
#'
rb_calc_step_acceleration <- function(lon, lat, time) {
  .rb_track_speed(lon, lat, time) / .rb_track_time(time)
}
#' Track time duration
#'
#' Calculate time duration based on sequential difference of date-time input.
#' The unit of time duration is seconds.
#'
#' Function is a modification of traipse::track_time allowing for calculation
#' of time from previous points rather than by convention the time to next
#' point. It also allows for puting some weighing a.la. the datacall::intvTacsat
#' function.
#'
#' @param date date-time in POSIXct
#' @param weight A numerical vector of length 2 weight to apply to calculation
#' of mean interval rate towards and away from ping
#' @param fill Boolean (default TRUE)
#'
#' @return A vector of duration in seconds
#' @export
#'
rb_calc_track_time <- function(date, weight = c(0, 1), fill = TRUE) {
  # tests
  cls <- class(date)
  if (!inherits(date, "POSIXct")) {
    date <- try(as.POSIXct(date), silent = TRUE)
    if (inherits(date, "try-error")) {
      stop(sprintf(
        "Cannot convert 'date' of class '%s' to POSIXct",
        paste(cls, collapse = ",")
      ))
    }
  }
  # Check if 'weight' is a length 2 numeric vector
  if (length(weight) != 2) {
    stop("weight must be specified as a length 2 numeric vector")
  }

  # Normalize 'weight' to sum to 1
  weight <- weight / sum(weight, na.rm = TRUE)

  if (any(weight == 0)) {
    if (weight[1] == 0) {
      int <- c(NA_real_, diff(unclass(date)))
      if (fill) int[1] <- int[2]
    }
    if (weight[2] == 0) {
      int <- c(diff(unclass(date)), NA_real_)
      if (fill) {
        l <- length(int)
        int[l] <- int[l - 1]
      }
    }
  } else {
    int <- rowSums(cbind(
      c(NA_real_, diff(unclass(date))) * weight[1],
      c(diff(unclass(date)), NA_real_) * weight[2]
    ))
    if (fill) {
      l <- length(int)
      int[1] <- int[2]
      int[l] <- int[l - 1]
    }
  }

  return(int)
}
#' Track speed
#'
#' @description Calculates speed in nautical miles per hour. Based on
#' traipse::track_speed but returns knots not meters per second and sets the
#' first value as the second value. If the length of the vector is one,
#' na values are automatically returned (rather than an error returned
#' by traipse::track_speed)
#'
#'
#' @param lon longitude
#' @param lat latitude
#' @param time datetime (POSIXct)
#'
#' @return numerical vector in nautical miles per hour
#' @export
#'
rb_calc_speed <- function(lon, lat, time) {
  n <- length(lon)
  if (n >= 2) {
    x <- .rb_track_speed(lon, lat, time) |> .rb_ms2kn()
    x[1] <- x[2]
  } else {
    x <- rep(NA_real_, n)
  }
  return(x)
}
#' Event id
#'
#'
#'
#' @param x A vector, normally character or integer
#'
#' @return A numerical vector that labels each discreet event
#' @export
#'
rb_number_runs <- function(x) {
  x <- dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L)
  x <- cumsum(x)
  return(x)
}


#' rb_ms2kn
#'
#' meters per second to knots for those of us that forget the convertion number
#'
#' @param x A numerical vector of speed in meters per second
#'
#' @return A vector, speed in knots (nautical miles per hour)
#' @noRd
#'
#' @author Einar Hjörleifsson, \email{einar.hjorleifsson@gmail.com}
#'
.rb_ms2kn <- function(x) {
  x * 1.94384449
}

#' rb_kn2ms
#'
#' knots to meters per second for those of us that forget the convertion number
#'
#' @param x A numerical vector of speed in knots
#'
#' @return A vector, speed in meters per second
#' @noRd
#'
#' @author Einar Hjörleifsson, \email{einar.hjorleifsson@gmail.com}
#'
.rb_kn2ms <- function(x) {
  x / 1.94384449
}


#' rb_summarise_track
#'
#' @param d ...
#'
#' @return A summary tibble, dependent on grouping upstream
#' @export
#'
rb_summarise_track <- function(d) {
  d |>
    dplyr::summarise(
      pings = dplyr::n(),
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
      n.harb = dplyr::n_distinct(hid, na.rm = TRUE)
    )
}

#' Convert speed between knots and metres per second
#'
#' @param x A numeric vector of speed.
#' @param from,to Units: `"kn"` (knots) or `"ms"` (metres per second).
#'
#' @return A numeric vector, speed in the `to` unit.
#' @export
#'
#' @examples
#' rb_convert_speed(0:8, from = "ms", to = "kn")
rb_convert_speed <- function(x, from = c("kn", "ms"), to = c("ms", "kn")) {
  from <- match.arg(from)
  to <- match.arg(to)
  if (from == to) {
    return(x)
  }
  if (from == "ms") .rb_ms2kn(x) else .rb_kn2ms(x)
}
