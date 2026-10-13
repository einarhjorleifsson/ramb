#' Great-circle distance between two points
#'
#' Two methods:
#' * `"haversine"` (default): on a sphere of radius 6,371,000 m — the radius the
#'   impossible-position filters ([rb_flag_ping_impossible()]) use, so a distance computed
#'   here agrees with theirs.
#' * `"geodesic"`: Karney's algorithm on the WGS84 ellipsoid (vendored GeographicLib C,
#'   see `inst/COPYRIGHTS`). Exact everywhere, including nearly antipodal points.
#'
#' Which method: use the default for anything measured against the impossible-position
#' filters (a speed computed on the ellipsoid can disagree with a filter threshold purely
#' from the sphere mismatch) and for large data — Haversine is about 40-50x faster and is
#' the only method expressible in SQL. Use `"geodesic"` when the absolute distance itself
#' matters, e.g. distance-steamed totals: the Haversine error is systematic, up to ~0.6 %
#' depending on latitude and bearing, so it does not cancel in sums.
#'
#' Two forms. The vector form takes the four coordinate vectors; they recycle to a common
#' length, so it works inside `mutate()` with `dplyr::lag()` and `dplyr::lead()`. The
#' data-frame form takes a data frame as the first argument and the coordinate columns as
#' tidy-eval arguments, for point-to-point columns such as ping to port. A lazy DuckDB
#' table is not supported — collect first.
#'
#' @param x The vector form: longitude of the first point, decimal degrees. The data-frame
#'   form: a data frame with the coordinate columns.
#' @param ... The vector form: `lat1`, `lon2` and `lat2`, in that order.
#' @param method `"haversine"` (default) or `"geodesic"`.
#' @param lon1,lat1,lon2,lat2 The data-frame form: the coordinate columns (tidy-eval).
#'   `lon1` and `lat1` default to the grammar names `lon` and `lat`; `lon2` and `lat2`
#'   are required. The named vector form needs all four.
#'
#' @return Numeric vector of distances in metres; `NA` where any coordinate is `NA`.
#'
#' @references For `"geodesic"`: Karney, C.F.F. (2013). "Algorithms for
#'   geodesics". Journal of Geodesy, 87(1), 43–55.
#'   \doi{10.1007/s00190-012-0578-z}; the engine is GeographicLib's
#'   `geodesic.c` (\url{https://geographiclib.sourceforge.io/}), vendored
#'   unchanged.
#'
#' @examples
#' rb_calc_distance(0, 64, 0, 65)                 # one degree of latitude, 111,195 m
#' rb_calc_distance(-21.93, 64.15, -23.93, 65.65) # Reykjavik to Talknafjordur, about 190 km
#'
#' # the data-frame form: distance from each ping to a fixed port
#' d <- data.frame(
#'   lon = c(-21.93, -22.50),
#'   lat = c(64.15, 63.90),
#'   port_lon = -21.94,
#'   port_lat = 64.15
#' )
#' rb_calc_distance(d, lon2 = port_lon, lat2 = port_lat)
#' rb_calc_distance(d, lon2 = port_lon, lat2 = port_lat, method = "geodesic")
#' \dontrun{
#' pings |>
#'   group_by(vid) |>
#'   mutate(m = rb_calc_distance(dplyr::lag(lon), dplyr::lag(lat), lon, lat)) |>
#'   ungroup()
#' }
#'
#' @export
rb_calc_distance <- function(
  x,
  ...,
  method = c("haversine", "geodesic"),
  lon1 = lon,
  lat1 = lat,
  lon2,
  lat2
) {
  method <- match.arg(method)

  # Named vector form: rb_calc_distance(lon1 = ..., lat1 = ..., lon2 = ..., lat2 = ...).
  # All four are required: the defaults lon1 = lon, lat1 = lat name columns of the
  # data-frame form, and evaluated here they would silently pick up any `lon`/`lat`
  # on the search path (a global `lon <- 5` gave 1,258,560 m instead of an error).
  if (missing(x)) {
    if (missing(lon1) || missing(lat1) || missing(lon2) || missing(lat2)) {
      stop(
        "The named vector form needs all of `lon1`, `lat1`, `lon2` and `lat2`.",
        call. = FALSE
      )
    }
    return(.rb_dist(lon1, lat1, lon2, lat2, method))
  }

  if (is.data.frame(x) || inherits(x, "tbl_lazy")) {
    if (inherits(x, "tbl_lazy")) {
      stop(
        "`rb_calc_distance()` does not support lazy DuckDB tables; collect first.",
        call. = FALSE
      )
    }
    if (...length()) {
      stop(
        "In the data-frame form the columns are named arguments, e.g. ",
        "`rb_calc_distance(d, lon2 = x2, lat2 = y2)`.",
        call. = FALSE
      )
    }
    if (
      rlang::quo_is_missing(rlang::enquo(lon2)) ||
        rlang::quo_is_missing(rlang::enquo(lat2))
    ) {
      stop("The data-frame form needs `lon2` and `lat2`.", call. = FALSE)
    }
    map <- .rb_cols(
      lon1 = {{ lon1 }},
      lat1 = {{ lat1 }},
      lon2 = {{ lon2 }},
      lat2 = {{ lat2 }}
    )
    cols <- lapply(stats::setNames(nm = names(map)), function(nm) {
      cl <- map[[nm]]
      if (!cl %in% names(x)) {
        stop(
          sprintf("Column `%s` not found in the data frame.", cl),
          call. = FALSE
        )
      }
      v <- x[[cl]]
      if (!is.numeric(v)) {
        stop(
          sprintf("Column `%s` must be numeric, not <%s>.", cl, class(v)[1]),
          call. = FALSE
        )
      }
      v
    })
    return(.rb_dist(cols$lon1, cols$lat1, cols$lon2, cols$lat2, method))
  }

  # Positional vector form: rb_calc_distance(lon1, lat1, lon2, lat2)
  if (!missing(lon1) || !missing(lat1) || !missing(lon2) || !missing(lat2)) {
    stop(
      "Column arguments (`lon1`, ...) only apply to the data-frame form.",
      call. = FALSE
    )
  }
  dots <- list(...)
  if (length(dots) != 3L) {
    stop(
      "The vector form is `rb_calc_distance(lon1, lat1, lon2, lat2)`.",
      call. = FALSE
    )
  }
  .rb_dist(x, dots[[1]], dots[[2]], dots[[3]], method)
}

# The computation behind both forms of rb_calc_distance().
.rb_dist <- function(lon1, lat1, lon2, lat2, method) {
  if (method == "haversine") {
    return(.rb_haversine(lon1, lat1, lon2, lat2))
  }
  # .rb_geodesic() does not recycle; bring the vectors to a common length first
  l <- lengths(list(lon1, lat1, lon2, lat2))
  n <- max(l)
  if (any(!l %in% c(1L, n))) {
    stop(
      "Coordinate vectors must have length 1 or a common length.",
      call. = FALSE
    )
  }
  .rb_geodesic(
    rep_len(lon1, n),
    rep_len(lat1, n),
    rep_len(lon2, n),
    rep_len(lat2, n)
  )
}

# Haversine on the filters' sphere (R = 6,371,000 m). Vectorised, recycles, NA passes through.
.rb_haversine <- function(lon1, lat1, lon2, lat2) {
  k <- pi / 180
  a <- sin((lat2 - lat1) * k / 2)^2 +
    cos(lat1 * k) * cos(lat2 * k) * sin((lon2 - lon1) * k / 2)^2
  2 * 6371000 * asin(sqrt(pmin(1, a)))
}
# geodesic.R — distance on the WGS84 ellipsoid.
#
# Karney's algorithm, via the vendored GeographicLib C code (src/geodesic.c, src/geodesic.h,
# taken in unchanged — plan 013's recorded exception to the recode rule; see inst/COPYRIGHTS)
# behind the Rcpp entry point geod_karney() (src/geodesic_karney.cpp). Exact everywhere,
# including nearly antipodal pairs, with no convergence failure and no fallback branch —
# which is why it replaced the earlier Vincenty recode (2026-10-12; Vincenty failed silently
# near the antipode, measured 7 km short on the pair in test-track.R). Agrees with
# traipse::track_distance() to nanometres; about 0.7 s per million legs.

.rb_geodesic <- function(lon1, lat1, lon2, lat2) {
  geod_karney(lon1, lat1, lon2, lat2)
}

# The step from the previous fix: NA for the first. As traipse::track_distance(), track_time(), track_speed().
# method is the rb_calc_distance() one; the default is "haversine", the filters' sphere, everywhere in the family.
.rb_track_distance <- function(x, y, method = "haversine") {
  n <- length(x)
  if (n == 0) {
    return(numeric(0))
  }
  fun <- if (method == "geodesic") .rb_geodesic else .rb_haversine
  c(NA_real_, fun(x[-n], y[-n], x[-1], y[-1]))
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
.rb_track_speed <- function(x, y, date, method = "haversine") {
  .rb_track_distance(x, y, method) / .rb_track_time(date)
}
# Compare track_time vs rb_time (rb_calc_step_time)

#' Time between consecutive positions
#'
#' The elapsed time from the previous position to the current one, in the
#' chosen unit. The first value is `NA`.
#'
#' @param time Position times (`POSIXct`).
#' @param unit The unit of the result, passed to [base::difftime()]:
#'   `"secs"` (default), `"mins"`, `"hours"`, `"days"` or `"weeks"`.
#'
#' @return A numeric vector the length of `time`.
#' @export
#'
#' @seealso [rb_calc_track_time()], which can instead spread each interval
#'   over the two adjacent positions.
rb_calc_step_time <- function(time, unit = "secs") {
  difftime(time, dplyr::lag(time), units = unit) |>
    as.numeric()
}

#' Step acceleration
#'
#' For each step, the distance travelled divided by the squared elapsed time,
#' in m/s^2: the acceleration implied if the vessel went from rest to the
#' step's mean speed. `NA` for the first position. For several vessels or
#' trips, compute within groups, e.g. inside `dplyr::mutate()` on a grouped
#' data frame.
#'
#' @param lon,lat Longitude and latitude, decimal degrees.
#' @param time Position times (`POSIXct`).
#' @param method The distance method. The default `"haversine"` is the sphere of
#'   [rb_flag_ping_impossible()] and much the faster of the two; `"geodesic"` (WGS84,
#'   Karney) is the more accurate — see [rb_calc_distance()] for guidance.
#'
#' @return A numeric vector of accelerations in m/s^2.
#' @export
#'
rb_calc_step_acceleration <- function(
  lon,
  lat,
  time,
  method = c("haversine", "geodesic")
) {
  method <- match.arg(method)
  .rb_track_speed(lon, lat, time, method) / .rb_track_time(time)
}
# The interval convention is flipped relative to traipse::track_time() (time
# *from* the previous position, not to the next), and the weights spread an
# interval over the two adjacent positions, as vmstools's intvTacsat does.

#' Time the track spent at each position
#'
#' For each position, the time between the previous and the next position,
#' split by `weight`: `c(0, 1)` (default) counts the step from the previous
#' position, `c(1, 0)` the step to the next, `c(0.5, 0.5)` half of each. With
#' `fill_ends = TRUE` the two end positions take their neighbour's value
#' instead of `NA`.
#'
#' @param time Position times; coerced to `POSIXct`.
#' @param weight A length-2 numeric vector: the share of the bracketing
#'   interval counted towards the previous and the next step. Normalised to
#'   sum to 1.
#' @param fill_ends Logical; if `TRUE` (default) the first and last value take
#'   their neighbour's value.
#'
#' @return A numeric vector of durations in seconds.
#' @export
rb_calc_track_time <- function(time, weight = c(0, 1), fill_ends = TRUE) {
  # tests
  cls <- class(time)
  if (!inherits(time, "POSIXct")) {
    time <- try(as.POSIXct(time), silent = TRUE)
    if (inherits(time, "try-error")) {
      stop(sprintf(
        "Cannot convert 'time' of class '%s' to POSIXct",
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
      int <- c(NA_real_, diff(unclass(time)))
      if (fill_ends) int[1] <- int[2]
    }
    if (weight[2] == 0) {
      int <- c(diff(unclass(time)), NA_real_)
      if (fill_ends) {
        l <- length(int)
        int[l] <- int[l - 1]
      }
    }
  } else {
    int <- rowSums(cbind(
      c(NA_real_, diff(unclass(time))) * weight[1],
      c(diff(unclass(time)), NA_real_) * weight[2]
    ))
    if (fill_ends) {
      l <- length(int)
      int[1] <- int[2]
      int[l] <- int[l - 1]
    }
  }

  return(int)
}
# As traipse::track_speed(), but in knots, and a single-position track gets
# NA rather than an error.

#' Speed between consecutive positions
#'
#' The speed of each step in knots: the distance from the previous position
#' divided by the elapsed time. The first position takes the second's value;
#' a single-position track gets `NA`.
#'
#' @param lon,lat Longitude and latitude, decimal degrees.
#' @param time Position times (`POSIXct`).
#' @param method The distance method. The default `"haversine"` is the sphere of
#'   [rb_flag_ping_impossible()] and much the faster of the two; `"geodesic"` (WGS84,
#'   Karney) is the more accurate — see [rb_calc_distance()] for guidance.
#'
#' @return A numeric vector of speeds in knots (nautical miles per hour).
#' @export
rb_calc_speed <- function(
  lon,
  lat,
  time,
  method = c("haversine", "geodesic")
) {
  method <- match.arg(method)
  n <- length(lon)
  if (n >= 2) {
    x <- .rb_track_speed(lon, lat, time, method) |> .rb_ms2kn()
    x[1] <- x[2]
  } else {
    x <- rep(NA_real_, n)
  }
  return(x)
}
#' Number runs of consecutive equal values
#'
#' Each run of consecutive equal values gets a sequential number, starting
#' at 1.
#'
#' @param x A vector, normally character or integer.
#'
#' @return An integer vector numbering the run each element belongs to.
#' @export
#'
rb_number_runs <- function(x) {
  x <- dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L)
  x <- cumsum(x)
  return(x)
}


# metres per second to knots
.rb_ms2kn <- function(x) {
  x * 1.94384449
}

# knots to metres per second
.rb_kn2ms <- function(x) {
  x / 1.94384449
}


#' Summarise a vessel track
#'
#' A one-row summary per group of a track table: the number of pings, the
#' time range, the ranges of speed, per-step distance and per-step duration,
#' and the number of distinct trips and harbours.
#'
#' @param d A (grouped) data frame with `time`, `speed`, `dist`, `dura` and,
#'   for the counts, `tid` and `hid`.
#'
#' @return A tibble with one row per group: `pings`, `t_min`, `t_max`,
#'   `s_min`, `s_med`, `s_max`, `di_min`, `di_med`, `di_max`, `du_min`,
#'   `du_med`, `du_max`, `n_trips` and `n_harbours`.
#' @export
#'
rb_summarise_track <- function(d) {
  d |>
    dplyr::summarise(
      pings = dplyr::n(),
      t_min = min(time),
      t_max = max(time),
      s_min = min(speed),
      s_med = median(speed),
      s_max = max(speed),
      di_min = min(dist, na.rm = TRUE),
      di_med = median(dist, na.rm = TRUE),
      di_max = max(dist, na.rm = TRUE),
      du_min = min(dura, na.rm = TRUE),
      du_med = median(dura, na.rm = TRUE),
      du_max = max(dura, na.rm = TRUE),
      n_trips = dplyr::n_distinct(tid, na.rm = TRUE),
      n_harbours = dplyr::n_distinct(hid, na.rm = TRUE)
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
