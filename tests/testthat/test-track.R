test_that("rb_calc_distance() gives haversine metres on the filters' sphere", {
  expect_equal(rb_calc_distance(0, 64, 0, 65), 2 * pi * 6371000 / 360)
  expect_equal(rb_calc_distance(-21.9, 64.1, -21.9, 64.1), 0)
  expect_equal(rb_calc_distance(10, 0, 11, 0), 2 * pi * 6371000 / 360) # one degree of longitude at the equator
  expect_equal(
    rb_calc_distance(-22, 64, -24, 65),
    rb_calc_distance(-24, 65, -22, 64)
  )
})

test_that("rb_calc_distance() is vectorised, recycles and passes NA through", {
  d <- rb_calc_distance(c(0, 0, NA), 64, 0, c(65, 64, 64))
  expect_length(d, 3)
  expect_equal(d[1:2], c(2 * pi * 6371000 / 360, 0))
  expect_true(is.na(d[3]))
  expect_equal(rb_calc_distance(0, 0, 180, 0), pi * 6371000) # antipodes: asin() argument capped at 1
})

test_that("rb_calc_distance() keeps the named vector form", {
  expect_equal(
    rb_calc_distance(lon1 = 0, lat1 = 64, lon2 = 0, lat2 = 65),
    rb_calc_distance(0, 64, 0, 65)
  )
  expect_error(
    rb_calc_distance(0, 64, 0, 65, lon1 = 1),
    "data-frame form"
  )
  expect_error(rb_calc_distance(0, 64, 0), "vector form")
})

test_that("rb_calc_distance() named vector form needs all four coordinates", {
  # lon1 = lon is the data-frame default; in the vector form it used to pick up a
  # `lon` on the search path and return a wrong distance without complaint
  expect_error(
    rb_calc_distance(lat1 = 64, lon2 = -21, lat2 = 64),
    "needs all of"
  )
  expect_error(
    rb_calc_distance(lon1 = -22, lat1 = 64, lon2 = -21),
    "needs all of"
  )
})

test_that("rb_calc_distance(method = \"geodesic\") matches traipse's geodesic", {
  skip_if_not_installed("traipse")
  set.seed(7)
  n <- 100
  x <- cumsum(c(-25, stats::rnorm(n - 1, 0, 0.05)))
  y <- pmin(89, pmax(-89, cumsum(c(64, stats::rnorm(n - 1, 0, 0.05)))))
  expect_equal(
    rb_calc_distance(x[-n], y[-n], x[-1], y[-1], method = "geodesic"),
    traipse::track_distance(x, y)[-1],
    tolerance = 1e-9
  )
  # short steps: within half a percent of the haversine value
  d_h <- rb_calc_distance(-21.93, 64.15, -23.93, 65.65)
  d_g <- rb_calc_distance(-21.93, 64.15, -23.93, 65.65, method = "geodesic")
  expect_lt(abs(d_g - d_h) / d_h, 0.005)
  # recycles, as the haversine form does
  expect_length(rb_calc_distance(0, 0, 1:3, 1:3, method = "geodesic"), 3)
})

test_that("geodesic (Karney) is exact at and near the antipode, with no warning", {
  skip_if_not_installed("traipse")
  # the exact antipode
  expect_no_warning(
    d <- rb_calc_distance(0, 0, 180, 0, method = "geodesic")
  )
  expect_equal(
    d,
    traipse::track_distance(c(0, 180), c(0, 0))[2],
    tolerance = 1e-9
  )
  # the near-antipodal pair where the earlier Vincenty recode failed (plan 013:
  # 7,059 m short; with the 2026-10-12 fallback it was the haversine value)
  expect_no_warning(
    d2 <- rb_calc_distance(-22, 64, 157.9, -63.95, method = "geodesic")
  )
  expect_equal(
    d2,
    traipse::track_distance(c(-22, 157.9), c(64, -63.95))[2],
    tolerance = 1e-9
  )
  # NA passes through
  expect_true(is.na(rb_calc_distance(0, NA, 1, 1, method = "geodesic")))
})

test_that("rb_calc_distance() data-frame form: tidy-eval columns, same values as vectors", {
  d <- data.frame(
    lon = c(-21.93, -22.50, NA),
    lat = c(64.15, 63.90, 64.00),
    port_lon = -21.94,
    port_lat = 64.15
  )
  expect_equal(
    rb_calc_distance(d, lon2 = port_lon, lat2 = port_lat),
    rb_calc_distance(d$lon, d$lat, d$port_lon, d$port_lat)
  )
  expect_equal(
    rb_calc_distance(
      d,
      lon1 = port_lon,
      lat1 = port_lat,
      lon2 = lon,
      lat2 = lat,
      method = "geodesic"
    ),
    rb_calc_distance(
      d$port_lon,
      d$port_lat,
      d$lon,
      d$lat,
      method = "geodesic"
    )
  )
  expect_true(is.na(rb_calc_distance(d, lon2 = port_lon, lat2 = port_lat)[3]))
})

test_that("rb_calc_distance() data-frame form: clear errors", {
  d <- data.frame(lon = 1, lat = 2, chr = "a")
  expect_error(rb_calc_distance(d), "lon2")
  expect_error(rb_calc_distance(d, lon2 = nope, lat2 = lat), "not found")
  expect_error(rb_calc_distance(d, lon2 = chr, lat2 = lat), "numeric")
  expect_error(rb_calc_distance(d, lat, lon, lat), "named arguments")
})

test_that("rb_calc_distance() refuses lazy tables, for now", {
  d <- as_lazy(data.frame(lon = 1, lat = 2, x2 = 3, y2 = 4))
  expect_error(
    rb_calc_distance(d, lon2 = x2, lat2 = y2),
    "lazy DuckDB"
  )
})

test_that("rb_calc_speed() threads the distance method, geodesic by default", {
  lon <- c(-21.93, -22.10, -22.30)
  lat <- c(64.15, 64.20, 64.30)
  time <- as.POSIXct("2024-01-01", tz = "UTC") + c(0, 600, 1200)
  s_h <- rb_calc_speed(lon, lat, time, method = "haversine")
  s_g <- rb_calc_speed(lon, lat, time, method = "geodesic")
  expect_equal(rb_calc_speed(lon, lat, time), s_g)
  # first value is the second value, in both methods
  expect_equal(s_h[1], s_h[2])
  # haversine speed is the haversine distance over the step time, in knots
  d <- rb_calc_distance(lon[-3], lat[-3], lon[-1], lat[-1])
  expect_equal(s_h[-1], d / 600 * 1.94384449)
  # the methods differ by well under half a percent on such steps
  expect_lt(max(abs(s_h - s_g) / s_g, na.rm = TRUE), 0.005)
})

test_that("rb_calc_step_acceleration() threads the distance method", {
  lon <- c(-21.93, -22.10, -22.30)
  lat <- c(64.15, 64.20, 64.30)
  time <- as.POSIXct("2024-01-01", tz = "UTC") + c(0, 600, 1200)
  a_h <- rb_calc_step_acceleration(lon, lat, time, method = "haversine")
  a_g <- rb_calc_step_acceleration(lon, lat, time, method = "geodesic")
  expect_equal(rb_calc_step_acceleration(lon, lat, time), a_g)
  expect_true(is.na(a_h[1]) && is.na(a_g[1]))
  expect_lt(max(abs(a_h - a_g) / abs(a_g), na.rm = TRUE), 0.005)
})
# The traipse recode (R/geodesic.R): same steps as traipse/geodist's geodesic, to well under a millimetre.

test_that("track distance, time and speed match traipse", {
  skip_if_not_installed("traipse")
  set.seed(1)
  n <- 5000
  x <- cumsum(c(-25, stats::rnorm(n - 1, 0, 0.01)))
  y <- pmin(80, pmax(-80, cumsum(c(64, stats::rnorm(n - 1, 0, 0.005)))))
  t <- as.POSIXct("2024-01-01", tz = "UTC") +
    cumsum(c(0, stats::rexp(n - 1, 1 / 60)))
  expect_lt(
    max(
      abs(
        .rb_track_distance(x, y, "geodesic") - traipse::track_distance(x, y)
      ),
      na.rm = TRUE
    ),
    1e-3
  )
  expect_equal(.rb_track_time(t), traipse::track_time(t))
  expect_equal(
    .rb_track_speed(x, y, t, "geodesic"),
    traipse::track_speed(x, y, t),
    tolerance = 1e-9
  )
  lx <- c(-179.9, 179.9, 0, 0, 10, NA, 10)
  ly <- c(0, 0, 89.99, -89.99, 10, 10, 10)
  expect_equal(
    .rb_track_distance(lx, ly, "geodesic"),
    traipse::track_distance(lx, ly),
    tolerance = 1e-9
  )
  expect_length(.rb_track_distance(numeric(0), numeric(0)), 0)
})

test_that("rb_summarise_track() returns one snake_case row per group", {
  d <- tibble::tibble(
    time = as.POSIXct("2022-01-01 00:00:00") + c(0, 600, 1200),
    speed = c(0, 5, 10),
    dist = c(NA, 100, 200),
    dura = c(NA, 600, 600),
    voyage_id = c(1, 1, 2),
    port_id = c(1, 1, 2)
  )
  out <- rb_summarise_track(d)
  expect_named(
    out,
    c(
      "pings",
      "t_min",
      "t_max",
      "s_min",
      "s_med",
      "s_max",
      "di_min",
      "di_med",
      "di_max",
      "du_min",
      "du_med",
      "du_max",
      "n_voyages",
      "n_ports"
    )
  )
  expect_equal(out$pings, 3)
  expect_equal(out$n_voyages, 2)
  expect_equal(out$n_ports, 2)
  expect_equal(out$s_med, 5)
})

test_that("rb_calc_speed_smooth: the median within the window, in any order; NA for no speed", {
  t <- as.POSIXct("2024-01-01", tz = "UTC") + 60 * c(0:9, 30)
  s <- c(3, 3, 0.5, 3, 3, 3, 9, 3, 3, NA, 7)
  w <- rb_calc_speed_smooth(s, t, window_s = 120)
  brute <- vapply(
    seq_along(t),
    function(i) {
      if (is.na(s[i])) {
        return(NA_real_)
      }
      stats::median(s[abs(as.numeric(t - t[i])) <= 120], na.rm = TRUE)
    },
    numeric(1)
  )
  expect_equal(w, brute)
  expect_equal(w[c(3, 7, 11)], c(3, 3, 7)) # the dip and the spike are smoothed; a lone ping keeps its own
  o <- c(11, 3, 7, 1, 10, 2, 5, 4, 9, 8, 6)
  expect_equal(rb_calc_speed_smooth(s[o], t[o], window_s = 120), w[o])
  expect_equal(rb_calc_speed_smooth(s, t, window_s = 0), s)
})
