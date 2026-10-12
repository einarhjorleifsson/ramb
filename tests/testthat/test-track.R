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

test_that("geodesic falls back to haversine, with a warning, for antipodal points", {
  expect_warning(
    d <- rb_calc_distance(0, 0, 180, 0, method = "geodesic"),
    "did not converge"
  )
  expect_equal(d, rb_calc_distance(0, 0, 180, 0))
  # NA pairs do not count as non-converged
  expect_no_warning(rb_calc_distance(0, NA, 1, 1, method = "geodesic"))
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
  skip_if_not_installed("duckdbfs")
  d <- duckdbfs::as_dataset(data.frame(lon = 1, lat = 2, x2 = 3, y2 = 4))
  expect_error(
    rb_calc_distance(d, lon2 = x2, lat2 = y2),
    "lazy DuckDB"
  )
})

test_that("rb_calc_speed() threads the distance method, haversine by default", {
  lon <- c(-21.93, -22.10, -22.30)
  lat <- c(64.15, 64.20, 64.30)
  time <- as.POSIXct("2024-01-01", tz = "UTC") + c(0, 600, 1200)
  s_h <- rb_calc_speed(lon, lat, time)
  s_g <- rb_calc_speed(lon, lat, time, method = "geodesic")
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
  a_h <- rb_calc_step_acceleration(lon, lat, time)
  a_g <- rb_calc_step_acceleration(lon, lat, time, method = "geodesic")
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
