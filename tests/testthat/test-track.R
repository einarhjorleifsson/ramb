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
      abs(.rb_track_distance(x, y) - traipse::track_distance(x, y)),
      na.rm = TRUE
    ),
    1e-3
  )
  expect_equal(.rb_track_time(t), traipse::track_time(t))
  expect_equal(
    .rb_track_speed(x, y, t),
    traipse::track_speed(x, y, t),
    tolerance = 1e-9
  )
  lx <- c(-179.9, 179.9, 0, 0, 10, NA, 10)
  ly <- c(0, 0, 89.99, -89.99, 10, 10, 10)
  expect_equal(
    .rb_track_distance(lx, ly),
    traipse::track_distance(lx, ly),
    tolerance = 1e-9
  )
  expect_length(.rb_track_distance(numeric(0), numeric(0)), 0)
})
