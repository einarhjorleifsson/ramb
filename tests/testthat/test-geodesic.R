# The traipse recode (R/geodesic.R): same steps as traipse/geodist's geodesic, to well under a millimetre.

test_that("track distance, time and speed match traipse", {
  skip_if_not_installed("traipse")
  set.seed(1)
  n <- 5000
  x <- cumsum(c(-25, stats::rnorm(n - 1, 0, 0.01)))
  y <- pmin(80, pmax(-80, cumsum(c(64, stats::rnorm(n - 1, 0, 0.005)))))
  t <- as.POSIXct("2024-01-01", tz = "UTC") + cumsum(c(0, stats::rexp(n - 1, 1 / 60)))
  expect_lt(max(abs(.rb_track_distance(x, y) - traipse::track_distance(x, y)), na.rm = TRUE), 1e-3)
  expect_equal(.rb_track_time(t), traipse::track_time(t))
  expect_equal(.rb_track_speed(x, y, t), traipse::track_speed(x, y, t), tolerance = 1e-9)
  lx <- c(-179.9, 179.9, 0, 0, 10, NA, 10); ly <- c(0, 0, 89.99, -89.99, 10, 10, 10)
  expect_equal(.rb_track_distance(lx, ly), traipse::track_distance(lx, ly), tolerance = 1e-9)
  expect_length(.rb_track_distance(numeric(0), numeric(0)), 0)
})
