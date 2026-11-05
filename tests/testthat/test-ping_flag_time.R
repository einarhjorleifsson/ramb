t0 <- as.POSIXct("2024-03-01", tz = "UTC")
# a vessel steaming east along 64 N at `kn` knots, a ping every `step` seconds, its times as written
steam <- function(vid = 1L, kn = 10, step = 60, n = 60, speed = kn) {
  m <- 6371000 * pi / 180 * cos(64 * pi / 180)
  s <- step * (seq_len(n) - 1)
  data.frame(vid = vid, time = t0 + 7 + s, lon = -20 + kn * 1852 / 3600 * s / m, lat = 64, speed = speed)
}
limits_of <- function(d, ...) rb_create_ping_limits(d, speed = speed, min_recorded = 10, min_moving = 10, ...)

test_that("a ping written late is flagged for its time, its error estimated, its neighbours and positions kept", {
  d <- steam()
  lim <- limits_of(d)
  d$time[20] <- d$time[20] + 30
  f <- rb_flag_ping_time(d, lim)
  expect_equal(which(!is.na(f$ping_flag)), 20)
  expect_equal(f$ping_flag[20], "time")
  expect_equal(f$ping_flag_basis[20], "recorded")
  expect_equal(f$ping_time_error_s[20], 30, tolerance = 1e-3)
  expect_equal(f$ping_time_error_s[c(19, 21)], c(0, 0), tolerance = 1e-3)
  expect_equal(f$lon, d$lon)
  # today's rule blames a position beside the late time; after the time flag it blames none
  g <- rb_flag_ping_impossible(f, method = "local", speed = speed, limits = lim)
  expect_equal(which(!is.na(g$ping_flag)), 20)
  # an early time reads below 0
  d$time[20] <- t0 + 7 + 19 * 60 - 25
  expect_equal(rb_flag_ping_time(d, lim)$ping_time_error_s[20], -25, tolerance = 1e-3)
})

test_that("the worst first: a good ping between two pings written late is not flagged", {
  d <- steam()
  lim <- limits_of(d)
  d$time[c(20, 22)] <- d$time[c(20, 22)] + c(25, 20)
  f <- rb_flag_ping_time(d, lim)
  expect_equal(which(f$ping_flag %in% "time"), c(20, 22))
  expect_equal(f$ping_time_error_s[21], 0, tolerance = 1e-3)
})

test_that("a clock's grain is allowed: times written to the minute are not flagged", {
  d <- steam(step = 61, n = 120)
  d$time <- as.POSIXct(round(as.numeric(d$time) / 60) * 60, origin = "1970-01-01", tz = "UTC")
  lim <- limits_of(d)
  expect_equal(lim$grain_s, 60)
  f <- rb_flag_ping_time(d, lim)
  expect_true(all(is.na(f$ping_flag)))
})

test_that("a ping is judged only where its speed is recorded, steady and over min_kn", {
  d <- steam()
  lim <- limits_of(d)
  d$time[20] <- d$time[20] + 30
  slow <- d
  slow$speed[19:21] <- 1.5
  expect_true(is.na(rb_flag_ping_time(slow, lim)$ping_time_error_s[20]))
  changing <- d
  changing$speed[21] <- 14
  expect_true(is.na(rb_flag_ping_time(changing, lim)$ping_time_error_s[20]))
  none <- d
  none$speed[20] <- NA
  expect_true(all(is.na(rb_flag_ping_time(none, lim)$ping_flag)))
  # a ping already flagged is not judged, nor a neighbour: the next pings are its neighbours
  d$ping_flag <- NA_character_
  d$ping_flag[20] <- "invalid"
  f <- rb_flag_ping_time(d, lim)
  expect_equal(f$ping_flag[20], "invalid")
  expect_true(all(is.na(f$ping_flag[-20])))
})

test_that("pings are judged within their stream: two clocks 17 s apart flag nothing", {
  a <- steam()
  a$stream <- "a"
  b <- steam()
  b$time <- b$time + 17
  b$lon <- b$lon + 10 * 1852 / 3600 * 17 / (6371000 * pi / 180 * cos(64 * pi / 180))
  b$stream <- "b"
  d <- rbind(a, b)
  lim <- limits_of(d, stream = stream)
  expect_true(all(is.na(rb_flag_ping_time(d, lim, stream = stream)$ping_flag)))
  # read as one stream, the pings of both clocks are judged against each other
  lim1 <- limits_of(d)
  expect_false(all(is.na(rb_flag_ping_time(d, lim1)$ping_time_error_s)))
})

test_that("a lazy table gives the same labels", {
  d <- rbind(steam(), steam(2L, kn = 8))
  lim <- limits_of(d)
  d$time[c(20, 80)] <- d$time[c(20, 80)] + 30
  f <- rb_flag_ping_time(d, lim)
  lz <- dplyr::collect(rb_flag_ping_time(as_lazy(d), lim)) |> dplyr::arrange(vid, time)
  expect_equal(lz$ping_flag, f$ping_flag[order(f$vid, f$time)])
  expect_equal(sum(lz$ping_flag %in% "time"), 2)
})

test_that("the limits must fit the pings", {
  d <- steam()
  d$stream <- "a"
  lim <- limits_of(d, stream = stream)
  expect_error(rb_flag_ping_time(d[names(d) != "stream"], lim), "stream")
  expect_error(rb_flag_ping_time(d, lim[0, -1]), "vid")
  expect_error(rb_flag_ping_time(d, rbind(lim, lim)), "twice")
  expect_error(rb_flag_ping_time(d, lim, steady = 0), "steady")
})
