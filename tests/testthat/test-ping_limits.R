t0 <- as.POSIXct("2024-03-01", tz = "UTC")
# a vessel steaming east along 64 N at `kn` knots, a ping every `step` seconds for `n` pings
steam <- function(vid = 1L, kn = 12, step = 10, n = 720, speed = kn, lat = 64) {
  m <- 6371000 * pi / 180 * cos(lat * pi / 180)
  s <- step * (seq_len(n) - 1)
  data.frame(vid = vid, time = t0 + s, lon = -20 + kn * 1852 / 3600 * s / m, lat = lat, speed = speed)
}

test_that("the ceiling is the highest speed recorded twice in a row; one wrong report cannot set it", {
  d <- steam()
  d$speed[300] <- 60
  d$speed[1] <- 50 # a stream's first report too
  lim <- rb_create_ping_limits(d, speed = speed)
  expect_equal(nrow(lim), 1)
  expect_equal(lim$ceiling_kn, 12)
  expect_equal(lim$ceiling_basis, "recorded")
  expect_equal(lim$recorded_kn, 12)
  expect_equal(lim$recorded_n, 720)
  # the track agrees with it
  expect_equal(lim$track_kn, 12, tolerance = 1e-3)
  expect_true(lim$ceilings_agree)
  # a speed of 102.3 or more, or below 0, is AIS's "not available": no report
  d$speed[1:10] <- 102.3
  expect_equal(rb_create_ping_limits(d, speed = speed)$recorded_n, 710)
})

test_that("without a recorded speed the ceiling is the track's, and a copy interleaved in the stream cannot set it", {
  # two vessels to keep the dataset's noise, a third whose every other ping is a copy 5 km north
  d <- rbind(steam(1L, step = 20, n = 400), steam(2L, kn = 8, step = 20, n = 400))
  three <- steam(3L, kn = 10, step = 10, n = 800)
  three$lat[seq(2, 800, by = 2)] <- 64.05
  d <- rbind(d, three)
  lim <- rb_create_ping_limits(d)
  expect_equal(lim$ceiling_basis, rep("track", 3))
  expect_equal(lim$ceiling_kn, c(12, 8, 10), tolerance = 1e-3)
  expect_true(all(is.na(lim$recorded_kn)))
  expect_true(all(is.na(lim$ceilings_agree)))
})

test_that("a vessel too little seen takes the default ceiling, said", {
  lim <- rb_create_ping_limits(steam(n = 50), speed = speed, default_kn = 20)
  expect_equal(lim$ceiling_kn, 20)
  expect_equal(lim$ceiling_basis, "default")
  expect_true(is.na(lim$recorded_kn))
  expect_true(is.na(lim$track_kn))
  # under min_moving reports over 1 kn, no recorded ceiling either
  d <- steam(speed = 0.5)
  d$speed[101:120] <- 2
  expect_true(is.na(rb_create_ping_limits(d, speed = speed)$recorded_kn))
  expect_equal(rb_create_ping_limits(d, speed = speed, min_moving = 10)$recorded_kn, 2)
})

test_that("the grain is the coarsest unit the times are written in", {
  grain <- function(dt) {
    d <- steam(step = 60, n = 200)
    d$time <- d$time + dt
    rb_create_ping_limits(d)$grain_s
  }
  expect_equal(grain(0), 60)
  expect_equal(grain(7), 1)
  expect_equal(grain(0.3), 0.1)
  expect_equal(grain(0.012), 0.001)
  expect_equal(grain(1e-4), 0)
})

test_that("the time error is measured against the recorded speed, else the grain stands for it", {
  d <- steam(step = 60, n = 600)
  e <- withr::with_seed(1, round(stats::runif(600, -20, 20)))
  d$time <- d$time + e
  lim <- rb_create_ping_limits(d, speed = speed)
  expect_equal(lim$time_error_basis, "recorded")
  # a step written off by the change in the error from one ping to the next: up to 40 s
  expect_gt(lim$time_error_s, 30)
  expect_lte(lim$time_error_s, 40)
  expect_equal(lim$time_error_n, 599)
  lim <- rb_create_ping_limits(d)
  expect_equal(lim$time_error_basis, "grain")
  expect_equal(lim$time_error_s, 1)
  expect_equal(lim$time_error_n, 0)
})

test_that("with a stream, a row per vessel and stream: the clock's by stream, the ceiling the vessel's", {
  a <- steam(step = 60, n = 300)
  a$stream <- "a"
  b <- steam(step = 60, n = 300)
  b$time <- b$time + 17
  b$stream <- "b"
  d <- rbind(a, b)
  lim <- rb_create_ping_limits(d, stream = stream, speed = speed)
  expect_equal(lim$stream, c("a", "b"))
  expect_equal(lim$grain_s, c(60, 1))
  expect_equal(lim$pings, c(300, 300))
  expect_equal(lim$ceiling_kn, c(12, 12))
  # steps are taken within a stream: the two clocks 17 s apart make no step of 17 s
  expect_equal(lim$dt_median_s, c(60, 60))
  # without the stream, the two read as one clock
  one <- rb_create_ping_limits(d, speed = speed)
  expect_equal(nrow(one), 1)
  expect_false("stream" %in% names(one))
})

test_that("the noise is the dataset's: the vessels' steps at rest, else the geometry of short steps", {
  rest <- steam(kn = 0, n = 300, speed = 0)
  rest$lat <- rest$lat + rep(c(0, 1e-5), 150) # 1.1 m apart
  lim <- rb_create_ping_limits(rbind(rest, steam(2L)), speed = speed)
  expect_equal(lim$noise_basis, c("rest", "rest"))
  expect_equal(lim$noise_m, rep(rb_calc_distance(-20, 64, -20, 64 + 1e-5), 2), tolerance = 1e-3)
  lim <- rb_create_ping_limits(steam())
  expect_equal(lim$noise_basis, "geometry")
  expect_lt(lim$noise_m, 0.01)
  expect_equal(rb_create_ping_limits(steam(n = 50))$noise_basis, "none")
})

test_that("the columns come back under the user's names; another speed column does not clash", {
  d <- steam()
  d$sog <- d$speed
  d$speed <- 99
  names(d)[names(d) == "vid"] <- "vessel_id"
  names(d)[names(d) == "time"] <- "time_stamp"
  lim <- rb_create_ping_limits(d, speed = sog, vid = vessel_id, time = time_stamp)
  expect_equal(names(lim)[1], "vessel_id")
  expect_equal(lim$recorded_kn, 12)
})

test_that("a lazy table gives a lazy table of the same limits", {
  d <- rbind(steam(), steam(2L, kn = 8))
  lim <- rb_create_ping_limits(d, speed = speed)
  lz <- rb_create_ping_limits(as_lazy(d), speed = speed)
  expect_s3_class(lz, "tbl_lazy")
  expect_equal(as.data.frame(dplyr::collect(lz)), as.data.frame(lim))
})

test_that("on dansk_truth every stream's grain is the true one, and the recorded ceiling 0.94-1.01 of it at 1 min", {
  d <- dplyr::collect(rb_read_example("dansk_truth"))
  d$sp <- ifelse(d$speed_basis == "derived", NA, d$speed)
  lim <- rb_create_ping_limits(d, stream = stream, speed = sp)
  tr <- unique(d[, c("vid", "rate_s", "true_ceiling_kn", "true_grain_s")])
  j <- merge(lim, tr, by = "vid")
  expect_equal(j$grain_s, j$true_grain_s)
  k <- j$rate_s == 60 & j$ceiling_basis == "recorded"
  r <- j$ceiling_kn[k] / j$true_ceiling_kn[k]
  expect_true(all(r > 0.94 & r < 1.01))
})

test_that("the arguments are checked", {
  d <- steam()
  expect_error(rb_create_ping_limits(d, default_kn = 0), "default_kn")
  expect_error(rb_create_ping_limits(d, min_recorded = NA), "min_recorded")
  expect_error(rb_create_ping_limits(d[, c("vid", "time", "lon")]), "lat")
})

test_that("rb_flag_ping_impossible(limits = ): each vessel judged by its own ceiling, the basis said", {
  fast <- steam(1L, kn = 30, step = 60, n = 200)
  slow <- steam(2L, kn = 8, step = 60, n = 200)
  slow$lat[100] <- 64.02 # 2.2 km off: 70 kn in and out
  d <- rbind(fast, slow)
  lim <- rb_create_ping_limits(d, speed = speed)
  expect_equal(lim$ceiling_kn, c(30, 8))
  # the fixed 25 kn flags the fast vessel's every leg it judges; its own ceiling keeps them
  fixed <- rb_flag_ping_impossible(d, method = "local")
  expect_gt(sum(!is.na(fixed$ping_flag[fixed$vid == 1])), 100)
  own <- rb_flag_ping_impossible(d, method = "local", limits = lim)
  expect_equal(which(!is.na(own$ping_flag)), 300)
  expect_equal(own$ping_flag_basis[300], "ceiling")
  # with the recorded speed near, it judges as before, the margin the vessel's
  rec <- rb_flag_ping_impossible(d, method = "local", speed = speed, limits = lim)
  expect_equal(which(!is.na(rec$ping_flag)), 300)
  expect_equal(rec$ping_flag_basis[300], "recorded")
  # a vessel the limits lack is judged by the fixed limit
  part <- rb_flag_ping_impossible(d, method = "local", limits = lim[lim$vid == 2, ])
  expect_identical(part$ping_flag[part$vid == 1], fixed$ping_flag[fixed$vid == 1])
  # the evidence reads the same limits
  ev <- rb_flag_ping_impossible(d, method = "local", limits = lim, evidence = TRUE)
  expect_equal(ev$ping_flag_side[300], "both")
})

test_that("limits are for the local rule, by keys the pings carry", {
  d <- steam()
  d$stream <- "a"
  lim <- rb_create_ping_limits(d, stream = stream, speed = speed)
  expect_error(rb_flag_ping_impossible(d, method = "clean", limits = lim), "local")
  expect_error(rb_flag_ping_impossible(d[names(d) != "stream"], method = "local", limits = lim), "stream")
  expect_error(rb_flag_ping_impossible(d, method = "local", limits = lim[, c("vid", "ceiling_kn")]), "grain_s")
})
