mk <- function(n, start, lon0, vid = 1) {
  data.frame(
    vid = vid,
    time = start + 60 * (0:(n - 1)),
    lon = lon0 + 0.0003 * (0:(n - 1)),
    lat = 64 + 0.0002 * (0:(n - 1))
  )
}
t0 <- as.POSIXct("2022-10-15 00:00:00", tz = "UTC")

test_that("the clean recipe labels, never drops, and sorts by vid, time", {
  d <- whacks1[sample(nrow(whacks1)), ]
  out <- .whack_clean_df(d)
  expect_equal(nrow(out), nrow(d))
  expect_false(is.unsorted(order(out$vid, out$time)))
  expect_true(all(is.na(out$whack_stage) == !out$whack))
})

test_that("the clean recipe flags every labelled whack in the fixture", {
  d <- whacks1[order(whacks1$vid, whacks1$time), ]
  out <- .whack_clean_df(d)
  expect_true(all(out$whack[d$whacks]))
})

test_that("an isolated spike is flagged by the first stage (vmask or spike step)", {
  d <- mk(40, t0, -23)
  d$lon[20] <- d$lon[20] + 2
  out <- .whack_clean_df(d)
  expect_equal(which(out$whack), 20L)
  expect_true(out$whack_stage[20] %in% c("vmask", "spike"))
})

test_that("a bad first ping after a long gap is flagged, not adopted as the anchor", {
  d <- rbind(mk(15, t0, -23), mk(15, t0 + 4.5 * 3600, -22.99))
  d$lon[16] <- d$lon[16] + 6
  out <- .whack_clean_df(d)
  expect_equal(which(out$whack), 16L)
  # the forward scan alone gets this wrong: it keeps the bad ping and flags the good ones
  expect_equal(sum(.rb_whack_forward(d)$whack2), 14L)
})

test_that("vessels are independent", {
  a <- mk(40, t0, -23, vid = 1)
  a$lon[20] <- a$lon[20] + 2
  b <- mk(40, t0, -20, vid = 2)
  both <- .whack_clean_df(rbind(a, b))
  expect_equal(.whack_clean_df(a)$whack, both$whack[both$vid == 1])
  expect_equal(.whack_clean_df(b)$whack, both$whack[both$vid == 2])
})

# argosfilter adds 1 s to every time step; .rb_whack_sda floors the step at min_dt_s instead.
# With steps of 1-15 min the two speeds differ by under 2 %, and the flags should not.
test_that("the sda filter flags as argosfilter::sdafilter at its own settings", {
  skip_if_not_installed("argosfilter")
  set.seed(1)
  n <- 400
  d <- data.frame(
    vid = 1,
    time = t0 + cumsum(sample(60:900, n, TRUE)),
    lon = -23 + cumsum(rnorm(n, 0, 0.01)),
    lat = 64 + cumsum(rnorm(n, 0, 0.01))
  )
  k <- sample(10:390, 8)
  d$lon[k] <- d$lon[k] + rnorm(8, 0, 0.5)
  r <- argosfilter::sdafilter(d$lat, d$lon, d$time, rep("1", n), vmax = 12.86)
  ours <- .rb_whack_sda(
    d,
    kn_max = 12.86 / 0.514444,
    ang = c(15, 25),
    distlim = c(2500, 5000),
    speedlim_kn = c(0, 0),
    vmask_min_dist = 5000
  )
  ref <- r == "removed"
  # the R function labels the first/last two pings "end_location" and never removes them
  expect_equal(
    !is.na(ours$whack_sda)[-c(1:2, n - 1, n)],
    ref[-c(1:2, n - 1, n)]
  )
})

test_that("the result does not depend on the row order of the input (ties in time)", {
  d <- mk(40, t0, -23)
  d <- rbind(d, transform(d[20, ], lon = lon + 1, lat = lat + 1)) # two pings, same time
  set.seed(2)
  a <- .whack_clean_df(d)
  b <- .whack_clean_df(d[sample(nrow(d)), ])
  expect_equal(a$whack, b$whack)
  expect_equal(a$lon, b$lon)
})

test_that("a step shorter than min_dt_s counts as min_dt_s, in all five filters", {
  d <- data.frame(
    vid = 1,
    time = t0 + 5 * (0:29),
    lon = -23,
    lat = 64 + 0.000045 * (0:29)
  )
  d$lon[15] <- d$lon[15] + 0.00205 # 100 m off the track, 5 s from either neighbour
  for (m in c(1, 10)) {
    hit <- m == 1 # 100 m over 5 s is 20 m/s; over 10 s, 10 m/s (< 12.9)
    expect_equal(any(.whack_clean_df(d, min_dt_s = m)$whack), hit)
    expect_equal(any(!is.na(.rb_whack_sda(d, min_dt_s = m)$whack_sda)), hit)
    expect_equal(any(.rb_whack_forward(d, min_dt_s = m)$whack2), hit)
    expect_equal(any(.rb_whack_fwdbwd(d, min_dt_s = m)$whack), hit)
    expect_equal(
      any(.rb_whack_sequential_fast(d$lon, d$lat, d$time, min_dt_s = m)),
      hit
    )
  }
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  lz <- dplyr::copy_to(con, d, "pings")
  for (m in c(1, 10)) {
    expect_equal(
      any(dplyr::pull(.rb_whack_fwdbwd(lz, min_dt_s = m), whack), na.rm = TRUE),
      m == 1
    )
    flagged <- rb_flag_ping_impossible(lz, method = "clean", min_dt_s = m) |>
      dplyr::pull(ping_flag)
    expect_equal(any(!is.na(flagged)), m == 1)
  }
})
t0 <- as.POSIXct("2020-06-01 12:00:00", tz = "UTC")
dlon <- function(m) m / (111194.9 * cos(64 * pi / 180)) # degrees of longitude for m metres at 64 N

test_that("the lower-ranked copy of a report is flagged, the higher-ranked one kept", {
  d <- .rb_whack_duplicates(
    c(1, 1),
    t0 + c(0, 1),
    c(-22, -22 + dlon(0.5)),
    c(64, 64),
    c("stk", "astd")
  )
  expect_equal(d, c(FALSE, TRUE))
  d <- .rb_whack_duplicates(
    c(1, 1),
    t0 + c(0, 1),
    c(-22, -22 + dlon(0.5)),
    c(64, 64),
    c("astd", "stk")
  )
  expect_equal(d, c(TRUE, FALSE))
})

test_that("a pair outside the time or distance window, or of two vessels, is not a copy", {
  expect_equal(
    .rb_whack_duplicates(
      c(1, 1),
      t0 + c(0, 11),
      c(-22, -22),
      c(64, 64),
      c("stk", "astd")
    ),
    c(FALSE, FALSE)
  )
  expect_equal(
    .rb_whack_duplicates(
      c(1, 1),
      t0 + c(0, 5),
      c(-22, -22 + dlon(200)),
      c(64, 64),
      c("stk", "astd")
    ),
    c(FALSE, FALSE)
  )
  expect_equal(
    .rb_whack_duplicates(
      c(1, 2),
      t0 + c(0, 1),
      c(-22, -22),
      c(64, 64),
      c("stk", "astd")
    ),
    c(FALSE, FALSE)
  )
  # the window is kn_max over max_dt_s: 128.6 m at the defaults
  expect_equal(
    .rb_whack_duplicates(
      c(1, 1),
      t0 + c(0, 1),
      c(-22, -22 + dlon(120)),
      c(64, 64),
      c("stk", "astd")
    ),
    c(FALSE, TRUE)
  )
  expect_equal(
    .rb_whack_duplicates(
      c(1, 1),
      t0 + c(0, 1),
      c(-22, -22 + dlon(140)),
      c(64, 64),
      c("stk", "astd")
    ),
    c(FALSE, FALSE)
  )
})

test_that("same-feed and unranked pairs are left alone; an unranked feed loses to a ranked one", {
  expect_equal(
    .rb_whack_duplicates(
      c(1, 1),
      t0 + c(0, 0),
      c(-22, -22),
      c(64, 64),
      c("astd", "astd")
    ),
    c(FALSE, FALSE)
  )
  expect_equal(
    .rb_whack_duplicates(
      c(1, 1),
      t0 + c(0, 1),
      c(-22, -22),
      c(64, 64),
      c("x", "y")
    ),
    c(FALSE, FALSE)
  )
  expect_equal(
    .rb_whack_duplicates(
      c(1, 1),
      t0 + c(0, 1),
      c(-22, -22),
      c(64, 64),
      c("x", "emodnet")
    ),
    c(TRUE, FALSE)
  )
})

test_that("three feeds on one fix keep only the top one, whatever the input order", {
  vid <- c(1, 1, 1, 1)
  time <- t0 + c(0, 2, 3, 300)
  lon <- c(-22, -22 + dlon(1), -22 + dlon(2), -22.1)
  lat <- rep(64, 4)
  feed <- c("stk", "astd", "astdB", "astd")
  expect_equal(
    .rb_whack_duplicates(vid, time, lon, lat, feed),
    c(FALSE, TRUE, TRUE, FALSE)
  )
  o <- c(4, 2, 1, 3)
  expect_equal(
    .rb_whack_duplicates(vid[o], time[o], lon[o], lat[o], feed[o]),
    c(FALSE, TRUE, TRUE, FALSE)[o]
  )
})

test_that("short and NA input do not fail", {
  expect_equal(.rb_whack_duplicates(1, t0, -22, 64, "stk"), FALSE)
  expect_equal(
    .rb_whack_duplicates(
      numeric(0),
      t0[0],
      numeric(0),
      numeric(0),
      character(0)
    ),
    logical(0)
  )
  expect_equal(
    .rb_whack_duplicates(
      c(1, 1),
      t0 + c(0, 1),
      c(-22, NA),
      c(64, 64),
      c("stk", "astd")
    ),
    c(FALSE, FALSE)
  )
})
test_that("the forward filter matches the labelled fixture whacks1", {
  d <- whacks1[order(whacks1$vid, whacks1$time), ]
  out <- .rb_whack_forward(d, kn_max = 25)
  expect_equal(out$whack2, d$whacks)
})

test_that("the forward filter's compiled core matches the original R loop", {
  # Pre-2026-09-23 reference implementation of .fwd() (time-step floor min_dt since
  # 2026-10-08, as in the port), kept here only to
  # verify the Rcpp port (src/whack_forward.cpp) is byte-identical, including
  # its NA-propagation quirk (see whackies.R and the port's header comment).
  ref_fwd <- function(lon, lat, time, kn_max = 25, max_gap_h = 4, min_dt = 10) {
    ms_max <- kn_max * 0.514444
    max_gap_sec <- max_gap_h * 3600
    r <- 6371000
    n <- length(lon)
    flag <- rep(FALSE, n)
    if (n < 2L) {
      return(flag)
    }
    prev <- 1L
    for (i in 2:n) {
      if (is.na(lon[i]) || is.na(lat[i]) || is.na(time[i])) {
        prev <- i
        next
      }
      dt <- as.numeric(time[i] - time[prev], units = "secs")
      if (is.na(dt) || dt > max_gap_sec) {
        prev <- i
        next
      }
      phi1 <- lat[prev] * pi / 180
      phi2 <- lat[i] * pi / 180
      dphi <- (lat[i] - lat[prev]) * pi / 180
      dlam <- (lon[i] - lon[prev]) * pi / 180
      d <- 2 *
        r *
        asin(pmin(
          1,
          sqrt(
            sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
          )
        ))
      if (is.na(d) || d / max(dt, min_dt) > ms_max) {
        flag[i] <- TRUE
      } else {
        prev <- i
      }
    }
    flag
  }

  set.seed(42)
  n <- 5000
  lon <- -25 + cumsum(rnorm(n, 0, 0.0002))
  lat <- 62 + cumsum(rnorm(n, 0, 0.0002))
  time <- as.POSIXct("2020-01-01", tz = "UTC") + sort(sample(1:200000, n))
  lon[100] <- lon[100] + 5 # isolated spike
  lat[500:510] <- lat[500:510] + 3 # an 11-point run
  time[2000:n] <- time[2000:n] + 5 * 3600 # a >4h gap (tests max_gap_h reset)

  expect_identical(
    ref_fwd(lon, lat, time),
    ramb:::whack_forward_cpp(
      lon,
      lat,
      as.numeric(time),
      25 * 0.514444,
      4 * 3600,
      10
    )
  )

  # A single NA lon (valid lat/time) — exercises the NA-propagation cascade
  # the port intentionally reproduces rather than "fixing" here.
  lon_na <- lon
  lon_na[50] <- NA_real_
  expect_identical(
    ref_fwd(lon_na, lat, time),
    ramb:::whack_forward_cpp(
      lon_na,
      lat,
      as.numeric(time),
      25 * 0.514444,
      4 * 3600,
      10
    )
  )
})

test_that("the forward filter handles degenerate inputs", {
  empty <- whacks1[0, ]
  expect_equal(nrow(.rb_whack_forward(empty)), 0)

  one <- whacks1[1, ]
  out <- .rb_whack_forward(one)
  expect_equal(out$whack2, FALSE)
})

test_that("a second report in the same second is judged by its distance", {
  t0 <- as.POSIXct("2022-10-15 00:00:00", tz = "UTC")
  d <- data.frame(
    vid = 1,
    time = t0 + 60 * c(0, 1, 1, 2, 3),
    lon = -23 + 0.0003 * c(0, 1, 1, 2, 3),
    lat = 64 + 0.0002 * c(0, 1, 1, 2, 3)
  )
  d$lon[3] <- d$lon[3] + 0.00001 # duplicate, about 0.5 m away
  expect_false(any(.rb_whack_forward(d)$whack2))
  d$lon[3] <- d$lon[3] + 0.1 # same second, 5 km away
  expect_equal(which(.rb_whack_forward(d)$whack2), 3L)
})
