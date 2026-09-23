test_that("whack_forward matches the labelled fixture whacks1", {
  d <- whacks1[order(whacks1$vid, whacks1$time), ]
  out <- whack_forward(d, kn_max = 25)
  expect_equal(out$whack2, d$whacks)
})

test_that("whack_forward's compiled core matches the original R loop", {
  # Pre-2026-09-23 reference implementation of .fwd(), kept here only to
  # verify the Rcpp port (src/whack_forward.cpp) is byte-identical, including
  # its NA-propagation quirk (see whackies.R and the port's header comment).
  ref_fwd <- function(lon, lat, time, kn_max = 25, max_gap_h = 4) {
    ms_max      <- kn_max * 0.514444
    max_gap_sec <- max_gap_h * 3600
    r           <- 6371000
    n    <- length(lon)
    flag <- rep(FALSE, n)
    if (n < 2L) return(flag)
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
      phi1 <- lat[prev] * pi / 180;  phi2 <- lat[i] * pi / 180
      dphi <- (lat[i] - lat[prev]) * pi / 180
      dlam <- (lon[i] - lon[prev]) * pi / 180
      d    <- 2 * r * asin(pmin(1, sqrt(
        sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlam/2)^2)))
      if (is.na(d) || d / max(dt, 1e-6) > ms_max) {
        flag[i] <- TRUE
      } else {
        prev <- i
      }
    }
    flag
  }

  set.seed(42)
  n <- 5000
  lon  <- -25 + cumsum(rnorm(n, 0, 0.0002))
  lat  <-  62 + cumsum(rnorm(n, 0, 0.0002))
  time <- as.POSIXct("2020-01-01", tz = "UTC") + sort(sample(1:200000, n))
  lon[100]         <- lon[100] + 5           # isolated spike
  lat[500:510]     <- lat[500:510] + 3       # an 11-point run
  time[2000:n]     <- time[2000:n] + 5 * 3600 # a >4h gap (tests max_gap_h reset)

  expect_identical(
    ref_fwd(lon, lat, time),
    ramb:::whack_forward_cpp(lon, lat, as.numeric(time), 25 * 0.514444, 4 * 3600)
  )

  # A single NA lon (valid lat/time) — exercises the NA-propagation cascade
  # the port intentionally reproduces rather than "fixing" here.
  lon_na <- lon
  lon_na[50] <- NA_real_
  expect_identical(
    ref_fwd(lon_na, lat, time),
    ramb:::whack_forward_cpp(lon_na, lat, as.numeric(time), 25 * 0.514444, 4 * 3600)
  )
})

test_that("whack_forward handles degenerate inputs", {
  empty <- whacks1[0, ]
  expect_equal(nrow(whack_forward(empty)), 0)

  one <- whacks1[1, ]
  out <- whack_forward(one)
  expect_equal(out$whack2, FALSE)
})
