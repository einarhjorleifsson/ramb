# rb_flag_ping_impossible(method = "local", local_speed = ) (fishycode plan 068): how a ping's local speed is taken from
# the recorded speeds around it.

# a short hop: at rest, three minutes at 8 knots, at rest again; a report every minute, each recording its own speed
hop <- function() {
  sp <- c(rep(0, 8), 8, 8, 8, rep(0, 8))
  along <- c(0, cumsum(sp[-1]) * 1852 / 60)
  data.frame(
    vid = 1L,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * (seq_along(sp) - 1),
    lon = -20 + along / (111320 * cos(64 * pi / 180)),
    lat = 64,
    speed = sp
  )
}
# steaming at about 10 knots, a report every minute
steady <- function(n = 30) {
  data.frame(
    vid = 1L,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * (seq_len(n) - 1),
    lon = -20 + 0.0071 * seq_len(n),
    lat = 64,
    speed = 10
  )
}
flagged <- function(d, ...) which(!is.na(rb_flag_ping_impossible(d, method = "local", speed = speed, ...)$ping_flag))

test_that("a vessel's short hop between two berths is not outvoted by the pings at rest", {
  d <- hop()
  # the median of both sides: the pings at rest outvote the three that move, and the good hop is flagged
  expect_equal(flagged(d, local_speed = "both_sides"), 9:12)
  # the larger side follows the speed where it changes
  expect_length(flagged(d, local_speed = "larger_side"), 0)
  # "larger_side" is the default
  expect_identical(
    rb_flag_ping_impossible(d, method = "local", speed = speed),
    rb_flag_ping_impossible(d, method = "local", speed = speed, local_speed = "larger_side")
  )
})

test_that("a spike and a run of wrong pings are caught either way", {
  spike <- steady()
  spike$lat[12] <- spike$lat[12] + 0.03
  run <- steady()
  run$lat[15:19] <- run$lat[15:19] + 0.027
  for (ls in c("larger_side", "both_sides")) {
    expect_equal(flagged(spike, local_speed = ls), 12L, info = ls)
    expect_equal(flagged(run, local_speed = ls), 15:19, info = ls)
  }
})

test_that("a constant passed in ... reaches the method and is never taken for kn_max", {
  d <- steady()
  # `k` once matched `kn_max` partially, the arguments after `...` now named in full
  expect_length(which(!is.na(rb_flag_ping_impossible(d, method = "forward", k = 1)$ping_flag)), 0)
  expect_length(which(!is.na(rb_flag_ping_impossible(d, method = "forward", kn_max = 1)$ping_flag)), 29)
  # with one ping a side, the hop's moving pings look like their neighbours at rest
  h <- hop()
  expect_false(identical(flagged(h, local_speed = "both_sides", k = 1), flagged(h, local_speed = "both_sides")))
})

test_that("local_speed is for the method local", {
  expect_error(
    rb_flag_ping_impossible(steady(), method = "clean", local_speed = "both_sides"),
    "used by the method \"local\""
  )
  expect_error(rb_flag_ping_impossible(steady(), method = "local", local_speed = "median"), "should be one of")
})
