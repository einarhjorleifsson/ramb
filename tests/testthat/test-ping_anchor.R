# rb_flag_ping_impossible(anchor = "vote") (fishycode plan 066): the pings after a jump decide which end is wrong.

# a vessel at a berth, a report every minute, then a silence of 6 min, a report placed 1 km off (a port event row
# written at the harbour's coordinates), and 10 s later the berth's reports again
berth <- function() {
  s <- c(0, 60, 120, 480, 490, 550, 610, 670, 730, 790)
  d <- data.frame(vid = 1L, time = as.POSIXct("2024-01-01", tz = "UTC") + s, lon = -20, lat = 64)
  d$lat[4] <- 64 + 0.009
  d
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
flags <- function(f) ifelse(is.na(f$ping_flag), ".", f$ping_flag)

test_that("an anchor placed off, its leg in long and its leg out short, is found wrong by the pings after it", {
  d <- berth()
  # today: the report placed off is kept as the anchor, and the two good reports after it flagged
  expect_equal(which(!is.na(rb_flag_ping_impossible(d, method = "forward")$ping_flag)), 5:6)
  expect_equal(which(!is.na(rb_flag_ping_impossible(d, method = "local")$ping_flag)), 5:6)
  f <- rb_flag_ping_impossible(d, method = "forward", anchor = "vote")
  expect_equal(flags(f), c(".", ".", ".", "anchor", ".", ".", ".", ".", ".", "."))
  l <- rb_flag_ping_impossible(d, method = "local", anchor = "vote", evidence = TRUE)
  expect_equal(which(!is.na(l$ping_flag)), 4L)
  expect_equal(l$ping_flag[4], "local_anchor")
  expect_equal(l$ping_flag_basis[4], "track")
  # its leg out is the impossible one
  expect_equal(l$ping_flag_side[4], "next")
  # "kept" is the default
  expect_identical(
    rb_flag_ping_impossible(d, method = "local", anchor = "kept"),
    rb_flag_ping_impossible(d, method = "local")
  )
})

test_that("a run of wrong pings before a silence is still caught at one-minute reports", {
  d <- steady()
  # five reports moved 3 km north together, then 40 min of silence
  d$lat[15:19] <- d$lat[15:19] + 0.027
  d$time[20:30] <- d$time[20:30] + 40 * 60
  runs <- list(
    forward = function(a) rb_flag_ping_impossible(d, method = "forward", anchor = a),
    local = function(a) rb_flag_ping_impossible(d, method = "local", speed = speed, anchor = a)
  )
  # at 25 knots the fifth is reachable from the ping before the run (19 knots over 5 min), with or without the vote
  caught <- list(forward = 15:18, local = 15:19)
  for (m in names(runs)) {
    vote <- runs[[m]]("vote")
    expect_equal(which(!is.na(vote$ping_flag)), caught[[m]], info = m)
    expect_equal(vote$ping_flag, runs[[m]]("kept")$ping_flag, info = m)
  }
})

test_that("a spike and a run inside a track: as the default, or the run's last ping in place of the good ones after", {
  d <- steady()
  # a run of three 2 km north, a spike 2 km south
  d$lat[10:12] <- d$lat[10:12] + 0.018
  d$lat[22] <- d$lat[22] - 0.018
  # "local" (its first stage takes the run and the spike) and "clean" (its first stage decides here): as the default
  l <- rb_flag_ping_impossible(d, method = "local", speed = speed, anchor = "vote")
  expect_equal(l$ping_flag, rb_flag_ping_impossible(d, method = "local", speed = speed)$ping_flag)
  expect_equal(which(!is.na(l$ping_flag)), c(10:12, 22L))
  expect_equal(
    rb_flag_ping_impossible(d, method = "clean", anchor = "vote")$ping_flag,
    rb_flag_ping_impossible(d, method = "clean")$ping_flag
  )
  # "forward" at 25 knots reaches the run's last ping from the ping before the run (24 knots) and keeps it as its
  # anchor, flagging the two good pings after it; the pings after vote it wrong
  expect_equal(which(!is.na(rb_flag_ping_impossible(d, method = "forward")$ping_flag)), c(10L, 11L, 13L, 14L, 22L))
  f <- rb_flag_ping_impossible(d, method = "forward", anchor = "vote")
  expect_equal(which(!is.na(f$ping_flag)), c(10:12, 22L))
  expect_equal(f$ping_flag[12], "anchor")
})

test_that("a tie is decided by the detour, and a wrong first ping after a gap is found by the pings after it", {
  d <- berth()
  # one report after the jump only: no ping votes, the report placed off makes the detour
  d <- d[1:6, ]
  d$time[6] <- d$time[5] + 300
  expect_equal(flags(rb_flag_ping_impossible(d, method = "forward", anchor = "vote"))[4], "anchor")
  # the first report after a gap placed off: it has no ping before it, and the pings after vote
  g <- steady(12)
  g$time[7:12] <- g$time[7:12] + 6 * 3600
  g$lat[7] <- g$lat[7] + 0.05
  expect_equal(which(!is.na(rb_flag_ping_impossible(g, method = "forward")$ping_flag)), 8:12)
  expect_equal(flags(rb_flag_ping_impossible(g, method = "forward", anchor = "vote"))[7:12],
               c("anchor", ".", ".", ".", ".", "."))
})

test_that("the vote gives the same on a lazy table as on a data frame", {
  d <- rbind(berth(), transform(berth(), vid = 2L, lon = -21))
  d$rid <- seq_len(nrow(d))
  a <- rb_flag_ping_impossible(d, method = "local", anchor = "vote", evidence = TRUE)
  b <- dplyr::collect(rb_flag_ping_impossible(as_lazy(d), method = "local", anchor = "vote", evidence = TRUE))
  b <- b[order(b$rid), ]
  for (k in c("ping_flag", "ping_flag_basis", "ping_flag_side")) {
    expect_equal(b[[k]], a[[k]], info = k)
  }
})

test_that("the vote is for clean, forward and local, and its constants go with it", {
  d <- berth()
  expect_error(
    suppressWarnings(rb_flag_ping_impossible(d, method = "fwdbwd", anchor = "vote")),
    "clean"
  )
  expect_error(rb_flag_ping_impossible(d, method = "local", vote_n = 3), "anchor = \"vote\"")
  expect_error(rb_flag_ping_impossible(d, method = "local", anchor = "vote", vote_n = 0), "whole number")
  expect_error(rb_flag_ping_impossible(d, method = "forward", anchor = "vote", detour_m = -1), "0 or more")
  expect_error(rb_flag_ping_impossible(d, anchor = "both"), "should be one of")
  # the constants reach the scan: with no pings voting, a detour margin of 5 km leaves the tie to the default
  t <- berth()[1:6, ]
  t$time[6] <- t$time[5] + 300
  expect_equal(which(!is.na(
    rb_flag_ping_impossible(t, method = "forward", anchor = "vote", detour_m = 5000)$ping_flag
  )), 5L)
})
