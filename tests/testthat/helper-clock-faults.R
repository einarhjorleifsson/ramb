# A merged track with known clock faults (fishycode plan 060 §8 item 6), for the receipt and track methods and the
# guards. No random numbers: every value follows from the ping's index.
#
# Seven vessels move east along 64 N at 8 knots for 60 days, each with a reference stream "ais" (every 10 min, stamped
# at its receipt, on the true clock) and a stream "vms" (every 30 min) whose reports take 60-120 s to arrive (a
# deterministic wobble). The vms stream's own time is the true time less the unit's clock error c:
#   vid 1-4  on time (c = 0);
#   vid 5    a drifting clock: c grows 20 s a day from 0, is set right on day 30, and grows again (a sawtooth);
#   vid 6    a fast clock: c = -300 s throughout;
#   vid 7    on time, but on day 10 every report takes 10 min more to arrive (a slow day of delivery), and on day 20
#            every report is held 30 h and delivered at once (a backlog).
# Each vessel's vms unit is its own (`mid` = 100 + vid). `true_time` is kept for the tests; `lag_true_s` is the lag
# ramb's sign gives a vms ping (its time less the true time: -c).
clock_faults <- function(days = 60) {
  t0 <- as.POSIXct("2024-03-01", tz = "UTC")
  m <- 6371000 * pi / 180
  pos <- function(s) -20 + 8 * 1852 / 3600 * s / (m * cos(64 * pi / 180))
  out <- list()
  for (v in 1:7) {
    ta <- seq(0, days * 86400 - 1, by = 600)
    tv <- seq(300, days * 86400 - 1, by = 1800)
    day <- tv / 86400
    c_s <- switch(
      as.character(v),
      "5" = ifelse(day < 30, 20 * day, 20 * (day - 30)),
      "6" = rep(-300, length(tv)),
      rep(0, length(tv))
    )
    delivery <- 90 + 30 * sin(seq_along(tv))
    if (v == 7) {
      delivery <- delivery +
        ifelse(floor(day) == 10, 600, 0) +
        ifelse(floor(day) == 20, 30 * 3600, 0)
    }
    out[[length(out) + 1]] <- data.frame(
      vid = v,
      stream = "ais",
      mid = 100L + v,
      time = t0 + ta,
      true_time = t0 + ta,
      lon = pos(ta),
      lat = 64,
      speed = 8,
      receipt = t0 + ta
    )
    out[[length(out) + 1]] <- data.frame(
      vid = v,
      stream = "vms",
      mid = 100L + v,
      time = t0 + tv - c_s,
      true_time = t0 + tv,
      lon = pos(tv),
      lat = 64,
      speed = 8,
      receipt = t0 + tv + delivery
    )
  }
  d <- do.call(rbind, out)
  d$lag_true_s <- as.numeric(d$time) - as.numeric(d$true_time)
  d[order(d$vid, d$time, d$stream), ]
}
