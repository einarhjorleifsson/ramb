# truth.R — dansk_truth in inst/extdata: four of dansk_planted's vessels written through the clocks and streams real data
# come with, every row holding the report as written and as it truly was; read with rb_read_example("dansk_truth").
#
# For scoring a flagger on both kinds of error (fishycode plan 065): a position that is not the vessel's, and a time
# that is not the report's. dansk_planted (data-raw/planted_faults.R) has one clock, whole seconds, every ping with a
# recorded speed and no fill; fishycode's study of 2026-11-05 ($FISHYDATA/archive/2026-11-05-speed-ceiling/) found
# that the clock decides what a speed rule blames: EM4SSF's case study, written to the whole minute, made 15 good
# positions look impossible to a 26 kn ceiling (a time written a minute late doubles the next step).
#
#   The source   dansk_planted's vessels EX_6 (trammel net), EX_10 (gillnet, slow), EX_12 and EX_14 (bottom trawl), at
#                1 min and 6 min, faults and all. Each vessel's true track: its untouched pings at 10 s (and its fast
#                stretch), between which a report's true position is interpolated.
#   The clocks   each `clock` a copy of the same tracks, written as:
#     fix           the fix's own time, whole seconds, as dansk_planted
#     minute_trunc  every time cut to its minute, as a device that writes whole minutes
#     minute_round  every time rounded to its minute, a ping dropped where two land on one minute; one ping per track
#                   written a step late (late_stamp), as the case study's 08:23
#     jitter        a third of the pings written up to half a step off, at random (as an Adriatic example)
#     receipt       two streams: `fix` with the fix's time, and `rx` stamped when received, 1-90 s late and drifting; a
#                   report in both is a copy in `rx` (most AIS reaches a database this way)
#     hourly        the 1 min track, and beside it a VMS-like stream, a report each hour, its unit's clock 0-120 s
#                   ahead
#     fills         resampled to the minute: stretches of 3-10 fixes lost and filled along straight lines (`fill`);
#                   one lost stretch per track ends at a misplaced fix, its fills drawn to it (misplaced_fill)
#     derived       the fix clock with the speed derived from the positions as delivered (no recorded speed)
#   and on the fix clock, per track: one stretch of 5 pings before a silence of 30 min or more moved 2-10 km together
#   (relocation), one ping written half a step late, keeping its place in the order (late_stamp), one speed report of
#   60 kn (speed_glitch).
#
# Every row: as written (time, lon, lat, speed), as it truly was (true_time, true_lon, true_lat: the report's own time
# and position), what is wrong (wrong_position, wrong_time, wrong_speed), what an ideal flagger does (`expect`), and
# the track's truth (true_ceiling_kn, true_grain_s). A stream's own lag (rx, vms) and a clock's grain are the stream's,
# not a ping's: `time_error_s` shows them, `wrong_time` does not. `wrong` is dansk_planted's: the pings step 0 should
# take out (a time written off is not one: its position is kept and its time flagged).
#
# Run from the package root after data-raw/planted_faults.R, then data-raw/data_dict.R. The seed is fixed.

library(DBI)
library(dplyr)

set.seed(20261105)
KN <- 1.943844 # knots per m/s
R_EARTH <- 6371008.8
VESSELS <- c("EX_6", "EX_10", "EX_12", "EX_14")
RATES <- c(60, 360)
hav <- function(lon1, lat1, lon2, lat2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * R_EARTH * asin(sqrt(pmin(1, a)))
}
dest <- function(lon, lat, d, b) {
  r <- pi / 180
  dl <- d / R_EARTH
  lat2 <- asin(sin(lat * r) * cos(dl) + cos(lat * r) * sin(dl) * cos(b * r))
  lon2 <- lon * r + atan2(sin(b * r) * sin(dl) * cos(lat * r), cos(dl) - sin(lat * r) * sin(lat2))
  data.frame(lon = ((lon2 / r + 540) %% 360) - 180, lat = lat2 / r)
}
as_time <- function(s) as.POSIXct(s, origin = "1970-01-01", tz = "UTC")

## The source, and each vessel's true track ---------------------------------------
p <- ramb::rb_read_example("dansk_planted") |> collect() |> as.data.frame()
attr(p$time, "tzone") <- "UTC"
p <- p[p$vessel %in% VESSELS, ]
track <- p[p$rate_s == 10 & p$feed == "a" & (is.na(p$fault) | p$fault == "fast"), ]
track <- track[order(track$vessel, track$time), ]
# where the vessel was at time s (interpolated between its true pings), and its recorded speed there
at <- function(vessel, s) {
  tr <- track[track$vessel == vessel, ]
  tt <- as.numeric(tr$time)
  lo <- pmax(1, findInterval(s, tt))
  hi <- pmin(nrow(tr), lo + 1)
  w <- ifelse(tt[hi] > tt[lo], pmin(1, pmax(0, (s - tt[lo]) / (tt[hi] - tt[lo]))), 0)
  data.frame(lon = tr$lon[lo] + w * (tr$lon[hi] - tr$lon[lo]), lat = tr$lat[lo] + w * (tr$lat[hi] - tr$lat[lo]),
             speed = tr$speed[lo] + w * (tr$speed[hi] - tr$speed[lo]))
}
# a vessel's true top speed: the highest it records twice in a row (a running median of 3) on its true track
ceiling_kn <- vapply(VESSELS, function(v) max(stats::runmed(track$speed[track$vessel == v], 3)), 0)

## The report as it truly was ---------------------------------------------------
# dansk_planted's rows at 1 min and 6 min. A copy carries a genuine position at the wrong time (its true time is
# planted_s earlier); a moved position (spike, run, detour, lon_eq_lat, first_after_gap, the tie's second ping) is at
# its true time, its true position where the vessel was then.
s <- p[p$rate_s %in% RATES, ]
t0 <- as.numeric(s$time)
copy_time <- s$fault %in% c("shifted_copy", "clock_offset", "summer_time", "near_copy")
s$true_time <- as_time(ifelse(copy_time, t0 - s$planted_s, t0))
moved <- s$wrong & !copy_time
s$true_lon <- s$lon
s$true_lat <- s$lat
for (v in VESSELS) {
  k <- which(moved & s$vessel == v)
  if (length(k)) {
    w <- at(v, t0[k])
    s$true_lon[k] <- w$lon
    s$true_lat[k] <- w$lat
  }
}
s$stream <- s$feed
s$stream_lag_s <- 0
s$speed_basis <- "recorded"
s$fill <- FALSE
s$wrong_speed <- FALSE
s$time_fault <- copy_time & s$fault != "near_copy" # a copy's time is not the report's
s <- s[, c("vessel", "rate_s", "feed", "stream", "time", "lon", "lat", "speed", "speed_basis", "course", "gear", "fishing",
           "fill", "fault", "fault_id", "wrong", "planted_m", "planted_s", "true_time", "true_lon", "true_lat",
           "stream_lag_s", "wrong_speed", "time_fault")]
s <- s[order(s$vessel, s$rate_s, s$time, s$feed), ]
track_of <- function(x) paste(x$vessel, x$rate_s)

## The clocks --------------------------------------------------------------------
# a clock rewrites `time`; its grain goes with it
clocked <- function(x, clock, grain) {
  x$clock <- clock
  x$true_grain_s <- grain
  x
}
# the next free fault id within each track
next_id <- function(x, tr) max(c(0L, x$fault_id[track_of(x) == tr]), na.rm = TRUE) + 1L
# pings a fault may take: untouched, on the vessel's own stream, inside the track
free_rows <- function(x, tr) which(track_of(x) == tr & is.na(x$fault) & x$stream %in% c("a", "fix") & !x$fill)

fix <- clocked(s, "fix", 1)

# on the fix clock: a relocation, a late stamp and a speed glitch per track
for (tr in unique(track_of(fix))) {
  j <- free_rows(fix, tr)
  t <- as.numeric(fix$time)
  # relocation: the 5 pings before a silence of 30 min or more, moved 2-10 km together
  gap_after <- j[c(diff(t[j]) >= 1800, FALSE)]
  ok <- gap_after[vapply(gap_after, function(g) all((g - 4):g %in% j), TRUE)]
  if (length(ok)) {
    g <- ok[sample.int(length(ok), 1)]
    k <- (g - 4):g
    x <- dest(fix$lon[k], fix$lat[k], runif(1, 2000, 10000), runif(1, 0, 360))
    fix$lon[k] <- x$lon
    fix$lat[k] <- x$lat
    fix$fault[k] <- "relocation"
    fix$fault_id[k] <- next_id(fix, tr)
    fix$wrong[k] <- TRUE
  }
  # late stamp: one ping written a step late (its true time stays); its neighbours a step either side
  j <- free_rows(fix, tr)
  rate <- fix$rate_s[j[1]]
  cand <- j[j - 1 %in% j & (j + 1) %in% j & c(FALSE, diff(t[j]) == rate)[seq_along(j)] &
              c(diff(t[j]) == rate, FALSE)]
  cand <- cand[(cand - 1) %in% j & (cand + 1) %in% j]
  if (length(cand)) {
    k <- cand[sample.int(length(cand), 1)]
    fix$time[k] <- fix$time[k] + rate / 2   # half a step late keeps its place in the order, as 08:23 did
    fix$fault[k] <- "late_stamp"
    fix$fault_id[k] <- next_id(fix, tr)
    fix$time_fault[k] <- TRUE
  }
  # speed glitch: one report's recorded speed 60 kn, its position untouched
  j <- free_rows(fix, tr)
  k <- j[sample.int(length(j), 1)]
  fix$speed[k] <- 60
  fix$fault[k] <- "speed_glitch"
  fix$fault_id[k] <- next_id(fix, tr)
  fix$wrong_speed[k] <- TRUE
}

trunc_min <- clocked(s, "minute_trunc", 60)
trunc_min$time <- as_time(floor(as.numeric(trunc_min$time) / 60) * 60)

round_min <- clocked(s, "minute_round", 60)
round_min$time <- as_time(round(as.numeric(round_min$time) / 60) * 60)
round_min <- round_min[!duplicated(round_min[, c("vessel", "rate_s", "feed", "time")]), ]
for (tr in unique(track_of(round_min))) {
  j <- free_rows(round_min, tr)
  t <- as.numeric(round_min$time)
  rate <- round_min$rate_s[j[1]]
  cand <- j[(j - 1) %in% j & (j + 1) %in% j]
  cand <- cand[t[cand] - t[cand - 1] == rate & t[cand + 1] - t[cand] == rate]
  if (length(cand)) {
    k <- cand[sample.int(length(cand), 1)]
    round_min$time[k] <- round_min$time[k] + 60 # a minute late, the next ping's minute not reached
    if (rate == 60) round_min <- round_min[-(k + 1), ] # at 1 min it lands on the next ping's minute: that one lost
    round_min$fault[k] <- "late_stamp"
    round_min$fault_id[k] <- next_id(round_min, tr)
    round_min$time_fault[k] <- TRUE
  }
}

jit <- clocked(s, "jitter", 1)
for (tr in unique(track_of(jit))) {
  j <- free_rows(jit, tr)
  rate <- jit$rate_s[j[1]]
  k <- j[runif(length(j)) < 1 / 3]
  jit$time[k] <- jit$time[k] + round(runif(length(k), -rate / 2, rate / 2))
  jit$fault[k] <- "jitter"
  jit$time_fault[k] <- TRUE
}

# receipt: at 1 min, each report on the fix stream (30 %), the rx stream (30 %) or both (40 %); rx stamped late by a
# drifting latency, 1-90 s
rec <- clocked(s[s$rate_s == 60, ], "receipt", 1)
u <- runif(nrow(rec))
on_fix <- u < 0.7
on_rx <- u >= 0.3
lag_of <- function(vessel, tt) {
  ph <- match(vessel, VESSELS)
  per <- c(6, 12, 24, 48)[ph] * 3600
  round(1 + 89 * (sin(2 * pi * tt / per + ph) + 1) / 2)
}
rx <- rec[on_rx, ]
rx$stream <- "rx"
rx$feed <- ifelse(rx$feed == "a", "rx", rx$feed)
rx$stream_lag_s <- lag_of(rx$vessel, as.numeric(rx$time))
rx$time <- rx$time + rx$stream_lag_s
both <- which(on_fix[on_rx])
rx$fault[both] <- ifelse(is.na(rx$fault[both]), "rx_copy", rx$fault[both])
rec <- rec[on_fix, ]
rec$stream <- ifelse(rec$feed == "a", "fix", rec$feed)
rec <- rbind(rec, rx)

# hourly: the 1 min track, and a VMS-like report each hour, its unit's clock 0-120 s ahead
hr <- clocked(s[s$rate_s == 60, ], "hourly", 1)
hr$stream <- ifelse(hr$feed == "a", "fix", hr$feed)
vms <- do.call(rbind, lapply(VESSELS, function(v) {
  tr <- track[track$vessel == v, ]
  tt <- as.numeric(tr$time)
  h <- which(!duplicated(floor(tt / 3600)))
  off <- round(runif(1, 0, 120))
  data.frame(vessel = v, rate_s = 60L, feed = "vms", stream = "vms", time = as_time(tt[h] + off), lon = tr$lon[h],
             lat = tr$lat[h], speed = tr$speed[h], speed_basis = "recorded", course = tr$course[h], gear = tr$gear[h],
             fishing = tr$fishing[h], fill = FALSE, fault = NA_character_, fault_id = NA_integer_, wrong = FALSE,
             planted_m = NA_real_, planted_s = NA_real_, true_time = tr$time[h], true_lon = tr$lon[h],
             true_lat = tr$lat[h], stream_lag_s = off, wrong_speed = FALSE, time_fault = FALSE, clock = "hourly",
             true_grain_s = 1)
}))
hr <- rbind(hr, vms)

# fills: at 1 min, the feed's own fixes rounded to the minute, stretches of 3-10 lost and filled along straight lines;
# one lost stretch per vessel ends at a misplaced fix (moved 1-3 km), its fills drawn to it
fl <- clocked(s[s$rate_s == 60 & s$feed == "a", ], "fills", 60)
fl$time <- as_time(round(as.numeric(fl$time) / 60) * 60)
fl <- fl[!duplicated(fl[, c("vessel", "time")]), ]
out <- list()
for (v in VESSELS) {
  x <- fl[fl$vessel == v, ]
  x <- x[order(x$time), ]
  n <- nrow(x)
  t <- as.numeric(x$time)
  # stretches to lose: inside the track, untouched, moving, each followed by an untouched fix
  lost <- rep(FALSE, n)
  starts <- sample(which(is.na(x$fault) & x$speed > 2)[-(1:5)], 12)
  for (a in sort(starts)) {
    len <- sample(3:10, 1)
    b <- a + len - 1
    if (b + 1 > n || any(lost[(a - 1):(b + 1)]) || any(!is.na(x$fault[(a - 1):(b + 1)])) ||
        t[b + 1] - t[a - 1] != (len + 1) * 60) next
    lost[a:b] <- TRUE
  }
  runs <- rle(lost)
  end <- cumsum(runs$lengths)
  begin <- end - runs$lengths + 1
  r <- which(runs$values)
  misplace <- if (length(r)) r[sample.int(length(r), 1)] else NA
  fills <- list()
  for (q in r) {
    a <- begin[q] - 1
    b <- end[q] + 1
    if (identical(q, misplace)) {
      m <- dest(x$lon[b], x$lat[b], runif(1, 1000, 3000), runif(1, 0, 360))
      x$true_lon[b] <- x$lon[b]
      x$true_lat[b] <- x$lat[b]
      x$lon[b] <- m$lon
      x$lat[b] <- m$lat
      x$fault[b] <- "misplaced_fill"
      x$fault_id[b] <- max(c(0L, x$fault_id), na.rm = TRUE) + 1L
      x$wrong[b] <- TRUE
    }
    k <- (begin[q]):(end[q])
    w <- (t[k] - t[a]) / (t[b] - t[a])
    f <- x[k, ]
    f$lon <- x$lon[a] + w * (x$lon[b] - x$lon[a])
    f$lat <- x$lat[a] + w * (x$lat[b] - x$lat[a])
    f$speed <- x$speed[a] + w * (x$speed[b] - x$speed[a])
    f$fill <- TRUE
    tw <- at(v, t[k])
    f$true_lon <- tw$lon
    f$true_lat <- tw$lat
    f$true_time <- f$time
    if (identical(q, misplace)) {
      f$fault <- "misplaced_fill"
      f$fault_id <- x$fault_id[b]
      f$wrong <- TRUE
    }
    fills[[length(fills) + 1]] <- f
  }
  out[[v]] <- rbind(x[!lost, ], do.call(rbind, fills))
}
fl <- do.call(rbind, out)

# derived: the fix clock, the speed derived from the positions as delivered, step by step within each stream
der <- clocked(s, "derived", 1)
der <- der[order(der$vessel, der$rate_s, der$feed, der$time), ]
same <- c(FALSE, track_of(der)[-1] == track_of(der)[-nrow(der)] & der$feed[-1] == der$feed[-nrow(der)])
dt <- c(NA, diff(as.numeric(der$time)))
dd <- c(NA, hav(der$lon[-nrow(der)], der$lat[-nrow(der)], der$lon[-1], der$lat[-1]))
der$speed <- ifelse(same & dt > 0, dd / dt * KN, NA)
der$speed_basis <- "derived"

## Together, and the truth of each row ------------------------------------------
x <- rbind(fix, trunc_min, round_min, jit, rec, hr, fl, der)
x$time_error_s <- as.numeric(x$time) - as.numeric(x$true_time)
x$wrong_position <- !is.na(x$true_lon) & hav(x$lon, x$lat, x$true_lon, x$true_lat) > 1
x$wrong_time <- x$time_fault
x$true_ceiling_kn <- unname(ceiling_kn[x$vessel])
x$expect <- ifelse(x$fault %in% c("near_copy", "clock_offset", "summer_time", "rx_copy"), "copy",
            ifelse(x$wrong_position, "flag position",
            ifelse(x$wrong_time, "flag time", "keep")))
x$vid <- sprintf("%s_%ss_%s", x$vessel, x$rate_s, x$clock)
x <- x[order(x$clock, x$rate_s, x$vessel, x$time, x$stream, x$lon, x$lat), ]
x$.rid <- seq_len(nrow(x))
x$rate_s <- as.integer(x$rate_s)
x$fault_id <- as.integer(x$fault_id)
x <- x[, c(".rid", "vid", "vessel", "rate_s", "clock", "stream", "feed", "time", "lon", "lat", "speed", "speed_basis",
           "course", "gear", "fishing", "fill", "fault", "fault_id", "wrong", "wrong_position", "wrong_time",
           "wrong_speed", "expect", "true_time", "true_lon", "true_lat", "time_error_s", "stream_lag_s", "planted_m",
           "planted_s", "true_ceiling_kn", "true_grain_s")]
rownames(x) <- NULL

## Checks ------------------------------------------------------------------------
# a row's two truths agree with its fault
stopifnot(
  all(x$wrong_position[x$fault %in% c("spike", "run", "detour", "lon_eq_lat", "first_after_gap", "relocation")]),
  !any(x$wrong_position[x$fault %in% c("late_stamp", "jitter", "speed_glitch", "rx_copy", "clock_offset",
                                       "summer_time", "near_copy")]),
  all(x$wrong_time[x$fault %in% c("late_stamp", "jitter", "clock_offset", "summer_time", "shifted_copy")]),
  all(x$wrong_speed == (x$fault %in% "speed_glitch")),
  all(x$wrong_position[x$fault %in% "misplaced_fill"])
)
# untouched pings: position and time as they were, but for a clock's grain or a stream's lag
u <- is.na(x$fault) & !x$fill
stopifnot(!any(x$wrong_position[u]), all(abs(x$time_error_s[u] - x$stream_lag_s[u]) <= x$true_grain_s[u]))
# every clock at both rates where it is written at both, every new fault on its clock
stopifnot(all(c("relocation", "late_stamp", "speed_glitch") %in% x$fault[x$clock == "fix"]),
          "late_stamp" %in% x$fault[x$clock == "minute_round"], "misplaced_fill" %in% x$fault[x$clock == "fills"],
          any(x$fill), all(x$speed_basis[x$clock == "derived"] == "derived"))
print(table(x$clock, x$rate_s))
print(table(x$fault, x$clock, useNA = "ifany"))
print(table(x$expect, x$clock))

## Write -------------------------------------------------------------------------
con <- dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "x", x)
dbExecute(
  con,
  "COPY (SELECT * FROM x) TO 'inst/extdata/dansk_truth.parquet' (FORMAT parquet, COMPRESSION zstd, COMPRESSION_LEVEL 19)"
)
duckdb::duckdb_unregister(con, "x")
dbDisconnect(con, shutdown = TRUE)
cat("rows:", nrow(x), "| file:", round(file.size("inst/extdata/dansk_truth.parquet") / 1e6, 1), "MB\n")
