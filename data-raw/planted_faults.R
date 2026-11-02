# planted_faults.R — dansk_planted in inst/extdata: dansk's tracks made clean, thinned to four ping rates, with faults
# planted, each ping labelled with its fault; read with rb_read_example("dansk_planted").
#
# For scoring step 0's flaggers (fishycode plan 062, steps 2 and 3): every ping says what was done to it, so a method's
# labels can be counted against the truth, ping by ping, by fault, ping rate, gear and fishing.
#
#   The base   dansk (data-raw/DATASET.R), 14 vessels, a ping about every 10 s, with an expert's label of fishing.
#              dansk has faults of its own (387 legs faster than 25 kn): a forward scan takes out every ping more than
#              20 kn from the last one kept, restarting after an hour's gap (369 pings, from four vessels; the recorded
#              speed never passes 11.6 kn). What is left is taken as the truth.
#   Fast       One steaming stretch an hour long per vessel, where it has one (not fishing, recorded speed 6 kn or
#              more, steps of 30 s or less), its times compressed so that it truly runs at 30-40 kn; the later pings
#              move earlier by the time saved, and the recorded speed is scaled with it. Not a fault: a filter should
#              keep it.
#   Rates      10 s, 1 min, 6 min and 1 h: a ping is kept when the rate has passed since the last one kept.
#   Faults     planted in each vessel's track at each rate, two while fishing and two not where the track allows, at
#              pings whose neighbours are within three steps, and at least 20 pings from another planted fault (10 at
#              6 min, 5 at 1 h, where 20 pings are a day):
#     spike         one position moved 0.2-100 km (log-uniform), in any direction
#     run           3-8 positions in a row moved together by one such vector
#     detour        one position moved aside so that both its legs stay under 25 kn and it turns back sharply (under
#                   60 degrees): an out-and-back a speed filter cannot see
#     lon_eq_lat    a placeholder: the latitude written into the longitude
#     shifted_copy  6-12 positions of the same vessel from another day, 12-72 h away, stamped into this stretch
#                   between its own pings, in the same feed (as a day-shifted copy of a track in fishycode, TODO 88)
#     clock_offset  6-12 pings received again by a second feed whose clock runs 10-90 s late
#     summer_time   6-12 pings received again by a second feed stamped an hour late, in summer time
#     near_copy     a ping repeated in its own feed a second later, 1-5 m away: not wrong, a copy
#     tie           a second feed reporting the same second 0.2-2 km away: that one wrong, the first kept
#
# Every row's `fishing` and `gear` are the expert's for the vessel at the row's time (the base ping nearest in time),
# so a copy stamped elsewhere takes the label of where it is stamped. `planted_m` is how far a wrong ping is from where
# the vessel was at its time (the base, interpolated), `planted_s` how far a copy's time is moved.
#
# Times are whole seconds. Run from the package root, then data-raw/data_dict.R. The seed is fixed, so a rerun gives
# the same file.

library(DBI)
library(dplyr)

set.seed(20261102)
KN_MAX <- 25
KN <- 1.943844 # knots per m/s
RATES <- c(10, 60, 360, 3600)
FAULTS <- c("spike", "run", "detour", "lon_eq_lat", "shifted_copy", "clock_offset", "summer_time", "near_copy", "tie")
SPACING <- c(`10` = 20, `60` = 20, `360` = 10, `3600` = 5) # pings between two planted faults, by rate

R_EARTH <- 6371008.8
hav <- function(lon1, lat1, lon2, lat2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * R_EARTH * asin(sqrt(pmin(1, a)))
}
bearing <- function(lon1, lat1, lon2, lat2) {
  r <- pi / 180
  y <- sin((lon2 - lon1) * r) * cos(lat2 * r)
  x <- cos(lat1 * r) * sin(lat2 * r) - sin(lat1 * r) * cos(lat2 * r) * cos((lon2 - lon1) * r)
  (atan2(y, x) / r) %% 360
}
dest <- function(lon, lat, d, b) {
  r <- pi / 180
  dl <- d / R_EARTH
  lat2 <- asin(sin(lat * r) * cos(dl) + cos(lat * r) * sin(dl) * cos(b * r))
  lon2 <- lon * r + atan2(sin(b * r) * sin(dl) * cos(lat * r), cos(dl) - sin(lat * r) * sin(lat2))
  data.frame(lon = ((lon2 / r + 540) %% 360) - 180, lat = lat2 / r)
}
rlogunif <- function(n, a, b) exp(runif(n, log(a), log(b)))

## The base --------------------------------------------------------------------
d <- ramb::rb_read_example("dansk") |>
  collect() |>
  transmute(
    vessel = vessel_id,
    time = time_stamp,
    lon,
    lat,
    speed,
    course,
    gear,
    fishing = behaviour == "Fishing"
  ) |>
  arrange(vessel, time)

forward <- function(t, lon, lat, kn = 20, gap = 3600) {
  t <- as.numeric(t)
  keep <- logical(length(t))
  keep[1] <- TRUE
  j <- 1
  for (i in seq_along(t)[-1]) {
    dt <- t[i] - t[j]
    if (dt > gap || hav(lon[j], lat[j], lon[i], lat[i]) / max(dt, 1) * KN <= kn) {
      keep[i] <- TRUE
      j <- i
    }
  }
  keep
}
d <- d |> group_by(vessel) |> filter(forward(time, lon, lat)) |> ungroup()
stopifnot(nrow(d) == 395558 - 369)

## A vessel truly fast ---------------------------------------------------------
d$fault <- NA_character_
fast <- function(v) {
  t <- as.numeric(v$time)
  ok <- !v$fishing & !is.na(v$speed) & v$speed >= 6 & c(Inf, diff(t)) <= 30
  r <- rle(ok)
  end <- cumsum(r$lengths)
  start <- end - r$lengths + 1
  long <- which(r$values & t[end] - t[start] >= 3600)
  if (!length(long)) {
    return(v)
  }
  k <- long[sample.int(length(long), 1)]
  first <- (start[k]:end[k])[t[start[k]:end[k]] <= t[end[k]] - 3600] # a start with an hour of the run after it
  a <- first[sample.int(length(first), 1)]
  b <- max(which(t <= t[a] + 3600))
  f <- stats::median(v$speed[a:b]) / runif(1, 30, 40)
  saved <- (t[b] - t[a]) * (1 - f)
  t[a:b] <- round(t[a] + (t[a:b] - t[a]) * f) # whole seconds, as AIS reports them
  if (b < length(t)) t[(b + 1):length(t)] <- t[(b + 1):length(t)] - round(saved)
  v$time <- as.POSIXct(t, origin = "1970-01-01", tz = "UTC")
  v$speed[a:b] <- v$speed[a:b] / f
  v$fault[a:b] <- "fast"
  v
}
d <- d |> group_by(vessel) |> group_modify(~ fast(.x)) |> ungroup() |> arrange(vessel, time)
truth <- d # the vessel's true track, every ping, for the labels and the distances of the planted pings

## Thinned to each rate --------------------------------------------------------
thin <- function(t, rate) {
  t <- as.numeric(t)
  keep <- logical(length(t))
  keep[1] <- TRUE
  last <- t[1]
  for (i in seq_along(t)[-1]) {
    if (t[i] - last >= rate) {
      keep[i] <- TRUE
      last <- t[i]
    }
  }
  keep
}

## Planting --------------------------------------------------------------------
# One vessel at one rate: `v` its pings in time order. Returns the pings as delivered, with the planted ones.
plant <- function(v, rate) {
  n <- nrow(v)
  t <- as.numeric(v$time)
  sp <- SPACING[[as.character(rate)]]
  v0 <- v # the track as it was: copies take their values from it, never from a planted ping
  v$feed <- "a"
  v$wrong <- FALSE
  v$fault_id <- NA_integer_
  v$planted_s <- NA_real_
  step_ok <- c(FALSE, diff(t) <= 3 * rate) # the step from the ping before is within three steps
  cs <- c(0, cumsum(!step_ok)) # cs[j + 1]: the steps not ok among the first j
  busy <- !is.na(v$fault) # the fast stretch takes no other fault
  added <- list()
  id <- 0L
  plan <- unlist(lapply(FAULTS, function(f) rep(f, 4)))
  want_fishing <- rep(c(TRUE, TRUE, FALSE, FALSE), length(FAULTS))
  o <- sample.int(length(plan))
  for (k in o) {
    f <- plan[k]
    len <- switch(f, run = sample(3:8, 1), shifted_copy = , clock_offset = , summer_time = sample(6:12, 1), 1L)
    if (n < len + 2) next
    i <- seq_len(n - len)[-1] # the first planted ping; it needs a ping before and one after the stretch
    cont <- cs[i + len + 1] - cs[i] == 0 # the steps into i .. i + len all ok
    cb <- c(0, cumsum(busy))
    free <- cb[pmin(n, i + len - 1 + sp) + 1] - cb[pmax(1, i - sp)] == 0
    cand <- i[cont & free & v$fishing[i] == want_fishing[k]]
    if (f == "shifted_copy") {
      # a source stretch of the same length, 12-72 h away and itself continuous
      j0 <- seq_len(n - len + 1)
      src_ok <- cs[j0 + len] - cs[j0 + 1] == 0
      if (!any(src_ok)) next
      ts <- t[j0][src_ok]
      within <- function(lo, hi) findInterval(hi, ts) - findInterval(lo, ts, left.open = TRUE) > 0
      cand <- cand[within(t[cand] + 12 * 3600, t[cand] + 72 * 3600) | within(t[cand] - 72 * 3600, t[cand] - 12 * 3600)]
    }
    if (f == "summer_time") {
      # the copy lands an hour on, 360 pings at 10 s: the pings around where it lands must be free too
      l2 <- pmax(1, findInterval(t[cand] + 3600, t) - sp)
      h2 <- pmin(n, findInterval(t[cand + len - 1] + 3600, t) + 1 + sp)
      cand <- cand[cb[h2 + 1] - cb[l2] == 0]
    }
    if (!length(cand)) next
    a <- cand[sample.int(length(cand), 1)]
    s <- a:(a + len - 1)
    if (f == "detour") {
      # aside from the chord between the neighbours, both legs under 0.9 kn_max, the turn at it under 60 degrees
      p <- a - 1
      q <- a + 1
      lmax <- 0.9 * KN_MAX / KN * min(t[a] - t[p], t[q] - t[a])
      side <- bearing(v$lon[p], v$lat[p], v$lon[q], v$lat[q]) + sample(c(-90, 90), 1)
      dd <- runif(1, 0.5, 0.9) * lmax
      ok <- FALSE
      while (dd > 20) {
        x <- dest(v$lon[a], v$lat[a], dd, side)
        l1 <- hav(v$lon[p], v$lat[p], x$lon, x$lat)
        l2 <- hav(x$lon, x$lat, v$lon[q], v$lat[q])
        turn <- abs(((bearing(x$lon, x$lat, v$lon[p], v$lat[p]) - bearing(x$lon, x$lat, v$lon[q], v$lat[q]) + 540) %% 360) - 180)
        if (l1 / (t[a] - t[p]) * KN < KN_MAX && l2 / (t[q] - t[a]) * KN < KN_MAX && turn < 60) {
          ok <- TRUE
          break
        }
        dd <- dd * 0.8
      }
      if (!ok) next
    }
    id <- id + 1L
    busy[max(1, a - sp):min(n, a + len - 1 + sp)] <- TRUE
    if (f == "summer_time") {
      busy[max(1, findInterval(t[a] + 3600, t) - sp):min(n, findInterval(t[a + len - 1] + 3600, t) + 1 + sp)] <- TRUE
    }
    if (f %in% c("spike", "run")) {
      x <- dest(v$lon[s], v$lat[s], rlogunif(1, 200, 1e5), runif(1, 0, 360))
      v$lon[s] <- x$lon
      v$lat[s] <- x$lat
    } else if (f == "detour") {
      v$lon[a] <- x$lon
      v$lat[a] <- x$lat
    } else if (f == "lon_eq_lat") {
      v$lon[a] <- v$lat[a]
    }
    if (f %in% c("spike", "run", "detour", "lon_eq_lat")) {
      v$fault[s] <- f
      v$fault_id[s] <- id
      v$wrong[s] <- TRUE
      next
    }
    cp <- v0[s, ]
    cp$feed <- "a"
    cp$planted_s <- NA_real_
    cp$fault <- f
    cp$fault_id <- id
    cp$wrong <- f != "near_copy"
    if (f == "shifted_copy") {
      far <- j0[src_ok & abs(t[j0] - t[a]) >= 12 * 3600 & abs(t[j0] - t[a]) <= 72 * 3600]
      j <- far[sample.int(length(far), 1)]
      cp[, c("lon", "lat", "speed", "course")] <- v0[j:(j + len - 1), c("lon", "lat", "speed", "course")]
      # between the stretch's own pings
      stamp <- round(t[s] + runif(len, 0.2, 0.8) * (t[s + 1] - t[s]))
      cp$planted_s <- stamp - t[j:(j + len - 1)]
      cp$time <- as.POSIXct(stamp, origin = "1970-01-01", tz = "UTC")
    } else if (f == "clock_offset") {
      cp$feed <- "b"
      cp$planted_s <- round(runif(1, 10, 90))
      cp$time <- cp$time + cp$planted_s
    } else if (f == "summer_time") {
      cp$feed <- "b"
      cp$planted_s <- 3600
      cp$time <- cp$time + 3600
    } else if (f == "near_copy") {
      x <- dest(cp$lon, cp$lat, runif(1, 1, 5), runif(1, 0, 360))
      cp$lon <- x$lon
      cp$lat <- x$lat
      cp$planted_s <- 1
      cp$time <- cp$time + 1
    } else if (f == "tie") {
      x <- dest(cp$lon, cp$lat, runif(1, 200, 2000), runif(1, 0, 360))
      cp$lon <- x$lon
      cp$lat <- x$lat
      cp$feed <- "b"
      v$fault[a] <- "tie"
      v$fault_id[a] <- id
    }
    added[[length(added) + 1]] <- cp
  }
  bind_rows(v, added)
}

out <- list()
for (rate in RATES) {
  base <- d |> group_by(vessel) |> filter(thin(time, rate)) |> ungroup()
  p <- base |> group_by(vessel) |> group_modify(~ plant(.x, rate)) |> ungroup()
  p$rate_s <- rate
  out[[as.character(rate)]] <- p
}
x <- bind_rows(out)

## The truth at each row's time ------------------------------------------------
# the label of the base ping nearest in time, and the distance from where the vessel was (interpolated)
x <- x |>
  group_by(vessel) |>
  group_modify(function(g, key) {
    tr <- truth[truth$vessel == key$vessel, ]
    tt <- as.numeric(tr$time)
    tg <- as.numeric(g$time)
    lo <- pmax(1, findInterval(tg, tt))
    hi <- pmin(nrow(tr), lo + 1)
    w <- ifelse(tt[hi] > tt[lo], pmin(1, pmax(0, (tg - tt[lo]) / (tt[hi] - tt[lo]))), 0)
    near <- ifelse(abs(tg - tt[lo]) <= abs(tt[hi] - tg), lo, hi)
    g$fishing <- tr$fishing[near]
    g$gear <- tr$gear[near]
    lon_t <- tr$lon[lo] + w * (((tr$lon[hi] - tr$lon[lo] + 540) %% 360) - 180)
    lat_t <- tr$lat[lo] + w * (tr$lat[hi] - tr$lat[lo])
    g$planted_m <- ifelse(is.na(g$fault) | g$fault == "fast", NA_real_, hav(lon_t, lat_t, g$lon, g$lat))
    g
  }) |>
  ungroup()

x <- x |>
  mutate(vid = sprintf("%s_%ss", vessel, rate_s)) |>
  arrange(rate_s, vessel, time, feed, lon, lat) |>
  mutate(.rid = row_number()) |>
  select(.rid, vid, vessel, rate_s, feed, time, lon, lat, speed, course, gear, fishing, fault, fault_id, wrong,
         planted_m, planted_s) |>
  as.data.frame()
x$rate_s <- as.integer(x$rate_s)

## Checks ----------------------------------------------------------------------
# no wrong ping outside a planted fault; every fault at every rate
stopifnot(!any(x$wrong & is.na(x$fault)), !any(x$wrong & x$fault %in% c("fast", "near_copy")))
stopifnot(all(table(x$fault, x$rate_s) > 0))
# a ping's neighbours in its track are the rows next to it (rows are in vid and time order)
nb <- function(r, k) {
  y <- x[pmin(nrow(x), pmax(1, r + k)), ]
  y[y$vid != x$vid[r], c("lon", "lat")] <- NA
  y
}
leg_kn <- function(a, b) hav(a$lon, a$lat, b$lon, b$lat) / abs(as.numeric(b$time) - as.numeric(a$time)) * KN
# the detours: both legs under kn_max, and a sharp turn
r <- which(x$fault %in% "detour")
p <- nb(r, -1)
q <- nb(r, 1)
o <- x[r, ]
turn <- abs(((bearing(o$lon, o$lat, p$lon, p$lat) - bearing(o$lon, o$lat, q$lon, q$lat) + 540) %% 360) - 180)
stopifnot(all(leg_kn(p, o) < KN_MAX), all(leg_kn(o, q) < KN_MAX), all(turn < 60))
# the untouched track: no leg over 20 kn between two untouched pings within an hour of each other (the fast
# stretches are untouched too, and their legs are left out of this)
u <- x[is.na(x$fault) | x$fault == "fast" | (x$fault == "tie" & !x$wrong), ]
same <- u$vid[-1] == u$vid[-nrow(u)] & diff(as.numeric(u$time)) <= 3600 &
  !(u$fault[-1] %in% "fast") & !(u$fault[-nrow(u)] %in% "fast")
stopifnot(all(leg_kn(u[-nrow(u), ], u[-1, ])[same] <= 20 + 1e-9))
# the fast stretches truly fast: their legs at 10 s
f <- x[x$fault %in% "fast" & x$rate_s == 10, ]
fl <- leg_kn(f[-nrow(f), ], f[-1, ])[f$vid[-1] == f$vid[-nrow(f)]]
stopifnot(stats::median(fl) > 28)
# a spike or run is 0.2-100 km from where the vessel was
stopifnot(all(dplyr::between(x$planted_m[x$fault %in% c("spike", "run")], 150, 1.01e5)))
cat(sprintf(
  "%d vessels with a fast stretch; detours %.0f-%.0f m; ties %d; spikes %.1f-%.1f km\n",
  length(unique(x$vessel[x$fault %in% "fast"])), min(x$planted_m[r]), max(x$planted_m[r]),
  sum(x$fault %in% "tie" & x$wrong), min(x$planted_m[x$fault %in% "spike"]) / 1e3,
  max(x$planted_m[x$fault %in% "spike"]) / 1e3
))
print(table(x$fault, x$rate_s, useNA = "ifany"))
print(table(wrong = x$wrong, rate = x$rate_s))

## Write -----------------------------------------------------------------------
con <- dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "x", x)
dbExecute(
  con,
  "COPY (SELECT * FROM x) TO 'inst/extdata/dansk_planted.parquet' (FORMAT parquet, COMPRESSION zstd, COMPRESSION_LEVEL 19)"
)
duckdb::duckdb_unregister(con, "x")
dbDisconnect(con, shutdown = TRUE)
