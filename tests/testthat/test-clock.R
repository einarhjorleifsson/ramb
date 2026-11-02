# The streams' clocks (clock.R): the dt pattern, the lag against a reference stream, a ping's own lag, and the
# harmonized time; on tracks made to a known lag, and on ramb's `dansk`.

# One vessel moving east at a steady speed along 64 N: the reference stream "a", and a stream "b" whose clock runs
# `lag` seconds late (its position at a stamped time is the vessel's position `lag` seconds before).
clock_track <- function(
  lag = 30,
  vid = 1L,
  n_a = 120,
  dt_a = 60,
  dt_b = 37,
  kn = 10,
  streams = c("a", "b"),
  t0 = as.POSIXct("2024-03-01", tz = "UTC")
) {
  m <- 6371000 * pi / 180
  pos <- function(s) -20 + kn * 1852 / 3600 * s / (m * cos(64 * pi / 180))
  ta <- seq(0, by = dt_a, length.out = n_a)
  tb <- seq(5, max(ta), by = dt_b)
  d <- rbind(
    data.frame(
      vid = vid,
      time = t0 + ta,
      lon = pos(ta),
      lat = 64,
      speed = kn,
      course = 90,
      stream = streams[1]
    ),
    data.frame(
      vid = vid,
      time = t0 + tb,
      lon = pos(tb - lag),
      lat = 64,
      speed = kn,
      course = 90,
      stream = streams[2]
    )
  )
  d[order(d$time), ]
}

test_that("rb_add_ping_stream() reads the cadence, the grid and the crossings", {
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  d <- data.frame(
    vid = 1L,
    time = t0 + c(0, 10, 20, 29, 30, 40, 50, 70),
    lon = -20 + 0:7 * 1e-3,
    lat = 64
  )
  s <- rb_add_ping_stream(d)
  expect_equal(s$stream, rep("1", 8))
  expect_equal(s$stream_dt_s, c(NA, 10, 10, 9, 1, 10, 10, 20))
  expect_equal(unique(s$stream_cadence_s), 10)
  # the inserted ping and the one after it make the short interval; 20 s is two cadences, on the grid
  expect_equal(
    s$stream_off_grid,
    c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_equal(s$stream_phase_s, c(0, 0, 0, 9, 0, 0, 0, 0))
  expect_false(any(s$stream_crossed))
  # two streams: each its own cadence; a crossing where the merged track changes stream
  d$feed <- c("a", "a", "a", "b", "a", "a", "a", "a")
  s <- rb_add_ping_stream(d, stream = feed)
  expect_equal(s$feed, d$feed)
  expect_equal(s$stream_dt_s, c(NA, 10, 10, NA, 10, 10, 10, 20))
  expect_false(any(s$stream_off_grid))
  expect_equal(
    s$stream_crossed,
    c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  # a ping with no stream is in no stream's pattern
  e <- d
  e$feed[4] <- NA
  e <- rb_add_ping_stream(e, stream = feed)
  expect_true(is.na(e$stream_dt_s[4]) && is.na(e$stream_cadence_s[4]))
  expect_equal(e$stream_dt_s[5], 10)
  # a lazy table gives the same
  l <- dplyr::collect(rb_add_ping_stream(as_lazy(d), stream = feed))
  expect_equal(
    as.data.frame(l[order(l$time), names(s)]),
    as.data.frame(s),
    ignore_attr = TRUE
  )
  # a named stream column that is not there is an error, not one stream
  expect_error(rb_add_ping_stream(d, stream = provider), "no column `provider`")
  expect_error(rb_add_ping_stream(s, stream = feed), "already has")
})

test_that("rb_create_ping_lags() finds a known lag, and every unit lands in one outcome", {
  d <- rbind(
    clock_track(lag = 30, vid = 1L),
    clock_track(lag = 0, vid = 2L),
    clock_track(lag = 30, vid = 3L, streams = c("b", "b")),
    clock_track(lag = 30, vid = 4L, streams = c("b", "c")),
    clock_track(lag = 30, vid = 5L, n_a = 10)
  )
  l <- rb_create_ping_lags(d, reference = "a")
  expect_s3_class(l, "data.frame")
  o <- stats::setNames(l$outcome, paste(l$vid, l$stream))
  expect_equal(
    o[c("1 a", "1 b", "2 b", "3 b", "4 b", "4 c", "5 b")],
    c(
      "1 a" = "reference",
      "1 b" = "offset",
      "2 b" = "aligned",
      "3 b" = "one stream",
      "4 b" = "no reference",
      "4 c" = "no reference",
      "5 b" = "not measurable"
    )
  )
  b1 <- l[l$vid == 1 & l$stream == "b", ]
  expect_equal(b1$lag_s, 30, tolerance = 1e-6)
  expect_gt(b1$pairs, 100)
  expect_equal(b1$dist_zero_m, 30 * 10 * 1852 / 3600, tolerance = 1e-6)
  expect_lt(b1$dist_lag_m, 1e-6)
  expect_equal(b1$across_m, 0, tolerance = 1e-6)
  expect_equal(l$reference, rep("a", nrow(l)))
  expect_equal(l$period, rep(as.Date("2024-03-01"), nrow(l)))
  # lazy in, lazy out, the same table
  ll <- rb_create_ping_lags(as_lazy(d), reference = "a")
  expect_s3_class(ll, "tbl_lazy")
  ll <- dplyr::arrange(dplyr::collect(ll), vid, stream)
  expect_equal(as.data.frame(ll), as.data.frame(l), ignore_attr = TRUE)
  # a sparse stream against a dense reference: the residual pair
  s <- clock_track(lag = 30, n_a = 1000, dt_a = 20, dt_b = 600)
  r <- rb_create_ping_lags(
    s,
    reference = "a",
    method = "residual",
    min_pairs = 5
  )
  expect_equal(r$lag_s[r$stream == "b"], 30, tolerance = 1e-6)
  expect_equal(r$outcome[r$stream == "b"], "offset")
  expect_true(is.na(r$across_m[r$stream == "b"]))
  # guidance
  expect_error(rb_create_ping_lags(d), "reference")
  expect_error(
    rb_create_ping_lags(d[names(d) != "stream"], reference = "a"),
    "rb_add_ping_stream"
  )
  expect_error(
    rb_create_ping_lags(d, reference = "a", period = "fortnight"),
    "period"
  )
})

test_that("rb_create_ping_lags(method = \"track\") places a sparse stream on the reference's track", {
  d <- clock_faults()
  l <- rb_create_ping_lags(
    d,
    reference = "ais",
    method = "track",
    period = "week",
    min_pairs = 10
  )
  v <- l[l$stream == "vms", ]
  # each week's lag is the median of its pings' true lags, here exactly: a straight track at a steady speed
  tr <- d[d$stream == "vms", ]
  tr$period <- as.Date(lubridate::floor_date(tr$time, "week", week_start = 1))
  tr <- stats::aggregate(lag_true_s ~ vid + period, tr, stats::median)
  m <- merge(v, tr, by = c("vid", "period"))
  expect_equal(nrow(m), nrow(v))
  expect_lt(max(abs(m$lag_s - m$lag_true_s)), 1e-6)
  # the on-time units aligned, the drifting and the fast clock offset; no recorded speed is needed
  expect_true(all(m$outcome[m$vid %in% c(1:4, 7)] == "aligned"))
  expect_true(all(m$outcome[m$vid %in% 5:6] == "offset"))
  expect_true(all(l$outcome[l$stream == "ais"] == "reference"))
  s <- rb_create_ping_lags(
    d[names(d) != "speed"],
    reference = "ais",
    method = "track",
    period = "week",
    min_pairs = 10
  )
  expect_equal(s$lag_s, l$lag_s)
  # a lag beyond the search is not found: the pings find no segment near enough
  far <- d
  far$time[far$vid == 6 & far$stream == "vms"] <- far$time[
    far$vid == 6 & far$stream == "vms"
  ] +
    3 * 3600
  f <- rb_create_ping_lags(
    far,
    reference = "ais",
    method = "track",
    period = "week",
    min_pairs = 10
  )
  expect_true(all(
    f$outcome[f$vid == 6 & f$stream == "vms"] %in%
      c("not measurable", "unclear")
  ))
  # lazy, the same table
  ll <- dplyr::collect(rb_create_ping_lags(
    as_lazy(d),
    reference = "ais",
    method = "track",
    period = "week",
    min_pairs = 10
  ))
  ll <- ll[order(ll$vid, ll$stream, ll$period), ]
  expect_equal(as.data.frame(ll), as.data.frame(l), ignore_attr = TRUE)
})

test_that("rb_add_ping_own_lag() finds a copy of a reference report, and a displaced ping", {
  d <- clock_track(lag = 0, n_a = 30)
  d <- d[d$stream == "a", ]
  m <- 6371000 * pi / 180
  east <- function(metres) metres / (m * cos(64 * pi / 180))
  cp <- rbind(
    transform(d[5, ], time = time + 50, stream = "b"), # the same report, 50 s later
    transform(d[10, ], time = time - 20, lon = lon + east(1.5), stream = "b"), # 1.5 m off, 20 s earlier
    transform(d[15, ], time = time + 50, lon = lon + east(3), stream = "b"), # 3 m off: another report
    transform(d[20, ], time = time + 200, stream = "b") # too far apart in time
  )
  x <- rbind(d, cp)
  o <- rb_add_ping_own_lag(x, reference = "a")
  b <- o[o$stream == "b", ]
  expect_equal(b$lag_own_s, c(50, -20, NA, NA))
  expect_equal(b$lag_own_basis, c("copy", "copy", NA, NA))
  expect_true(all(is.na(o$lag_own_s[o$stream == "a"])))
  # a vessel at rest: two reports at one place are not copies
  y <- x
  y$speed <- 0
  expect_true(all(is.na(rb_add_ping_own_lag(y, reference = "a")$lag_own_s)))
  # `streams` limits the pings judged
  expect_true(all(is.na(
    rb_add_ping_own_lag(x, reference = "a", streams = "c")$lag_own_s
  )))
  # a single ping dropped 40 s late into a 10-s stream: (-40, +40) against its neighbours
  t0 <- as.POSIXct("2024-03-01", tz = "UTC")
  v <- 10 * 1852 / 3600
  pos <- function(s) -20 + east(v * s)
  s <- data.frame(
    vid = 1L,
    time = t0 + c(0:60 * 10, 299),
    lat = 64,
    speed = 10,
    course = 90,
    stream = "1"
  )
  s$lon <- pos(c(0:60 * 10, 299 - 40))
  r <- rb_add_ping_own_lag(s, method = "residual")
  expect_equal(r$lag_own_s[62], 40, tolerance = 1e-6)
  expect_equal(r$lag_own_basis[62], "residual")
  expect_equal(sum(!is.na(r$lag_own_s)), 1)
  # both: a copy comes first; the two that are not copies are displaced against the reference's pings around them
  w <- rb_add_ping_own_lag(x, reference = "a", method = c("copy", "residual"))
  expect_equal(
    w$lag_own_basis[w$stream == "b"],
    c("copy", "copy", "residual", "residual")
  )
  expect_equal(w$lag_own_s[w$stream == "b"][4], 200, tolerance = 1e-6)
  # lazy
  l <- dplyr::collect(rb_add_ping_own_lag(as_lazy(x), reference = "a"))
  expect_equal(sort(l$lag_own_s), sort(o$lag_own_s))
  # guidance
  expect_error(rb_add_ping_own_lag(x), "needs `reference`")
  expect_error(
    rb_add_ping_own_lag(x, reference = "a", stream = vid),
    "all name the column"
  )
  expect_error(rb_add_ping_own_lag(o, reference = "a"), "already has")
})

test_that("rb_add_ping_own_lag() finds dansk's displaced pings", {
  s <- rb_add_ping_stream(dansk, vid = vessel_id, time = time_stamp)
  r <- rb_add_ping_own_lag(
    s,
    method = "residual",
    vid = vessel_id,
    time = time_stamp
  )
  d <- r[!is.na(r$lag_own_s), ]
  # 288 pings some 39 s late and one 39 s early, in four vessels. The phase-model study (fishycode plan 059) counted
  # 289 late at a speed floor of 1 m/s (1.94 kn); ramb's `kn` starts at 2 kn, and the one more is at 1.96 kn.
  expect_equal(nrow(d), 289)
  expect_equal(sum(d$lag_own_s > 0), 288)
  expect_equal(round(stats::median(d$lag_own_s), 1), 38.8)
  expect_equal(sort(unique(d$vessel_id)), c("EX_10", "EX_11", "EX_13", "EX_14"))
  # the dt pattern alone puts 200 of them off the 10-s grid
  expect_equal(sum(d$stream_off_grid), 200)
  expect_equal(unique(s$stream_cadence_s[s$vessel_id == "EX_10"]), 10)
})

test_that("rb_add_ping_own_lag() reads a unit's clock from its receipt, and tells a drift from a slow delivery", {
  d <- clock_faults()
  v <- d[d$stream == "vms", ]
  o <- rb_add_ping_own_lag(v, method = "receipt", unit = mid)
  expect_equal(unique(o$lag_own_basis), "receipt")
  # on time, a slow day of delivery (vid 7, day 10) and a backlog held 30 h (day 20): no lag
  expect_true(all(o$lag_own_s[o$vid %in% c(1:4, 7)] == 0))
  # a fast clock, 300 s: its lag to within the delivery's wobble
  expect_lt(max(abs(o$lag_own_s[o$vid == 6] - o$lag_true_s[o$vid == 6])), 2)
  # the sawtooth: under the least departure (120 s) unmoved; beyond 5 min within 10 s, but for the 3.5 days before the
  # clock is set right (days 26-29: the week around reads it as on time) and the series' last 2 pings
  expect_true(all(o$lag_own_s[o$vid == 5 & abs(o$lag_true_s) <= 120] == 0))
  s <- o[o$vid == 5 & abs(o$lag_true_s) > 300, ]
  expect_equal(nrow(s), 1440)
  expect_equal(sum(abs(s$lag_own_s - s$lag_true_s) > 10), 169)
  # each vessel's stream as the unit, here one unit each: the same
  expect_equal(
    rb_add_ping_own_lag(v, method = "receipt")$lag_own_s,
    o$lag_own_s
  )
  # the stream's usual lag, a number or a table by stream and period; a period the table lacks gets none
  u <- rb_add_ping_own_lag(v, method = "receipt", unit = mid, lag_usual = -60)
  expect_true(all(u$lag_own_s[u$vid %in% 1:4] == -60))
  expect_equal(u$lag_own_s - o$lag_own_s, rep(-60, nrow(o)))
  tb <- data.frame(stream = "vms", period = as.Date("2024-01-01"), lag_s = -60)
  expect_equal(
    rb_add_ping_own_lag(
      v,
      method = "receipt",
      unit = mid,
      lag_usual = tb
    )$lag_own_s,
    u$lag_own_s
  )
  tb$period <- as.Date("2023-01-01")
  expect_true(all(is.na(
    rb_add_ping_own_lag(
      v,
      method = "receipt",
      unit = mid,
      lag_usual = tb
    )$lag_own_s
  )))
  # a ping stamped at its receipt takes lag_at_receipt; a ping with no receipt is not judged
  a <- d[d$vid == 1, ]
  a$receipt[a$stream == "vms"] <- NA
  r <- rb_add_ping_own_lag(a, method = "receipt", lag_at_receipt = 5)
  expect_true(all(r$lag_own_s[r$stream == "ais"] == 5))
  expect_true(all(is.na(r$lag_own_basis[r$stream == "vms"])))
  # harmonized (the lag rounded to the second): the fast clock's pings back on the true time
  h <- rb_add_ping_time(o)
  expect_equal(unique(h$lag_basis), "receipt")
  expect_lte(
    max(abs(
      as.numeric(h$time[h$vid == 6]) - as.numeric(h$true_time[h$vid == 6])
    )),
    2
  )
  # lazy, the same lags; the rows as given
  l <- dplyr::collect(rb_add_ping_own_lag(
    as_lazy(v),
    method = "receipt",
    unit = mid
  ))
  l <- l[order(l$vid, l$time), ]
  expect_equal(l$lag_own_s, o$lag_own_s[order(o$vid, o$time)])
  expect_equal(as.numeric(o$time), as.numeric(v$time))
  # guidance
  expect_error(
    rb_add_ping_own_lag(v[names(v) != "receipt"], method = "receipt"),
    "receipt"
  )
  expect_error(
    rb_add_ping_own_lag(v, method = "receipt", lag_usual = "a"),
    "lag_usual"
  )
})

test_that("a backlog the receiver flushes at once is no unit's clock", {
  d <- clock_faults(days = 10)
  v <- d[d$stream == "vms", ]
  t0 <- as.POSIXct("2024-03-01", tz = "UTC")
  s <- as.numeric(difftime(v$true_time, t0, units = "secs"))
  # an outage: a day of every unit's reports held and received in one hour (day 6.5, 12:10-12:16)
  out <- s >= 5.5 * 86400 & s < 6.5 * 86400
  v$receipt[out] <- t0 + 6.5 * 86400 + 600 + seq_len(sum(out))
  # vid 1, on time, stamps at its receipt but in the outage, and the first of its held reports are dated a day late:
  # their gaps, within the backlog bound, are all its level has
  one <- v$vid == 1
  v$receipt[one & !out] <- v$time[one & !out]
  late <- one & out & s < 5.6 * 86400
  v$time[late] <- v$time[late] + 86400
  o <- rb_add_ping_own_lag(v, method = "receipt", unit = mid)
  expect_true(all(
    is.na(o$lag_own_s[o$vid == 1]) | o$lag_own_s[o$vid == 1] == 0
  ))
  expect_true(all(o$lag_own_s[o$vid %in% c(2:4, 7)] == 0))
  # the fast clock still found, in the outage from the gaps around it (fewer: within the delivery's wobble)
  expect_lt(max(abs(o$lag_own_s[o$vid == 6] - 300)), 5)
  # without the guard the flush moves the on-time unit by its misdated and held reports' gaps
  n <- rb_add_ping_own_lag(v, method = "receipt", unit = mid, mass_share = NA)
  expect_gt(max(abs(n$lag_own_s[n$vid == 1]), na.rm = TRUE), 600)
  expect_error(rb_add_ping_own_lag(v, method = "receipt", mass_share = 2))
})

test_that("a copy of a report the receipt moves follows its original", {
  d <- clock_faults(days = 3)
  d$receipt[d$stream == "ais"] <- NA
  # a feed that delivers every third vms report again, 40 s later
  k <- d[d$stream == "vms", ]
  k <- k[seq(1, nrow(k), by = 3), ]
  k$stream <- "copy"
  k$time <- k$time + 40
  k$receipt <- NA
  x <- rbind(d, k)
  o <- rb_add_ping_own_lag(
    x,
    reference = c("ais", "vms"),
    method = c("copy", "receipt"),
    streams = "copy",
    unit = mid
  )
  cp <- o[o$stream == "copy", ]
  vm <- o[o$stream == "vms", ]
  orig <- match(
    paste(cp$vid, as.numeric(cp$time) - 40),
    paste(vm$vid, as.numeric(vm$time))
  )
  expect_false(anyNA(orig))
  expect_equal(unique(cp$lag_own_basis), "copy")
  # the fast clock (vid 6) moved by its receipt, its copies by the same and their 40 s
  expect_true(all(abs(vm$lag_own_s[vm$vid == 6] - 300) < 2))
  expect_equal(cp$lag_own_s, 40 + vm$lag_own_s[orig])
  # harmonized, each copy on its original's time
  h <- rb_add_ping_time(o)
  expect_equal(
    as.numeric(h$time[h$stream == "copy"]),
    as.numeric(h$time[h$stream == "vms"][orig])
  )
  # without the receipt, the copy's lag is its gap alone
  expect_equal(
    rb_add_ping_own_lag(
      x,
      reference = c("ais", "vms"),
      streams = "copy"
    )$lag_own_s[x$stream == "copy"],
    rep(40, nrow(k))
  )
})

test_that("step 0's order is guarded: clocks before the flags, streams compared once harmonized", {
  d <- clock_faults()
  # a clock measured on flagged pings
  f <- d
  f$ping_flag <- NA_character_
  expect_warning(
    rb_create_ping_lags(
      f,
      reference = "ais",
      method = "track",
      period = "week",
      min_pairs = 10
    ),
    class = "ramb_after_clean"
  )
  expect_warning(
    rb_add_ping_own_lag(f[f$stream == "vms", ], method = "receipt", unit = mid),
    class = "ramb_after_clean"
  )
  # two streams compared before harmonizing, and not once harmonized
  x <- d[d$vid == 1, c("vid", "time", "lon", "lat", "stream")]
  expect_warning(
    rb_flag_ping_duplicate(x, priority = "ais", feed = stream),
    class = "ramb_unharmonised"
  )
  expect_warning(rb_flag_ping_impossible(x), class = "ramb_unharmonised")
  expect_warning(
    rb_flag_ping_impossible(as_lazy(x)),
    class = "ramb_unharmonised"
  )
  h <- rb_add_ping_time(rb_add_ping_own_lag(
    d[d$vid == 1, ],
    method = "receipt",
    unit = mid
  ))
  expect_no_warning(rb_flag_ping_impossible(h[c(
    "vid",
    "time",
    "lon",
    "lat",
    "stream",
    "lag_basis"
  )]))
  # one stream, or no stream column: nothing to say
  expect_no_warning(rb_flag_ping_impossible(x[x$stream == "ais", ]))
  expect_no_warning(rb_flag_ping_impossible(x[names(x) != "stream"]))
})

test_that("rb_add_ping_time() harmonizes by the first lag that applies, and moves no unmeasured ping", {
  t0 <- as.POSIXct("2024-03-10", tz = "UTC")
  x <- data.frame(
    vid = c(1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L),
    time = t0 + 0:7 * 60,
    stream = c("a", "b", "b", NA, "a", "b", "a", "b"),
    lag_own_s = c(NA, 50, NA, NA, NA, NA, NA, NA),
    lag_own_basis = c(NA, "copy", NA, NA, NA, NA, NA, NA)
  )
  lags <- data.frame(
    vid = c(1L, 1L, 2L, 2L, 3L, 3L),
    stream = c("a", "b", "a", "b", "a", "b"),
    period = as.Date("2024-03-01"),
    lag_s = c(NA, 30.4, NA, 4, NA, NA),
    outcome = c(
      "reference",
      "offset",
      "reference",
      "aligned",
      "reference",
      "not measurable"
    )
  )
  h <- rb_add_ping_time(x, lags)
  expect_equal(
    h$lag_basis,
    c(
      "none",
      "copy",
      "measured",
      "unmeasured",
      "none",
      "none",
      "none",
      "unmeasured"
    )
  )
  expect_equal(h$lag_s, c(0, 50, 30, NA, 0, 0, 0, NA))
  expect_equal(h$time_delivered, x$time)
  expect_equal(
    as.numeric(difftime(h$time, x$time, units = "secs")),
    -c(0, 50, 30, 0, 0, 0, 0, 0)
  )
  # not rounded
  expect_equal(rb_add_ping_time(x, lags, digits = NULL)$lag_s[3], 30.4)
  # without lags: one stream in the period is its own reference; several are unmeasured
  n <- rb_add_ping_time(x[x$vid == 2L & x$stream == "a", ])
  expect_equal(n$lag_basis, "none")
  expect_equal(
    rb_add_ping_time(x[x$vid == 2L, ])$lag_basis,
    c("unmeasured", "unmeasured")
  )
  # lazy, and lazy with data-frame lags
  l <- dplyr::collect(rb_add_ping_time(as_lazy(x), lags))
  l <- l[order(l$time_delivered), ]
  expect_equal(l$lag_basis, h$lag_basis)
  expect_equal(as.numeric(l$time), as.numeric(h$time))
  # guidance
  expect_error(rb_add_ping_time(x, rbind(lags, lags)), "one row per vessel")
  expect_error(
    rb_add_ping_time(x, lags, period = "year"),
    "not measured by year"
  )
  expect_error(rb_add_ping_time(x, lags[, -4]), "lag_s")
  expect_error(rb_add_ping_time(h, lags), "already has")
})

test_that("the clock functions chain on a lazy table and on a data frame alike", {
  d <- rbind(clock_track(lag = 30, vid = 1L), clock_track(lag = 0, vid = 2L))
  d$stream_raw <- d$stream
  d$stream <- NULL
  run <- function(x) {
    s <- rb_add_ping_stream(x, stream = stream_raw)
    l <- rb_create_ping_lags(s, reference = "a", stream = stream_raw)
    o <- rb_add_ping_own_lag(s, reference = "a", stream = stream_raw)
    rb_add_ping_time(o, l, stream = stream_raw)
  }
  a <- run(d)
  b <- dplyr::collect(run(as_lazy(d)))
  b <- b[order(b$vid, b$time_delivered, b$stream_raw), ]
  a <- a[order(a$vid, a$time_delivered, a$stream_raw), ]
  expect_equal(as.data.frame(b), as.data.frame(a), ignore_attr = TRUE)
  # vessel 1's b is 30 s late, by its unit's lag and, for the one b ping that sits on an a report, by the copy
  expect_equal(unique(a$lag_s[a$vid == 1 & a$stream_raw == "b"]), 30)
  expect_equal(
    sort(unique(a$lag_basis[a$vid == 1 & a$stream_raw == "b"])),
    c("copy", "measured")
  )
  expect_true(all(a$lag_s[a$vid == 2] == 0))
})
