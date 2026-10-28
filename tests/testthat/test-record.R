# Step 2 (record windows, record assignment) and the "window" fishing method: ports of fishycode's
# curate/station_window.R and R/trail_sql.R (fishycode plan 014 phase 2). Their exactness is checked on the
# Icelandic data; here: data frame = lazy table, and the rules on a small fixture.

t0 <- as.POSIXct("2024-03-01 06:00:00", tz = "UTC")
h <- function(x) t0 + x * 3600
records <- tibble::tibble(
  .sid = 1:6,
  schema = "a",
  vid = c(1L, 1L, 2L, 2L, 3L, 3L),
  gear = c("OTB", "OTB", "GNS", "GNS", "LHM", "OTB"),
  gear_class = c("towed", "towed", "static", "static", "jigged", "towed"),
  set_max_min = c(NA, NA, 60, 60, NA, NA),
  t1 = h(c(NA, NA, 0, 1, 0, NA)),
  t2 = h(c(0, 3, NA, NA, NA, NA)),
  t3 = h(c(4, 5, 10, NA, NA, NA)),
  t4 = h(c(NA, NA, 12, 14, 6, NA)),
  duration_m = c(240, 120, NA, NA, NA, NA),
  date = as.Date("2024-03-01"),
  s1 = c(2, 2, NA, NA, 0, 2),
  s2 = c(5, 5, NA, NA, 3, 5)
)

test_that("windows: one per phase, capped tows, the day fallback; data frame = lazy table", {
  w <- rb_create_record_windows(records, min_hauls = 1)
  wl <- dplyr::collect(rb_create_record_windows(
    as_lazy(records),
    min_hauls = 1
  ))
  wl <- wl[order(wl$vid, wl$w_start), names(w)]
  expect_equal(as.data.frame(wl), as.data.frame(w), ignore_attr = TRUE)
  expect_equal(
    sort(w$phase),
    sort(c("tow", "tow", "set", "haul", "set", "haul", "jig", "day"))
  )
  tow1 <- w[w$.sid == 1, ]
  expect_equal(tow1$w_end, h(3)) # cut back at the next recorded tow
  expect_equal(tow1$win_basis, "capped")
  expect_equal(w$win_basis[w$.sid == 6], "synthesised") # no usable time: the whole day
  expect_equal(w$events[w$.sid == 3 & w$phase == "haul"], "t1+t3+t4")
  expect_equal(w$w_end[w$.sid == 6], h(18)) # the day is half-open: it ends at midnight
})

test_that("windows never overlap: what cuts what, what yields, and ties", {
  na <- as.POSIXct(NA, tz = "UTC")
  r <- tibble::tibble(
    .sid = c(11:14, 21:22, 31:34, 41:42),
    schema = "a",
    vid = c(10L, 10L, 10L, 10L, 11L, 11L, 12L, 12L, 12L, 12L, 13L, 13L),
    gear = c(rep("GNS", 4), "LHM", "LHM", rep("OTB", 4), "OTB", "GNS"),
    gear_class = c(
      rep("static", 4),
      "jigged",
      "jigged",
      rep("towed", 5),
      "static"
    ),
    set_max_min = c(60, 60, 60, 60, NA, NA, NA, NA, NA, NA, NA, 60),
    t1 = c(h(c(0, 0.5, 3, 5.5, 0, 2)), rep(na, 6)),
    t2 = c(rep(na, 6), h(c(0, 0, 5, 5, 0)), na),
    t3 = c(h(c(2, 7, 10, 12)), na, na, h(c(2, 3, 6, 6, 3)), na),
    t4 = c(h(c(6, 9, 11, 13, 6, 4)), rep(na, 5), h(4)),
    duration_m = NA_real_,
    date = as.Date("2024-03-01")
  )
  w <- rb_create_record_windows(r, min_hauls = 1)
  wl <- dplyr::collect(rb_create_record_windows(
    as_lazy(r),
    min_hauls = 1
  ))
  expect_equal(
    as.data.frame(wl[order(wl$vid, wl$w_start, wl$.sid), names(w)]),
    as.data.frame(w[order(w$vid, w$w_start, w$.sid), ]),
    ignore_attr = TRUE
  )
  g <- function(sid, ph) w[w$.sid == sid & w$phase == ph, ]
  # gillnets: set 11, then set 12, then haul 11. A set cuts the set before it...
  expect_equal(g(11, "set")$w_end, h(0.5))
  expect_equal(g(11, "set")$win_closed_by, "next_event")
  expect_equal(g(12, "set")$win_closed_by, "max")
  # ...but never a haul: set 13 starts inside haul 11 and yields to it, here to nothing, set 14 in part
  expect_equal(g(11, "haul")$w_end, h(6))
  expect_equal(g(11, "haul")$win_basis, "recorded")
  expect_equal(g(13, "set")$w_start, g(13, "set")$w_end)
  expect_equal(g(13, "set")$win_opened_by, "previous_event")
  expect_equal(g(14, "set")$w_start, h(6))
  expect_equal(g(14, "set")$w_end, h(6.5))
  # a jig is cut at the next recorded start
  expect_equal(g(21, "jig")$w_end, h(2))
  expect_equal(g(21, "jig")$win_basis, "capped")
  # the same start: the window that ends first is cut to nothing; identical windows: the higher key
  expect_equal(g(31, "tow")$w_end, h(0))
  expect_equal(g(32, "tow")$w_end, h(3))
  expect_equal(g(33, "tow")$w_end, h(6))
  expect_equal(g(34, "tow")$w_end, h(5))
  # a synthesised start (t4 less the typical haul, 1.5 h) never cuts: it starts where the tow ends
  expect_equal(g(41, "tow")$w_end, h(3))
  expect_equal(g(42, "haul")$w_start, h(3))
  expect_equal(g(42, "haul")$win_basis, "synthesised")
  expect_equal(g(42, "haul")$win_opened_by, "previous_event")
  # no two windows of a vessel overlap
  x <- w[w$phase != "day" & w$w_end > w$w_start, ]
  x <- x[order(x$vid, x$w_start), ]
  same <- x$vid[-1] == x$vid[-nrow(x)]
  expect_true(all(x$w_start[-1][same] >= x$w_end[-nrow(x)][same]))
  # half-open: a ping at a window's end is not in it
  p <- tibble::tibble(vid = 12L, time = h(c(3, 5.5)), speed = 3)
  m <- rb_link_record_pings(p, dplyr::mutate(w, speed_min = 2, speed_max = 5))
  expect_equal(m$.sid, 33L)
})

test_that("event times: ties go to the earlier event time, then the lower key", {
  na <- as.POSIXct(NA, tz = "UTC")
  r <- tibble::tibble(
    .sid = 1:4,
    schema = "b",
    vid = c(1L, 1L, 2L, 2L),
    t1 = na,
    t2 = h(c(0, 0, 0, 3)),
    t3 = h(c(2, 2, 1, 5)),
    t4 = na
  )
  p <- tibble::tibble(
    vid = c(1L, 1L, 2L, 2L, 2L),
    time = h(c(-0.5, 2.5, -1, 2, 6))
  )
  e <- rb_link_record_events(p, r)
  k <- function(sid, ev) e[e$.sid == sid & e$event == ev, ]
  # twins: the lower key keeps both pings
  expect_equal(k(1, "t2")$time, h(-0.5))
  expect_equal(k(1, "t3")$time, h(2.5))
  expect_true(all(e$ping_lost[e$.sid == 2]))
  # tow 3's t3 (1:00, the next ping is 2:00) and tow 4's t2 (3:00, the previous ping is 2:00): 1 h each, the
  # earlier event time keeps it
  expect_equal(k(3, "t3")$time, h(2))
  expect_true(k(4, "t2")$ping_lost)
  # a set and a haul that start at the same time: the setting (the earlier phase) keeps the ping, every time
  r2 <- tibble::tibble(
    .sid = 5L,
    schema = "b",
    vid = 3L,
    t1 = h(0),
    t2 = na,
    t3 = h(0),
    t4 = na
  )
  p2 <- tibble::tibble(vid = 3L, time = h(0.5))
  for (i in 1:5) {
    e2 <- rb_link_record_events(p2[sample(nrow(p2)), ], r2)
    expect_equal(e2$ping_lost, c(FALSE, TRUE))
    expect_equal(e2$event, c("t1", "t3"))
  }
  el <- dplyr::collect(rb_link_record_events(
    as_lazy(p),
    as_lazy(r)
  ))
  el <- el[order(el$vid, el$event_time, el$event, el$.sid), names(e)]
  expect_equal(
    as.data.frame(el),
    as.data.frame(e[order(e$vid, e$event_time, e$event, e$.sid), ]),
    ignore_attr = TRUE
  )
})

test_that("pings: tier-1 windows win over the day; the counts, the record and fishing", {
  w <- rb_create_record_windows(records, min_hauls = 1)
  pings <- tibble::tibble(
    vid = c(1L, 1L, 1L, 3L, 3L, 4L),
    time = h(c(1, 3.5, 20, 2, 17, 1)),
    speed = c(3, NA, 3, 1, 9, 3)
  )
  m <- rb_link_record_pings(
    pings,
    w,
    carry = "gear",
    speed_min = "s1",
    speed_max = "s2"
  )
  ml <- dplyr::collect(rb_link_record_pings(
    as_lazy(pings),
    as_lazy(w),
    carry = "gear",
    speed_min = "s1",
    speed_max = "s2"
  ))
  expect_equal(
    as.data.frame(ml[order(ml$vid, ml$time), names(m)]),
    as.data.frame(m),
    ignore_attr = TRUE
  )
  r <- rb_add_record(pings, m, carry = "gear") |>
    rb_add_fishing(m, speed_min = "s1", speed_max = "s2")
  expect_equal(r$n_records, c(1, 1, 0, 1, 1, 0)) # vid 3 at 23:00: only the day window of .sid 6
  expect_equal(r$.sid, c(1L, 2L, NA, 5L, 6L, NA))
  expect_equal(r$fishing, c(TRUE, NA, FALSE, TRUE, FALSE, FALSE))
})

test_that("fishing on the windowed speed: a spike inside a tow is fishing; data frame = lazy table", {
  w <- rb_create_record_windows(records[1, ], min_hauls = 1)
  pings <- tibble::tibble(
    vid = 1L,
    time = h(seq(0.5, 2.5, by = 1 / 60)),
    speed = 3
  )
  pings$speed[61] <- 9 # 07:30, mid-tow
  pings$speed[62] <- NA
  m <- rb_link_record_pings(pings, w, speed_min = "s1", speed_max = "s2")
  f0 <- rb_add_fishing(pings, m, speed_min = "s1", speed_max = "s2")
  f10 <- rb_add_fishing(
    pings,
    m,
    speed_min = "s1",
    speed_max = "s2",
    window_s = 600
  )
  fl <- dplyr::collect(rb_add_fishing(
    as_lazy(pings),
    as_lazy(m),
    speed_min = "s1",
    speed_max = "s2",
    window_s = 600
  ))
  expect_equal(
    as.data.frame(fl[order(fl$time), ]),
    as.data.frame(f10),
    ignore_attr = TRUE
  )
  expect_false(f0$fishing[61])
  expect_true(f10$fishing[61])
  expect_true(is.na(f10$fishing[62])) # no speed: unknown, as before
  expect_equal(
    sum(f10$fishing, na.rm = TRUE),
    sum(f0$fishing, na.rm = TRUE) + 1
  )
  # the matches test the same windowed speed
  m10 <- rb_link_record_pings(
    pings,
    w,
    speed_min = "s1",
    speed_max = "s2",
    window_s = 600
  )
  expect_equal(m10$fishing, f10$fishing[match(m10$time, f10$time)])
  expect_true(m10$fishing[m10$time == pings$time[61]])
})

test_that("event times: a start takes the next ping, an end the previous; the gap; data frame = lazy table", {
  pings <- tibble::tibble(
    vid = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L),
    time = h(c(-1, 0.25, 3.5, 6, 11, 11, 13, 0)) # vid 2 has a duplicate ping at 11:00 + 6
  )
  e <- rb_link_record_events(pings, records)
  el <- dplyr::collect(rb_link_record_events(
    as_lazy(pings),
    as_lazy(records)
  ))
  el <- el[order(el$vid, el$event_time, el$event), names(e)]
  expect_equal(as.data.frame(el), as.data.frame(e), ignore_attr = TRUE)
  # 11 event times present; record 6 has none
  expect_equal(nrow(e), 11)
  expect_false(6L %in% e$.sid)
  k <- function(sid, ev) e[e$.sid == sid & e$event == ev, ]
  # a tow: t2 (end of setting) takes the ping before, t3 (start of hauling) the ping after
  expect_equal(k(1, "t2")$time, h(-1))
  expect_equal(k(1, "t2")$dt_s, -3600)
  expect_equal(k(1, "t2")$gap_s, 4500)
  expect_equal(k(2, "t3")$time, h(6))
  expect_equal(k(2, "t3")$dt_s, 3600)
  # tow 1's t3 bounds the same ping, 2 h away against tow 2's 1 h: it loses it, and keeps its gap
  expect_true(is.na(k(1, "t3")$time))
  expect_true(is.na(k(1, "t3")$dt_s))
  expect_true(k(1, "t3")$ping_lost)
  expect_equal(k(1, "t3")$gap_s, 9000)
  # a haul: t3 the ping after, t4 the ping before; the pings in between are the haul. A haul of one ping is
  # one phase: t3 and t4 share it
  expect_equal(k(3, "t3")$time, h(11))
  expect_equal(k(3, "t4")$time, h(11))
  expect_equal(k(3, "t4")$gap_s, 7200)
  expect_false(k(3, "t4")$ping_lost)
  # the setting of the same record and record 4's set want that ping too, 11 and 10 h away: both lose it
  expect_true(k(3, "t1")$ping_lost)
  expect_true(k(4, "t1")$ping_lost)
  expect_true(is.na(k(3, "t1")$gap_s)) # no ping before the event
  expect_equal(k(5, "t1")$dt_s, 0) # a ping on the event
  expect_equal(k(5, "t1")$gap_s, 0)
  expect_true(k(5, "t4")$ping_lost) # the jig's t1 and t4 are two phases by default
  expect_equal(sum(e$ping_lost), 4)
  # one record and phase per ping
  kept <- e[!is.na(e$time), ]
  ph <- ifelse(kept$event %in% c("t1", "t2"), 1, 2)
  expect_equal(
    nrow(unique(cbind(kept[c("vid", "time")], ph, kept[".sid"]))),
    nrow(unique(kept[c("vid", "time")]))
  )
  expect_true(all(e$dt_s[e$event %in% c("t1", "t3")] >= 0, na.rm = TRUE))
  expect_true(all(e$dt_s[e$event %in% c("t2", "t4")] <= 0, na.rm = TRUE))
  # farther than max_dt_s: no ping, the gap stays
  m <- rb_link_record_events(pings, records, max_dt_s = 3600)
  expect_equal(sum(is.na(m$time)), 5)
  expect_equal(m$dt_s[m$.sid == 2 & m$event == "t3"], 3600) # at the limit: kept
  expect_true(is.na(m$time[m$.sid == 4 & m$event == "t1"]))
  # an end with no ping before it has no ping
  x <- rb_link_record_events(
    pings,
    records,
    starts = character(0),
    ends = c("t1", "t2", "t3", "t4")
  )
  expect_true(is.na(x$time[x$.sid == 3 & x$event == "t1"]))
  expect_error(
    rb_link_record_events(pings, records[, -1]),
    "records lacks: .sid"
  )
  expect_error(
    rb_link_record_events(pings, records, starts = "t1", ends = "t1"),
    "both a start and an end"
  )
})

test_that("durations above the gear's limit become NA, the raw value is kept", {
  d <- data.frame(gid = c(6, 6, 7, 9), duration_m = c(100, 900, NA, 5000))
  lim <- data.frame(gid = c(6, 7), dur_max_min = c(600, 60))
  r <- rb_limit_record_duration(d, lim)
  expect_equal(r$duration_m, c(100, NA, NA, 5000))
  expect_equal(r$duration_m_raw, d$duration_m)
  expect_equal(r$duration_source, c("data", "implausible", "missing", "data"))
  rl <- dplyr::collect(rb_limit_record_duration(as_lazy(d), lim))
  expect_equal(
    as.data.frame(rl[order(rl$gid, rl$duration_m_raw), ]),
    as.data.frame(r[order(r$gid, r$duration_m_raw), ]),
    ignore_attr = TRUE
  )
})

test_that("fishing by speed and as a cascade: the window where a record holds the ping, the track elsewhere", {
  w <- rb_create_record_windows(records[1, ], min_hauls = 1) # one tow, 06:00-10:00 (t2 0, t3 4)
  pings <- tibble::tibble(
    vid = 1L,
    time = h(c(1, 2, 12, 13, 14, 15)),
    speed = c(3, 9, 3, 9, 3, NA),
    lo = c(2, 2, 2, 2, NA, 2),
    hi = c(5, 5, 5, 5, NA, 5)
  )
  m <- rb_link_record_pings(pings, w, speed_min = "s1", speed_max = "s2")
  win <- rb_add_fishing(pings, m, speed_min = "s1", speed_max = "s2")
  expect_equal(win$fishing, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)) # unchanged: no window, not fishing
  expect_false("fishing_basis" %in% names(win))
  spd <- rb_add_fishing(
    pings,
    method = "speed",
    ping_speed_min = "lo",
    ping_speed_max = "hi"
  )
  expect_equal(spd$fishing, c(TRUE, FALSE, TRUE, FALSE, NA, NA))
  expect_equal(
    spd$fishing_basis,
    c("speed", "speed", "speed", "speed", "none", "speed")
  )
  cas <- rb_add_fishing(
    pings,
    m,
    method = c("window", "speed"),
    speed_min = "s1",
    speed_max = "s2",
    ping_speed_min = "lo",
    ping_speed_max = "hi",
    name = "fishing_flow"
  )
  expect_equal(cas$fishing_flow, c(TRUE, FALSE, TRUE, FALSE, NA, NA))
  expect_equal(
    cas$fishing_flow_basis,
    c("window", "window", "speed", "speed", "none", "speed")
  )
  # side by side: a second approach is a column of its own; the same name twice stops
  both <- rb_add_fishing(
    win,
    m,
    method = c("window", "speed"),
    speed_min = "s1",
    speed_max = "s2",
    ping_speed_min = "lo",
    ping_speed_max = "hi",
    name = "fishing_flow"
  )
  expect_equal(both$fishing, win$fishing)
  expect_error(
    rb_add_fishing(win, m, speed_min = "s1", speed_max = "s2"),
    "already has"
  )
  expect_error(
    rb_add_fishing(pings, method = "window"),
    "rb_link_record_pings"
  )
  lz <- dplyr::collect(rb_add_fishing(
    as_lazy(pings),
    as_lazy(m),
    method = c("window", "speed"),
    speed_min = "s1",
    speed_max = "s2",
    ping_speed_min = "lo",
    ping_speed_max = "hi",
    name = "fishing_flow"
  ))
  expect_equal(
    as.data.frame(lz[order(lz$time), ]),
    as.data.frame(cas),
    ignore_attr = TRUE
  )
})

test_that("positions for records with no pings: along the record's line, every interval, one for no length", {
  r <- tibble::tibble(
    id = 1:3,
    vid = c(7L, 7L, 8L),
    start = h(c(0, 2, 0)),
    end = h(c(1, 2, 3)),
    lon1 = c(-22, -21, -20),
    lat1 = c(64, 64, 65),
    lon2 = c(-22.2, NA, -20),
    lat2 = c(64.05, NA, 65.3)
  )
  p <- rb_create_record_pings(r, key = "id", interval_s = 900)
  expect_equal(as.vector(table(p$id)), c(4, 1, 12)) # an hour in 15 min, no length, three hours
  one <- p[p$id == 1, ]
  expect_equal(one$lon, -22 - 0.2 * c(0, 0.25, 0.5, 0.75))
  expect_true(all(p$pos_basis == "synthesised"))
  # the speed is the straight line over the window, on rb_calc_distance's sphere
  d <- rb_calc_distance(
    lon1 = -20,
    lat1 = 65,
    lon2 = -20,
    lat2 = 65.3,
    method = "haversine"
  )
  expect_equal(
    unique(p$speed[p$id == 3]),
    d / (3 * 3600) * 3600 / 1852,
    tolerance = 1e-9
  )
  expect_true(is.na(p$speed[p$id == 2]))
  l <- dplyr::collect(rb_create_record_pings(
    as_lazy(r),
    key = "id",
    interval_s = 900
  ))
  expect_equal(
    as.data.frame(l[order(l$vid, l$time), ]),
    as.data.frame(p),
    ignore_attr = TRUE
  )
  # they run through effort like any pings
  e <- rb_add_effort(p, by = "id")
  expect_equal(sum(e$hours, na.rm = TRUE), 0.75 + 2.75)
})

test_that("sets of static-gear hauls: a pass along the haul's line in the hours before it", {
  line <- function(t0, n, lat0 = 64, dlat = 0.05) {
    tibble::tibble(
      vid = 1L,
      time = t0 + (0:(n - 1)) * 120,
      lon = -22 + 0.0005 * (0:(n - 1)) %% 2, # a few tens of metres off the line, back and forth
      lat = lat0 + dlat * (0:(n - 1)) / (n - 1)
    )
  }
  d1 <- as.POSIXct("2024-03-01 06:00", tz = "UTC")
  d2 <- as.POSIXct("2024-03-02 12:00", tz = "UTC")
  steam <- tibble::tibble(
    vid = 1L,
    time = d1 + 3600 * (1:20),
    lon = -21,
    lat = 64.5
  )
  set <- line(d1, 16)
  pings <- dplyr::bind_rows(set, steam, line(d2, 31)) # set 06:00-06:30, haul 12:00-13:00
  hauls <- tibble::tibble(id = 1L, vid = 1L, start = d2, end = d2 + 3600)
  s <- rb_link_record_sets(hauls, pings, key = "id")
  expect_equal(s$set_start, d1)
  expect_equal(s$set_end, d1 + 15 * 120)
  # iapesca's soak: from the set's start to the haul's end
  expect_equal(
    s$soak_h,
    as.numeric(difftime(d2 + 3600, d1, units = "hours"))
  )
  # the mean of the set's pings' speeds, each from the ping before it (0 for the track's first)
  step <- rb_calc_distance(
    lon1 = set$lon[-16],
    lat1 = set$lat[-16],
    lon2 = set$lon[-1],
    lat2 = set$lat[-1],
    method = "haversine"
  )
  expect_equal(s$set_speed, mean(c(0, step * 0.539957 / 1000 / (120 / 3600))))
  expect_equal(s$n_set_pings, 16)
  expect_equal(s$set_basis, "reconstructed")
  expect_equal(s$set_method, "line")
  expect_equal(s$buffer_m, 330)
  expect_false(s$set_consolidated)
  # a haul of one ping has no line: none
  lone <- tibble::tibble(vid = 1L, time = d2 + 5 * 3600, lon = -19, lat = 63)
  far <- tibble::tibble(
    id = 2L,
    vid = 1L,
    start = d2 + 5 * 3600 - 1,
    end = d2 + 5 * 3600 + 1
  )
  n <- rb_link_record_sets(far, dplyr::bind_rows(pings, lone), key = "id")
  expect_equal(n$set_basis, "none")
  expect_true(is.na(n$set_method))
  l <- dplyr::collect(rb_link_record_sets(
    as_lazy(hauls),
    as_lazy(pings),
    key = "id"
  ))
  expect_equal(as.data.frame(l), as.data.frame(s), ignore_attr = TRUE)
})

test_that("sets: the haul's line from its pings, a line through them, or its declared positions", {
  t0 <- as.POSIXct("2024-03-01 06:00", tz = "UTC")
  along <- function(t, lat) {
    tibble::tibble(vid = 1L, time = t, lon = -22, lat = lat)
  }
  # a set over the middle of the line, then the haul with a ping at each end only (5.6 km apart), and a haul
  # with no pings at all: only its declared positions say where the gear lies
  set <- along(t0 + (0:9) * 60, seq(64.02, 64.03, length.out = 10))
  haul <- along(t0 + 86400 + c(0, 3600), c(64, 64.05))
  pings <- dplyr::bind_rows(set, haul)
  hauls <- tibble::tibble(
    id = 1:2,
    vid = 1L,
    start = t0 + 86400 + c(0, 2 * 86400),
    end = t0 + 86400 + c(3601, 2 * 86400 + 3600),
    lon1 = -22,
    lat1 = 64,
    lon2 = -22,
    lat2 = c(64.05, NA)
  )
  by_pings <- rb_link_record_sets(hauls, pings, key = "id", method = "pings")
  expect_equal(by_pings$set_basis, c("none", "none")) # the set is 2 km from either haul ping
  expect_equal(by_pings$set_method, c("pings", NA))
  by_line <- rb_link_record_sets(hauls, pings, key = "id", method = "line")
  expect_equal(by_line$set_start, c(t0, NA))
  expect_equal(by_line$set_method, c("line", NA))
  # the second haul's record has a start only: a point at 64.00, 2.2 km from the set, so no set
  both <- rb_link_record_sets(
    hauls,
    pings,
    key = "id",
    method = c("line", "record")
  )
  expect_equal(both$set_method, c("line", "record"))
  expect_equal(both$set_basis, c("reconstructed", "none"))
  hauls$lat2[2] <- 64.05
  rec <- rb_link_record_sets(hauls, pings, key = "id", method = "record")
  expect_equal(rec$set_start, c(t0, t0))
  expect_equal(rec$set_end, c(t0 + 9 * 60, t0 + 9 * 60))
  expect_equal(rec$set_method, c("record", "record"))
  l <- dplyr::collect(rb_link_record_sets(
    as_lazy(hauls),
    as_lazy(pings),
    key = "id",
    method = c("pings", "record")
  ))
  expect_equal(
    as.data.frame(l[order(l$id), ]),
    as.data.frame(rb_link_record_sets(
      hauls,
      pings,
      key = "id",
      method = c("pings", "record")
    )),
    ignore_attr = TRUE
  )
  expect_error(
    rb_link_record_sets(hauls, pings, key = "id", method = "lines"),
    "method"
  )
  expect_error(
    rb_link_record_sets(hauls[, 1:4], pings, key = "id", method = "record"),
    "lon1"
  )
})

test_that("sets: the hour before the haul is left out; a pass scores its lateness, length and setting labels", {
  t0 <- as.POSIXct("2024-03-01 06:00", tz = "UTC")
  t1 <- t0 + 86400
  along <- function(t, lat) {
    tibble::tibble(vid = 1L, time = t, lon = -22, lat = lat)
  }
  pings <- dplyr::bind_rows(
    along(t0 + (0:15) * 120, seq(64, 64.05, length.out = 16)), # the set: 16 pings along the line
    tibble::tibble(vid = 1L, time = t0 + 3600 * (1:20), lon = -21, lat = 64.5), # elsewhere
    along(t1 - (3:1) * 60, c(63.998, 63.999, 64)), # the run in to the buoy, straight into the haul
    along(t1 + (0:30) * 120, seq(64, 64.05, length.out = 31)) # the haul
  )
  hauls <- tibble::tibble(id = 1L, vid = 1L, start = t1, end = t1 + 3601)
  # the run in ends a minute before the haul, inside the hour left out
  s <- rb_link_record_sets(hauls, pings, key = "id")
  expect_equal(s$set_start, t0)
  expect_equal(s$n_set_pings, 16)
  # with that hour let in, the run in is the later pass (1 against 0.5), but the set is 25 times as long
  # (1 against 0.04): the set wins
  s0 <- rb_link_record_sets(hauls, pings, key = "id", soak_h = c(0, 240))
  expect_equal(s0$set_start, t0)
  # the run in labelled as setting gets 1 more and the set 0: the run in wins
  pl <- dplyr::mutate(pings, set = time >= t1 - 180 & time < t1)
  s1 <- rb_link_record_sets(
    hauls,
    pl,
    key = "id",
    soak_h = c(0, 240),
    setting = "set"
  )
  expect_equal(s1$set_start, t1 - 180)
  expect_equal(s1$n_set_pings, 3)
  # every candidate labelled: the labels tell nothing, and every pass gets 1
  s2 <- rb_link_record_sets(
    hauls,
    dplyr::mutate(pings, set = TRUE),
    key = "id",
    soak_h = c(0, 240),
    setting = "set"
  )
  expect_equal(s2$set_start, t0)
  l <- dplyr::collect(rb_link_record_sets(
    as_lazy(hauls),
    as_lazy(pl),
    key = "id",
    soak_h = c(0, 240),
    setting = "set"
  ))
  expect_equal(l$set_start, t1 - 180)
})

test_that("sets: a vessel's hauls share one buffer; a set is sought in the haul's trip and those just before it", {
  t0 <- as.POSIXct("2024-03-01 06:00", tz = "UTC")
  along <- function(t, lat, lon = -22) {
    tibble::tibble(vid = 1L, time = t, lon = lon, lat = lat)
  }
  pings <- dplyr::bind_rows(
    along(t0 + (0:9) * 60, seq(64.01, 64.04, length.out = 10), lon = -22.0103), # a pass ~500 m from haul A's line
    along(t0 + 2 * 86400 + (0:9) * 60, seq(64, 64.05, length.out = 10)), # haul A, two days later
    along(t0 + 2 * 86400 + 3 * 3600, 64.025, lon = -21.9931), # ~336 m from A's line, after A
    along(t0 + 3 * 86400 + (0:9) * 60, seq(65, 65.05, length.out = 10)) # haul B, nothing near it
  )
  hauls <- tibble::tibble(
    id = 1:2,
    vid = 1L,
    start = t0 + c(2, 3) * 86400,
    end = t0 + c(2, 3) * 86400 + 601
  )
  # A alone: the buffer stops at 340 m, where the ping after A lies, and the pass at 500 m is not a candidate
  a <- rb_link_record_sets(hauls[1, ], pings, key = "id")
  expect_equal(a$buffer_m, 340)
  expect_equal(a$set_basis, "none")
  # with B, which has no ping within 550 m, the vessel's buffer is 550 m and A finds the pass
  ab <- rb_link_record_sets(hauls, pings, key = "id")
  expect_equal(ab$buffer_m, c(550, 550))
  expect_equal(ab$set_start, c(t0, NA))
  expect_equal(ab$set_basis, c("reconstructed", "none"))
  # trips: the pass in trip 1, A and the ping after it in trip 2, B in trip 3
  pings$trip <- rep(c(1, 2, 3), c(10, 11, 10))
  tr <- rb_link_record_sets(hauls, pings, key = "id", trip = "trip")
  expect_equal(tr$set_start, c(t0, NA))
  expect_equal(tr$set_trip, c(1, NA))
  # a window to 47.9 h holds the pass's last three pings, but trip 1 started 48 h before trip 2: no set
  w <- rb_link_record_sets(hauls, pings, key = "id", soak_h = c(1, 47.9))
  expect_equal(w$set_start, c(t0 + 420, NA))
  wt <- rb_link_record_sets(
    hauls,
    pings,
    key = "id",
    soak_h = c(1, 47.9),
    trip = "trip"
  )
  expect_equal(wt$set_basis, c("none", "none"))
  # a pass over two trips ends with its first trip, and its speed is taken there
  pings$trip <- rep(c(1, 2, 3), c(5, 16, 10))
  sp <- rb_link_record_sets(hauls, pings, key = "id", trip = "trip")
  expect_equal(sp$set_start[1], t0)
  expect_equal(sp$set_end[1], t0 + 240)
  expect_equal(sp$n_set_pings[1], 10)
  expect_equal(sp$set_trip[1], 1)
  l <- dplyr::collect(rb_link_record_sets(
    as_lazy(hauls),
    as_lazy(pings),
    key = "id",
    trip = "trip"
  ))
  expect_equal(
    as.data.frame(l[order(l$id), ]),
    as.data.frame(sp),
    ignore_attr = TRUE
  )
})

# rb_link_record_sets() against iapesca 1.3's own Retrieve_SettingOperations(), as data-raw/iapesca_sets.R ran it on
# two labelled pot tracks (the Italian one always; iapesca's example vessel where iapesca is installed). A haul is a
# run of pings labelled hauling, the same as there.
labelled <- function(p) {
  p <- p[order(p$vid, p$time, p$lon, p$lat), ]
  p <- p[!duplicated(p[c("vid", "time")]), ]
  p$op[is.na(p$op)] <- "other"
  r <- cumsum(c(TRUE, p$op[-1] != p$op[-nrow(p)] | p$vid[-1] != p$vid[-nrow(p)]))
  i <- which(p$op == "haul")
  h <- do.call(rbind, lapply(split(i, r[i]), function(j) {
    data.frame(
      vid = p$vid[j[1]],
      trip = p$trip[j[1]],
      start = min(p$time[j]),
      end = max(p$time[j]) + 1,
      n = length(j)
    )
  }))
  h <- h[order(h$vid, h$start), ]
  h$id <- seq_len(nrow(h))
  rownames(h) <- NULL
  list(pings = p, hauls = h)
}

test_that("sets: iapesca 1.3's sets on labelled pot tracks, with and without labels and the second search", {
  ref <- utils::read.csv(
    system.file("testdata", "iapesca_sets.csv", package = "ramb"),
    comment.char = "#"
  )
  tracks <- list(
    it_pots = labelled(as.data.frame(dplyr::collect(rb_read_example(
      "wkssfgeo2_it_pots"
    )))[c("vid", "trip", "time", "lon", "lat", "op")])
  )
  if (requireNamespace("iapesca", quietly = TRUE)) {
    ip <- iapesca::positions
    tracks$iapesca <- labelled(data.frame(
      vid = ip$VESSEL_FK,
      trip = ip$FISHING_TRIP_FK,
      time = as.POSIXct(ip$DATE_TIME, tz = "UTC"),
      lon = ip$LONGITUDE,
      lat = ip$LATITUDE,
      op = c(Setting = "set", Hauling = "haul", NotFishing = NA)[as.character(
        ip$FishingOperation
      )]
    ))
  }
  variants <- list(
    labels = list(setting = "is_set"),
    no_labels = list(),
    first_search = list(setting = "is_set", consolidate = FALSE)
  )
  fmt <- function(x) format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  for (ds in names(tracks)) {
    p <- tracks[[ds]]$pings
    p$is_set <- p$op == "set"
    for (v in names(variants)) {
      s <- do.call(
        rb_link_record_sets,
        c(
          list(
            tracks[[ds]]$hauls[c("id", "vid", "start", "end")],
            p,
            key = "id",
            trip = "trip"
          ),
          variants[[v]]
        )
      )
      s <- s[!is.na(s$set_method), ]
      r <- ref[ref$ds == ds & ref$variant == v, ]
      expect_equal(s$id, r$id)
      expect_equal(fmt(s$set_start), r$set_start)
      expect_equal(fmt(s$set_end), r$set_end)
      expect_equal(s$soak_h, r$soak_h, tolerance = 1e-9)
      # iapesca's distances on the plane of the UTM zone, ramb's on its sphere
      expect_equal(s$set_speed, r$set_speed, tolerance = 0.01)
      if (ds == "it_pots" && v == "labels") {
        # the second search changes 4 of these sets
        expect_equal(sum(s$set_consolidated), 4)
      }
    }
  }
})

test_that("sets: the second search trims as iapesca's CleanSkewness() does", {
  skip_if_not_installed("iapesca")
  skip_if_not_installed("e1071")
  withr::with_seed(1, {
    cases <- list(
      rexp(40),
      rexp(200, 0.1),
      -rexp(60),
      rnorm(50),
      c(rexp(35), NA, NA),
      rexp(10),
      rlnorm(100, 0, 2),
      c(rexp(31), 1000),
      runif(30),
      numeric(0),
      rgamma(500, 0.3)
    )
  })
  for (v in cases) {
    expect_identical(.rb_clean_skewness(v), iapesca:::CleanSkewness(v))
  }
  # where every value left is the same, iapesca stops with an error; here the trimming stops
  expect_identical(.rb_clean_skewness(c(rep(1, 40), 50)), c(rep(1, 40), NA))
})

test_that("sets: the connection comes from whichever input is lazy; two connections stop", {
  t0 <- as.POSIXct("2024-03-01 06:00", tz = "UTC")
  along <- function(t, lat) {
    tibble::tibble(vid = 1L, time = t, lon = -22, lat = lat)
  }
  pings <- dplyr::bind_rows(
    along(t0 + (0:9) * 60, seq(64, 64.01, length.out = 10)),
    along(t0 + 86400 + (0:9) * 60, seq(64, 64.01, length.out = 10))
  )
  hauls <- tibble::tibble(
    id = 1L,
    vid = 1L,
    start = t0 + 86400,
    end = t0 + 86400 + 601
  )
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "pings", pings)
  lp <- dplyr::tbl(con, "pings")
  s <- rb_link_record_sets(hauls, lp, key = "id")
  expect_equal(s$set_start, t0)
  expect_error(
    rb_link_record_sets(as_lazy(hauls), lp, key = "id"),
    "different DuckDB connections"
  )
})
