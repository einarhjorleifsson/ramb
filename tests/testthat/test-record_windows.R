# Step 2 (record windows, record assignment) and the "window" fishing method: ports of fishycode's
# curate/station_window.R and R/trail_sql.R (fishycode plan 014 phase 2). Their exactness is checked on the
# Icelandic data; here: data frame = lazy table, and the rules on a small fixture.

t0 <- as.POSIXct("2024-03-01 06:00:00", tz = "UTC")
h <- function(x) t0 + x * 3600
records <- tibble::tibble(
  .sid = 1:6, schema = "a", vid = c(1L, 1L, 2L, 2L, 3L, 3L),
  gear = c("OTB", "OTB", "GNS", "GNS", "LHM", "OTB"),
  gear_class = c("towed", "towed", "static", "static", "jigged", "towed"),
  set_max_min = c(NA, NA, 60, 60, NA, NA),
  t1 = h(c(NA, NA, 0, 1, 0, NA)), t2 = h(c(0, 3, NA, NA, NA, NA)),
  t3 = h(c(4, 5, 10, NA, NA, NA)), t4 = h(c(NA, NA, 12, 14, 6, NA)),
  duration_m = c(240, 120, NA, NA, NA, NA), date = as.Date("2024-03-01"),
  s1 = c(2, 2, NA, NA, 0, 2), s2 = c(5, 5, NA, NA, 3, 5))

test_that("windows: one per phase, capped tows, the day fallback; data frame = lazy table", {
  skip_if_not_installed("duckdbfs")
  w <- rb_build_record_windows(records, min_hauls = 1)
  wl <- dplyr::collect(rb_build_record_windows(duckdbfs::as_dataset(records), min_hauls = 1))
  wl <- wl[order(wl$vid, wl$w_start), names(w)]
  expect_equal(as.data.frame(wl), as.data.frame(w), ignore_attr = TRUE)
  expect_equal(sort(w$phase), sort(c("tow", "tow", "set", "haul", "set", "haul", "jig", "day")))
  tow1 <- w[w$.sid == 1, ]
  expect_equal(tow1$w_end, h(3))                      # cut back at the next recorded tow
  expect_equal(tow1$win_basis, "capped")
  expect_equal(w$win_basis[w$.sid == 6], "synthesised")  # no usable time: the whole day
  expect_equal(w$events[w$.sid == 3 & w$phase == "haul"], "t1+t3+t4")
})

test_that("pings: tier-1 windows win over the day; the counts, the record and fishing", {
  skip_if_not_installed("duckdbfs")
  w <- rb_build_record_windows(records, min_hauls = 1)
  pings <- tibble::tibble(vid = c(1L, 1L, 1L, 3L, 3L, 4L), time = h(c(1, 3.5, 20, 2, 17, 1)),
                          speed = c(3, NA, 3, 1, 9, 3))
  m <- rb_link_record_pings(pings, w, carry = "gear", speed_min = "s1", speed_max = "s2")
  ml <- dplyr::collect(rb_link_record_pings(duckdbfs::as_dataset(pings), duckdbfs::as_dataset(w),
                                            carry = "gear", speed_min = "s1", speed_max = "s2"))
  expect_equal(as.data.frame(ml[order(ml$vid, ml$time), names(m)]), as.data.frame(m), ignore_attr = TRUE)
  r <- rb_assign_record(pings, m, carry = "gear") |>
    rb_assign_fishing(m, speed_min = "s1", speed_max = "s2")
  expect_equal(r$n_records, c(1, 1, 0, 1, 1, 0))      # vid 3 at 23:00: only the day window of .sid 6
  expect_equal(r$.sid, c(1L, 2L, NA, 5L, 6L, NA))
  expect_equal(r$fishing, c(TRUE, NA, FALSE, TRUE, FALSE, FALSE))
})

test_that("durations above the gear's limit become NA, the raw value is kept", {
  d <- data.frame(gid = c(6, 6, 7, 9), duration_m = c(100, 900, NA, 5000))
  lim <- data.frame(gid = c(6, 7), dur_max_min = c(600, 60))
  r <- rb_limit_record_duration(d, lim)
  expect_equal(r$duration_m, c(100, NA, NA, 5000))
  expect_equal(r$duration_m_raw, d$duration_m)
  expect_equal(r$duration_source, c("data", "implausible", "missing", "data"))
  skip_if_not_installed("duckdbfs")
  rl <- dplyr::collect(rb_limit_record_duration(duckdbfs::as_dataset(d), lim))
  expect_equal(as.data.frame(rl[order(rl$gid, rl$duration_m_raw), ]),
               as.data.frame(r[order(r$gid, r$duration_m_raw), ]), ignore_attr = TRUE)
})
