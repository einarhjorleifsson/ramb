# Step 5, rb_add_effort(): the interval each ping stands for, kW-hours and swept area, for every ping;
# data frame = lazy table; the same columns with no vessels and no width.

t0 <- as.POSIXct("2024-03-01 06:00:00", tz = "UTC")
pings <- tibble::tibble(
  vid = c(1L, 1L, 1L, 1L, 2L, 2L),
  voyage = c(1L, 1L, 2L, 2L, 1L, 1L),
  time = t0 + c(0, 600, 1200, 12000, 0, 2),
  speed = c(3, 3, 4, 10, 2, 2),
  fishing = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
  width = c(60, 60, 60, 60, NA, NA)
)
vessels <- tibble::tibble(vid = 1:2, kw = c(1000, 500))
cols <- c(
  "hours",
  "kw_hours",
  "pressure",
  "pressure_unit",
  "interval_flag",
  "effort_basis"
)

test_that("hours: the interval to the next ping, capped; the last ping has none", {
  e <- rb_add_effort(pings, vessels)
  expect_equal(e$hours, c(1 / 6, 1 / 6, 2, NA, 2 / 3600, NA))
  expect_equal(e$interval_flag, c(NA, NA, "capped", "edge", NA, "edge"))
  expect_equal(
    e$effort_basis,
    c("derived", "derived", "derived", "none", "derived", "none")
  )
  expect_equal(e$kw_hours, e$hours * c(1000, 1000, 1000, 1000, 500, 500))
  expect_equal(names(e), c(names(pings), cols))
  # the effort of the pings called fishing is a filter and a sum
  expect_equal(
    sum(e$hours[e$fishing], na.rm = TRUE),
    1 / 6 + 1 / 6 + 2 + 2 / 3600
  )
})

test_that("by: an interval never crosses a voyage; previous; drop; the ICES rule", {
  e <- rb_add_effort(pings, vessels, by = "voyage")
  expect_equal(e$hours, c(1 / 6, NA, 2, NA, 2 / 3600, NA))
  p <- rb_add_effort(pings, vessels, interval = "previous")
  expect_equal(p$hours, c(NA, 1 / 6, 1 / 6, 2, NA, 2 / 3600))
  d <- rb_add_effort(pings, vessels, long = "drop", min_interval_s = 5)
  expect_equal(d$hours, c(1 / 6, 1 / 6, NA, NA, NA, NA))
  expect_equal(
    d$interval_flag,
    c(NA, NA, "dropped_long", "edge", "dropped_short", "edge")
  )
})

test_that("pressure: swept area where there is a width; kW from the pings; no vessels, no width: same columns", {
  e <- rb_add_effort(pings, vessels, width = "width")
  # 3 knots for 10 minutes with 60 m: 3 * 1.852 / 6 km * 0.06 km
  expect_equal(e$pressure[1], 3 * 1.852 / 6 * 0.06)
  expect_equal(e$pressure_unit, c("km2", "km2", "km2", NA, NA, NA))
  k <- rb_add_effort(dplyr::left_join(pings, vessels, by = "vid"))
  expect_equal(k$kw_hours, rb_add_effort(pings, vessels)$kw_hours)
  n <- rb_add_effort(pings)
  expect_equal(names(n), c(names(pings), cols))
  expect_true(all(is.na(n$kw_hours)) && all(is.na(n$pressure)))
  expect_equal(n$hours, e$hours)
})

test_that("data frame = lazy table", {
  skip_if_not_installed("duckdbfs")
  e <- rb_add_effort(pings, vessels, by = "voyage", width = "width")
  l <- rb_add_effort(
    duckdbfs::as_dataset(pings),
    duckdbfs::as_dataset(vessels),
    by = "voyage",
    width = "width"
  )
  expect_s3_class(l, "tbl_lazy")
  l <- dplyr::collect(l)
  l <- l[order(l$vid, l$time), ]
  expect_equal(as.data.frame(l), as.data.frame(e), ignore_attr = TRUE)
})

test_that("misuse: a missing column, a second run, kW in both tables", {
  expect_error(rb_add_effort(pings[, c("vid", "speed")]), "`time`")
  expect_error(rb_add_effort(rb_add_effort(pings)), "already")
  expect_error(
    rb_add_effort(dplyr::left_join(pings, vessels, by = "vid"), vessels),
    "both have"
  )
  expect_error(rb_add_effort(pings, width = "gear_width"), "gear_width")
  q <- dplyr::rename(pings, boat = vid, t = time)
  expect_equal(
    rb_add_effort(q, vid = "boat", time = "t")$hours,
    rb_add_effort(pings)$hours
  )
})

test_that("lazy results compose: two of them in a union", {
  skip_if_not_installed("duckdbfs")
  a <- rb_add_effort(
    duckdbfs::as_dataset(pings),
    duckdbfs::as_dataset(vessels)
  )
  b <- rb_add_effort(
    duckdbfs::as_dataset(pings),
    duckdbfs::as_dataset(vessels),
    interval = "previous"
  )
  u <- dplyr::collect(dplyr::union_all(a, b))
  expect_equal(nrow(u), 2 * nrow(pings))
})
