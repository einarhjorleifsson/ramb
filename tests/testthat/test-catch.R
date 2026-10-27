#  tests/testthat/test-sum_across.R
test_that("rb_sum_across sums columns correctly and removes originals", {
  df <- data.frame(
    id = 1:3,
    catch_cod = c(1, 2, 3),
    catch_had = c(0, 1, 0),
    catch_her = c(2, 1, 2)
  )
  summed <- rb_sum_across(df, prefix = "catch_")
  expect_true("catch_" %in% names(summed))
  expect_false(any(grepl("^catch_", names(summed)[names(summed) != "catch_"])))
  expect_equal(summed$catch_, c(3, 4, 5))
})

test_that("rb_sum_across works with remove = FALSE", {
  df <- data.frame(
    id = 1:2,
    catch_cod = c(1, 2),
    catch_had = c(0, 1)
  )
  summed <- rb_sum_across(df, prefix = "catch_", remove = FALSE)
  expect_true("catch_" %in% names(summed))
  expect_true(all(c("catch_cod", "catch_had") %in% names(summed)))
  expect_equal(summed$catch_, c(1, 3))
})

test_that("rb_sum_across errors if no matching columns", {
  df <- data.frame(id = 1:2, foo = c(1, 2))
  expect_error(rb_sum_across(df, prefix = "bar_"), "No columns found")
})

test_that("rb_sum_across errors if non-numeric columns are present", {
  df <- data.frame(
    id = 1:2,
    catch_cod = c(1, 2),
    catch_had = c("a", "b")
  )
  expect_error(rb_sum_across(df, prefix = "catch_"), "must be numeric")
})

test_that("rb_sum_across errors if prefix is not a single string", {
  df <- data.frame(id = 1:2, catch_cod = c(1, 2))
  expect_error(
    rb_sum_across(df, prefix = NULL),
    "must be a single character string"
  )
  expect_error(
    rb_sum_across(df, prefix = c("catch_", "foo_")),
    "must be a single character string"
  )
})

# Step 6: catch units and rb_share_catch(method = "units") (fishycode plan 013, item 2) --------------------------

t0 <- as.POSIXct("2024-03-04", tz = "UTC")
h <- function(x) t0 + x * 3600

test_that("units: overlapping events of a level merge, a shared key joins windows, a landing's start is filled", {
  events <- tibble::tibble(
    id = c("haul", "d1", "d2", "d2", "L", "r1", "r2"),
    vid = 1L,
    start = c(h(6), h(0), h(0), h(30), NA, h(48), h(48)),
    end = c(h(9), h(24), h(24), h(32), h(48), h(72), h(72)),
    level = c(1L, 2L, 2L, 2L, 4L, 2L, 2L),
    resolution = c("haul", "day", "day", "day", "landing", "day", "day"),
    rect = c(NA, NA, NA, NA, NA, "54D9", "54E0")
  )
  u <- rb_add_catch_units(events, key = "id", match_by = "rect")
  uid <- function(i) unique(u$unit_id[u$id == i])
  expect_length(unique(u$unit_id), 5)
  expect_equal(uid("d1"), uid("d2")) # they overlap; d2's second window joins by key
  expect_equal(u$n_events[u$id == "d1"], 2)
  expect_false(uid("haul") == uid("d1")) # another level
  expect_false(uid("r1") == uid("r2")) # another rectangle
  expect_equal(u$start[u$id == "L"], h(48) - 31 * 86400) # no earlier landing: max_days back
  expect_true(u$start_filled[u$id == "L"])
  skip_if_not_installed("duckdbfs")
  l <- dplyr::collect(rb_add_catch_units(
    duckdbfs::as_dataset(events),
    key = "id",
    match_by = "rect"
  ))
  # the data frame comes back in the events' order, the lazy table in none: both by event
  l <- l[order(l$id, l$start), names(u)]
  u <- u[order(u$id, u$start), ]
  expect_equal(as.data.frame(l), as.data.frame(u), ignore_attr = TRUE)
})

pings <- tibble::tibble(
  vid = 1L,
  time = h(c(1, 2, 3, 7, 8, 12, 13, 30, 31, 60)),
  fishing = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  hours = c(1, 3, 1, 1, 1, 2, 2, 1, 1, 1)
)
events <- tibble::tibble(
  id = c("h1", "h2", "d1", "d1b", "t1"),
  vid = 1L,
  start = h(c(0, 26, 0, 0, 0)),
  end = h(c(4, 27, 24, 24, 50)),
  level = c(1L, 1L, 2L, 2L, 3L),
  resolution = c("haul", "haul", "day", "day", "trip")
)
catch <- tibble::tibble(
  id = c("h1", "h1", "h2", "d1", "d1b", "t1", "zz"),
  species = c("cod", "had", "cod", "cod", "cod", "cod", "cod"),
  kg = c(400, 40, 50, 60, 30, 10, 7)
)

test_that("sharing: by hours, coarser units on the pings left free, moved up, kg in = kg out", {
  u <- rb_add_catch_units(events, key = "id")
  s <- rb_share_catch(pings, u, catch, key = "id")
  expect_equal(sum(s$kg), sum(catch$kg))
  # the haul's cod goes to its two fishing pings by hours (1 and 3); the non-fishing ping gets nothing
  h1 <- s[s$unit_id %in% u$unit_id[u$id == "h1"] & s$species == "cod", ]
  expect_equal(h1$kg, c(100, 300))
  expect_false(h(3) %in% s$time)
  # the day's two records are one unit, shared over the day's fishing pings the haul did not take
  d <- s[s$unit_id %in% u$unit_id[u$id == "d1"], ]
  expect_equal(sort(unique(d$time)), h(c(7, 8, 12, 13)))
  expect_equal(sum(d$kg), 90)
  expect_equal(d$kg[d$time == h(12)], 90 * 2 / 6)
  # h2 has no fishing ping: its catch moves up to the trip, onto the trip's pings no finer unit took
  h2 <- s[s$unit_id %in% u$unit_id[u$id == "h2"], ]
  expect_equal(unique(h2$shared_resolution), "trip")
  expect_equal(sort(unique(h2$time)), h(c(30, 31)))
  # catch tied to no unit is kept, with no time and basis none
  expect_equal(s$kg[is.na(s$unit_id)], 7)
  expect_equal(s$catch_basis[is.na(s$unit_id)], "none")
  expect_equal(unique(s$approach), "fishing")
})

test_that("the last resorts: the vessel's month, then no ping at all", {
  ev <- tibble::tibble(
    id = c("a", "b"),
    vid = c(1L, 2L),
    start = h(c(100, 100)),
    end = h(c(110, 110)),
    level = 1L,
    resolution = "haul"
  )
  ca <- tibble::tibble(id = c("a", "b"), species = "cod", kg = c(20, 5))
  s <- rb_share_catch(
    pings,
    rb_add_catch_units(ev, key = "id"),
    ca,
    key = "id"
  )
  expect_equal(sum(s$kg), 25)
  a <- s[s$vid %in% 1L & !is.na(s$time), ]
  expect_equal(unique(a$shared_resolution), "vessel_month") # vessel 1's March fishing pings
  expect_equal(sum(a$kg), 20)
  b <- s[s$kg == 5, ]
  expect_true(is.na(b$time)) # vessel 2 has no fishing ping that month
  expect_equal(b$catch_basis, "none")
})

test_that("data frame = lazy table", {
  skip_if_not_installed("duckdbfs")
  u <- rb_add_catch_units(events, key = "id")
  s <- rb_share_catch(pings, u, catch, key = "id")
  l <- rb_share_catch(
    duckdbfs::as_dataset(pings),
    duckdbfs::as_dataset(u),
    duckdbfs::as_dataset(catch),
    key = "id"
  )
  expect_s3_class(l, "tbl_lazy")
  l <- dplyr::collect(l)
  o <- function(x) x[order(x$unit_id, x$time, x$species, x$kg), names(s)]
  expect_equal(as.data.frame(o(l)), as.data.frame(o(s)), ignore_attr = TRUE)
})

test_that("acceptance: landings only, per trip, per week, per month and since the previous landing", {
  # a vessel with no logbook: two voyages a week for four weeks, fishing at 2-4 knots on its way
  tt <- seq(
    as.POSIXct("2024-03-04", tz = "UTC"),
    by = "30 min",
    length.out = 28 * 48
  )
  p <- tibble::tibble(
    vid = 7L,
    time = tt,
    speed = ifelse(as.integer(format(tt, "%H")) %in% 8:15, 3, 10)
  )
  p$voyage <- cumsum(c(1, diff(as.integer(format(tt, "%u")) %in% c(1, 4)) > 0))
  # fishing from the track: the gear's speed range on every ping, no record needed
  p$speed_min <- 2
  p$speed_max <- 4
  p <- rb_add_fishing(p, method = "speed")
  p <- rb_add_effort(p, by = "voyage")
  voy <- dplyr::summarise(
    p,
    start = min(time),
    end = max(time),
    .by = c(vid, voyage)
  )
  check <- function(ev) {
    u <- rb_add_catch_units(ev, key = "id")
    ca <- tibble::tibble(id = unique(ev$id), species = "cod", kg = 100)
    s <- rb_share_catch(p, u, ca, key = "id")
    expect_equal(sum(s$kg), 100 * length(unique(ev$id)))
    expect_true(all(s$catch_basis == "derived"))
    # every kilogram lands on a fishing ping inside its own landing's period
    inside <- dplyr::left_join(
      s,
      dplyr::distinct(u, unit_id, start, end),
      by = "unit_id"
    )
    expect_true(all(inside$time >= inside$start & inside$time < inside$end))
    expect_true(all(p$fishing[match(s$time, p$time)]))
    s
  }
  # per trip: each landing linked to its voyage
  check(tibble::tibble(
    id = paste0("L", voy$voyage),
    vid = 7L,
    start = voy$start,
    end = voy$end + 1,
    level = 4L,
    resolution = "trip"
  ))
  # per week and per month
  wk <- as.POSIXct("2024-03-04", tz = "UTC") + (0:3) * 7 * 86400
  check(tibble::tibble(
    id = paste0("W", 1:4),
    vid = 7L,
    start = wk,
    end = wk + 7 * 86400,
    level = 4L,
    resolution = "week"
  ))
  check(tibble::tibble(
    id = "M3",
    vid = 7L,
    start = as.POSIXct("2024-03-01", tz = "UTC"),
    end = as.POSIXct("2024-04-01", tz = "UTC"),
    level = 4L,
    resolution = "month"
  ))
  # a date only: each landing reaches back to the previous one
  s <- check(tibble::tibble(
    id = paste0("D", 1:4),
    vid = 7L,
    start = as.POSIXct(NA, tz = "UTC"),
    end = wk + 7 * 86400,
    level = 4L,
    resolution = "landing"
  ))
  expect_equal(length(unique(s$unit_id)), 4)
})

test_that("misuse names the step to run first; method eflalo keeps the ICES sharing", {
  u <- rb_add_catch_units(events, key = "id")
  expect_error(
    rb_share_catch(pings[, c("vid", "time", "fishing")], u, catch, key = "id"),
    "rb_add_effort"
  )
  expect_error(
    rb_share_catch(pings, events, catch, key = "id"),
    "rb_add_catch_units"
  )
  ep <- tibble::tibble(
    .pid = 1:4L,
    VE_COU = "IS",
    VE_REF = "1",
    FT_REF = "T1",
    time = as.POSIXct("2024-06-01 06:00", tz = "UTC") + (0:3) * 3600,
    ir = "54D9",
    state = c(1L, 1L, 0L, 1L)
  )
  ee <- tibble::tibble(
    .eid = 1L,
    VE_COU = "IS",
    VE_REF = "1",
    FT_REF = "T1",
    date = as.Date("2024-06-01"),
    LE_RECT = "54D9",
    LE_KG = 90,
    LE_EURO = 9
  )
  o <- rb_share_catch(ep, ee, method = "eflalo")
  expect_equal(sum(o$LE_KG, na.rm = TRUE), 90)
})

test_that("units are the same every run: landings tied on their day, and a landing over two islands", {
  skip_if_not_installed("duckdbfs")
  d <- as.POSIXct("2024-05-01", tz = "UTC")
  ev <- tibble::tibble(
    id = c("L1", "L2", "L3", "L4", "L4"),
    vid = 1L,
    start = d + c(NA, NA, NA, 20, 23) * 86400,
    end = c(
      d + 86400,
      d + 86400,
      d + 5 * 86400,
      d + 21 * 86400,
      d + 24 * 86400
    ),
    level = 4L,
    resolution = c(
      "landing",
      "landing",
      "landing",
      "landing_trip",
      "landing_trip"
    )
  )
  runs <- lapply(1:5, function(i) {
    u <- dplyr::collect(rb_add_catch_units(
      duckdbfs::as_dataset(ev),
      key = "id"
    ))
    u[order(u$id, u$start), c("id", "start", "unit_id")]
  })
  for (r in runs[-1]) {
    expect_equal(as.data.frame(r), as.data.frame(runs[[1]]), ignore_attr = TRUE)
  }
  u <- runs[[1]]
  expect_true(all(tapply(u$unit_id, u$id, function(x) length(unique(x))) == 1)) # one unit per event
  # L1 and L2 land the same day: the same start, one unit; L3 starts where they end
  expect_equal(u$start[u$id == "L2"], u$start[u$id == "L1"])
  expect_equal(u$unit_id[u$id == "L2"], u$unit_id[u$id == "L1"])
  expect_equal(u$start[u$id == "L3"], d + 86400)
})
