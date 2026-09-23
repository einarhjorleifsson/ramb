library(duckdb)  # attaches DBI S4 methods duckdbfs::as_dataset() needs

# Reference: a direct, unoptimised transcription of the original
# fishycode/curate/logbook_sensor_gps.R group_modify() logic - kept here only
# to verify rb_interpolate_track() against it, not for production use.
ref_interpolate_track <- function(x, track, by = c("vid", "trip"),
                                   time_var = "time", value_vars = c("lon", "lat")) {
  x |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, time_var)))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(function(g, key) {
      a <- track
      for (b in by) a <- a[a[[b]] == key[[b]][1], ]
      if (nrow(a) < 2) {
        for (v in value_vars) g[[v]] <- NA_real_
        g$dt_sec <- NA_real_
        return(g)
      }
      tn <- as.numeric(g[[time_var]]); an <- as.numeric(a[[time_var]])
      for (v in value_vars)
        g[[v]] <- stats::approx(an, a[[v]], tn, rule = 1, ties = mean)$y
      before <- stats::approx(an, an, tn, method = "constant", f = 0, rule = 1)$y
      after  <- stats::approx(an, an, tn, method = "constant", f = 1, rule = 1)$y
      g$dt_sec <- after - before
      g
    }) |>
    dplyr::ungroup()
}

mk_case <- function(n_groups = 20, track_pts_per_group = 8, events_per_group = 5) {
  track <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    tt <- sort(as.numeric(as.POSIXct("2020-01-01", tz = "UTC")) +
                 cumsum(sample(30:300, track_pts_per_group, TRUE)))
    data.frame(vid = g %% 4, trip = g,
               time = as.POSIXct(tt, origin = "1970-01-01", tz = "UTC"),
               lon = -25 + cumsum(rnorm(track_pts_per_group, 0, 0.01)),
               lat =  62 + cumsum(rnorm(track_pts_per_group, 0, 0.01)))
  }))
  events <- do.call(rbind, lapply(seq_len(n_groups), function(g) {
    trk_t <- track$time[track$trip == g]
    span <- range(as.numeric(trk_t))
    tn <- c(
      runif(events_per_group, span[1], span[2]),
      trk_t[1], trk_t[length(trk_t)],   # exact matches at both ends
      span[1] - 500, span[2] + 500       # outside range - must stay NA
    )
    data.frame(vid = g %% 4, trip = g, time = as.POSIXct(tn, origin = "1970-01-01", tz = "UTC"))
  }))
  events$.rid <- seq_len(nrow(events))
  list(track = track, events = events)
}

test_that("data.frame path matches the reference group_modify() implementation", {
  set.seed(1)
  d <- mk_case()
  r_ref <- ref_interpolate_track(d$events, d$track) |> dplyr::arrange(.rid)
  r_new <- rb_interpolate_track(d$events, d$track) |> dplyr::arrange(.rid)
  expect_equal(r_new$lon, r_ref$lon, tolerance = 1e-9)
  expect_equal(r_new$lat, r_ref$lat, tolerance = 1e-9)
  expect_equal(r_new$dt_sec, r_ref$dt_sec, tolerance = 1e-9)
})

test_that("a group with fewer than 2 track points yields all-NA, not an error", {
  d <- mk_case(n_groups = 3, track_pts_per_group = 1, events_per_group = 3)
  r_ref <- ref_interpolate_track(d$events, d$track) |> dplyr::arrange(.rid)
  r_new <- rb_interpolate_track(d$events, d$track) |> dplyr::arrange(.rid)
  expect_true(all(is.na(r_new$lon)))
  expect_equal(r_new$lon, r_ref$lon)
})

test_that("an event group entirely absent from the track yields NA, not an error", {
  set.seed(2)
  d <- mk_case()
  ev <- data.frame(vid = 99, trip = 999, time = as.POSIXct("2020-06-01", tz = "UTC"))
  out <- rb_interpolate_track(ev, d$track)
  expect_true(is.na(out$lon))
  expect_true(is.na(out$dt_sec))
})

test_that("duplicate track timestamps within a group are averaged (ties = mean)", {
  trk <- data.frame(vid = 1, trip = 1,
                     time = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 0, 10, 20),
                     lon = c(1, 3, 5, 7), lat = c(10, 12, 14, 16))
  ev  <- data.frame(vid = 1, trip = 1, time = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 5, 20))
  # stats::approx() itself warns "collapsing to unique 'x' values" - it's
  # describing ties = mean, not a problem; expected from this reference path.
  r_ref <- suppressWarnings(ref_interpolate_track(ev, trk))
  r_new <- rb_interpolate_track(ev, trk)
  expect_equal(r_new$lon, r_ref$lon, tolerance = 1e-9)
  expect_equal(r_new$lat, r_ref$lat, tolerance = 1e-9)
})

test_that("empty x returns zero rows without error", {
  d <- mk_case()
  out <- rb_interpolate_track(d$events[0, ], d$track)
  expect_equal(nrow(out), 0)
})

test_that("the tbl_lazy (DuckDB) path matches the reference too", {
  set.seed(3)
  d <- mk_case()
  r_ref <- ref_interpolate_track(d$events, d$track) |> dplyr::arrange(vid, trip, time)

  ev_lazy  <- duckdbfs::as_dataset(d$events) |> dplyr::mutate(time = dplyr::sql("time::TIMESTAMP"))
  trk_lazy <- duckdbfs::as_dataset(d$track)  |> dplyr::mutate(time = dplyr::sql("time::TIMESTAMP"))
  r_sql <- rb_interpolate_track(ev_lazy, trk_lazy) |>
    dplyr::collect() |> dplyr::arrange(vid, trip, time)

  expect_equal(r_sql$lon, r_ref$lon, tolerance = 1e-6)
  expect_equal(r_sql$lat, r_ref$lat, tolerance = 1e-6)
  expect_equal(r_sql$dt_sec, r_ref$dt_sec, tolerance = 1e-6)
  expect_equal(nrow(r_sql), nrow(r_ref))
})
