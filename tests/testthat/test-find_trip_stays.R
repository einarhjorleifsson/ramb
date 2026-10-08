# rb_find_trip_stays(): port of fishycode's curate/harbour_stay.R (plan 013 phase 2). Its exactness was
# checked on the Icelandic data (1,551,604 stays, identical); here: data frame = lazy table, and the window.

pings <- dansk |>
  dplyr::filter(vessel_id %in% unique(dansk$vessel_id)[1:2]) |>
  dplyr::transmute(vid = as.integer(factor(vessel_id)), time = time_stamp, lon, lat) |>
  dplyr::slice_head(n = 20000, by = vid)
harbours <- sf::st_transform(dansk_harbours, 4326) |> dplyr::mutate(harbour_id = as.character(SI_HARB))

test_that("a data frame and a lazy table give the same stays", {
  skip_if_not_installed("duckdbfs")
  a <- rb_find_trip_stays(pings, harbours, min_stay_h = 2)
  b <- dplyr::collect(rb_find_trip_stays(duckdbfs::as_dataset(pings), harbours, min_stay_h = 2))
  b <- b[order(b$vid, b$T_in), ]
  expect_gt(nrow(a), 0)
  expect_equal(as.data.frame(b), as.data.frame(a), ignore_attr = TRUE)
  expect_setequal(names(a), c("vid", "harbour_id", "T_in", "T_out", "dur_h", "rule", "evidence",
                              "T_in_ping", "T_out_ping", "T_in_ev", "T_out_ev", "basis"))
  expect_true(all(a$dur_h >= 2 | a$rule == "event_pair" | grepl("tag_span|gap", a$evidence)))
})

test_that("the window keeps only stays that begin in it; a longer minimum gives fewer stays", {
  a <- rb_find_trip_stays(pings, harbours, min_stay_h = 2)
  w <- range(a$T_in)
  w <- c(w[1], w[1] + (w[2] - w[1]) / 2)
  b <- rb_find_trip_stays(pings, harbours, min_stay_h = 2, t_in_window = w)
  expect_true(all(b$T_in >= w[1] & b$T_in < w[2]))
  expect_lt(nrow(b), nrow(a))
  ms <- data.frame(harbour_id = unique(a$harbour_id), min_stay_h = 24)
  expect_lte(nrow(rb_find_trip_stays(pings, harbours, min_stay_h = 2, min_stay = ms)), nrow(a))
})
