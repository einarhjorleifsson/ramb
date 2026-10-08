# rb_cut_trip_voyages() and rb_assign_trip(): port of fishycode's curate/ais_trip.R (plan 013 phase 2). Exact on
# the Icelandic data (1,372,656 voyages; 535,613,643 pings, every year's fingerprint equal); here: data frame =
# lazy table, every ping kept, the minimum-pings rule.

pings <- dansk |>
  dplyr::filter(vessel_id %in% unique(dansk$vessel_id)[1:2]) |>
  dplyr::transmute(vid = as.integer(factor(vessel_id)), time = time_stamp, lon, lat) |>
  dplyr::slice_head(n = 20000, by = vid)
harbours <- sf::st_transform(dansk_harbours, 4326) |> dplyr::mutate(harbour_id = as.character(SI_HARB))
stays <- rb_find_trip_stays(pings, harbours, min_stay_h = 2)

test_that("voyages and trip labels agree between a data frame and a lazy table", {
  skip_if_not_installed("duckdbfs")
  v <- rb_cut_trip_voyages(pings, stays, min_pings = 10)
  vl <- dplyr::collect(rb_cut_trip_voyages(duckdbfs::as_dataset(pings), duckdbfs::as_dataset(stays), min_pings = 10))
  vl <- vl[order(vl$vid, vl$T1), ]
  expect_gt(nrow(v), 0)
  expect_equal(as.data.frame(vl), as.data.frame(v), ignore_attr = TRUE)
  expect_true(all(v$n_pings >= 10))
  a <- rb_assign_trip(pings, v, stays = stays)
  al <- dplyr::collect(rb_assign_trip(duckdbfs::as_dataset(pings), duckdbfs::as_dataset(v), stays = duckdbfs::as_dataset(stays)))
  expect_equal(nrow(a), nrow(pings))
  k <- function(d) d[order(d$vid, d$time, d$lon, d$lat), c("vid", "time", "stay_harbour_id", "voyage_id", "trip_basis")]
  expect_equal(as.data.frame(k(al)), as.data.frame(k(a)), ignore_attr = TRUE)
  expect_setequal(unique(a$trip_basis), c("reconstructed", "none"))
  # a ping in a stay is never on a voyage
  expect_true(all(is.na(a$voyage_id[!is.na(a$stay_harbour_id)])))
})
