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

# The "runs" and "jepol" methods, against the functions they replace (snapshot taken before the recode).
old <- readRDS(test_path("fixtures", "pre_jepol.rds"))
tagged <- dansk |>
  dplyr::transmute(rid = dplyr::row_number(), vid = vessel_id, time = time_stamp, lon, lat,
                   harbour_id = ifelse(old$in_harbour == 1, "h", NA_character_))

# Each old trip id must map to one new (vid, voyage_id) and back, and the same pings must be outside every trip.
same_partition <- function(a, b) {
  expect_equal(is.na(a), is.na(b))
  pairs <- unique(data.frame(a, b)[!is.na(a), ])
  expect_false(any(duplicated(pairs$a)) || any(duplicated(pairs$b)))
}

test_that("method jepol reproduces rb_trip_jepol(), with and without splits", {
  for (md in c(72, 48, 24)) {
    v <- suppressWarnings(rb_cut_trip_voyages(tagged, method = "jepol", by_year = FALSE, max_dur_h = md))
    a <- rb_assign_trip(tagged, v, by_year = FALSE)
    a <- a[order(a$rid), ]
    same_partition(old[[paste0("jepol_", md)]], ifelse(is.na(a$voyage_id), NA, paste(a$vid, a$voyage_id)))
  }
  v <- rb_cut_trip_voyages(tagged, method = "jepol", by_year = FALSE, max_dur_h = 12, split = FALSE)
  a <- rb_assign_trip(tagged, v, by_year = FALSE)
  a <- a[order(a$rid), ]
  same_partition(old$jepol_nosplit, ifelse(is.na(a$voyage_id), NA, paste(a$vid, a$voyage_id)))
  expect_true(all(v$harbour_from == "h" | is.na(v$harbour_from)))
})

test_that("method runs numbers the sea runs as rb_trip() did, vessel by vessel", {
  v <- rb_cut_trip_voyages(tagged, method = "runs", by_year = FALSE)
  a <- rb_assign_trip(tagged, v, by_year = FALSE)
  a <- a[order(a$vid, a$time), ]
  r <- unlist(lapply(split(!is.na(a$harbour_id), a$vid), function(x) suppressWarnings(rb_trip(x))))
  expect_equal(ifelse(r > 0, r, NA), a$voyage_id, ignore_attr = TRUE)
})

test_that("methods runs and jepol: a data frame and a lazy table give the same voyages", {
  skip_if_not_installed("duckdbfs")
  for (m in c("runs", "jepol")) {
    a <- rb_cut_trip_voyages(tagged, method = m)
    b <- dplyr::collect(rb_cut_trip_voyages(duckdbfs::as_dataset(tagged), method = m, batch_pings = 1e5))
    b <- b[order(b$vid, b$T1), ]
    expect_equal(as.data.frame(b), as.data.frame(a), ignore_attr = TRUE, info = m)
  }
})
