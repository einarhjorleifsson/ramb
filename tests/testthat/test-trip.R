# rb_find_trip_stays(): port of fishycode's curate/port_stay.R (plan 013 phase 2). Its exactness was
# checked on the Icelandic data (1,551,604 stays, identical); here: data frame = lazy table, and the window.

pings <- dansk |>
  dplyr::filter(vessel_id %in% unique(dansk$vessel_id)[1:2]) |>
  dplyr::transmute(
    vid = as.integer(factor(vessel_id)),
    time = time_stamp,
    lon,
    lat
  ) |>
  dplyr::slice_head(n = 20000, by = vid)
ports <- sf::st_transform(dansk_harbours, 4326) |>
  dplyr::mutate(port_id = as.character(SI_HARB))

test_that("a data frame and a lazy table give the same stays", {
  skip_if_not_installed("duckdbfs")
  a <- rb_find_trip_stays(pings, ports, min_stay_h = 2)
  b <- dplyr::collect(rb_find_trip_stays(
    duckdbfs::as_dataset(pings),
    ports,
    min_stay_h = 2
  ))
  b <- b[order(b$vid, b$T_in), ]
  expect_gt(nrow(a), 0)
  expect_equal(as.data.frame(b), as.data.frame(a), ignore_attr = TRUE)
  expect_setequal(
    names(a),
    c(
      "vid",
      "port_id",
      "T_in",
      "T_out",
      "dur_h",
      "rule",
      "evidence",
      "T_in_ping",
      "T_out_ping",
      "T_in_ev",
      "T_out_ev",
      "basis"
    )
  )
  expect_true(all(
    a$dur_h >= 2 | a$rule == "event_pair" | grepl("tag_span|gap", a$evidence)
  ))
})

test_that("the window keeps only stays that begin in it; a longer minimum gives fewer stays", {
  a <- rb_find_trip_stays(pings, ports, min_stay_h = 2)
  w <- range(a$T_in)
  w <- c(w[1], w[1] + (w[2] - w[1]) / 2)
  b <- rb_find_trip_stays(pings, ports, min_stay_h = 2, t_in_window = w)
  expect_true(all(b$T_in >= w[1] & b$T_in < w[2]))
  expect_lt(nrow(b), nrow(a))
  ms <- data.frame(port_id = unique(a$port_id), min_stay_h = 24)
  expect_lte(
    nrow(rb_find_trip_stays(pings, ports, min_stay_h = 2, min_stay = ms)),
    nrow(a)
  )
})
# rb_cut_trip_voyages() and rb_assign_trip(): port of fishycode's curate/ais_trip.R (plan 013 phase 2). Exact on
# the Icelandic data (1,372,656 voyages; 535,613,643 pings, every year's fingerprint equal); here: data frame =
# lazy table, every ping kept, the minimum-pings rule.

pings <- dansk |>
  dplyr::filter(vessel_id %in% unique(dansk$vessel_id)[1:2]) |>
  dplyr::transmute(
    vid = as.integer(factor(vessel_id)),
    time = time_stamp,
    lon,
    lat
  ) |>
  dplyr::slice_head(n = 20000, by = vid)
ports <- sf::st_transform(dansk_harbours, 4326) |>
  dplyr::mutate(port_id = as.character(SI_HARB))
stays <- rb_find_trip_stays(pings, ports, min_stay_h = 2)

test_that("voyages and trip labels agree between a data frame and a lazy table", {
  skip_if_not_installed("duckdbfs")
  v <- rb_cut_trip_voyages(pings, stays, min_pings = 10)
  vl <- dplyr::collect(rb_cut_trip_voyages(
    duckdbfs::as_dataset(pings),
    duckdbfs::as_dataset(stays),
    min_pings = 10
  ))
  vl <- vl[order(vl$vid, vl$T1), ]
  expect_gt(nrow(v), 0)
  expect_equal(as.data.frame(vl), as.data.frame(v), ignore_attr = TRUE)
  expect_true(all(v$n_pings >= 10))
  a <- rb_assign_trip(pings, v, stays = stays)
  al <- dplyr::collect(rb_assign_trip(
    duckdbfs::as_dataset(pings),
    duckdbfs::as_dataset(v),
    stays = duckdbfs::as_dataset(stays)
  ))
  expect_equal(nrow(a), nrow(pings))
  k <- function(d) {
    d[
      order(d$vid, d$time, d$lon, d$lat),
      c("vid", "time", "stay_port_id", "voyage_id", "trip_basis")
    ]
  }
  expect_equal(as.data.frame(k(al)), as.data.frame(k(a)), ignore_attr = TRUE)
  expect_setequal(unique(a$trip_basis), c("reconstructed", "none"))
  # a ping in a stay is never on a voyage
  expect_true(all(is.na(a$voyage_id[!is.na(a$stay_port_id)])))
})

# The "runs" and "jepol" methods, against the functions they replace (snapshot taken before the recode).
old <- readRDS(test_path("fixtures", "pre_jepol.rds"))
tagged <- dansk |>
  dplyr::transmute(
    rid = dplyr::row_number(),
    vid = vessel_id,
    time = time_stamp,
    lon,
    lat,
    port_id = ifelse(old$in_harbour == 1, "h", NA_character_)
  )

# Each old trip id must map to one new (vid, voyage_id) and back, and the same pings must be outside every trip.
same_partition <- function(a, b) {
  expect_equal(is.na(a), is.na(b))
  pairs <- unique(data.frame(a, b)[!is.na(a), ])
  expect_false(any(duplicated(pairs$a)) || any(duplicated(pairs$b)))
}

test_that("method jepol reproduces rb_trip_jepol() except at the fixed off-by-one cut", {
  # The fixtures pin the old function, which cut a split voyage one ping before its longest
  # gap. The 2026-10-12 fix moves only those cuts: on dansk none at max_dur_h = 72, two at 48
  # and three at 24; no voyage is gained or lost. At 24 the moved cut also re-times two
  # segments across the min_dur_h threshold, flipping two pings' at-sea status. Compare ping
  # sets per voyage, not id labels (voyage numbers shift after a moved cut).
  class_sets <- function(id) {
    lapply(split(tagged$rid, id), sort)
  }
  n_moved <- function(old_id, new_id) {
    new_keys <- vapply(class_sets(new_id), paste, character(1), collapse = ",")
    sum(
      !vapply(class_sets(old_id), paste, character(1), collapse = ",") %in%
        new_keys
    )
  }
  for (md in c(72, 48, 24)) {
    v <- suppressWarnings(rb_cut_trip_voyages(
      tagged,
      method = "jepol",
      by_year = FALSE,
      max_dur_h = md
    ))
    a <- rb_assign_trip(tagged, v, by_year = FALSE)
    a <- a[order(a$rid), ]
    old_id <- old[[paste0("jepol_", md)]]
    new_id <- ifelse(is.na(a$voyage_id), NA, paste(a$vid, a$voyage_id))
    # at-sea status changes only where the moved cut crosses the min_dur_h threshold
    expect_equal(
      sum(is.na(old_id) != is.na(new_id)),
      c("72" = 0, "48" = 0, "24" = 2)[as.character(md)],
      ignore_attr = TRUE
    )
    # the same number of voyages
    expect_equal(length(class_sets(new_id)), length(class_sets(old_id)))
    # and only the counted cuts moved
    expect_equal(
      n_moved(old_id, new_id),
      c("72" = 0, "48" = 2, "24" = 3)[as.character(md)],
      ignore_attr = TRUE
    )
  }
  # without splits the reproduction is still exact
  v <- rb_cut_trip_voyages(
    tagged,
    method = "jepol",
    by_year = FALSE,
    max_dur_h = 12,
    split = FALSE
  )
  a <- rb_assign_trip(tagged, v, by_year = FALSE)
  a <- a[order(a$rid), ]
  same_partition(
    old$jepol_nosplit,
    ifelse(is.na(a$voyage_id), NA, paste(a$vid, a$voyage_id))
  )
  expect_true(all(v$port_from == "h" | is.na(v$port_from)))
})

test_that("method jepol splits a long voyage at its longest gap", {
  # one voyage, at sea from hour 0 to hour 15, longest gap 10 h between hours 2 and 12
  base <- as.POSIXct("2023-06-01", tz = "UTC")
  d <- data.frame(
    vid = 1,
    time = base + c(-1, 0, 1, 2, 12, 13, 14, 15) * 3600,
    port_id = c("h", NA, NA, NA, NA, NA, NA, "h")
  )
  v <- rb_cut_trip_voyages(d, method = "jepol", by_year = FALSE, max_dur_h = 5)
  expect_equal(nrow(v), 2)
  # the cut falls at the gap: the first voyage ends at hour 2, the second starts at hour 12
  expect_equal(v$T2[1], base + 2 * 3600)
  expect_equal(v$T1[2], base + 12 * 3600)
})

test_that("method runs numbers the sea runs as the old rb_trip() did, vessel by vessel", {
  # reference: the body of the removed rb_trip()
  rb_trip_ref <- function(x) {
    tibble::tibble(x = x) |>
      dplyr::mutate(
        tid = dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L),
        tid = ifelse(x, -tid, tid)
      ) |>
      dplyr::group_by(x) |>
      dplyr::mutate(tid = cumsum(tid)) |>
      dplyr::ungroup() |>
      dplyr::pull(tid)
  }
  v <- rb_cut_trip_voyages(tagged, method = "runs", by_year = FALSE)
  a <- rb_assign_trip(tagged, v, by_year = FALSE)
  a <- a[order(a$vid, a$time), ]
  r <- unlist(lapply(split(!is.na(a$port_id), a$vid), rb_trip_ref))
  expect_equal(ifelse(r > 0, r, NA), a$voyage_id, ignore_attr = TRUE)
})

test_that("methods runs and jepol: a data frame and a lazy table give the same voyages", {
  skip_if_not_installed("duckdbfs")
  for (m in c("runs", "jepol")) {
    a <- rb_cut_trip_voyages(tagged, method = m)
    b <- dplyr::collect(rb_cut_trip_voyages(
      duckdbfs::as_dataset(tagged),
      method = m,
      batch_pings = 1e5
    ))
    b <- b[order(b$vid, b$T1), ]
    expect_equal(
      as.data.frame(b),
      as.data.frame(a),
      ignore_attr = TRUE,
      info = m
    )
  }
})

test_that("a stay key in the stays reaches the pings (stay_id), the stay's values do not", {
  st <- stays |>
    dplyr::arrange(vid, T_in) |>
    dplyr::mutate(stay_id = dplyr::row_number(), .by = vid)
  v <- rb_cut_trip_voyages(pings, st, min_pings = 10)
  a <- rb_assign_trip(pings, v, stays = st)
  expect_true(all(c("stay_id", "stay_port_id") %in% names(a)))
  expect_equal(is.na(a$stay_id), is.na(a$stay_port_id))
  b <- rb_assign_trip(pings, v, stays = stays)
  expect_false("stay_id" %in% names(b))
})
test_that("rb_add_trips assigns trip ids correctly with robust defaults and warnings", {
  # Minimal mock VMS data with default column names
  ve <- tibble::tibble(
    vid = c("A", "A", "A", "B"),
    time = as.POSIXct(c(
      "2022-01-01 10:00:00",
      "2022-01-01 15:00:00",
      "2022-01-02 08:00:00",
      "2022-01-01 12:00:00"
    ))
  )

  # Minimal mock logbook data with default column names
  le <- tibble::tibble(
    vid = c("A", "A", "B"),
    tid = c(101, 102, 201),
    T1 = as.POSIXct(c(
      "2022-01-01 09:00:00",
      "2022-01-02 07:00:00",
      "2022-01-01 11:00:00"
    )),
    T2 = as.POSIXct(c(
      "2022-01-01 16:00:00",
      "2022-01-02 09:00:00",
      "2022-01-01 13:00:00"
    ))
  )

  # Run function (no warning expected: nothing missing, no name collision)
  expect_warning(
    out <- rb_link_trip_logbook(ve, le),
    regexp = NA
  )

  # Join should assign tid 101 to first two, tid 102 to third, tid 201 to fourth
  expect_equal(out$tid, c(101, 101, 102, 201))
  # Departure and arrival columns should not be present
  expect_false("T1" %in% names(out))
  expect_false("T2" %in% names(out))
  # Output should have same number of rows as input ve
  expect_equal(nrow(out), nrow(ve))

  # Test warning on missing columns
  ve_bad <- ve[, -1, drop = FALSE] # drop 'vid'
  expect_warning(
    rb_link_trip_logbook(ve_bad, le),
    "Missing the following columns in VMS data"
  )

  le_bad <- le[, -1, drop = FALSE] # drop 'vid'
  expect_warning(
    rb_link_trip_logbook(ve, le_bad),
    "Missing the following columns in logbook data"
  )

  # Test warning if no trip ids matched
  ve_nomatch <- tibble::tibble(
    vid = "C",
    time = as.POSIXct("2022-01-01 10:00:00")
  )
  expect_warning(
    rb_link_trip_logbook(ve_nomatch, le),
    "No trip IDs were matched to the VMS data"
  )

  # A ve column named like the departure/arrival columns collides: the join
  # suffixes (.x/.y) and the logbook side survives the drop, with a warning
  ve_clash <- dplyr::mutate(ve, T1 = 1)
  expect_warning(
    out_clash <- rb_link_trip_logbook(ve_clash, le),
    "occur in both"
  )
  expect_true("T1.x" %in% names(out_clash))
  expect_true("T1.y" %in% names(out_clash))
})
