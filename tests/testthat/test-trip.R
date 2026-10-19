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
old <- as.list(ramb:::.rb_read_parquet(system.file(
  "testdata",
  "pre_jepol.parquet",
  package = "ramb"
)))
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

test_that("method jepol: a last ping that is itself a departure neither merges nor stops", {
  # define_trips() overwrote the last ping's departure with an arrival, one departure
  # short: after one voyage the departures were recycled into a voyage across the port
  # stay, after two the run stopped ("departures and arrivals do not pair"), as it did
  # on 2023 of the Icelandic data. The one-ping voyage at the end now has no duration
  # and is dropped; the voyages before it are untouched.
  mk <- function(inh) {
    data.frame(
      vid = 1L,
      time = as.POSIXct("2024-03-01", tz = "UTC") + 3600 * (seq_along(inh) - 1),
      port_id = ifelse(inh == 1, "P1", NA_character_)
    )
  }
  one <- rb_cut_trip_voyages(mk(c(1, 1, 0, 0, 0, 1, 1, 0)), method = "jepol")
  expect_equal(nrow(one), 1)
  expect_equal(one$port_to, "P1")
  expect_equal(one$n_pings, 4)
  two <- rb_cut_trip_voyages(mk(c(1, 0, 0, 1, 0, 0, 1, 0)), method = "jepol")
  expect_equal(nrow(two), 2)
  expect_true(all(two$port_to == "P1"))
  expect_true(all(two$T1[-1] >= two$T2[-nrow(two)]))
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

test_that("trips to landings: a cascade of unambiguous steps, then many trips to one landing", {
  trips <- tibble::tibble(
    tid = 1:5,
    vid = c(1L, 1L, 1L, 1L, 2L),
    port = c("A", "A", "B", "B", "C"),
    T2 = as.POSIXct(
      c(
        "2024-03-01 18:00",
        "2024-03-03 20:00",
        "2024-03-05 23:30",
        "2024-03-06 17:00",
        "2024-03-01 12:00"
      ),
      tz = "UTC"
    )
  )
  landings <- tibble::tibble(
    lid = c(10, 11, 12, 20, 21),
    vid = c(1L, 1L, 1L, 2L, 2L),
    port = c("A", "A", "B", "C", "C"),
    date = as.Date(c(
      "2024-03-01",
      "2024-03-04",
      "2024-03-07",
      "2024-03-01",
      "2024-03-01"
    ))
  )
  k <- rb_link_trip_landings(
    trips,
    landings,
    "tid",
    "lid",
    port_trip = "port",
    port_landing = "port"
  )
  expect_equal(k$tid, c(1L, 2L, 4L))
  expect_equal(k$lid, c(10, 11, 12))
  expect_equal(k$match_type, c("date_port", "date1_port", "date1_port"))
  # vessel 2 has two landings on its trip's day: ambiguous, so not matched
  expect_false(5L %in% k$tid)
  s <- rb_link_trip_landings(
    trips,
    landings,
    "tid",
    "lid",
    port_trip = "port",
    port_landing = "port",
    since_previous_days = 3
  )
  expect_equal(s$lid[s$tid == 3L], 12)
  expect_equal(s$match_type[s$tid == 3L], "since_previous")
  expect_equal(s$n_trips[s$lid == 12], c(2, 2))
  expect_error(
    rb_link_trip_landings(
      trips,
      dplyr::rename(landings, tid = lid),
      "tid",
      "tid"
    ),
    "different names"
  )
  skip_if_not_installed("duckdbfs")
  l <- rb_link_trip_landings(
    duckdbfs::as_dataset(trips),
    duckdbfs::as_dataset(landings),
    "tid",
    "lid",
    port_trip = "port",
    port_landing = "port",
    since_previous_days = 3
  )
  expect_equal(as.data.frame(l), as.data.frame(s), ignore_attr = TRUE)
})

test_that("rb_pad_harbour() puts the ports on a voyage's end pings, in port NA or an id of 0 or less", {
  expect_identical(names(formals(rb_pad_harbour)), c("voyage_id", "port_id"))
  # rb_assign_trip() leaves a ping in port with no voyage
  expect_equal(
    rb_pad_harbour(c(NA, 1, 1, 1, NA), c(7, NA, NA, NA, 9)),
    c(7, 7, NA, 9, 9)
  )
  # the older convention: a ping in port has an id of 0 or less
  expect_equal(
    rb_pad_harbour(
      c(0, 0, 1, 1, 1, 0, 2, 2, 0),
      c(3, 3, NA, NA, NA, 4, NA, NA, 5)
    ),
    c(3, 3, 3, NA, 4, 4, 4, 5, 5)
  )
  # a one-ping voyage takes the port before it
  expect_equal(rb_pad_harbour(c(NA, 1, NA), c(7, NA, 9)), c(7, 7, 9))
  # a neighbour with no port id, or none at all, leaves the ping its own
  expect_equal(rb_pad_harbour(c(NA, 1, 1, 1), c(NA, 8, NA, 6)), c(NA, 8, NA, 6))
})

# rb_cut_trip_voyages(method = "cnr"): CNR-IRBIM's create_fishing_trip(), recoded in C++. The fixture is the R
# function's own output on its own Italian tracks (no coastal zone: ramb does not carry CNR's), made by
# data-raw/cnr_trips.R.
cnr_italy <- function() {
  h <- dplyr::collect(rb_read_example("wkssfgeo_cnr_med_harbours"))
  hp <- sf::st_sf(
    port_id = seq_len(nrow(h)),
    harbour = h$harbour,
    geometry = sf::st_as_sfc(structure(h$geometry, class = "WKB"), crs = 4326)
  )
  pings <- rb_read_example("wkssfgeo_cnr_ais") |>
    rb_flag_ping_port(hp["port_id"], lon = longitude, lat = latitude)
  list(hp = hp, pings = pings)
}

test_that("method cnr gives the R function's trips and ports on its Italian tracks", {
  skip_if_not_installed("lwgeom")
  d <- cnr_italy()
  # collected once: a data-frame call unregisters the tables behind lazy ones, here the harbours behind d$pings
  p <- as.data.frame(dplyr::collect(d$pings))
  v <- rb_cut_trip_voyages(
    p,
    method = "cnr",
    ports = d$hp,
    by_year = FALSE,
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  ref <- utils::read.csv(
    system.file("testdata", "cnr_italy_trips.csv", package = "ramb"),
    comment.char = "#",
    na.strings = ""
  )
  got <- data.frame(
    vid = v$MMSI,
    T1 = format(v$T1, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    T2 = format(v$T2, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    from = d$hp$harbour[v$port_from],
    to = d$hp$harbour[v$port_to]
  )
  expect_equal(got, ref, ignore_attr = TRUE)
  # on these tracks the sphere gives the same ports as the ellipsoid
  skip_if_not_installed("s2")
  w <- rb_cut_trip_voyages(
    p,
    method = "cnr",
    ports = d$hp,
    by_year = FALSE,
    cnr = list(distance = "sphere"),
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  expect_equal(w, v)
})

test_that("method cnr: parquet read lazily, in batches, gives the data frame's voyages", {
  skip_if_not_installed("lwgeom")
  d <- cnr_italy()
  lz <- rb_cut_trip_voyages(
    d$pings,
    method = "cnr",
    ports = d$hp,
    batch_pings = 3000,
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  expect_s3_class(lz, "tbl_lazy")
  lz <- as.data.frame(dplyr::collect(lz))
  df <- rb_cut_trip_voyages(
    as.data.frame(dplyr::collect(d$pings)),
    method = "cnr",
    ports = d$hp,
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  lz <- lz[order(lz$MMSI, lz$T1), ]
  expect_equal(lz, df, ignore_attr = TRUE)
  expect_true(all(c("year", "voyage_id", "n_pings") %in% names(df)))
})

test_that("method cnr joins trips split by a silence as the R function does", {
  j <- function(
    s,
    e,
    from,
    to,
    cs = 0L,
    ce = 0L,
    time = seq(0, by = 3600, length.out = 100)
  ) {
    n <- length(s)
    ramb:::cnr_join_cpp(
      rep(1L, n),
      as.integer(s),
      as.integer(e),
      as.integer(from),
      as.integer(to),
      rep_len(as.integer(cs), n),
      rep_len(as.integer(ce), n),
      time,
      24
    )
  }
  # open on both sides of a silence, touching: one trip; a chain of three: one trip
  r <- j(c(1, 11), c(10, 20), c(1, NA), c(NA, 2))
  expect_equal(c(r$s, r$e, r$port_from, r$port_to, r$joined), c(1, 20, 1, 2, 1))
  r <- j(c(1, 11, 21), c(10, 20, 30), c(1, NA, NA), c(NA, NA, 3))
  expect_equal(nrow(r), 1)
  expect_equal(r$joined, 2)
  # a port on either side of the silence: no join
  expect_equal(nrow(j(c(1, 11), c(10, 20), c(1, 5), c(NA, 2))), 2)
  # pings between the two trips: broken, both open ends to the closest port
  r <- j(c(1, 12), c(10, 20), c(1, NA), c(NA, 2))
  expect_equal(r$joined, c(-2, -2))
  expect_equal(c(r$to_closest[1], r$from_closest[2]), c(TRUE, TRUE))
  # both ends in the coastal zone: joined under 24 h, broken from 24 h on
  expect_equal(
    nrow(j(c(1, 11), c(10, 20), c(1, NA), c(NA, 2), cs = 1, ce = 1)),
    1
  )
  r <- j(c(1, 11), c(10, 30), c(1, NA), c(NA, 2), cs = 1, ce = 1)
  expect_equal(r$joined, c(-1, -1))
  # one end in it: joined whatever the length
  expect_equal(
    nrow(j(
      c(1, 11),
      c(10, 30),
      c(1, NA),
      c(NA, 2),
      cs = c(1, 1),
      ce = c(0, 0)
    )),
    1
  )
})

test_that("method cnr checks its input and settings", {
  d <- data.frame(
    vid = 1,
    time = as.POSIXct("2020-01-01", tz = "UTC") + 1:20 * 300,
    lon = 0,
    lat = 0,
    port_id = NA_integer_
  )
  expect_error(rb_cut_trip_voyages(d, method = "cnr"), "speed")
  d$speed <- 3
  expect_error(
    rb_cut_trip_voyages(d, method = "cnr", cnr = list(n_pong = 4)),
    "Unknown `cnr` setting"
  )
  expect_error(
    rb_cut_trip_voyages(d, method = "cnr", cnr = list(distance = "planar")),
    "geodesic"
  )
  # all at sea, no silence: no voyage, and the result has the voyage columns
  v <- rb_cut_trip_voyages(d, method = "cnr")
  expect_equal(nrow(v), 0)
  expect_named(
    v,
    c("vid", "year", "voyage_id", "T1", "T2", "port_from", "port_to", "n_pings")
  )
})
