# rb_create_port_calls(method = "dwell"): port of fishycode's curate/port_stay.R (plan 013 phase 2), where a port
# call was a stay. Its exactness was checked on the Icelandic data (1,551,604 port calls, identical); here: data
# frame = lazy table, and the window.

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

test_that("a data frame and a lazy table give the same port calls", {
  a <- rb_create_port_calls(pings, ports, min_dwell_h = 2)
  b <- dplyr::collect(rb_create_port_calls(
    as_lazy(pings),
    ports,
    min_dwell_h = 2
  ))
  b <- b[order(b$vid, b$T_in), ]
  expect_gt(nrow(a), 0)
  expect_equal(as.data.frame(b), as.data.frame(a), ignore_attr = TRUE)
  expect_setequal(
    names(a),
    c(
      "vid",
      "year",
      "port_call_id",
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

test_that("the window keeps only port calls that begin in it; a longer minimum gives fewer", {
  a <- rb_create_port_calls(pings, ports, min_dwell_h = 2)
  w <- range(a$T_in)
  w <- c(w[1], w[1] + (w[2] - w[1]) / 2)
  b <- rb_create_port_calls(pings, ports, min_dwell_h = 2, t_in_window = w)
  expect_true(all(b$T_in >= w[1] & b$T_in < w[2]))
  expect_lt(nrow(b), nrow(a))
  ms <- data.frame(port_id = unique(a$port_id), min_dwell_h = 24)
  expect_lte(
    nrow(rb_create_port_calls(pings, ports, min_dwell_h = 2, min_dwell = ms)),
    nrow(a)
  )
})
# rb_create_voyages() and rb_add_voyages(): port of fishycode's curate/ais_trip.R (plan 013 phase 2). Exact on
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
calls <- rb_create_port_calls(pings, ports, min_dwell_h = 2)

# The time budget (speed_kn) and the minimum from the data (min_dwell_h = "valley"): fishycode plan 031 §4.1.
budget_tracks <- local({
  sq <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(-22.01, 64.145),
      c(-21.99, 64.145),
      c(-21.99, 64.155),
      c(-22.01, 64.155),
      c(-22.01, 64.145)
    ))),
    crs = 4326
  )
  t0 <- as.POSIXct("2024-03-01 00:00:00", tz = "UTC")
  dlat <- function(km) km / 111.2
  list(
    ports = sf::st_sf(port_id = "P1", geometry = sq),
    pings = rbind(
      # silent 5 h in port; the next ping is already 8 km out, beyond the 2 km departure radius
      data.frame(
        vid = 1L,
        time = t0 +
          c(
            seq(0, 3000, 600),
            3600,
            3600 + 5 * 3600,
            3600 + 5 * 3600 + seq(600, 3000, 600)
          ),
        lon = -22,
        lat = 64.15 -
          dlat(c(30, 25, 20, 15, 10, 5, 1.4, 8.5, 13, 18, 23, 28, 33))
      ),
      # hourly pings: a call of about 3 h holds two pings an hour apart
      data.frame(
        vid = 2L,
        time = t0 + 3600 * (0:6),
        lon = -22,
        lat = 64.15 - dlat(c(20, 10, 5.5, 0, 0, 5.5, 10))
      ),
      # a silence of 6 h at sea, 50 km from the port
      data.frame(
        vid = 3L,
        time = t0 + 3600 * c(0, 1, 7, 8),
        lon = -22,
        lat = 64.15 - dlat(c(55, 50, 50, 55))
      )
    )
  )
})

test_that("the time budget finds the calls that sparse pings hide, and none at sea", {
  p <- budget_tracks$pings
  ports <- budget_tracks$ports
  expect_equal(nrow(rb_create_port_calls(p, ports, min_dwell_h = 2)), 0)
  b <- rb_create_port_calls(p, ports, min_dwell_h = 2, speed_kn = 6)
  expect_equal(b$vid, c(1L, 2L))
  expect_equal(b$rule, c("gap_budget", "span_budget"))
  expect_equal(attr(b, "min_dwell_h"), 2)
  # each vessel's own speed: these tracks have too few legs, so the fallback, 6 knots
  v <- rb_create_port_calls(p, ports, min_dwell_h = 2, speed_kn = "vessel")
  expect_equal(as.data.frame(v), as.data.frame(b), ignore_attr = TRUE)
  # a slower vessel needs longer to sail in and out: at 1 knot, the 5 h silence leaves too little
  expect_equal(
    rb_create_port_calls(p, ports, min_dwell_h = 2, speed_kn = 1)$vid,
    integer(0)
  )
  expect_error(rb_create_port_calls(p, ports, speed_kn = "fast"), "speed_kn")
})

test_that("the time budget: a data frame and a lazy table give the same port calls", {
  a <- rb_create_port_calls(
    budget_tracks$pings,
    budget_tracks$ports,
    min_dwell_h = 2,
    speed_kn = 6
  )
  b <- dplyr::collect(rb_create_port_calls(
    as_lazy(budget_tracks$pings),
    budget_tracks$ports,
    min_dwell_h = 2,
    speed_kn = 6
  ))
  b <- b[order(b$vid, b$T_in), ]
  expect_equal(as.data.frame(b), as.data.frame(a), ignore_attr = TRUE)
})

test_that("the minimum from the data lies between pass-throughs and calls", {
  set.seed(1)
  h <- c(
    stats::rlnorm(3000, log(10 / 60), 0.6),
    stats::rlnorm(3000, log(10), 0.6)
  )
  x <- .rb_dwell_valley(h)
  expect_gt(x, 0.7)
  expect_lt(x, 2.5)
  expect_error(.rb_dwell_valley(1:10), "Too few")
  # two vessels give too few candidate calls; all fourteen Danish vessels give a minimum of about an hour
  expect_error(
    rb_create_port_calls(pings, ports, min_dwell_h = "valley"),
    "Too few"
  )
  all14 <- data.frame(
    vid = as.integer(factor(dansk$vessel_id)),
    time = dansk$time_stamp,
    lon = dansk$lon,
    lat = dansk$lat
  )
  a <- suppressMessages(rb_create_port_calls(
    all14,
    ports,
    min_dwell_h = "valley"
  ))
  expect_gt(attr(a, "min_dwell_h"), 0.5)
  expect_lt(attr(a, "min_dwell_h"), 2)
  expect_gte(nrow(a), nrow(rb_create_port_calls(all14, ports, min_dwell_h = 2)))
  expect_error(
    rb_create_port_calls(pings, ports, min_dwell_h = "long"),
    "min_dwell_h"
  )
})

test_that("voyages are labelled short and open, and with min_pings = 1 none is dropped", {
  p <- budget_tracks$pings
  calls <- rb_create_port_calls(
    p,
    budget_tracks$ports,
    min_dwell_h = 2,
    speed_kn = 6
  )
  v <- rb_create_voyages(p, port_calls = calls, min_pings = 1, by_year = FALSE)
  expect_equal(nrow(v), 5)
  expect_equal(v$open_start, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(v$open_end, c(FALSE, TRUE, FALSE, TRUE, TRUE))
  expect_true(all(v$short))
  expect_equal(
    nrow(rb_create_voyages(p, port_calls = calls, by_year = FALSE)),
    0
  )
})

test_that("voyages and trip labels agree between a data frame and a lazy table", {
  v <- rb_create_voyages(pings, calls, min_pings = 10)
  vl <- dplyr::collect(rb_create_voyages(
    as_lazy(pings),
    as_lazy(calls),
    min_pings = 10
  ))
  vl <- vl[order(vl$vid, vl$T1), ]
  expect_gt(nrow(v), 0)
  expect_equal(as.data.frame(vl), as.data.frame(v), ignore_attr = TRUE)
  expect_true(all(v$n_pings >= 10))
  a <- rb_add_voyages(pings, v, port_calls = calls)
  al <- dplyr::collect(rb_add_voyages(
    as_lazy(pings),
    as_lazy(v),
    port_calls = as_lazy(calls)
  ))
  expect_equal(nrow(a), nrow(pings))
  k <- function(d) {
    d[
      order(d$vid, d$time, d$lon, d$lat),
      c(
        "vid",
        "time",
        "port_call_port_id",
        "port_call_id",
        "voyage_id",
        "trip_basis"
      )
    ]
  }
  expect_equal(as.data.frame(k(al)), as.data.frame(k(a)), ignore_attr = TRUE)
  expect_setequal(unique(a$trip_basis), c("reconstructed", "none"))
  # a ping in a port call is never on a voyage
  expect_true(all(is.na(a$voyage_id[!is.na(a$port_call_port_id)])))
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
    v <- suppressWarnings(rb_create_voyages(
      tagged,
      method = "jepol",
      by_year = FALSE,
      max_dur_h = md
    ))
    a <- rb_add_voyages(tagged, v, by_year = FALSE)
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
  v <- rb_create_voyages(
    tagged,
    method = "jepol",
    by_year = FALSE,
    max_dur_h = 12,
    split = FALSE
  )
  a <- rb_add_voyages(tagged, v, by_year = FALSE)
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
  v <- rb_create_voyages(d, method = "jepol", by_year = FALSE, max_dur_h = 5)
  expect_equal(nrow(v), 2)
  # the cut falls at the gap: the first voyage ends at hour 2, the second starts at hour 12
  expect_equal(v$T2[1], base + 2 * 3600)
  expect_equal(v$T1[2], base + 12 * 3600)
})

test_that("method jepol: a last ping that is itself a departure neither merges nor stops", {
  # define_trips() overwrote the last ping's departure with an arrival, one departure
  # short: after one voyage the departures were recycled into a voyage across the port
  # call, after two the run stopped ("departures and arrivals do not pair"), as it did
  # on 2023 of the Icelandic data. The one-ping voyage at the end now has no duration
  # and is dropped; the voyages before it are untouched.
  mk <- function(inh) {
    data.frame(
      vid = 1L,
      time = as.POSIXct("2024-03-01", tz = "UTC") + 3600 * (seq_along(inh) - 1),
      port_id = ifelse(inh == 1, "P1", NA_character_)
    )
  }
  one <- rb_create_voyages(mk(c(1, 1, 0, 0, 0, 1, 1, 0)), method = "jepol")
  expect_equal(nrow(one), 1)
  expect_equal(one$port_to, "P1")
  expect_equal(one$n_pings, 4)
  two <- rb_create_voyages(mk(c(1, 0, 0, 1, 0, 0, 1, 0)), method = "jepol")
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
  v <- rb_create_voyages(tagged, method = "runs", by_year = FALSE)
  a <- rb_add_voyages(tagged, v, by_year = FALSE)
  a <- a[order(a$vid, a$time), ]
  r <- unlist(lapply(split(!is.na(a$port_id), a$vid), rb_trip_ref))
  expect_equal(ifelse(r > 0, r, NA), a$voyage_id, ignore_attr = TRUE)
})

test_that("methods runs and jepol: a data frame and a lazy table give the same voyages", {
  for (m in c("runs", "jepol")) {
    a <- rb_create_voyages(tagged, method = m)
    b <- dplyr::collect(rb_create_voyages(
      as_lazy(tagged),
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

test_that("a port call's key reaches the pings (port_call_id), its values do not", {
  # numbered per vessel by T_in, whatever the year
  expect_equal(
    calls$port_call_id,
    as.numeric(stats::ave(seq_len(nrow(calls)), calls$vid, FUN = seq_along))
  )
  expect_equal(calls$year, as.numeric(format(calls$T_in, "%Y")))
  v <- rb_create_voyages(pings, calls, min_pings = 10)
  a <- rb_add_voyages(pings, v, port_calls = calls)
  expect_true(all(c("port_call_id", "port_call_port_id") %in% names(a)))
  expect_equal(is.na(a$port_call_id), is.na(a$port_call_port_id))
  expect_false(any(c("T_in", "T_out", "dur_h", "year") %in% names(a)))
  # the key finds the port call the ping lies in
  j <- dplyr::inner_join(
    a[!is.na(a$port_call_id), c("vid", "time", "port_call_id")],
    calls[, c("vid", "port_call_id", "T_in", "T_out")],
    by = c("vid", "port_call_id")
  )
  expect_true(all(j$time >= j$T_in & j$time <= j$T_out))
  b <- rb_add_voyages(
    pings,
    v,
    port_calls = dplyr::select(calls, -port_call_id)
  )
  expect_false("port_call_id" %in% names(b))
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
    out <- rb_add_logbook_trip(ve, le),
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
    rb_add_logbook_trip(ve_bad, le),
    "Missing the following columns in VMS data"
  )

  le_bad <- le[, -1, drop = FALSE] # drop 'vid'
  expect_warning(
    rb_add_logbook_trip(ve, le_bad),
    "Missing the following columns in logbook data"
  )

  # Test warning if no trip ids matched
  ve_nomatch <- tibble::tibble(
    vid = "C",
    time = as.POSIXct("2022-01-01 10:00:00")
  )
  expect_warning(
    rb_add_logbook_trip(ve_nomatch, le),
    "No trip IDs were matched to the VMS data"
  )

  # A ve column named like the departure/arrival columns collides: the join
  # suffixes (.x/.y) and the logbook side survives the drop, with a warning
  ve_clash <- dplyr::mutate(ve, T1 = 1)
  expect_warning(
    out_clash <- rb_add_logbook_trip(ve_clash, le),
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
  l <- rb_link_trip_landings(
    as_lazy(trips),
    as_lazy(landings),
    "tid",
    "lid",
    port_trip = "port",
    port_landing = "port",
    since_previous_days = 3
  )
  # lazy trips give a lazy result, in no order
  expect_s3_class(l, "tbl_lazy")
  l <- dplyr::collect(l)
  l <- l[order(l$vid, l$tid), ]
  expect_equal(as.data.frame(l), as.data.frame(s), ignore_attr = TRUE)
})

test_that("rb_pad_harbour() puts the ports on a voyage's end pings, in port NA or an id of 0 or less", {
  expect_identical(names(formals(rb_pad_harbour)), c("voyage_id", "port_id"))
  # rb_add_voyages() leaves a ping in port with no voyage
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

# rb_create_voyages(method = "cnr"): CNR-IRBIM's create_fishing_trip(), recoded in C++. The fixture is the R
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
  v <- rb_create_voyages(
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
  w <- rb_create_voyages(
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
  lz <- rb_create_voyages(
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
  df <- rb_create_voyages(
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
  expect_error(rb_create_voyages(d, method = "cnr"), "speed")
  d$speed <- 3
  expect_error(
    rb_create_voyages(d, method = "cnr", cnr = list(n_pong = 4)),
    "Unknown `cnr` setting"
  )
  expect_error(
    rb_create_voyages(d, method = "cnr", cnr = list(distance = "planar")),
    "geodesic"
  )
  # all at sea, no silence: no voyage, and the result has the voyage columns
  v <- rb_create_voyages(d, method = "cnr")
  expect_equal(nrow(v), 0)
  expect_named(
    v,
    c("vid", "year", "voyage_id", "T1", "T2", "port_from", "port_to", "n_pings")
  )
})

# rb_create_voyages(method = "iapesca"): Ifremer's Detect_FishingTrips() (iapesca 1.3), recoded as one DuckDB query.
# The fixture is iapesca's own output on CNR-IRBIM's Italian tracks, made by data-raw/iapesca_trips.R.
ia_italy <- function() {
  h <- dplyr::collect(rb_read_example("wkssfgeo_cnr_med_harbours"))
  list(
    hp = sf::st_sf(
      port_id = seq_len(nrow(h)),
      geometry = sf::st_as_sfc(structure(h$geometry, class = "WKB"), crs = 4326)
    ),
    p = as.data.frame(dplyr::collect(rb_read_example("wkssfgeo_cnr_ais")))
  )
}

test_that("method iapesca gives the published function's trips on the Italian tracks", {
  d <- ia_italy()
  v <- rb_create_voyages(
    d$p,
    method = "iapesca",
    ports = d$hp,
    by_year = FALSE,
    iapesca = list(crs_m = 32633),
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  ref <- utils::read.csv(
    system.file("testdata", "iapesca_italy_trips.csv", package = "ramb"),
    comment.char = "#"
  )
  got <- data.frame(
    vid = v$MMSI,
    T1 = format(v$T1, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    T2 = format(v$T2, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    n_pings = v$n_pings
  )
  expect_equal(got, ref, ignore_attr = TRUE)
  expect_true(all(v$port_from %in% c(NA, d$hp$port_id)))
  # the default CRS is the UTM zone of the mean position, here 33N
  w <- rb_create_voyages(
    d$p,
    method = "iapesca",
    ports = d$hp,
    by_year = FALSE,
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  expect_equal(w, v)
})

test_that("method iapesca: parquet read lazily gives the data frame's voyages", {
  d <- ia_italy()
  lz <- rb_create_voyages(
    rb_read_example("wkssfgeo_cnr_ais"),
    method = "iapesca",
    ports = d$hp,
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  expect_s3_class(lz, "tbl_lazy")
  lz <- as.data.frame(dplyr::collect(lz))
  lz <- lz[order(lz$MMSI, lz$T1), ]
  df <- rb_create_voyages(
    d$p,
    method = "iapesca",
    ports = d$hp,
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  expect_equal(lz, df, ignore_attr = TRUE)
  expect_named(
    df,
    c(
      "MMSI",
      "year",
      "voyage_id",
      "T1",
      "T2",
      "port_from",
      "port_to",
      "n_pings"
    )
  )
})

test_that("method iapesca ends a voyage at a stop of over an hour between pings at sea, as iapesca 1.3", {
  # a square port at 3 E, 50 N; a ping every 2 min, in port or out east and back at about 11 knots
  port <- sf::st_sf(
    port_id = 7L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(cbind(
        3 + c(-0.003, 0.003, 0.003, -0.003, -0.003),
        50 + c(-0.002, -0.002, 0.002, 0.002, -0.002)
      ))),
      crs = 4326
    )
  )
  track <- function(..., lat = 50) {
    lon <- unlist(lapply(list(...), function(l) {
      if (l < 1) {
        3 + c(seq(0.005, l, by = 0.005), seq(l, 0.005, by = -0.005))
      } else {
        rep(3, l)
      }
    }))
    data.frame(
      vid = 1,
      time = as.POSIXct("2021-12-01 06:00", tz = "UTC") +
        (seq_along(lon) - 1) * 120,
      lon = lon,
      lat = lat
    )
  }
  # the pings at sea, worked out here and not by the method: further from the port than `buf` metres, on the plane
  # of UTM zone 31N (the default CRS here)
  at_sea <- function(d, buf = 2000) {
    x <- sf::st_transform(sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 32631)
    which(as.numeric(sf::st_distance(x, sf::st_transform(port, 32631))) > buf)
  }
  ia <- function(d) {
    rb_create_voyages(d, method = "iapesca", ports = port, by_year = FALSE)
  }
  # 40 min out, 5 h in port, out again: two voyages
  a <- ia(track(3, 0.05, 150, 0.2, 3))
  expect_equal(nrow(a), 2)
  expect_equal(c(a$port_from, a$port_to), rep(7L, 4))
  # 2.7 h out, 10 min in port, out again: one voyage from the first ping at sea to the last, with the pings in
  # port during the stop
  d <- track(3, 0.2, 5, 0.2, 3)
  b <- ia(d)
  s <- at_sea(d)
  expect_equal(nrow(b), 1)
  expect_equal(c(b$T1, b$T2), d$time[range(s)])
  expect_equal(b$n_pings, diff(range(s)) + 1)
  expect_gt(b$n_pings, length(s))
  # a ping at the same second as an earlier one is dropped, wherever it is
  expect_equal(ia(rbind(d[1:30, ], transform(d[30, ], lon = 3.4), d[31:nrow(d), ])), b)
  # never within 2 km of the port: the buffer grows by a fifth of its way to 2 nautical miles, rounded, to 2,341 m
  d <- track(3, 0.2, 5, 0.2, 3, lat = 50 + 2300 / 111200)
  g <- ia(d)
  s <- at_sea(d, 2000 + round((2 / 0.539957 * 1000 - 2000) / 5))
  expect_equal(nrow(g), 1)
  expect_equal(c(g$T1, g$T2), d$time[range(s)])
  expect_equal(g$n_pings, diff(range(s)) + 1)
})

test_that("method iapesca checks its input and settings", {
  d <- data.frame(
    vid = 1,
    time = as.POSIXct("2020-01-01", tz = "UTC") + 1:20 * 300,
    lon = 3,
    lat = 50
  )
  expect_error(rb_create_voyages(d, method = "iapesca"), "needs `ports`")
  port <- sf::st_sf(
    port_id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(3, 50)), crs = 4326)
  )
  expect_error(
    rb_create_voyages(
      d,
      method = "iapesca",
      ports = port,
      iapesca = list(crs_m = 4326)
    ),
    "projected CRS in metres"
  )
  expect_error(
    rb_create_voyages(
      d,
      method = "iapesca",
      ports = port,
      iapesca = list(buffer = 1)
    ),
    "Unknown `iapesca` setting"
  )
})

# rb_create_port_calls() for the methods other than "dwell": the time in port between two of the method's
# voyages (fishycode plan 032). The expected port calls are built here in R from the voyages, independently of
# the function's SQL.
calls_between <- function(v, vid = "vid") {
  v <- v[order(v[[vid]], v$T1, v$T2), ]
  nxt <- function(x) {
    stats::ave(seq_along(x), v[[vid]], FUN = function(i) c(i[-1], NA))
  }
  k <- nxt(v$T1)
  d <- data.frame(
    vid = v[[vid]],
    T_in = v$T2,
    T_out = v$T1[k],
    port_id = dplyr::coalesce(v$port_to, v$port_from[k])
  )
  d <- d[!is.na(d$T_out) & !is.na(d$port_id), ]
  d$port_call_id <- as.numeric(stats::ave(
    seq_len(nrow(d)),
    d$vid,
    FUN = seq_along
  ))
  rownames(d) <- NULL
  d
}

test_that("port calls of runs and jepol lie between the method's voyages, a split at sea makes none", {
  for (m in c("runs", "jepol")) {
    v <- rb_create_voyages(tagged, method = m, by_year = FALSE, max_dur_h = 24)
    pc <- rb_create_port_calls(tagged, method = m, max_dur_h = 24)
    expect_gt(nrow(pc), 0)
    expect_true(all(
      pc$rule == m & pc$evidence == m & pc$basis == "reconstructed"
    ))
    expect_equal(pc$year, as.numeric(format(pc$T_in, "%Y")))
    want <- calls_between(as.data.frame(v))
    got <- as.data.frame(pc)[, c(
      "vid",
      "T_in",
      "T_out",
      "port_id",
      "port_call_id"
    )]
    rownames(got) <- NULL
    expect_equal(got, want[, names(got)], ignore_attr = TRUE, info = m)
  }
  # jepol splits voyages over max_dur_h at sea: those cuts have no port at either end, so fewer port calls
  # than gaps between voyages
  v <- rb_create_voyages(
    tagged,
    method = "jepol",
    by_year = FALSE,
    max_dur_h = 24
  )
  gaps <- nrow(v) - length(unique(v$vid))
  expect_lt(
    nrow(rb_create_port_calls(tagged, method = "jepol", max_dur_h = 24)),
    gaps
  )
})

test_that("port calls of runs: a lazy table gives the data frame's, and dwell refuses the other methods' arguments", {
  a <- rb_create_port_calls(tagged, method = "runs")
  b <- dplyr::collect(rb_create_port_calls(
    as_lazy(tagged),
    method = "runs"
  ))
  b <- b[order(b$vid, b$T_in), ]
  expect_equal(as.data.frame(b), as.data.frame(a), ignore_attr = TRUE)
  w <- range(a$T_in)
  w <- c(w[1], w[1] + (w[2] - w[1]) / 2)
  aw <- rb_create_port_calls(tagged, method = "runs", t_in_window = w)
  expect_true(all(aw$T_in >= w[1] & aw$T_in < w[2]))
  expect_lt(nrow(aw), nrow(a))
  expect_error(rb_create_port_calls(pings, ports, min_pings = 3), "other than")
  expect_error(rb_create_port_calls(pings), "needs `ports`")
})

test_that("port calls of iapesca lie between its voyages, columns renamed alike", {
  d <- ia_italy()
  v <- rb_create_voyages(
    d$p,
    method = "iapesca",
    ports = d$hp,
    by_year = FALSE,
    iapesca = list(crs_m = 32633),
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  pc <- rb_create_port_calls(
    d$p,
    method = "iapesca",
    ports = d$hp,
    iapesca = list(crs_m = 32633),
    vid = MMSI,
    time = datetime,
    lon = longitude,
    lat = latitude
  )
  expect_true("MMSI" %in% names(pc))
  expect_gt(nrow(pc), 0)
  want <- calls_between(as.data.frame(v), vid = "MMSI")
  got <- data.frame(
    vid = pc$MMSI,
    T_in = pc$T_in,
    T_out = pc$T_out,
    port_id = pc$port_id,
    port_call_id = pc$port_call_id
  )
  expect_equal(got, want[, names(got)], ignore_attr = TRUE)
})

test_that("rb_add_trip_time() takes each field from the best-ranked witness that holds it, and counts every trip", {
  t0 <- as.POSIXct("2024-03-01 06:00", tz = "UTC")
  trips <- data.frame(tid = 1:4)
  w <- data.frame(
    tid = c(1, 1, 2, 2, 4, 4),
    witness = c("track", "logbook", "logbook", "notice", "notice", "track"),
    time = t0 + c(0, 600, 0, 7200, 0, NA),
    place = c("A", "A", NA, "B", "C", "D")
  )
  rk <- c("track", "logbook", "notice")
  bs <- c(track = "reconstructed", logbook = "recorded", notice = "recorded")
  r <- rb_add_trip_time(trips, w, keys = "tid", rank = rk, basis = bs)
  r <- r[order(r$tid), ]
  expect_equal(r$T1, t0 + c(0, 0, NA, 0))
  expect_equal(r$T1_from, c("track", "logbook", NA, "notice"))
  expect_equal(r$T1_basis, c("reconstructed", "recorded", "none", "recorded"))
  expect_equal(r$T1_n, c(2, 2, 0, 1))
  expect_equal(r$T1_outcome, c("agree", "differ", "none", "one"))
  expect_equal(r$T1_spread_s, c(600, 7200, NA, 0))
  # the place on its own: trip 2's logbook holds no place, trip 4's track a place and no time
  expect_equal(r$pid1, c("A", "B", NA, "D"))
  expect_equal(r$pid1_from, c("track", "notice", NA, "track"))
  expect_equal(r$pid1_outcome, c("agree", "one", "none", "differ"))
  # a tighter tolerance, no basis given, other names
  r2 <- rb_add_trip_time(trips, w, keys = "tid", rank = rk, tolerance = 60, name = "T2", place_name = "pid2")
  r2 <- r2[order(r2$tid), ]
  expect_equal(r2$T2_outcome, c("differ", "differ", "none", "one"))
  expect_equal(r2$T2_basis, c(NA, NA, "none", NA))
  expect_true(all(c("pid2", "pid2_from") %in% names(r2)))
  # without places
  r3 <- rb_add_trip_time(trips, w[, c("tid", "witness", "time")], keys = "tid", rank = rk, place_name = NULL)
  expect_false("pid1" %in% names(r3))
})

test_that("rb_add_trip_time() gives the same on lazy tables, and refuses an unranked witness or a taken name", {
  t0 <- as.POSIXct("2024-03-01 06:00", tz = "UTC")
  trips <- data.frame(tid = 1:2, schema = "a")
  w <- data.frame(
    tid = c(1, 1, 2),
    schema = "a",
    witness = c("track", "logbook", "logbook"),
    time = t0 + c(0, 4000, 0),
    place = c("A", "B", "B")
  )
  rk <- c("track", "logbook")
  d <- rb_add_trip_time(trips, w, keys = c("tid", "schema"), rank = rk)
  l <- rb_add_trip_time(
    as_lazy(trips),
    as_lazy(w),
    keys = c("tid", "schema"),
    rank = rk
  )
  expect_s3_class(l, "tbl_lazy")
  l <- as.data.frame(dplyr::collect(l))
  expect_equal(l[order(l$tid), names(d)], d[order(d$tid), ], ignore_attr = TRUE)
  expect_error(rb_add_trip_time(trips, w, keys = c("tid", "schema"), rank = "track"), "logbook")
  expect_error(
    rb_add_trip_time(transform(trips, T1 = t0), w, keys = c("tid", "schema"), rank = rk),
    "already has"
  )
})

test_that("rb_add_trip_time(): a witness that knows only the day gives way to a time on that day, and agrees by date", {
  d0 <- as.POSIXct("2024-03-01", tz = "UTC")
  trips <- data.frame(tid = 1:3)
  w <- data.frame(
    tid = c(1, 1, 2, 3, 3),
    witness = c("logbook", "notice", "logbook", "logbook", "notice"),
    time = d0 + c(0, 14 * 3600, 0, 0, 86400 + 3600),
    place = c("A", "A", "B", "C", "C"),
    day = c(TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  r <- rb_add_trip_time(trips, w, keys = "tid", rank = c("logbook", "notice"), name = "T2", place_name = "pid2")
  r <- r[order(r$tid), ]
  # trip 1: the logbook's day, the notice's time on it; trip 2: the day alone; trip 3: the notice a day later
  expect_equal(r$T2, d0 + c(14 * 3600, 0, 0))
  expect_equal(r$T2_from, c("notice", "logbook", "logbook"))
  expect_equal(r$T2_day, c(FALSE, TRUE, TRUE))
  expect_equal(r$T2_outcome, c("agree", "one", "differ"))
  expect_equal(r$T2_spread_s, c(0, 0, 86400))
  expect_equal(r$pid2_from, c("logbook", "logbook", "logbook"))
})
