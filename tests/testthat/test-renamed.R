# The old names of fishycode plan 032 (R/renamed.R): each warns once per session, takes the old arguments and
# gives the old result.

test_that("every old name is an export that warns once and names its new one", {
  ns <- getNamespaceExports("ramb")
  expect_true(all(names(.rb_renamed_from) %in% ns))
  expect_true(all(.rb_renamed_from %in% ns))
  .rb_reset_renamed()
  expect_warning(
    rb_create_timestamp("14/09/2025", "15:30:00"),
    "rb_add_timestamp"
  )
  expect_silent(rb_create_timestamp("14/09/2025", "15:30:00"))
  .rb_reset_renamed()
})

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

test_that("the step-1 aliases take the old arguments and give the old result", {
  .rb_reset_renamed()
  calls <- rb_create_port_calls(pings, ports, min_dwell_h = 2)
  ms <- data.frame(port_id = unique(calls$port_id), min_stay_h = 24)
  expect_warning(
    stays24 <- rb_find_trip_stays(pings, ports, min_stay_h = 2, min_stay = ms),
    "min_dwell_h"
  )
  expect_equal(
    stays24,
    rb_create_port_calls(
      pings,
      ports,
      min_dwell_h = 2,
      min_dwell = dplyr::rename(ms, min_dwell_h = min_stay_h)
    ) |>
      dplyr::select(-year, -port_call_id)
  )
  stays <- rb_find_trip_stays(pings, ports, min_stay_h = 2)
  expect_equal(stays, dplyr::select(calls, -year, -port_call_id))
  expect_warning(
    v_old <- rb_cut_trip_voyages(pings, stays, method = "stays", min_pings = 10),
    "dwell"
  )
  v <- rb_create_voyages(pings, calls, method = "dwell", min_pings = 10)
  expect_equal(v_old, v)
  st <- dplyr::mutate(stays, stay_id = dplyr::row_number(), .by = vid)
  expect_warning(
    a_old <- rb_assign_trip(pings, v, stays = st),
    "port_calls"
  )
  a <- rb_add_voyages(pings, v, port_calls = calls)
  # the rows come back in DuckDB's order
  a_old <- a_old[order(a_old$vid, a_old$time, a_old$lon, a_old$lat), ]
  a <- a[order(a$vid, a$time, a$lon, a$lat), ]
  expect_equal(a_old$stay_port_id, a$port_call_port_id)
  expect_equal(a_old$stay_id, a$port_call_id)
  expect_equal(a_old$voyage_id, a$voyage_id)
  expect_false(any(c("port_call_id", "port_call_port_id") %in% names(a_old)))
  .rb_reset_renamed()
})

test_that("the other aliases pass through to the new functions", {
  .rb_reset_renamed()
  ve <- tibble::tibble(
    vid = c("A", "A"),
    time = as.POSIXct(c("2022-01-01 10:00", "2022-01-02 10:00"), tz = "UTC")
  )
  le <- tibble::tibble(
    vid = "A",
    tid = 1L,
    T1 = as.POSIXct("2022-01-01 09:00", tz = "UTC"),
    T2 = as.POSIXct("2022-01-01 11:00", tz = "UTC")
  )
  expect_warning(
    old <- rb_link_trip_logbook(ve, le),
    "rb_add_logbook_trip"
  )
  expect_equal(old, rb_add_logbook_trip(ve, le))
  # the arguments pass by name and by position, column names unquoted
  le2 <- dplyr::rename(le, trip = tid)
  expect_equal(
    suppressWarnings(rb_link_trip_logbook(ve, le2, trip_id = trip)),
    rb_add_logbook_trip(ve, le2, trip_id = trip)
  )
  for (old in setdiff(
    names(.rb_renamed_from),
    c(
      "rb_find_trip_stays",
      "rb_cut_trip_voyages",
      "rb_assign_trip",
      "rb_create_timestamp"
    )
  )) {
    body <- deparse(body(getExportedValue("ramb", old)))
    expect_true(
      any(grepl(paste0(.rb_renamed_from[[old]], "("), body, fixed = TRUE)),
      info = old
    )
  }
  .rb_reset_renamed()
})
