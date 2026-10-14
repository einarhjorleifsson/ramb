# The step registry (registry.R): every step function has a row, the rows hang together, and the check
# helper names the step to run first.

test_that("every exported step function has a row, and every row an export", {
  ns <- getNamespaceExports("ramb")
  rx <- "^rb_[a-z]+_(vessel|ping|trip|record|gear|fishing|effort|catch)(_|$)"
  steps <- grep(rx, ns, value = TRUE)
  steps <- steps[!grepl("_mfri$", steps)]
  reg <- .rb_registry()
  expect_setequal(reg$fn, steps)
})

test_that("rows hang together: kinds, stages, inputs and the step before", {
  reg <- .rb_registry()
  expect_true(all(reg$kind %in% c("step", "builder", "tool")))
  expect_true(all(
    reg$stage %in%
      c(
        "vessel",
        "ping",
        "trip",
        "record",
        "gear",
        "fishing",
        "effort",
        "catch"
      )
  ))
  # the stage word is in the name
  expect_true(all(mapply(
    function(f, s) grepl(paste0("_", s, "(_|$)"), f),
    reg$fn,
    reg$stage
  )))
  # the input is the function's first argument
  first <- vapply(
    reg$fn,
    function(f) names(formals(getExportedValue("ramb", f)))[1],
    ""
  )
  expect_equal(unname(first), reg$input)
  # the step before is itself a row
  expect_true(all(is.na(reg$after) | reg$after %in% reg$fn))
})

test_that("a missing column names the step that adds it; data frame and lazy table alike", {
  skip_if_not_installed("duckdbfs")
  t0 <- as.POSIXct("2024-03-01", tz = "UTC")
  p <- tibble::tibble(vid = 1L, time = t0 + 0:2)
  expect_snapshot(.rb_check_input(p[, "vid"], "rb_assign_effort"), error = TRUE)
  expect_snapshot(rb_assign_effort(p[, "vid"]), error = TRUE)
  # a column another step adds: the message names that step
  expect_snapshot(
    .rb_check_input(p, "rb_assign_effort", also = c(fishing = "fishing")),
    error = TRUE
  )
  expect_error(
    .rb_check_input(duckdbfs::as_dataset(p[, "vid"]), "rb_assign_effort"),
    "time"
  )
  # the user's own names are checked, and named in the message
  q <- dplyr::rename(p, boat = vid)
  expect_silent(.rb_check_input(q, "rb_assign_effort", cols = c(vid = "boat")))
  expect_error(.rb_check_input(q, "rb_assign_effort"), "`vid`")
})

test_that("types are checked on a lazy table's schema; a step's own columns mean it has run", {
  skip_if_not_installed("duckdbfs")
  p <- tibble::tibble(vid = 1L, time = "2024-03-01 00:00:00")
  expect_snapshot(
    .rb_check_input(p, "rb_assign_effort", types = c(time = "time")),
    error = TRUE
  )
  expect_error(
    .rb_check_input(
      duckdbfs::as_dataset(p),
      "rb_assign_effort",
      types = c(time = "time")
    ),
    "date-time"
  )
  t0 <- as.POSIXct("2024-03-01", tz = "UTC")
  done <- tibble::tibble(vid = 1L, time = t0, hours = 1)
  expect_error(
    .rb_check_input(done, "rb_assign_effort", fresh = TRUE),
    "already been run|already has"
  )
  expect_silent(.rb_check_input(done, "rb_assign_effort"))
})

test_that("the misuse suite: every checked step function refuses a table without its columns, by the registry", {
  checked <- c(
    "rb_flag_ping_invalid",
    "rb_flag_ping_duplicate",
    "rb_flag_ping_impossible",
    "rb_flag_ping_port",
    "rb_find_trip_stays",
    "rb_cut_trip_voyages",
    "rb_assign_trip",
    "rb_link_record_pings",
    "rb_assign_record",
    "rb_link_record_events",
    "rb_cut_record_stretches",
    "rb_assign_fishing",
    "rb_assign_effort",
    "rb_build_catch_units",
    "rb_share_catch",
    "rb_build_record_pings",
    "rb_link_record_sets"
  )
  reg <- .rb_registry()
  expect_true(all(checked %in% reg$fn))
  for (f in checked) {
    fn <- getExportedValue("ramb", f)
    args <- list(data.frame(nothing = 1))
    # a key with no default is an argument, not a column of the table
    if (
      "key" %in%
        names(formals(fn)) &&
        identical(formals(fn)$key, quote(expr = ))
    ) {
      args$key <- "id"
    }
    expect_error(do.call(fn, args), "must have a column", info = f)
  }
  # a wrong type is refused the same way
  expect_error(
    rb_assign_trip(data.frame(vid = 1L, time = "2024-01-01")),
    "date-time"
  )
})
