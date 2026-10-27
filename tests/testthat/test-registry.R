# The step registry (registry.R): every step function has a row, the rows hang together, and the check
# helper names the step to run first.

test_that("every exported step function has a row, and every row an export", {
  ns <- setdiff(getNamespaceExports("ramb"), names(.rb_renamed_from))
  rx <- "^rb_[a-z]+_([a-z]+_)?(vessel|ping|trip|record|gear|fishing|effort|catch|voyages|port_calls)(_|$)"
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
  # the stage word is in the name; step 1 may name its objects instead (fishycode plan 032)
  word <- function(s) if (s == "trip") "(trip|voyages|port_calls)" else s
  expect_true(all(mapply(
    function(f, s) grepl(paste0("_", word(s), "(_|$)"), f),
    reg$fn,
    reg$stage
  )))
  # the verb says the shape: a builder creates, links or shares a table; a step adds or flags columns
  verb <- sub("^rb_([a-z]+)_.*", "\\1", reg$fn)
  expect_true(all(verb[reg$kind == "step"] %in% c("add", "flag")))
  expect_true(all(
    verb[reg$kind == "builder"] %in%
      c("create", "link", "share", "add", "limit")
  ))
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
  t0 <- as.POSIXct("2024-03-01", tz = "UTC")
  p <- tibble::tibble(vid = 1L, time = t0 + 0:2)
  expect_snapshot(.rb_check_input(p[, "vid"], "rb_add_effort"), error = TRUE)
  expect_snapshot(rb_add_effort(p[, "vid"]), error = TRUE)
  # a column another step adds: the message names that step
  expect_snapshot(
    .rb_check_input(p, "rb_add_effort", also = c(fishing = "fishing")),
    error = TRUE
  )
  expect_error(
    .rb_check_input(as_lazy(p[, "vid"]), "rb_add_effort"),
    "time"
  )
  # the user's own names are checked, and named in the message
  q <- dplyr::rename(p, boat = vid)
  expect_silent(.rb_check_input(q, "rb_add_effort", cols = c(vid = "boat")))
  expect_error(.rb_check_input(q, "rb_add_effort"), "`vid`")
})

test_that("types are checked on a lazy table's schema; a step's own columns mean it has run", {
  p <- tibble::tibble(vid = 1L, time = "2024-03-01 00:00:00")
  expect_snapshot(
    .rb_check_input(p, "rb_add_effort", types = c(time = "time")),
    error = TRUE
  )
  expect_error(
    .rb_check_input(
      as_lazy(p),
      "rb_add_effort",
      types = c(time = "time")
    ),
    "date-time"
  )
  t0 <- as.POSIXct("2024-03-01", tz = "UTC")
  done <- tibble::tibble(vid = 1L, time = t0, hours = 1)
  expect_error(
    .rb_check_input(done, "rb_add_effort", fresh = TRUE),
    "already been run|already has"
  )
  expect_silent(.rb_check_input(done, "rb_add_effort"))
})

test_that("the misuse suite: every checked step function refuses a table without its columns, by the registry", {
  checked <- c(
    "rb_flag_ping_invalid",
    "rb_flag_ping_duplicate",
    "rb_flag_ping_impossible",
    "rb_flag_ping_port",
    "rb_create_port_calls",
    "rb_create_voyages",
    "rb_add_voyages",
    "rb_link_record_pings",
    "rb_add_record",
    "rb_link_record_events",
    "rb_create_record_stretches",
    "rb_add_fishing",
    "rb_add_effort",
    "rb_add_catch_units",
    "rb_share_catch",
    "rb_create_record_pings",
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
    rb_add_voyages(data.frame(vid = 1L, time = "2024-01-01")),
    "date-time"
  )
})
