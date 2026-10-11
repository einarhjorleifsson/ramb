t1 <- as.POSIXct(
  c("2007-06-04 14:03:36", "2007-06-08 07:40:43", NA),
  tz = "UTC"
)
t2 <- as.POSIXct(
  c("2007-06-07 05:25:45", "2007-06-09 10:02:21", NA),
  tz = "UTC"
)
secs <- as.numeric(difftime(t2, t1, units = "secs")) # 228129, 94898, NA

test_that("vector form returns plain numeric in the requested unit", {
  expect_equal(rb_calc_duration(t1, t2), secs / 3600)
  expect_equal(rb_calc_duration(t1, t2, unit = "secs"), secs)
  expect_equal(rb_calc_duration(t1, t2, unit = "mins"), secs / 60)
  expect_equal(rb_calc_duration(t1, t2, unit = "days"), secs / 86400)
  # unitless numeric, NA passed through
  expect_type(rb_calc_duration(t1, t2), "double")
  expect_identical(rb_calc_duration(t1, t2)[3], NA_real_)
})

test_that("vector form works inside dplyr::mutate()", {
  d <- tibble::tibble(T1 = t1, T2 = t2)
  out <- dplyr::mutate(d, h = rb_calc_duration(T1, T2))
  expect_equal(out$h, secs / 3600)
})

test_that("data frame form adds the column", {
  d <- tibble::tibble(T1 = t1, T2 = t2)
  out <- rb_calc_duration(d, T1, T2)
  expect_named(out, c("T1", "T2", "duration"))
  expect_equal(out$duration, secs / 3600)
  out <- rb_calc_duration(d, T1, T2, unit = "mins", name = "trip_mins")
  expect_named(out, c("T1", "T2", "trip_mins"))
  expect_equal(out$trip_mins, secs / 60)
})

test_that("lazy table form computes in the database", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- dplyr::copy_to(con, tibble::tibble(T1 = t1, T2 = t2), "duration_test")
  out <- rb_calc_duration(d, T1, T2) |> dplyr::collect()
  expect_equal(out$duration, secs / 3600)
  out <- rb_calc_duration(d, T1, T2, unit = "days", name = "dr_days") |>
    dplyr::collect()
  expect_equal(out$dr_days, secs / 86400)
})

test_that("table form refuses to overwrite an existing column", {
  d <- tibble::tibble(T1 = t1, T2 = t2)
  expect_error(rb_calc_duration(d, T1, T2, name = "T1"), "already exists")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  dl <- dplyr::copy_to(con, d, "duration_test")
  expect_error(rb_calc_duration(dl, T1, T2, name = "T2"), "already exists")
})

test_that("invalid unit is rejected on both table kinds", {
  d <- tibble::tibble(T1 = t1, T2 = t2)
  expect_error(rb_calc_duration(d, T1, T2, unit = "fortnight"))
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  dl <- dplyr::copy_to(con, d, "duration_test")
  expect_error(rb_calc_duration(dl, T1, T2, unit = "fortnight"), "secs")
})
