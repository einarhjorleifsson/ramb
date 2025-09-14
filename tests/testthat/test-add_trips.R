test_that("rb_add_trips assigns trip ids correctly with robust defaults and warnings", {
  # Minimal mock VMS data with default column names
  ve <- tibble::tibble(
    vid = c("A", "A", "A", "B"),
    time = as.POSIXct(c("2022-01-01 10:00:00", "2022-01-01 15:00:00",
                        "2022-01-02 08:00:00", "2022-01-01 12:00:00"))
  )
  
  # Minimal mock logbook data with default column names
  le <- tibble::tibble(
    vid = c("A", "A", "B"),
    tid = c(101, 102, 201),
    T1 = as.POSIXct(c("2022-01-01 09:00:00", "2022-01-02 07:00:00", "2022-01-01 11:00:00")),
    T2 = as.POSIXct(c("2022-01-01 16:00:00", "2022-01-02 09:00:00", "2022-01-01 13:00:00"))
  )
  
  # Run function (should not throw warnings for missing columns)
  expect_warning(
    out <- suppressWarnings(rb_add_trips(ve, le)),
    regexp = NA # No warning expected for missing columns here
  )
  
  # Join should assign tid 101 to first two, tid 102 to third, tid 201 to fourth
  expect_equal(out$tid, c(101, 101, 102, 201))
  # Departure and arrival columns should not be present
  expect_false("T1" %in% names(out))
  expect_false("T2" %in% names(out))
  # Output should have same number of rows as input ve
  expect_equal(nrow(out), nrow(ve))
  
  # Test warning on missing columns
  ve_bad <- ve[, -1, drop=FALSE] # drop 'vid'
  expect_warning(
    rb_add_trips(ve_bad, le),
    "Missing the following columns in VMS data"
  )
  
  le_bad <- le[, -1, drop=FALSE] # drop 'vid'
  expect_warning(
    rb_add_trips(ve, le_bad),
    "Missing the following columns in logbook data"
  )
  
  # Test warning if no trip ids matched
  ve_nomatch <- tibble::tibble(
    vid = "C", time = as.POSIXct("2022-01-01 10:00:00")
  )
  expect_warning(
    rb_add_trips(ve_nomatch, le),
    "No trip IDs were matched to the VMS data"
  )
})