test_that("robust method encodes interior points", {
  expect_identical(rb_encode_ices_rectangle(-10.5, 62.5), "54D9")
  expect_identical(rb_encode_ices_rectangle(100, 10), NA_character_)
  expect_identical(
    rb_encode_ices_rectangle(c(-10.5, 15.2, 100), c(62.5, 54.9, 10)),
    c("54D9", "38G5", NA_character_)
  )
})

test_that("robust method handles 10-degree boundaries", {
  # floor-binning puts -10 in cell [-10, -9), midpoint -9.5, band E
  expect_identical(rb_encode_ices_rectangle(-10, 62.5), "54E0")
  expect_identical(
    rb_encode_ices_rectangle(-10, 62.5, snap_to_midpoint = FALSE),
    "54E0"
  )
  expect_identical(
    rb_encode_ices_rectangle(-20, 62.5, snap_to_midpoint = FALSE),
    "54D0"
  )
})

test_that("vmstools method reproduces the legacy algorithm", {
  expect_identical(
    rb_encode_ices_rectangle(-10.5, 62.5, method = "vmstools"),
    "54D9"
  )
  # legacy band boundaries are inconsistent: right-inclusive A-D,
  # right-exclusive E-H, so the methods diverge on 10-degree longitudes
  expect_identical(
    rb_encode_ices_rectangle(-10, 62.5, method = "vmstools"),
    "54D9"
  )
  expect_identical(
    rb_encode_ices_rectangle(-20, 62.5, method = "vmstools"),
    "54C9"
  )
})

test_that("input validation", {
  expect_snapshot(
    rb_encode_ices_rectangle(1:3, 1:2),
    error = TRUE
  )
  expect_snapshot(
    rb_encode_ices_rectangle("a", 1),
    error = TRUE
  )
})
