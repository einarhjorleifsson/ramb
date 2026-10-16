# The example data (example.R): parquet in inst/extdata, read lazily; a Git LFS pointer is named as such.

test_that("example data are listed and read lazily, in the labelled tracks' columns", {
  skip_if_not_installed("duckdbfs")
  nm <- rb_read_example()
  expect_true(all(
    c("wkssfgeo2_it_pots", "wkssfgeo_pt_gps", "ird_mg_gillnet") %in% nm
  ))
  for (n in c("wkssfgeo2_it_pots", "wkssfgeo_pt_gps", "ird_mg_gillnet")) {
    x <- rb_read_example(n)
    expect_s3_class(x, "tbl_lazy")
    d <- dplyr::collect(x)
    expect_named(
      d,
      c("vid", "trip", "time", "lon", "lat", "speed", "gear", "label", "op")
    )
    expect_s3_class(d$time, "POSIXct")
    expect_setequal(unique(stats::na.omit(d$op)), c("set", "haul"))
  }
  expect_error(rb_read_example("nope"), "must be one of")
})

test_that("a Git LFS pointer is told from a parquet file", {
  f <- tempfile(fileext = ".parquet")
  writeLines(
    c(
      "version https://git-lfs.github.com/spec/v1",
      "oid sha256:0000",
      "size 1"
    ),
    f
  )
  expect_true(.rb_is_lfs_pointer(f))
  ex <- system.file("extdata", "wkssfgeo2_it_pots.parquet", package = "ramb")
  expect_false(.rb_is_lfs_pointer(ex))
})
