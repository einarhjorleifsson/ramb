# A unit square at the origin and a point inside (0.5, 0.5) and outside (5.5, 5.5)
.square <- sf::st_polygon(list(matrix(
  c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
  ncol = 2,
  byrow = TRUE
)))
.poly <- sf::st_sfc(.square, crs = 4326)
.pts <- sf::st_sfc(
  sf::st_point(c(0.5, 0.5)),
  sf::st_point(c(5.5, 5.5)),
  crs = 4326
)

test_that("rb_keep/drop_in_polygons() filter an sfc to the geometries inside/outside", {
  keep <- rb_keep_in_polygons(.pts, .poly)
  drop <- rb_drop_in_polygons(.pts, .poly)

  expect_s3_class(keep, "sfc")
  expect_s3_class(drop, "sfc")
  expect_length(keep, 1)
  expect_length(drop, 1)
  # keep and drop are complementary
  expect_equal(c(keep, drop), .pts)
})

test_that("rb_keep/drop_in_polygons() filter an sf data frame as before", {
  d <- sf::st_as_sf(
    data.frame(x = c(0.5, 5.5), y = c(0.5, 5.5), v = 1:2),
    coords = c("x", "y"),
    crs = 4326
  )

  expect_equal(rb_keep_in_polygons(d, .poly)$v, 1L)
  expect_equal(rb_drop_in_polygons(d, .poly)$v, 2L)
})
