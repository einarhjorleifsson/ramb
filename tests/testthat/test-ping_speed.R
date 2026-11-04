t0 <- as.POSIXct("2024-01-01", tz = "UTC")

test_that("the speed is the central difference, in knots, with its span", {
  d <- data.frame(
    vid = 1,
    time = t0 + 60 * 0:3,
    lon = -20,
    lat = 64 + c(0, 0.01, 0.02, 0.03)
  )
  s <- rb_add_ping_speed(d)
  m <- rb_calc_distance(-20, 64, -20, 64.02, method = "geodesic")
  expect_equal(s$speed_derived_span_s, c(60, 120, 120, 60))
  expect_equal(s$speed_derived[2], m / 120 * 1.9438444924406, tolerance = 1e-3)
  expect_equal(s$speed_derived_basis, rep("derived", 4))
  # the ends take their one leg
  expect_equal(
    s$speed_derived[1],
    rb_calc_distance(-20, 64, -20, 64.01, method = "geodesic") /
      60 *
      1.9438444924406,
    tolerance = 1e-3
  )
})

test_that("a flagged ping gets no speed and is no neighbour; a copy is a neighbour but gets no speed", {
  d <- data.frame(
    vid = 1,
    time = t0 + 60 * 0:4,
    lon = -20,
    lat = 64 + c(0, 0.01, 0.5, 0.03, 0.04),
    ping_flag = c(NA, NA, "forward", NA, NA)
  )
  s <- rb_add_ping_speed(d)
  expect_true(is.na(s$speed_derived[3]))
  expect_equal(s$speed_derived_basis[3], "none")
  # ping 2 spans ping 1 to ping 4, skipping the bad position
  expect_equal(s$speed_derived_span_s[2], 180)
  d$ping_flag[3] <- "duplicate"
  s <- rb_add_ping_speed(d)
  expect_true(is.na(s$speed_derived[3]))
  expect_equal(s$speed_derived_span_s[2], 120)
  expect_equal(s$speed_derived_span_s[4], 120)
})

test_that("each stream is its own track", {
  d <- data.frame(
    vid = 1,
    time = t0 + c(0, 30, 60, 90, 120, 150),
    lon = -20,
    lat = 64 + c(0, 0.0101, 0.01, 0.0201, 0.02, 0.0301),
    feed = c("a", "b", "a", "b", "a", "b")
  )
  s <- rb_add_ping_speed(d, stream = feed)
  expect_equal(s$speed_derived_span_s, c(60, 60, 120, 120, 60, 60))
  one <- rb_add_ping_speed(d)
  expect_equal(one$speed_derived_span_s[3], 60)
})

test_that("a ping with no vessel or position gets none; one alone in its stream gets none", {
  d <- data.frame(
    vid = c(1, 1, NA, 2),
    time = t0 + 60 * 0:3,
    lon = c(-20, NA, -20, -20),
    lat = 64
  )
  s <- rb_add_ping_speed(d)
  expect_equal(s$speed_derived_basis, rep("none", 4))
})

test_that("a data frame and a lazy table give the same speeds", {
  set.seed(1)
  d <- data.frame(
    vid = rep(1:2, each = 50),
    time = t0 + rep(sort(sample(0:20000, 50)), 2),
    lon = -20 + cumsum(runif(100, -0.01, 0.01)),
    lat = 64 + cumsum(runif(100, -0.005, 0.005)),
    ping_flag = sample(c(NA, NA, NA, "spike", "duplicate"), 100, replace = TRUE)
  )
  df <- rb_add_ping_speed(d)
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  duckdb::duckdb_register(con, "d", d)
  lz <- dplyr::collect(rb_add_ping_speed(dplyr::tbl(con, "d")))
  lz <- lz[order(lz$vid, lz$time), ]
  df <- df[order(df$vid, df$time), ]
  expect_equal(lz$speed_derived, df$speed_derived)
  expect_equal(lz$speed_derived_basis, df$speed_derived_basis)
})

test_that("the ellipsoid's local radii are within 0.1 % of Karney's geodesic", {
  # legs from 10 m to 20 km (a speed's span at 10 s to 1 h), every bearing, from the equator to 80 degrees
  g <- expand.grid(
    lat = c(0, 30, 60, 64, 72, 80),
    brg = seq(0, 315, 45),
    m = c(10, 1e3, 2e4)
  )
  g$lat1 <- g$lat + g$m * cos(g$brg * pi / 180) / 111e3
  g$lon1 <- -20 + g$m * sin(g$brg * pi / 180) / (111e3 * cos(g$lat * pi / 180))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  duckdb::duckdb_register(
    con,
    "g",
    data.frame(lon0 = -20, lat0 = g$lat, lon1 = g$lon1, lat1 = g$lat1)
  )
  e <- DBI::dbGetQuery(
    con,
    paste("SELECT", ramb:::.rb_ellipsoid_m_sql(), "AS m FROM g")
  )$m
  k <- rb_calc_distance(-20, g$lat, g$lon1, g$lat1, method = "geodesic")
  expect_lt(max(abs(e / k - 1)), 1e-3)
})
