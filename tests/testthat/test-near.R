# The proximity engine (near.R): the blocked join finds exactly the pairs a full cross join finds, at any latitude,
# across +-180 and with a time tolerance.

near_brute <- function(a, b, dist_m, dt_s = NULL, by = NULL) {
  g <- expand.grid(ia = seq_len(nrow(a)), ib = seq_len(nrow(b)))
  d <- rb_calc_distance(
    lon1 = a$lon[g$ia],
    lat1 = a$lat[g$ia],
    lon2 = b$lon[g$ib],
    lat2 = b$lat[g$ib],
    method = "haversine"
  )
  ok <- !is.na(d) & d <= dist_m
  if (!is.null(dt_s)) {
    ok <- ok &
      abs(as.numeric(b$time[g$ib]) - as.numeric(a$time[g$ia])) <= dt_s
  }
  if (!is.null(by)) {
    ok <- ok & a[[by]][g$ia] == b[[by]][g$ib]
  }
  g <- g[ok, ]
  sort(paste(a$rb_i[g$ia], b$rb_i[g$ib]))
}

near_sql <- function(a, b, dist_m, dt_s = NULL, by = character(0)) {
  con <- duckdbfs::cached_connection()
  na <- ramb:::.rb_register(con, a)
  nb <- ramb:::.rb_register(con, b)
  on.exit(ramb:::.rb_unregister(con))
  sql <- sprintf(
    "WITH a AS (SELECT * FROM %s), b AS (SELECT * FROM %s), p AS (%s) SELECT * FROM p",
    na,
    nb,
    ramb:::.rb_near_pairs("a", "b", by, dist_m, dt_s, con)
  )
  r <- DBI::dbGetQuery(con, sql)
  expect_equal(anyDuplicated(paste(r$rb_ia, r$rb_ib)), 0)
  sort(paste(r$rb_ia, r$rb_ib))
}

near_pts <- function(n, lon0, lat0, spread_m, seed) {
  set.seed(seed)
  dlat <- spread_m / 111195
  dlon <- spread_m / (111195 * max(cos(lat0 * pi / 180), 1e-4))
  lon <- lon0 + stats::runif(n, -dlon, dlon)
  lon <- ((lon + 180) %% 360) - 180
  data.frame(
    rb_i = seq_len(n),
    lon = lon,
    lat = pmin(90, pmax(-90, lat0 + stats::runif(n, -dlat, dlat))),
    time = as.POSIXct("2024-01-01", tz = "UTC") + stats::runif(n, 0, 3 * 3600),
    g = sample(1:2, n, TRUE)
  )
}

test_that("blocked pairs equal the full cross join, at any latitude and across +-180", {
  skip_if_not_installed("duckdbfs")
  for (s in list(
    c(0, 0),
    c(-20, 65),
    c(-20, 69.9),
    c(10, 80),
    c(0, 89.95),
    c(43.7, -23.4),
    c(180, 60),
    c(-179.99, -45)
  )) {
    a <- near_pts(300, s[1], s[2], 8000, 1)
    b <- near_pts(400, s[1], s[2], 8000, 2)
    expect_equal(
      near_sql(a, b, 2000),
      near_brute(a, b, 2000),
      info = paste(s, collapse = " ")
    )
    expect_equal(
      near_sql(a, b, 550),
      near_brute(a, b, 550),
      info = paste(s, collapse = " ")
    )
    expect_equal(
      near_sql(a, b, 2000, dt_s = 300),
      near_brute(a, b, 2000, dt_s = 300),
      info = paste(s, collapse = " ")
    )
    expect_equal(
      near_sql(a, b, 2000, by = "g"),
      near_brute(a, b, 2000, by = "g"),
      info = paste(s, collapse = " ")
    )
  }
})

test_that("a pair across the antimeridian and a pair across a pole are found", {
  skip_if_not_installed("duckdbfs")
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  a <- data.frame(
    rb_i = 1:2,
    lon = c(179.999, 0),
    lat = c(60, 89.999),
    time = t0
  )
  b <- data.frame(
    rb_i = 1:2,
    lon = c(-179.999, 180),
    lat = c(60, 89.999),
    time = t0 + 60
  )
  expect_equal(near_sql(a, b, 500, dt_s = 60), c("1 1", "2 2"))
  # a NULL coordinate meets nothing
  a$lat[1] <- NA
  expect_equal(near_sql(a, b, 500), "2 2")
})
