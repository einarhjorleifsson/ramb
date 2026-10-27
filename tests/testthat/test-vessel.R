# rb_link_vessel_tracks() (vessel.R): co-location of two feeds, against a plain computation in R, data frame = lazy.

vt_track <- function(
  id,
  t0,
  n,
  lon0,
  lat0,
  step_s = 60,
  dlon = 0.002,
  speed = 5
) {
  tibble::tibble(
    id = id,
    time = t0 + (0:(n - 1)) * step_s,
    lon = lon0 + dlon * (0:(n - 1)) * step_s / 60,
    lat = lat0,
    speed = speed
  )
}

# hits, opportunity and n_x as plain R loops over every pair of pings
vt_brute <- function(
  x,
  y,
  dt_s = 300,
  dist_m = 2000,
  min_speed = 1,
  min_hits = 3
) {
  x <- x[x$speed >= min_speed, ]
  y <- y[y$speed >= min_speed, ]
  x$per <- as.Date(format(x$time, "%Y-%m-01", tz = "UTC"))
  out <- list()
  for (j in unique(y$mmsi)) {
    yj <- y[y$mmsi == j, ]
    hit <- opp <- logical(nrow(x))
    for (i in seq_len(nrow(x))) {
      near_t <- abs(as.numeric(yj$time) - as.numeric(x$time[i])) <= dt_s
      opp[i] <- any(near_t)
      if (opp[i]) {
        d <- rb_calc_distance(
          lon1 = x$lon[i],
          lat1 = x$lat[i],
          lon2 = yj$lon[near_t],
          lat2 = yj$lat[near_t],
          method = "haversine"
        )
        hit[i] <- any(d <= dist_m)
      }
    }
    a <- stats::aggregate(
      cbind(hits = hit, opportunity = opp, n_x = 1) ~ unit + per,
      data = data.frame(unit = x$unit, per = x$per, hit = hit, opp = opp),
      FUN = sum
    )
    a$mmsi <- j
    out[[length(out) + 1]] <- a
  }
  out <- do.call(rbind, out)
  out <- out[out$hits >= min_hits, ]
  out$score <- out$hits / out$opportunity
  out[
    order(out$unit, out$per, out$mmsi),
    c("unit", "mmsi", "per", "n_x", "hits", "opportunity", "score")
  ]
}

test_that("co-location: a partner, a sparse feed, a crossing and a vessel in port", {
  t0 <- as.POSIXct("2024-03-01 06:00", tz = "UTC")
  x <- vt_track(1L, t0, 120, -22, 66.5)
  names(x)[1] <- "unit"
  y <- dplyr::bind_rows(
    vt_track(111, t0 + 20, 120, -22, 66.5 + 0.0005), # the unit's own transponder, 55 m and 20 s off
    vt_track(222, t0 + 30, 120, -22, 66.5 + 0.009), # its pair-trawl partner, 1 km abreast
    vt_track(444, t0, 6, -22, 66.5, step_s = 1200), # a sparse feed of the same vessel
    tibble::tibble(
      # crosses the unit's track at 8 knots, an hour in
      id = 333,
      time = t0 + 1800 + (0:60) * 60,
      lon = -22 + 0.002 * 60,
      lat = 66.5 + seq(-0.133, 0.133, length.out = 61),
      speed = 8
    ),
    vt_track(999, t0, 120, -22, 67) # 55 km away all along
  )
  names(y)[1] <- "mmsi"
  # in port: the unit and a neighbour lie still side by side; not evidence
  x <- dplyr::bind_rows(
    x,
    tibble::tibble(
      unit = 1L,
      time = t0 + 86400 + (0:30) * 60,
      lon = -21,
      lat = 66,
      speed = 0
    )
  )
  y <- dplyr::bind_rows(
    y,
    tibble::tibble(
      mmsi = 555,
      time = t0 + 86400 + (0:30) * 60,
      lon = -21,
      lat = 66,
      speed = 0
    )
  )
  r <- rb_link_vessel_tracks(x, y, id_x = "unit", id_y = "mmsi")
  expect_equal(r$mmsi, c(111, 222, 444, 333)) # 999 is never near, 555 lies still
  expect_equal(r$n_x, rep(120L, 4), ignore_attr = TRUE)
  expect_equal(r$score[1:3], c(1, 1, 1))
  expect_true(r$score[4] < 0.5) # near for a quarter of an hour of the hour it was there
  expect_equal(r$hits[3], 6 * 11 - 5, ignore_attr = TRUE) # x pings within 5 min of one of 6 pings 20 min apart
  expect_equal(r$mmsi_other, c(222, 111, 111, 111))
  expect_equal(r$score_other, c(1, 1, 1, 1))
  expect_equal(r$strong, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(r$period, rep(as.Date("2024-03-01"), 4))
  expect_true(r$median_m[2] > 900 && r$median_m[2] < 1100)
  # a key pair to check that has no hits is still scored
  k <- rb_link_vessel_tracks(
    x,
    y,
    "unit",
    "mmsi",
    pairs = data.frame(unit = 1L, mmsi = 999)
  )
  expect_equal(k$hits[k$mmsi == 999], 0, ignore_attr = TRUE)
  expect_equal(k$opportunity[k$mmsi == 999], 120, ignore_attr = TRUE)
  expect_equal(k$score[k$mmsi == 999], 0)
  # every ping counts without the speed rule: the neighbour in port then links
  a <- rb_link_vessel_tracks(
    x,
    y,
    "unit",
    "mmsi",
    min_speed = NULL,
    period = NULL
  )
  expect_true(555 %in% a$mmsi)
  expect_true(all(is.na(a$period)))
  l <- dplyr::collect(rb_link_vessel_tracks(
    as_lazy(x),
    as_lazy(y),
    "unit",
    "mmsi"
  ))
  l <- l[order(l$unit, l$period, l$rank), ]
  expect_equal(as.data.frame(l), as.data.frame(r), ignore_attr = TRUE)
})

test_that("co-location equals a plain computation over every pair of pings", {
  set.seed(7)
  t0 <- as.POSIXct("2024-03-31 20:00", tz = "UTC") # the tracks cross into April
  walk <- function(id, n, lon0, lat0) {
    tibble::tibble(
      id = id,
      time = t0 + cumsum(stats::runif(n, 20, 400)),
      lon = lon0 + cumsum(stats::rnorm(n, 0, 0.01)),
      lat = lat0 + cumsum(stats::rnorm(n, 0, 0.004)),
      speed = stats::runif(n, 0, 6)
    )
  }
  x <- dplyr::bind_rows(lapply(1:3, function(i) walk(i, 150, -20, 70)))
  names(x)[1] <- "unit"
  y <- dplyr::bind_rows(lapply(1:5, function(i) walk(100 + i, 200, -20, 70)))
  names(y)[1] <- "mmsi"
  y <- dplyr::bind_rows(
    y,
    dplyr::mutate(x[x$unit == 2, ], unit = NULL, mmsi = 102, time = time + 30)
  )
  b <- vt_brute(x, y)
  r <- rb_link_vessel_tracks(x, y, "unit", "mmsi")
  expect_true(nrow(b) > 3)
  r <- r[order(r$unit, r$period, r$mmsi), ]
  expect_equal(
    as.data.frame(r[, c(
      "unit",
      "mmsi",
      "period",
      "n_x",
      "hits",
      "opportunity",
      "score"
    )]),
    b,
    ignore_attr = TRUE
  )
  # ranks and the best other id, from the result itself
  for (g in split(r, list(r$unit, r$period), drop = TRUE)) {
    g <- g[order(g$rank), ]
    expect_equal(g$rank, seq_len(nrow(g)))
    expect_true(all(diff(g$score) <= 0))
    if (nrow(g) > 1) {
      expect_equal(g$mmsi_other, c(g$mmsi[2], rep(g$mmsi[1], nrow(g) - 1)))
    }
  }
  # the same names on both sides get suffixes
  names(y)[1] <- "unit"
  s <- rb_link_vessel_tracks(x, y, "unit", "unit")
  expect_true(all(c("unit_x", "unit_y", "unit_y_other") %in% names(s)))
})
