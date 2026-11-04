# Step 0 flaggers (fishycode plan 013, phase 2): same labels as the procedure they replace, on data
# frames and on lazy DuckDB tables alike.

# Two feeds: one Danish vessel as "stk", every third ping copied 1 s later as "astd" (a cross-feed copy),
# two placeholders (lon == lat), and two far-off jumps.
two_feeds <- function() {
  a <- head(dplyr::filter(dansk, vessel_id == dansk$vessel_id[2]), 400) |>
    dplyr::transmute(vid = 1L, time = time_stamp, lon, lat, provider = "stk")
  b <- a[seq(1, nrow(a), by = 3), ] |>
    dplyr::mutate(time = time + 1, lon = lon + 1e-6, provider = "astd")
  bad <- a[c(50, 200), ] |> dplyr::mutate(lon = lat, time = time + 2)
  jump <- a[c(100, 300), ] |> dplyr::mutate(lon = lon + 2, time = time + 3)
  d <- dplyr::bind_rows(a, b, bad, jump)
  d$rid <- seq_len(nrow(d))
  d[sample(nrow(d)), ]
}

# the procedure of fishycode's curate/ais_ping_clean.R before plan 013
old_procedure <- function(d, priority) {
  bad <- d$lon == d$lat
  dup <- logical(nrow(d))
  dup[!bad] <- with(
    d[!bad, ],
    .rb_whack_duplicates(vid, time, lon, lat, provider, priority = priority)
  )
  keep <- !bad & !dup
  out <- dplyr::bind_rows(
    .whack_clean_df(d[keep, ]),
    d[bad, ] |> dplyr::mutate(whack_stage = "invalid"),
    d[dup, ] |> dplyr::mutate(whack_stage = "duplicate")
  )
  out[order(out$rid), c("rid", "whack_stage")]
}

chain <- function(x, priority) {
  x |>
    rb_flag_ping_invalid(rules = "lon_eq_lat") |>
    rb_flag_ping_duplicate(priority = priority) |>
    rb_flag_ping_impossible(method = "clean")
}

test_that("the chain gives the labels of the procedure it replaces", {
  set.seed(1)
  d <- two_feeds()
  old <- suppressWarnings(old_procedure(d, c("stk", "astd")))
  new <- chain(d, c("stk", "astd"))
  new <- new[order(new$rid), ]
  expect_equal(nrow(new), nrow(d))
  expect_equal(new$ping_flag, old$whack_stage)
  expect_true(all(c("invalid", "duplicate") %in% new$ping_flag))
})

test_that("a lazy DuckDB table gives the same labels as a data frame", {
  set.seed(2)
  d <- two_feeds()
  df <- chain(d, c("stk", "astd"))
  lz <- chain(as_lazy(d), c("stk", "astd"))
  expect_s3_class(lz, "tbl_lazy")
  lz <- dplyr::collect(lz)
  expect_equal(lz$ping_flag[order(lz$rid)], df$ping_flag[order(df$rid)])
  for (m in c("sda", "forward", "local", "fwdbwd", "sequential")) {
    a <- suppress_withdrawn(rb_flag_ping_impossible(d, method = m))
    b <- dplyr::collect(suppress_withdrawn(rb_flag_ping_impossible(
      as_lazy(d),
      method = m
    )))
    expect_equal(b$ping_flag[order(b$rid)], a$ping_flag[order(a$rid)], info = m)
  }
})

test_that("pings of one second at two places do not make the flags depend on row order", {
  # Until 2026-10-13 forward, fwdbwd and sequential sorted by (vid, time) only, and the
  # fwdbwd SQL ordered its window by time only: on this track, reversing the rows changed
  # 5, 2 and 5 flags, and on one month of Icelandic AIS (3.3 M pings) forward changed 22-25
  # and fwdbwd 1-4. lon and lat now break the tie everywhere, as .rb_whack_sda() always did.
  n <- 60
  d <- data.frame(
    vid = 1L,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * (seq_len(n) - 1),
    lon = -20 + 0.003 * seq_len(n),
    lat = 64 + 0.001 * seq_len(n)
  )
  k <- c(9, 16, 40, 43)
  d <- rbind(
    d,
    data.frame(
      vid = 1L,
      time = d$time[k],
      lon = d$lon[k] + c(0.0084, -0.03, -0.0164, 0.0062),
      lat = d$lat[k] + c(0.0052, 0.0005, 0.0002, 0.0014)
    )
  )
  d$key <- seq_len(nrow(d))
  rev_d <- d[rev(seq_len(nrow(d))), ]
  flags <- function(x, m) {
    r <- dplyr::collect(suppress_withdrawn(rb_flag_ping_impossible(
      x,
      method = m
    )))
    r$ping_flag[order(r$key)]
  }
  for (m in c("clean", "sda", "forward", "local", "fwdbwd", "sequential")) {
    a <- flags(d, m)
    expect_identical(flags(rev_d, m), a, info = m)
    expect_identical(flags(as_lazy(rev_d), m), a, info = m)
  }
  # the SQL branch of the fwdbwd engine (lazy input) agrees with its data-frame branch
  sql <- dplyr::collect(.rb_whack_fwdbwd(as_lazy(rev_d)))
  df <- .rb_whack_fwdbwd(d)
  expect_identical(
    sql$whack[order(sql$key)] %in% TRUE,
    df$whack[order(df$key)]
  )
})

test_that("flags already set are kept, rows are never dropped, other column names work", {
  d <- data.frame(
    vid = 1,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * 0:5,
    lon = c(-20, 64, -20.01, 5, -20.02, -20.03),
    lat = 64
  )
  d$ping_flag <- c(NA, NA, "mine", NA, NA, NA)
  r <- rb_flag_ping_impossible(rb_flag_ping_invalid(d))
  expect_equal(nrow(r), nrow(d))
  expect_equal(r$ping_flag[2:3], c("invalid", "mine"))
  e <- dplyr::rename(d, vessel_id = vid, time_stamp = time, x = lon, y = lat)
  r2 <- rb_flag_ping_impossible(
    rb_flag_ping_invalid(e, lon = x, lat = y),
    vid = vessel_id,
    time = time_stamp,
    lon = x,
    lat = y
  )
  expect_equal(names(r2), c(names(e), "ping_flag_basis"))
  expect_equal(r2$ping_flag, r$ping_flag)
  # a flag set before the basis existed keeps none; the new ones say what decided them
  expect_equal(r$ping_flag_basis[2:3], c("recorded", NA))
})

test_that("the flag chain does not warn", {
  d <- data.frame(
    vid = 1,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * 0:3,
    lon = -20 - 0:3 / 100,
    lat = 64
  )
  expect_no_warning(rb_flag_ping_impossible(d))
})

test_that("the port tag: data frame = lazy table, one row per ping, the smaller of two polygons wins", {
  sq <- function(x0, y0, w) {
    sf::st_polygon(list(rbind(
      c(x0, y0),
      c(x0 + w, y0),
      c(x0 + w, y0 + w),
      c(x0, y0 + w),
      c(x0, y0)
    )))
  }
  h <- sf::st_sf(
    port_id = c("big", "small", "far"),
    code = c("B", "S", "F"),
    geometry = sf::st_sfc(
      sq(0, 0, 1),
      sq(0.2, 0.2, 0.1),
      sq(5, 5, 1),
      crs = 4326
    )
  )
  d <- data.frame(
    vid = 1L,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * 0:3,
    lon = c(0.25, 0.8, 5.5, 9),
    lat = c(0.25, 0.8, 5.5, 9)
  )
  a <- rb_flag_ping_port(d, h, keep = "code")
  expect_equal(nrow(a), nrow(d))
  expect_equal(a$port_id[order(a$time)], c("small", "big", "far", NA))
  expect_equal(a$code[order(a$time)], c("S", "B", "F", NA))
  b <- dplyr::collect(rb_flag_ping_port(
    as_lazy(d),
    h,
    keep = "code"
  ))
  expect_equal(
    as.data.frame(b[order(b$time), ]),
    as.data.frame(a[order(a$time), ]),
    ignore_attr = TRUE
  )
  e <- dplyr::rename(d, x = lon, y = lat)
  r <- rb_flag_ping_port(e, h, lon = x, lat = y)
  expect_equal(names(r), c(names(e), "port_id"))
  expect_equal(r$port_id[order(r$time)], a$port_id[order(a$time)])
  # two polygons of one area hold a ping: the lower port_id; a position shared by pings, and a missing one
  h2 <- sf::st_sf(
    port_id = c("b2", "a2", "small"),
    geometry = sf::st_sfc(
      sq(0, 0, 1),
      sq(0, 0, 1),
      sq(0.2, 0.2, 0.1),
      crs = 4326
    )
  )
  d2 <- data.frame(
    k = 1:5,
    lon = c(0.8, 0.25, 0.25, NA, 3),
    lat = c(0.8, 0.25, 0.25, 0.5, NA)
  )
  for (z in list(d2, as_lazy(d2))) {
    r2 <- dplyr::collect(rb_flag_ping_port(z, h2))
    r2 <- r2[order(r2$k), ]
    expect_equal(nrow(r2), 5)
    expect_equal(r2$port_id, c("a2", "small", "small", NA, NA))
  }
})

# The noise floor between feeds and the evidence columns ------------------------------------------------------
# (fishycode archive 2026-10-stk-trail-order, section 11: the false removals of the STK whacks are a cross-feed
# offset)

# a steady 60 s track of one vessel (feed "a"), 15 m a step, and a ping of feed "b" 5 s after ping 30, `off_m` metres
# east of it (1 m is 1 / (111320 * cos(64 deg)) degrees of longitude)
two_feed_track <- function(off_m = 300, n = 60, k = 30) {
  d <- data.frame(
    vid = 1L,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * (seq_len(n) - 1),
    lon = -20 + 0.0003 * (seq_len(n) - 1),
    lat = 64,
    feed = "a"
  )
  b <- d[k, ]
  b$time <- b$time + 5
  b$lon <- b$lon + off_m / (111320 * cos(64 * pi / 180))
  b$feed <- "b"
  d <- rbind(d, b)
  d[order(d$time), ]
}

test_that("the defaults are the rule as it was: no feed, no evidence columns", {
  d <- two_feed_track()
  a <- rb_flag_ping_impossible(d)
  expect_identical(
    a,
    rb_flag_ping_impossible(
      d,
      feed = NULL,
      evidence = FALSE
    )
  )
  expect_identical(names(a), c(names(d), "ping_flag", "ping_flag_basis"))
  expect_equal(a$ping_flag_basis[!is.na(a$ping_flag)], "track")
  # the cross-feed ping 5 s after ping 30, 300 m away, is 58 knots on the step rule: flagged without a floor
  expect_equal(sum(!is.na(a$ping_flag)), 1L)
  expect_equal(a$feed[!is.na(a$ping_flag)], "b")
})

test_that("a cross-feed leg within noise_nm is never impossible, at every stage", {
  d <- two_feed_track(300)
  for (m in c("clean", "sda", "forward")) {
    expect_equal(
      sum(!is.na(rb_flag_ping_impossible(d, method = m)$ping_flag)),
      1L,
      info = m
    )
    expect_equal(
      sum(
        !is.na(rb_flag_ping_impossible(d, method = m, feed = feed)$ping_flag)
      ),
      0L,
      info = m
    )
  }
  # 0.3 nautical miles is 555.6 m: the floor is a parameter, and a leg beyond it is judged as before
  expect_equal(
    sum(
      !is.na(
        rb_flag_ping_impossible(two_feed_track(500), feed = feed)$ping_flag
      )
    ),
    0L
  )
  expect_equal(
    sum(
      !is.na(
        rb_flag_ping_impossible(two_feed_track(600), feed = feed)$ping_flag
      )
    ),
    1L
  )
  expect_equal(
    sum(
      !is.na(
        rb_flag_ping_impossible(
          two_feed_track(300),
          feed = feed,
          noise_nm = 0.1
        )$ping_flag
      )
    ),
    1L
  )
  expect_equal(
    sum(
      !is.na(
        rb_flag_ping_impossible(
          two_feed_track(600),
          feed = feed,
          noise_nm = 0.35
        )$ping_flag
      )
    ),
    0L
  )
  # noise_nm = 0 is no floor
  expect_identical(
    rb_flag_ping_impossible(d, feed = feed, noise_nm = 0)$ping_flag,
    rb_flag_ping_impossible(d)$ping_flag
  )
})

test_that("the floor is between feeds only: the same offset inside one feed is flagged, an unknown feed has no floor", {
  d <- two_feed_track(300)
  d$feed[d$feed == "b"] <- "a"
  expect_equal(
    sum(!is.na(rb_flag_ping_impossible(d, feed = feed)$ping_flag)),
    1L
  )
  e <- two_feed_track(300)
  e$feed[e$feed == "b"] <- NA
  expect_equal(
    sum(!is.na(rb_flag_ping_impossible(e, feed = feed)$ping_flag)),
    1L
  )
})

test_that("the floor leaves a real jump alone and takes another column name", {
  d <- two_feed_track(300)
  d$lon[d$time == d$time[40]] <- d$lon[d$time == d$time[40]] + 1 # a jump of 49 km, feed "a"
  r <- rb_flag_ping_impossible(
    dplyr::rename(d, provider = feed),
    feed = provider
  )
  expect_equal(sum(!is.na(r$ping_flag)), 1L)
  expect_equal(which(!is.na(r$ping_flag)), 40L)
  expect_true("provider" %in% names(r))
  expect_error(
    rb_flag_ping_impossible(d, feed = no_such_column),
    "no_such_column"
  )
})

test_that("the floor reads each ping's own feed whatever the order of the rows", {
  set.seed(3)
  d <- two_feed_track(300)
  d$key <- seq_len(nrow(d))
  a <- rb_flag_ping_impossible(d, feed = feed)
  b <- rb_flag_ping_impossible(d[sample(nrow(d)), ], feed = feed)
  expect_identical(b$ping_flag[order(b$key)], a$ping_flag[order(a$key)])
})

test_that("feed and noise_nm are refused where the method has no use for them", {
  d <- two_feed_track()
  expect_error(
    rb_flag_ping_impossible(d, method = "fwdbwd", feed = feed),
    "feed"
  )
  expect_error(
    rb_flag_ping_impossible(d, method = "sequential", feed = feed),
    "feed"
  )
  expect_error(
    rb_flag_ping_impossible(d, feed = feed, noise_nm = -1),
    "noise_nm"
  )
})

test_that("the evidence columns give the legs to the pings that stay, and say which side is impossible", {
  d <- two_feed_track(300)
  r <- rb_flag_ping_impossible(d, evidence = TRUE)
  ev <- c(
    "ping_flag_side",
    "ping_flag_prev_dt_s",
    "ping_flag_prev_dist_m",
    "ping_flag_next_dt_s",
    "ping_flag_next_dist_m"
  )
  expect_true(all(ev %in% names(r)))
  expect_false("ping_flag_prev_feed" %in% names(r)) # no feed given
  expect_equal(nrow(r), nrow(d))
  expect_true(all(is.na(r$ping_flag_side[is.na(r$ping_flag)])))
  i <- which(!is.na(r$ping_flag))
  expect_equal(r$ping_flag_side[i], "prev")
  expect_equal(r$ping_flag_prev_dt_s[i], 5)
  expect_equal(r$ping_flag_prev_dist_m[i], 300, tolerance = 0.02)
  expect_equal(r$ping_flag_next_dt_s[i], 55)
  # with a feed the neighbours' feeds are named; the cross-feed leg within the floor is no evidence
  r2 <- rb_flag_ping_impossible(d, evidence = TRUE, feed = feed, noise_nm = 0.1)
  j <- which(!is.na(r2$ping_flag))
  expect_equal(r2$ping_flag_prev_feed[j], "a")
  expect_equal(r2$ping_flag_next_feed[j], "a")
  expect_equal(r2$ping_flag_side[j], "prev")
  # a spike is too fast from both sides
  s <- two_feed_track(0)
  s$lon[s$time == s$time[40]] <- s$lon[s$time == s$time[40]] + 1
  r3 <- rb_flag_ping_impossible(s, evidence = TRUE)
  k <- which(!is.na(r3$ping_flag) & r3$ping_flag_side != "none")
  expect_equal(r3$ping_flag_side[k], "both")
  # a run of flagged pings is judged against the pings that stay, not against each other (the first stage also
  # flags the pings beside the run, so the run's neighbours are the nearest pings that are not flagged)
  rr <- two_feed_track(0, n = 40)
  rr$lon[20:22] <- rr$lon[20:22] + 2
  r4 <- rb_flag_ping_impossible(rr, evidence = TRUE)
  expect_true(all(20:22 %in% which(!is.na(r4$ping_flag))))
  expect_equal(r4$ping_flag_side[20:22], rep("both", 3))
  kept <- which(is.na(r4$ping_flag))
  prev <- vapply(20:22, function(i) max(kept[kept < i]), 1L)
  nxt <- vapply(20:22, function(i) min(kept[kept > i]), 1L)
  expect_equal(
    r4$ping_flag_prev_dt_s[20:22],
    as.numeric(r4$time[20:22] - r4$time[prev], units = "secs")
  )
  expect_equal(
    r4$ping_flag_next_dt_s[20:22],
    as.numeric(r4$time[nxt] - r4$time[20:22], units = "secs")
  )
  # a flag with no impossible leg to the pings that stay says so
  expect_true("none" %in% r4$ping_flag_side)
})

test_that("the evidence is added to the flags of this call only, and a second call keeps the first's", {
  d <- two_feed_track(300)
  d$lon[d$time == d$time[40]] <- d$lon[d$time == d$time[40]] + 1
  inv <- rb_flag_ping_invalid(d)
  r1 <- rb_flag_ping_impossible(inv, evidence = TRUE, method = "sda")
  r2 <- rb_flag_ping_impossible(
    r1,
    evidence = TRUE,
    method = "forward",
    feed = feed
  )
  expect_equal(
    r2$ping_flag[!is.na(r1$ping_flag)],
    r1$ping_flag[!is.na(r1$ping_flag)]
  )
  expect_equal(
    r2$ping_flag_side[!is.na(r1$ping_flag)],
    r1$ping_flag_side[!is.na(r1$ping_flag)]
  )
  # a track whose pings are all flagged already still gets the columns
  all_flagged <- rb_flag_ping_impossible(
    transform(d, ping_flag = "mine"),
    evidence = TRUE
  )
  expect_true("ping_flag_side" %in% names(all_flagged))
  expect_true(all(is.na(all_flagged$ping_flag_side)))
})

test_that("a lazy DuckDB table gives the same flags and evidence as a data frame, with a feed", {
  skip_if_not_installed("duckdbfs")
  set.seed(5)
  d <- two_feed_track(300)
  d$lon[d$time == d$time[40]] <- d$lon[d$time == d$time[40]] + 1
  d$key <- seq_len(nrow(d))
  d <- d[sample(nrow(d)), ]
  for (args in list(
    list(),
    list(feed = quote(feed)),
    list(evidence = TRUE),
    list(feed = quote(feed), evidence = TRUE, noise_nm = 0.1)
  )) {
    a <- do.call(rb_flag_ping_impossible, c(list(d), args))
    b <- dplyr::collect(do.call(
      rb_flag_ping_impossible,
      c(list(duckdbfs::as_dataset(d)), args)
    ))
    a <- as.data.frame(a[order(a$key), ])
    b <- as.data.frame(b[order(b$key), names(a)])
    expect_equal(
      b,
      a,
      ignore_attr = TRUE,
      info = paste(names(args), collapse = " ")
    )
  }
})

test_that("rb_flag_ping_invalid() flags a ping stamped after its receipt, only when asked", {
  t0 <- as.POSIXct("2024-03-01", tz = "UTC")
  d <- data.frame(
    lon = c(-20, -20.1, -20.2, -20.3, 64.1),
    lat = c(64, 64, 64, 64, 64.1),
    time = t0 + c(0, 600, 1200, 1800, 2400),
    receipt = t0 + c(90, 598, 1100, 1800, NA)
  )
  # by default as before: no receipt needed, none flagged for it
  expect_equal(rb_flag_ping_invalid(d)$ping_flag, c(NA, NA, NA, NA, "invalid"))
  expect_equal(
    rb_flag_ping_invalid(d[names(d) != "receipt"])$ping_flag,
    c(NA, NA, NA, NA, "invalid")
  )
  # 2 s after its receipt is tolerated, 100 s is not; a ping with no receipt is not judged; an earlier flag stays
  a <- rb_flag_ping_invalid(d, rules = c("lon_eq_lat", "after_receipt"))
  expect_equal(a$ping_flag, c(NA, NA, "after receipt", NA, "invalid"))
  expect_equal(
    rb_flag_ping_invalid(d, rules = "after_receipt", ahead_s = 120)$ping_flag,
    rep(NA_character_, 5)
  )
  l <- dplyr::collect(rb_flag_ping_invalid(
    as_lazy(d),
    rules = c("lon_eq_lat", "after_receipt")
  ))
  expect_equal(l$ping_flag[order(l$time)], a$ping_flag)
  expect_error(
    rb_flag_ping_invalid(d[names(d) != "receipt"], rules = "after_receipt"),
    "receipt"
  )
})

# fishycode plan 062 step 4 (2026.11.04): the cascade -------------------------------------------------------------

test_that("the withdrawn methods warn once per session, and still run", {
  ramb:::.rb_reset_renamed()
  d <- two_feeds()
  expect_warning(
    rb_flag_ping_impossible(d, method = "sda"),
    class = "ramb_withdrawn"
  )
  expect_no_warning(rb_flag_ping_impossible(d, method = "sda"))
  expect_warning(
    rb_flag_ping_impossible(d, method = "fwdbwd"),
    class = "ramb_withdrawn"
  )
  expect_no_warning(rb_flag_ping_impossible(d, method = "clean"))
})

# a vessel towing at 3 knots, a ping every minute; ping 20 moved 1 km off, under 25 knots both ways
towing <- function() {
  n <- 40
  data.frame(
    vid = 1L,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * (seq_len(n) - 1),
    lon = -20 + 0.0021 * seq_len(n),
    lat = 64,
    speed = 3
  )
}

test_that("\"local\" judges a leg against the vessel's recorded speed, else at the fixed limit", {
  d <- towing()
  # 1 km north, its legs 2 min each: 16 knots there and back
  d$lat[20] <- d$lat[20] + 0.009
  d$time[20:40] <- d$time[20:40] + 60
  d$time[21:40] <- d$time[21:40] + 60
  l <- rb_flag_ping_impossible(d, method = "local", speed = speed)
  expect_equal(which(!is.na(l$ping_flag)), 20L)
  expect_equal(l$ping_flag[20], "local_spike")
  expect_equal(l$ping_flag_basis[20], "recorded")
  # an out-and-back under 25 knots: every fixed limit keeps it
  expect_true(all(is.na(
    rb_flag_ping_impossible(d, method = "clean")$ping_flag
  )))
  # with no recorded speed, the fixed limit
  f <- rb_flag_ping_impossible(d, method = "local")
  expect_true(all(is.na(f$ping_flag)))
  d$lat[20] <- d$lat[20] + 0.02
  f <- rb_flag_ping_impossible(d, method = "local")
  expect_equal(f$ping_flag_basis[!is.na(f$ping_flag)], "track")
  # the evidence: the legs, and the speed the ping was judged by
  e <- rb_flag_ping_impossible(
    d,
    method = "local",
    speed = speed,
    evidence = TRUE
  )
  expect_equal(e$ping_flag_speed_kn[20], 3)
  expect_equal(e$ping_flag_side[20], "both")
  expect_error(
    rb_flag_ping_impossible(d, method = "clean", speed = speed),
    "local"
  )
})

test_that("\"local\" keeps a vessel truly fast, which a fixed limit flags", {
  d <- towing()
  d$lon <- -20 + 0.0245 * seq_len(nrow(d)) # 35 knots
  d$speed <- 35
  expect_true(all(is.na(
    rb_flag_ping_impossible(d, method = "local", speed = speed)$ping_flag
  )))
  expect_gt(
    sum(!is.na(rb_flag_ping_impossible(d, method = "clean")$ping_flag)),
    0
  )
})

# one feed: a vessel steaming at 10 knots, reports every minute, with recorded speeds that vary
steaming <- function(n = 30) {
  data.frame(
    vid = 1L,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * (seq_len(n) - 1),
    lon = -20 + 0.0071 * seq_len(n),
    lat = 64,
    speed = 10 + (seq_len(n) %% 7) / 10,
    course = 90 + (seq_len(n) %% 3) / 10,
    provider = "a"
  )
}

test_that("a report sent twice within a feed: the later one is a duplicate, on its recorded values", {
  d <- steaming()
  d <- rbind(d, transform(d[10, ], time = time + 1))
  r <- rb_flag_ping_duplicate(d, speed = speed, course = course)
  expect_equal(which(!is.na(r$ping_flag)), 31L)
  expect_equal(r$ping_flag_basis[31], "recorded")
  # without speed, the rule within a feed does not run
  expect_true(all(is.na(rb_flag_ping_duplicate(d)$ping_flag)))
})

test_that("a stretch sent again a day later: a repeat in one feed, decided by a second feed, by priority in two", {
  d <- steaming()
  copy <- transform(d[11:15, ], time = time + 86400)
  one <- rbind(d, copy)
  r <- rb_flag_ping_duplicate(one, speed = speed, course = course)
  # one feed, no witness: both kept, labelled
  expect_true(all(is.na(r$ping_flag)))
  expect_equal(r$ping_repeat_dt_s[11:15], rep(86400, 5))
  expect_equal(r$ping_repeat_dt_s[31:35], rep(-86400, 5))
  expect_true(all(is.na(r$ping_repeat_dt_s[-c(11:15, 31:35)])))
  # a second feed hears the vessel on the day of the true stretch: the copy is shifted, on the witness
  w <- transform(
    d[c(12, 14), ],
    time = time + 5,
    lon = lon + 0.001,
    provider = "b",
    speed = speed + 0.05
  )
  r <- rb_flag_ping_duplicate(
    rbind(one, w),
    priority = c("a", "b"),
    speed = speed,
    course = course
  )
  expect_equal(which(r$ping_flag %in% "shifted"), 31:35)
  expect_equal(unique(r$ping_flag_basis[31:35]), "witness")
  expect_true(all(is.na(r$ping_repeat_dt_s)))
  # the copy in a second feed: the lower-ranked feed's
  two <- rbind(d, transform(copy, provider = "b"))
  r <- rb_flag_ping_duplicate(
    two,
    priority = c("a", "b"),
    speed = speed,
    course = course
  )
  expect_equal(which(!is.na(r$ping_flag)), 31:35)
  expect_equal(unique(stats::na.omit(r$ping_flag)), "shifted")
  expect_equal(unique(r$ping_flag_basis), c(NA, "priority"))
  expect_error(rb_flag_ping_duplicate(two, speed = speed), "priority")
})

test_that("a vessel passing the same place again is no copy: its speed and course differ", {
  d <- steaming()
  back <- transform(d[11:15, ], time = time + 86400, speed = speed + 0.3)
  r <- rb_flag_ping_duplicate(rbind(d, back), speed = speed, course = course)
  expect_true(all(is.na(r$ping_flag)) && all(is.na(r$ping_repeat_dt_s)))
})

test_that("the copy rule and \"local\" give the same on a lazy table as on a data frame", {
  d <- steaming()
  d <- rbind(
    d,
    transform(d[11:15, ], time = time + 86400),
    transform(d[3, ], time = time + 1)
  )
  d$lat[25] <- d$lat[25] + 0.02
  d$rid <- seq_len(nrow(d))
  chain2 <- function(x) {
    x |>
      rb_flag_ping_duplicate(speed = speed, course = course) |>
      rb_flag_ping_impossible(method = "local", speed = speed, evidence = TRUE)
  }
  a <- chain2(d)
  b <- dplyr::collect(chain2(as_lazy(d)))
  b <- b[order(b$rid), ]
  for (k in c(
    "ping_flag",
    "ping_flag_basis",
    "ping_repeat_dt_s",
    "ping_flag_side",
    "ping_flag_speed_kn"
  )) {
    expect_equal(b[[k]], a[[k]], info = k)
  }
})

test_that("the lazy route gives back every column as it was, and the pings with no vessel", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  d <- towing()
  d$lat[20] <- d$lat[20] + 0.05
  d$vid[3] <- NA
  duckdb::duckdb_register(con, "d0", d)
  DBI::dbExecute(
    con,
    "CREATE TABLE d AS SELECT *, CAST(vid AS BIGINT) AS big, CAST(speed AS DECIMAL(6, 2)) AS dec,
                         CAST(time AS DATE) AS day, timezone('UTC', time) AS tz FROM d0"
  )
  x <- dplyr::tbl(con, "d")
  r <- rb_flag_ping_impossible(x, method = "local", speed = speed)
  ty <- DBI::dbGetQuery(con, paste("DESCRIBE", dbplyr::sql_render(r)))
  ty0 <- DBI::dbGetQuery(con, "DESCRIBE d")
  expect_equal(
    ty$column_type[match(ty0$column_name, ty$column_name)],
    ty0$column_type
  )
  r <- dplyr::collect(r)
  expect_equal(nrow(r), nrow(d))
  expect_equal(sum(is.na(r$vid)), 1L)
  expect_equal(r$ping_flag[r$lat > 64.01 & !is.na(r$lat)], "local_spike")
  # a column R cannot give back as it was: the labels are joined back instead, and they are the same
  DBI::dbExecute(
    con,
    "CREATE TABLE d2 AS SELECT *, CAST('12:00:00' AS TIME) AS tod FROM d"
  )
  r2 <- dplyr::collect(rb_flag_ping_impossible(
    dplyr::tbl(con, "d2"),
    method = "local",
    speed = speed
  ))
  expect_equal(r2$ping_flag[order(r2$time)], r$ping_flag[order(r$time)])
})

test_that("a batch's total order breaks a tie in vessel, time and position by the other columns", {
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  d <- data.frame(
    vid = 1,
    time = t0 + c(60, 0, 0, 0),
    lon = 0,
    lat = c(0, 0, 0, 1),
    z = c("c", "b", "a", "a")
  )
  expect_equal(ramb:::.rb_total_order(d), c(3L, 2L, 4L, 1L))
})
