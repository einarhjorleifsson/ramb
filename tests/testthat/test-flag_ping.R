# Step 0 flaggers (fishycode plan 013, phase 2): same labels as the procedure they replace, on data
# frames and on lazy DuckDB tables alike.

# Two feeds: one Danish vessel as "stk", every third ping copied 1 s later as "astd" (a cross-feed copy),
# two placeholders (lon == lat), and two far-off jumps.
two_feeds <- function() {
  a <- head(dplyr::filter(dansk, vessel_id == dansk$vessel_id[2]), 400) |>
    dplyr::transmute(vid = 1L, time = time_stamp, lon, lat, provider = "stk")
  b <- a[seq(1, nrow(a), by = 3), ] |> dplyr::mutate(time = time + 1, lon = lon + 1e-6, provider = "astd")
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
  dup[!bad] <- with(d[!bad, ], rb_whack_duplicates(vid, time, lon, lat, provider, priority = priority))
  keep <- !bad & !dup
  out <- dplyr::bind_rows(rb_whack_clean(d[keep, ]),
                          d[bad, ] |> dplyr::mutate(whack_stage = "invalid"),
                          d[dup, ] |> dplyr::mutate(whack_stage = "duplicate"))
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
  skip_if_not_installed("duckdbfs")
  set.seed(2)
  d <- two_feeds()
  df <- chain(d, c("stk", "astd"))
  lz <- chain(duckdbfs::as_dataset(d), c("stk", "astd"))
  expect_s3_class(lz, "tbl_lazy")
  lz <- dplyr::collect(lz)
  expect_equal(lz$ping_flag[order(lz$rid)], df$ping_flag[order(df$rid)])
  for (m in c("sda", "forward", "fwdbwd", "sequential")) {
    a <- rb_flag_ping_impossible(d, method = m)
    b <- dplyr::collect(rb_flag_ping_impossible(duckdbfs::as_dataset(d), method = m))
    expect_equal(b$ping_flag[order(b$rid)], a$ping_flag[order(a$rid)], info = m)
  }
})

test_that("flags already set are kept, rows are never dropped, other column names work", {
  d <- data.frame(vid = 1, time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * 0:5,
                  lon = c(-20, 64, -20.01, 5, -20.02, -20.03), lat = 64)
  d$ping_flag <- c(NA, NA, "mine", NA, NA, NA)
  r <- rb_flag_ping_impossible(rb_flag_ping_invalid(d))
  expect_equal(nrow(r), nrow(d))
  expect_equal(r$ping_flag[2:3], c("invalid", "mine"))
  e <- dplyr::rename(d, vessel_id = vid, time_stamp = time, x = lon, y = lat)
  r2 <- rb_flag_ping_impossible(rb_flag_ping_invalid(e, lon = x, lat = y), vid = vessel_id, time = time_stamp, lon = x, lat = y)
  expect_equal(names(r2), names(e))
  expect_equal(r2$ping_flag, r$ping_flag)
})

test_that("a superseded function warns from user code, not from ramb's own", {
  .rb_reset_renamed()
  d <- data.frame(vid = 1, time = as.POSIXct("2024-01-01", tz = "UTC") + 60 * 0:3, lon = -20 - 0:3 / 100, lat = 64)
  expect_warning(evalq(rb_whack_clean(d), list(d = d, rb_whack_clean = rb_whack_clean), globalenv()), "superseded")
  .rb_reset_renamed()
  expect_no_warning(rb_flag_ping_impossible(d))
  .rb_reset_renamed()
})
