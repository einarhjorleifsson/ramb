# rb_flag_ping_impossible(stream = ) (fishycode plan 068): each vessel's streams judged apart, so no leg joins two
# streams whose clocks may differ.

# a vessel steaming at about 10 knots, two streams of its reports: "a" on time every minute, "b" half a minute later
# but written with a clock 90 s behind, so "b" holds the position of a minute before its written time
two_clocks <- function(n = 30) {
  t0 <- as.POSIXct("2024-01-01", tz = "UTC")
  ms <- 10 * 1852 / 3600
  at <- function(s) -20 + ms * s / (111320 * cos(64 * pi / 180))
  a <- data.frame(vid = 1L, stream = "a", time = t0 + 60 * (seq_len(n) - 1))
  b <- data.frame(vid = 1L, stream = "b", time = t0 + 60 * (seq_len(n) - 1) + 30)
  a$lon <- at(as.numeric(a$time - t0))
  b$lon <- at(as.numeric(b$time - t0) - 90)
  d <- rbind(a, b)
  d$lat <- 64
  d$speed <- 10
  d[order(d$time), ]
}

test_that("two streams on different clocks: legs between them flagged, each stream alone none", {
  d <- two_clocks()
  # across the streams: the warning on unharmonised clocks, and the legs between the streams flagged
  for (m in c("clean", "forward")) {
    expect_warning(f <- rb_flag_ping_impossible(d, method = m), class = "ramb_unharmonised")
    expect_gt(sum(!is.na(f$ping_flag)), 0)
    expect_no_warning(f <- rb_flag_ping_impossible(d, method = m, stream = stream))
    expect_equal(sum(!is.na(f$ping_flag)), 0, info = m)
  }
  expect_warning(f <- rb_flag_ping_impossible(d, method = "local", speed = speed), "stream = stream")
  expect_gt(sum(!is.na(f$ping_flag)), 0)
  expect_no_warning(f <- rb_flag_ping_impossible(d, method = "local", speed = speed, stream = stream))
  expect_equal(sum(!is.na(f$ping_flag)), 0)
})

test_that("stream = gives the flags of running each stream as its own vessel, and keeps the columns as given", {
  d <- as.data.frame(dplyr::collect(rb_read_example("dansk_truth")))
  d <- d[d$vid %in% unique(d$vid)[1:2], c("vid", "time", "lon", "lat", "speed", "speed_basis", "feed", "stream")]
  d$speed <- ifelse(d$speed_basis == "derived", NA_real_, d$speed)
  d$speed_basis <- NULL
  ds <- d
  ds$vid <- paste(d$vid, d$stream, sep = "\r")
  lim <- rb_create_ping_limits(d, stream = stream, speed = speed)
  lim_s <- lim
  lim_s$vid <- paste(lim$vid, lim$stream, sep = "\r")
  lim_s$stream <- NULL
  runs <- list(
    clean = list(a = list(method = "clean", feed = quote(feed)), b = NULL),
    forward = list(a = list(method = "forward", feed = quote(feed)), b = NULL),
    local = list(a = list(method = "local", feed = quote(feed), speed = quote(speed), limits = lim),
                 b = list(limits = lim_s))
  )
  for (m in names(runs)) {
    a <- runs[[m]]$a
    b <- utils::modifyList(a, as.list(runs[[m]]$b))
    f1 <- suppressWarnings(do.call(rb_flag_ping_impossible, c(list(d), a, list(stream = quote(stream), evidence = TRUE))))
    f2 <- suppressWarnings(do.call(rb_flag_ping_impossible, c(list(ds), b, list(evidence = TRUE))))
    ev <- grep("^ping_flag", names(f1), value = TRUE)
    expect_equal(f1[ev], f2[ev], ignore_attr = TRUE, info = m)
    expect_identical(f1$vid, d$vid)
    expect_identical(f1$stream, d$stream)
  }
})

test_that("a lazy DuckDB table gives the same flags with stream =", {
  skip_if_not_installed("duckdbfs")
  d <- two_clocks()
  d$key <- seq_len(nrow(d))
  a <- rb_flag_ping_impossible(d, method = "local", speed = speed, stream = stream, evidence = TRUE)
  b <- dplyr::collect(rb_flag_ping_impossible(duckdbfs::as_dataset(d), method = "local", speed = speed, stream = stream,
                                              evidence = TRUE))
  a <- as.data.frame(a[order(a$key), ])
  b <- as.data.frame(b[order(b$key), names(a)])
  expect_equal(b, a, ignore_attr = TRUE)
})

test_that("a stream column that is not there stops with a message", {
  expect_error(rb_flag_ping_impossible(two_clocks(), method = "clean", stream = channel), "channel")
})
