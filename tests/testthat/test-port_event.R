# rb_flag_port_event() (fishycode plan 048 step 2): recorded port events judged as events. Each test on a case made for
# it; a test runs only where its input is given; an event the track cannot judge is not good; data frame and lazy table
# alike; the misuse messages.

sq <- function(x0, y0, w) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + w), c(x0, y0 + w), c(x0, y0))))
}
# two ports 0.02 degrees square, about 1.1 km apart from edge to edge at 64 N
pe_ports <- sf::st_sf(
  port_id = c("a", "b"),
  geometry = sf::st_sfc(sq(-22, 64, 0.02), sq(-21.96, 64, 0.02), crs = 4326)
)
t0 <- as.POSIXct("2024-03-01", tz = "UTC")
ev <- function(...) tibble::tibble(vid = 1L, ...)

test_that("the position tests: another port, an arrival outside its own, a departure not judged by distance", {
  e <- ev(
    time = t0 + 3600 * 0:4,
    io = c("I", "U", "I", "U", "I"),
    port_id = c("a", "a", "b", "b", "a"),
    # in its own port; a marker (NA); in port a though it names b; far off (a departure); 5 km off (an arrival)
    lon = c(-21.99, NA, -21.99, -21.5, -21.99),
    lat = c(64.01, NA, 64.01, 64.3, 64.06)
  )
  r <- rb_flag_port_event(e, pe_ports, radius_m = 0)
  expect_equal(r$event_flag, c(NA, NA, "other_port", NA, "outside_port"))
  expect_equal(r$good, c(TRUE, TRUE, FALSE, TRUE, FALSE))
  # within radius_m of its own port an event is at it, though it lies in another: port b's edge is 1.1 km from a's
  r2 <- rb_flag_port_event(e, pe_ports, radius_m = 2000)
  expect_equal(r2$event_flag[3], NA_character_)
  # without ports, or without positions, no position test runs
  expect_true(all(is.na(rb_flag_port_event(e)$event_flag)))
  expect_true(all(is.na(rb_flag_port_event(e[, c("vid", "time", "io", "port_id")], pe_ports)$event_flag)))
})

test_that("the sequence tests: a flicker flags both, a repeat the later one", {
  e <- ev(
    time = t0 + c(0, 30, 3600, 7200, 7300, 10000),
    io = c("I", "U", "I", "I", "U", "U"),
    port_id = "a"
  )
  r <- rb_flag_port_event(e)
  expect_equal(r$event_flag, c("flicker", "flicker", NA, "repeat", NA, "repeat"))
  # a flicker is at one port: an arrival and a departure at two ports are a vessel passing between them
  e$port_id[2] <- "b"
  expect_equal(rb_flag_port_event(e)$event_flag[1:2], c(NA_character_, NA_character_))
  # an arrival and a departure at one instant: the arrival first
  e2 <- ev(time = t0 + c(0, 0), io = c("U", "I"), port_id = "a")
  expect_equal(rb_flag_port_event(e2)$event_flag, c("flicker", "flicker"))
})

test_that("the track test: the pings either side against the port, the worse side, and no ping no witness", {
  e <- ev(time = t0 + 3600 * c(1, 3, 5, 30), io = c("I", "U", "I", "U"), port_id = "a")
  p <- tibble::tibble(
    vid = 1L,
    time = t0 + 3600 * c(0.5, 1.5, 2.9, 3.05, 4.9, 5.2),
    # in port a; in port a; in port a; 3.9 km out 3 min after the departure; 47 km off 6 min before the arrival
    lon = c(-21.99, -21.99, -21.99, -21.99, -21.0, -21.99),
    lat = c(64.01, 64.01, 64.01, 64.055, 64.01, 64.01)
  )
  r <- rb_flag_port_event(e, pe_ports, p)
  expect_equal(r$event_flag, c(NA, NA, "track", NA))
  expect_equal(r$event_track_kn[1], 0)
  # 3.9 km, less the 2 km allowance, in 3 min is about 20 kn: within 25
  expect_lt(r$event_track_kn[2], 25)
  expect_gt(r$event_track_kn[2], 15)
  expect_gt(r$event_track_kn[3], 100)
  # the last departure has the last ping before it, a day earlier: judged on that side
  expect_false(is.na(r$event_track_kn[4]))
  # a vessel with no ping is not judged, and not good
  e$vid[4] <- 2L
  r <- rb_flag_port_event(e, pe_ports, p)
  expect_true(is.na(r$event_track_kn[4]))
  expect_true(is.na(r$good[4]))
  expect_true(is.na(r$event_flag[4]))
  # with no port, the event's own position is the target
  e3 <- ev(time = t0 + 3600 * 5, io = "I", lon = -21.99, lat = 64.01)
  expect_equal(rb_flag_port_event(e3, pings = p)$event_flag, "track")
  # tighter: no allowance turns the departure 3.9 km out into a flag
  expect_equal(rb_flag_port_event(e[2, ], pe_ports, p, radius_m = 0)$event_flag, "track")
})

test_that("the unit test: a unit that fails often, its other events too, unless the track vouches for them", {
  # unit u1 sends 10 events, 6 of them repeats; unit u2 10 good ones
  e <- tibble::tibble(
    vid = rep(1:2, each = 10),
    time = t0 + 3600 * rep(0:9, 2),
    io = c(rep("I", 7), "U", "I", "U", rep(c("I", "U"), 5)),
    port_id = "a",
    unit = rep(c("u1", "u2"), each = 10)
  )
  r <- rb_flag_port_event(e, unit = unit, unit_min = 5, unit_share = 0.5)
  expect_equal(sum(r$event_flag == "repeat", na.rm = TRUE), 6)
  expect_equal(sum(r$event_flag == "unit", na.rm = TRUE), 4)
  expect_true(all(is.na(r$event_flag[e$unit == "u2"])))
  # fewer events than unit_min, or a share under unit_share: no unit flag
  expect_equal(sum(rb_flag_port_event(e, unit = unit, unit_min = 50, unit_share = 0.5)$event_flag == "unit", na.rm = TRUE), 0)
  # the default share, 0.7, is above u1's 0.6
  expect_equal(sum(rb_flag_port_event(e, unit = unit, unit_min = 5)$event_flag == "unit", na.rm = TRUE), 0)
  # where the track judges an event, the unit test leaves it
  p <- tibble::tibble(vid = 1L, time = t0 + 3600 * (0:9) + 60, lon = -21.99, lat = 64.01)
  r <- rb_flag_port_event(e, pe_ports, p, unit = unit, unit_min = 5, unit_share = 0.5)
  expect_equal(sum(r$event_flag == "unit", na.rm = TRUE), 0)
  expect_true(all(r$good[e$unit == "u1" & is.na(r$event_flag)]))
})

test_that("data frame and lazy table alike; the rows in the order they came; the user's column names", {
  e <- ev(
    time = t0 + 3600 * c(0, 1, 2, 5, 3, 4),
    io = c("I", "U", "I", "U", "U", "I"),
    port_id = c("a", "a", "b", "b", "a", "a"),
    lon = c(-21.99, NA, -21.95, NA, NA, -21.99),
    lat = c(64.01, NA, 64.01, NA, NA, 64.01)
  )
  p <- tibble::tibble(vid = 1L, time = t0 + 1800 * 0:12, lon = -21.99, lat = 64.01)
  df <- rb_flag_port_event(e, pe_ports, p)
  expect_equal(df[, names(e)], e)
  con <- .rb_connection()
  lz <- rb_flag_port_event(as_lazy(e, con), pe_ports, as_lazy(p, con)) |> dplyr::collect()
  lz <- lz[order(lz$time), ]
  expect_equal(lz$event_flag, df$event_flag[order(df$time)])
  expect_equal(lz$good, df$good[order(df$time)])
  # other names
  e2 <- dplyr::rename(e, boat = vid, t = time, kind = io, harbour = port_id, x = lon, y = lat)
  p2 <- dplyr::rename(p, boat = vid, t = time, x = lon, y = lat)
  r2 <- rb_flag_port_event(e2, pe_ports, p2, vid = boat, time = t, io = kind, port_id = harbour, lon = x, lat = y)
  expect_equal(names(r2), c(names(e2), "event_flag", "event_track_kn", "good"))
  expect_equal(r2$event_flag, df$event_flag)
})

test_that("misuse is named", {
  e <- ev(time = t0 + 0:1, io = c("I", "U"), port_id = "a")
  expect_error(rb_flag_port_event(e[, c("vid", "time")]), "`io`")
  expect_error(rb_flag_port_event(dplyr::mutate(e, io = c("in", "out"))), "\"in\"")
  expect_error(rb_flag_port_event(e, ports = sf::st_drop_geometry(pe_ports)), "sf object")
  expect_error(rb_flag_port_event(e[, c("vid", "time", "io")], pe_ports), "port each event names")
  expect_error(rb_flag_port_event(e, pings = tibble::tibble(vid = 1L, time = t0)), "`lon`")
  expect_error(rb_flag_port_event(e, radius_m = -1), "radius_m")
  expect_error(rb_flag_port_event(rb_flag_port_event(e)), "already has")
  expect_error(rb_flag_port_event(e, unit = sender), "sender")
  expect_error(rb_flag_port_event(dplyr::mutate(e, time = "2024-03-01")), "date-time")
})

test_that("rb_create_port_calls() names the step that judges the events", {
  e <- ev(time = t0 + 0:1, io = c("I", "U"), port_id = "a")
  p <- tibble::tibble(vid = 1L, time = t0 + 3600 * 0:3, lon = -21.99, lat = 64.01)
  expect_error(rb_create_port_calls(p, pe_ports, events = e), "rb_flag_port_event")
})
