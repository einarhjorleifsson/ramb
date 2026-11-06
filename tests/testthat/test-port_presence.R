# rb_create_port_calls(presence = ) (fishycode plan 048 §7.3): a good event with a position is a ping for the dwell
# rules; a bad one, or one with no position, is none; the default as before; data frame and lazy table alike.

sq <- function(x0, y0, w) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + w), c(x0, y0 + w), c(x0, y0))))
}
pp_ports <- sf::st_sf(port_id = "a", geometry = sf::st_sfc(sq(-22, 64, 0.02), crs = 4326))
t0 <- as.POSIXct("2024-03-01", tz = "UTC")
# a vessel heard in port at t0, then silent until it is 60 km off at sea ten hours later
pp_pings <- tibble::tibble(vid = 1L, time = t0 + 3600 * c(-2, 0, 10, 11), lon = c(-21.0, -21.99, -21.0, -20.9),
                           lat = c(64.3, 64.01, 64.3, 64.35))
# its departure, reported in port eight hours after the arrival ping; and an arrival with no position
pp_events <- tibble::tibble(vid = 1L, time = t0 + 3600 * c(0, 8), io = c("I", "U"), port_id = "a",
                            good = c(NA, TRUE), lon = c(NA, -21.985), lat = c(NA, 64.005))

test_that("a good event with a position makes the port call its pings alone do not", {
  a <- rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = pp_events)
  expect_equal(nrow(a), 0)
  b <- rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = pp_events, presence = TRUE)
  expect_equal(nrow(b), 1)
  expect_equal(b$T_in, t0)
  expect_equal(b$T_out, t0 + 8 * 3600)
  expect_equal(b$port_id, "a")
  expect_false(grepl("event_pair", b$evidence))
})

test_that("a bad event, or one with no position, gives no presence; one equal to a ping is that ping", {
  bad <- pp_events; bad$good[2] <- FALSE
  expect_equal(nrow(rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = bad, presence = TRUE)), 0)
  nopos <- pp_events; nopos$lon[2] <- NA
  expect_equal(nrow(rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = nopos, presence = TRUE)), 0)
  # an event at a ping's time and position adds nothing
  same <- tibble::tibble(vid = 1L, time = t0, io = "I", port_id = "a", good = TRUE, lon = -21.99, lat = 64.01)
  expect_equal(rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = same, presence = TRUE),
               rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = same))
})

test_that("the default is as before, and a data frame and a lazy table agree", {
  expect_equal(rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = pp_events, presence = FALSE),
               rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = pp_events))
  con <- .rb_connection()
  lz <- rb_create_port_calls(as_lazy(pp_pings, con), pp_ports, min_dwell_h = 2, events = as_lazy(pp_events, con),
                             presence = TRUE) |> dplyr::collect()
  df <- rb_create_port_calls(pp_pings, pp_ports, min_dwell_h = 2, events = pp_events, presence = TRUE)
  expect_equal(lz$T_in, df$T_in)
  expect_equal(lz$T_out, df$T_out)
})

test_that("misuse is named", {
  expect_error(rb_create_port_calls(pp_pings, pp_ports, presence = TRUE), "give `events`")
  expect_error(rb_create_port_calls(pp_pings, pp_ports, events = pp_events[, c("vid", "time", "io", "port_id", "good")],
                                    presence = TRUE), "`lon`")
  expect_error(rb_create_port_calls(pp_pings, pp_ports, events = pp_events, presence = NA), "TRUE or FALSE")
  expect_error(rb_create_port_calls(pp_pings, pp_ports, method = "runs", presence = TRUE), "dwell")
})
