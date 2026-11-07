# rb_create_port_calls(pair_away_m = ) (fishycode plan 048 step 6): an event pair makes no port call where a ping
# between its arrival and departure, not at its port, lies farther than pair_away_m from the port's polygon; NULL as
# before; data frame and lazy table alike.

sq <- function(x0, y0, w) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + w), c(x0, y0 + w), c(x0, y0))))
}
pg_ports <- sf::st_sf(port_id = c("a", "b"), geometry = sf::st_sfc(sq(-22, 64, 0.02), sq(-21.4, 64, 0.02), crs = 4326))
t0 <- as.POSIXct("2024-03-01", tz = "UTC")
# heard at sea, in port a at t0, somewhere at t0 + 5 h, in port a again at t0 + 10 h, at sea after: no call by pings
pg_pings <- function(lon5, lat5) {
  tibble::tibble(vid = 1L, time = t0 + 3600 * c(-3, 0, 5, 10, 13), lon = c(-21.0, -21.99, lon5, -21.99, -21.0),
                 lat = c(64.3, 64.01, lat5, 64.01, 64.3))
}
# an arrival at t0 and a departure nine hours later, at a, both good: a pair
pg_events <- tibble::tibble(vid = 1L, time = t0 + 3600 * c(0, 9), io = c("I", "U"), port_id = "a", good = TRUE)

test_that("a pair over a ping far from its port makes no port call", {
  far <- pg_pings(-21.0, 64.3)                       # about 50 km off
  expect_equal(nrow(rb_create_port_calls(far, pg_ports, min_dwell_h = 2, events = pg_events)), 0)
  # the same pair with the vessel's pings near the port is a call
  near <- pg_pings(-21.88, 64.01)                     # about 5 km east of the polygon
  a <- rb_create_port_calls(near, pg_ports, min_dwell_h = 2, events = pg_events)
  expect_equal(nrow(a), 1)
  expect_equal(a$T_in, t0)
  expect_equal(a$T_out, t0 + 9 * 3600)
  expect_equal(a$rule, "event_pair")
  # a ping at another port, 30 km off, is the vessel away too
  at_b <- pg_pings(-21.39, 64.01)
  expect_equal(nrow(rb_create_port_calls(at_b, pg_ports, min_dwell_h = 2, events = pg_events)), 0)
})

test_that("the distance is the argument's, and NULL is as before", {
  far <- pg_pings(-21.0, 64.3)
  expect_equal(nrow(rb_create_port_calls(far, pg_ports, min_dwell_h = 2, events = pg_events, pair_away_m = NULL)), 1)
  expect_equal(nrow(rb_create_port_calls(far, pg_ports, min_dwell_h = 2, events = pg_events, pair_away_m = 1e5)), 1)
  near <- pg_pings(-21.88, 64.01)
  expect_equal(nrow(rb_create_port_calls(near, pg_ports, min_dwell_h = 2, events = pg_events, pair_away_m = 2000)), 0)
})

test_that("pings at the pair's ends, and at its own port, do not drop it", {
  # the vessel heard in port a right through the pair
  inport <- pg_pings(-21.99, 64.01)
  a <- rb_create_port_calls(inport, pg_ports, min_dwell_h = 2, events = pg_events)
  expect_equal(nrow(a), 1)
  expect_true(grepl("event_pair", a$evidence))
  # a far ping exactly at the departure's time is not between
  edge <- pg_pings(-21.99, 64.01); edge$time[3] <- t0 + 9 * 3600; edge$lon[3] <- -21.0; edge$lat[3] <- 64.3
  expect_equal(nrow(rb_create_port_calls(edge, pg_ports, min_dwell_h = 2, events = pg_events)), 1)
})

test_that("without events the guard does nothing; a data frame and a lazy table agree", {
  far <- pg_pings(-21.0, 64.3)
  expect_equal(rb_create_port_calls(far, pg_ports, min_dwell_h = 2),
               rb_create_port_calls(far, pg_ports, min_dwell_h = 2, pair_away_m = NULL))
  con <- .rb_connection()
  for (p in list(far, pg_pings(-21.88, 64.01))) {
    lz <- rb_create_port_calls(as_lazy(p, con), pg_ports, min_dwell_h = 2, events = as_lazy(pg_events, con)) |>
      dplyr::collect()
    df <- rb_create_port_calls(p, pg_ports, min_dwell_h = 2, events = pg_events)
    expect_equal(nrow(lz), nrow(df))
    expect_equal(lz$T_in, df$T_in)
  }
})

test_that("misuse is named", {
  far <- pg_pings(-21.0, 64.3)
  expect_error(rb_create_port_calls(far, pg_ports, events = pg_events, pair_away_m = -1), "pair_away_m")
  expect_error(rb_create_port_calls(far, pg_ports, events = pg_events, pair_away_m = "10 km"), "pair_away_m")
  expect_error(rb_create_port_calls(far, pg_ports, events = pg_events, pair_away_m = c(1, 2)), "pair_away_m")
})
