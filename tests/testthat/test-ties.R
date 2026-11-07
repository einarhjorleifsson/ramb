# Rows that share a time (fishycode TODO 195, ramb 2026.11.07.1): the port calls, the judged events and the voyages do
# not depend on the order the rows come in. A tie in time goes by position in rb_create_port_calls(), by port, position
# and unit in rb_flag_port_event(), and by port in rb_create_voyages().

sq <- function(x0, y0, w) {
  sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + w), c(x0, y0 + w), c(x0, y0))))
}
# two ports about 30 km apart
ti_ports <- sf::st_sf(port_id = c("a", "b"), geometry = sf::st_sfc(sq(-22, 64, 0.02), sq(-21.4, 64, 0.02), crs = 4326))
t0 <- as.POSIXct("2024-03-01", tz = "UTC")
rev_rows <- function(d) d[rev(seq_len(nrow(d))), ]

test_that("a ping and a good event at one time in two places give the same port calls in any row order", {
  # heard in port a at t0 and three hours later; at that second a good event puts it in port b; then at sea
  pings <- tibble::tibble(vid = 1L, time = t0 + 3600 * c(-3, 0, 3, 8), lon = c(-21.0, -21.99, -21.99, -21.0),
                          lat = c(64.3, 64.01, 64.01, 64.3))
  events <- tibble::tibble(vid = 1L, time = t0 + 3600 * 3, io = "I", port_id = "b", good = TRUE,
                           lon = -21.39, lat = 64.01)
  a <- rb_create_port_calls(pings, ti_ports, min_dwell_h = 2, events = events, presence = TRUE)
  b <- rb_create_port_calls(rev_rows(pings), ti_ports, min_dwell_h = 2, events = events, presence = TRUE)
  expect_equal(b, a)
  # by position: the ping in port a (lon -21.99) comes before the event in port b (lon -21.39), so a holds the vessel
  # from t0 to t0 + 3 h
  expect_equal(a$port_id, "a")
  expect_equal(a$T_in, t0)
  expect_equal(a$T_out, t0 + 3 * 3600)
  lz <- dplyr::collect(rb_create_port_calls(as_lazy(rev_rows(pings)), ti_ports, min_dwell_h = 2,
                                            events = as_lazy(events), presence = TRUE))
  expect_equal(nrow(lz), 1)
  expect_equal(lz$T_out, a$T_out)
})

test_that("two events of one vessel at one instant are judged the same in any row order", {
  # an arrival at a, then two departures in one second, from a and from b, then an arrival at a
  e <- tibble::tibble(vid = 1L, time = t0 + 3600 * c(0, 5, 5, 10), io = c("I", "U", "U", "I"),
                      port_id = c("a", "a", "b", "a"), lon = c(-21.99, -21.99, -21.39, -21.99), lat = 64.01)
  r1 <- rb_flag_port_event(e, ti_ports)
  r2 <- rb_flag_port_event(rev_rows(e), ti_ports)
  key <- function(r) r[order(r$time, r$io, r$port_id), ]
  expect_equal(key(r2), key(r1), ignore_attr = TRUE)
})

test_that("two port calls ending in one second give the voyage after them one departure port", {
  h <- function(x) t0 + x * 3600
  # port A holds the vessel from h(-5), port B from h(1), both to h(3); at sea from h(4) to h(8); then port A again
  calls <- data.frame(vid = 1L, port_id = c("A", "B", "A"), T_in = h(c(-5, 1, 9)), T_out = h(c(3, 3, 12)))
  sea <- data.frame(vid = 1L, time = h(4:8))
  v1 <- rb_create_voyages(sea, calls, min_pings = 1, by_year = FALSE)
  v2 <- rb_create_voyages(sea, rev_rows(calls), min_pings = 1, by_year = FALSE)
  expect_equal(v2, v1)
  # a tie in time goes by port: B's end sorts after A's, so the voyage leaves from B
  expect_equal(v1$port_from, "B")
  expect_equal(v1$port_to, "A")
})
