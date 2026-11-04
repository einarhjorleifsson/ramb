# ramb's Danish pings (dansk) on a map with a time panel under it: each vessel's speed and course over time, the pings
# rb_flag_ping_impossible() flags marked, and lanes for the labelled behaviour and gear. A drag across the chart sets
# the time window the map shows; a hover marks the ping on the map; a click on a ping marks its time on the chart.
# Run with source(system.file("examples", "map", "dansk_time_map.R", package = "ramb")); the map opens at
# http://127.0.0.1:8790/
library(ramb)

pings <- dplyr::collect(rb_read_example("dansk"))
pings <- data.frame(
  vid = pings$vessel_id,
  time = pings$time_stamp,
  lon = pings$lon,
  lat = pings$lat,
  speed = pings$speed,
  course = pings$course,
  gear = pings$gear,
  behaviour = pings$behaviour
)
pings <- rb_flag_ping_impossible(pings, method = "clean")

m <- pings |>
  rb_map_track(behaviour, title = "Danish pings over time") |>
  rb_map_time(
    c(speed, course),
    marks = ping_flag,
    lanes = list(behaviour = "behaviour", gear = "gear")
  )
m # printed at the console, the map is served and opens; rb_map_stop() when done
