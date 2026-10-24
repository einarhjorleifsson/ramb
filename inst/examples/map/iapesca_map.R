# iapesca's built-in example data with ramb: the voyages found from the track by ramb's recoding of iapesca's rule,
# and a map of the positions (labelled setting, hauling or not fishing by iapesca), the recorded trips, ramb's voyages
# and the harbour. Needs the iapesca package (https://github.com/JulRodrigu/r-packages_iapesca) for its data. Run with
# source(system.file("examples", "map", "iapesca_map.R", package = "ramb")); the map opens at http://127.0.0.1:8799/
library(ramb)
library(sf)

# iapesca's example: one anonymised vessel, 8 recorded trips, every position labelled Setting, Hauling or NotFishing
pos <- iapesca::positions
pings <- data.frame(
  vid = pos$VESSEL_FK,
  time = as.POSIXct(pos$DATE_TIME, tz = "UTC"),
  lon = pos$LONGITUDE,
  lat = pos$LATITUDE,
  trip = as.character(pos$FISHING_TRIP_FK),
  operation = as.character(pos$FishingOperation)
)
pings <- pings[order(pings$vid, pings$time), ]
ports <- st_sf(port_id = "harbour", geometry = st_geometry(iapesca::harbours))

# ramb: the voyages from the track alone, by iapesca's rule (rb_create_voyages(method = "iapesca")), numbered over the
# whole series (by_year = FALSE: the track runs over New Year)
voyages <- rb_create_voyages(
  pings,
  method = "iapesca",
  ports = ports,
  by_year = FALSE
)
print(voyages[, c("voyage_id", "T1", "T2", "n_pings")])

# each position's voyage, where it lies in one
pings$voyage <- NA_integer_
for (i in seq_len(nrow(voyages))) {
  pings$voyage[
    pings$time >= voyages$T1[i] & pings$time <= voyages$T2[i]
  ] <- voyages$voyage_id[i]
}

# The map: the tables in DuckDB, cut into vector tiles by rb_map_tiles_table()
con <- duckdbfs::cached_connection()
DBI::dbExecute(con, "LOAD spatial") # if it is not there: DBI::dbExecute(con, "INSTALL spatial")
duckdb::duckdb_register(
  con,
  "iapesca_pings",
  transform(pings, time = format(time, "%Y-%m-%d %H:%M:%S"))
)
line <- function(by) {
  dplyr::tbl(
    con,
    dplyr::sql(sprintf(
      "SELECT %1$s, count(*) AS positions, min(time) AS start, max(time) AS stop,
            ST_MakeLine(list(ST_Point(lon, lat) ORDER BY time)) AS geometry
     FROM iapesca_pings WHERE %1$s IS NOT NULL GROUP BY %1$s",
      by
    ))
  )
}

positions_layer <- rb_map_layer(
  "positions",
  "vector",
  label = "Positions",
  info = "Each position of the vessel, labelled by iapesca: setting (green), hauling (red) or not fishing (grey).",
  tiles = rb_map_tiles_table(
    dplyr::tbl(con, "iapesca_pings"),
    lon = "lon",
    lat = "lat",
    properties = c("time", "trip", "voyage", "operation")
  ),
  count = list(text = "{1} positions"),
  style = list(list(
    id = "positions-dot",
    type = "circle",
    "source-layer" = "features",
    paint = list(
      "circle-color" = list(
        "match",
        list("get", "operation"),
        "Setting",
        "#2e8b57",
        "Hauling",
        "#c0392b",
        "#8a959e"
      ),
      "circle-radius" = list(
        "interpolate",
        list("linear"),
        list("zoom"),
        8,
        2,
        13,
        4,
        16,
        6
      ),
      "circle-stroke-color" = "#fff",
      "circle-stroke-width" = 0.5
    )
  )),
  legend = list(
    list(swatch = list(dot = "#2e8b57"), label = "setting"),
    list(swatch = list(dot = "#c0392b"), label = "hauling"),
    list(swatch = list(dot = "#8a959e"), label = "not fishing")
  ),
  popup = list(
    click = "positions-dot",
    title = "time",
    fields = list(
      list(field = "operation", label = "operation"),
      list(field = "trip", label = "recorded trip"),
      list(field = "voyage", label = "ramb's voyage")
    )
  )
)

trips_layer <- rb_map_layer(
  "trips",
  "vector",
  label = "Recorded trips",
  info = "The trips as iapesca's data record them (FISHING_TRIP_FK), each a line through its positions in time order.",
  tiles = rb_map_tiles_table(
    line("trip"),
    properties = c("trip", "positions", "start", "stop")
  ),
  count = list(text = "{1} trips"),
  style = list(list(
    id = "trips-line",
    type = "line",
    "source-layer" = "features",
    paint = list(
      "line-color" = "#2c5d87",
      "line-width" = 1.5,
      "line-opacity" = 0.8
    )
  )),
  legend = list(list(swatch = list(fill = "#2c5d87"), label = "recorded trip")),
  popup = list(
    click = "trips-line",
    title = "trip",
    fields = list(
      list(field = "start", label = "start"),
      list(field = "stop", label = "stop"),
      list(field = "positions", label = "positions", format = "count")
    )
  )
)

voyages_layer <- rb_map_layer(
  "voyages",
  "vector",
  label = "ramb's voyages",
  visible = FALSE,
  info = "The voyages ramb finds from the track alone: rb_create_voyages(method = \"iapesca\"), iapesca's rule recoded in DuckDB.",
  tiles = rb_map_tiles_table(
    line("voyage"),
    properties = c("voyage", "positions", "start", "stop")
  ),
  count = list(text = "{1} voyages"),
  style = list(list(
    id = "voyages-line",
    type = "line",
    "source-layer" = "features",
    paint = list(
      "line-color" = "#d9822b",
      "line-width" = 3,
      "line-opacity" = 0.6,
      "line-dasharray" = list(2, 1)
    )
  )),
  legend = list(list(swatch = list(fill = "#d9822b"), label = "voyage")),
  popup = list(
    click = "voyages-line",
    title = "voyage",
    fields = list(
      list(field = "start", label = "start"),
      list(field = "stop", label = "stop"),
      list(field = "positions", label = "positions", format = "count")
    )
  )
)

harbour_layer <- rb_map_layer(
  "harbour",
  "vector",
  label = "Harbour",
  tiles = rb_map_tiles_table(
    transform(
      sf::st_drop_geometry(ports),
      lon = st_coordinates(ports)[, 1],
      lat = st_coordinates(ports)[, 2]
    ),
    lon = "lon",
    lat = "lat",
    properties = "port_id"
  ),
  style = list(list(
    id = "harbour-dot",
    type = "circle",
    "source-layer" = "features",
    paint = list(
      "circle-color" = "#000",
      "circle-radius" = 6,
      "circle-stroke-color" = "#fff",
      "circle-stroke-width" = 2
    )
  )),
  legend = list(list(swatch = list(dot = "#000"), label = "harbour"))
)

m <- rb_map(
  rb_map_group(
    "track",
    "Track",
    list(positions_layer, trips_layer, voyages_layer)
  ),
  rb_map_group("ref", "Reference", list(harbour_layer)),
  title = "iapesca's example vessel",
  view = list(
    center = c(mean(range(pings$lon)), mean(range(pings$lat))),
    zoom = 10
  )
)

rb_map_serve(m, port = 8799) # opens the page in RStudio's Viewer or a browser; it answers while the console is idle
# rb_map_stop()
