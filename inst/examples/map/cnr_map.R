# ramb's Italian data (CNR-IRBIM's Adriatic tracks of six vessels and the Mediterranean harbours, from WKSSFGEO):
# each vessel's voyages by two rules ramb recodes, CNR-IRBIM's own (method "cnr") and Ifremer's iapesca's, compared
# ping by ping, and a map of the positions, both sets of voyages and the harbours. Run with
# source(system.file("examples", "map", "cnr_map.R", package = "ramb")); the map opens at http://127.0.0.1:8800/
library(ramb)
library(sf)

# The harbours, numbered as the port of each ping
h <- dplyr::collect(rb_read_example("wkssfgeo_cnr_med_harbours"))
ports <- st_sf(
  port_id = seq_len(nrow(h)),
  harbour = h$harbour,
  geometry = st_as_sfc(structure(h$geometry, class = "WKB"), crs = 4326)
)

# The pings, each with the harbour it lies in (NA at sea): rb_flag_ping_port()
pings <- rb_read_example("wkssfgeo_cnr_ais") |>
  rb_flag_ping_port(ports["port_id"], lon = longitude, lat = latitude) |>
  dplyr::collect() |>
  as.data.frame()
pings <- data.frame(
  vid = pings$MMSI,
  time = pings$datetime,
  lon = pings$longitude,
  lat = pings$latitude,
  speed = pings$speed,
  port_id = pings$port_id
)
pings <- pings[order(pings$vid, pings$time), ]

# The voyages by each rule, numbered over the whole series of each vessel
cnr <- rb_create_voyages(pings, method = "cnr", ports = ports, by_year = FALSE)
iap <- rb_create_voyages(
  pings,
  method = "iapesca",
  ports = ports,
  by_year = FALSE,
  iapesca = list(crs_m = 32633)
) # UTM 33N, as iapesca's own run on these tracks
cat(sprintf("voyages: cnr %d, iapesca %d\n", nrow(cnr), nrow(iap)))
print(merge(
  stats::aggregate(cbind(cnr = voyage_id) ~ vid, cnr, length),
  stats::aggregate(cbind(iapesca = voyage_id) ~ vid, iap, length),
  all = TRUE
))

# Each ping's voyage under each rule, where it lies in one
voyage_of <- function(v) {
  out <- rep(NA_integer_, nrow(pings))
  for (i in seq_len(nrow(v))) {
    out[
      pings$vid == v$vid[i] & pings$time >= v$T1[i] & pings$time <= v$T2[i]
    ] <- v$voyage_id[i]
  }
  out
}
pings$cnr <- voyage_of(cnr)
pings$iapesca <- voyage_of(iap)
cat("pings in a voyage, by rule:\n")
print(table(cnr = !is.na(pings$cnr), iapesca = !is.na(pings$iapesca)))

# The map: the tables in DuckDB, cut into vector tiles by rb_map_tiles_table()
con <- duckdbfs::cached_connection()
DBI::dbExecute(con, "LOAD spatial") # if it is not there: DBI::dbExecute(con, "INSTALL spatial")
duckdb::duckdb_register(
  con,
  "italy_pings",
  transform(
    pings,
    vessel = as.character(vid),
    vid = NULL,
    time = format(time, "%Y-%m-%d %H:%M:%S")
  )
)
line <- function(rule) {
  dplyr::tbl(
    con,
    dplyr::sql(sprintf(
      "SELECT vessel, %1$s AS voyage, count(*) AS pings, min(time) AS start, max(time) AS stop,
            ST_MakeLine(list(ST_Point(lon, lat) ORDER BY time)) AS geometry
     FROM italy_pings WHERE %1$s IS NOT NULL GROUP BY ALL",
      rule
    ))
  )
}
voyage_layer <- function(rule, label, colour, dash, visible) {
  rb_map_layer(
    rule,
    "vector",
    label = label,
    visible = visible,
    info = sprintf(
      "Each vessel's voyages by rb_create_voyages(method = \"%s\"), a line through the voyage's pings.",
      rule
    ),
    tiles = rb_map_tiles_table(
      line(rule),
      properties = c("vessel", "voyage", "pings", "start", "stop")
    ),
    count = list(text = "{1} voyages"),
    style = list(list(
      id = paste0(rule, "-line"),
      type = "line",
      "source-layer" = "features",
      paint = c(
        list("line-color" = colour, "line-width" = 2, "line-opacity" = 0.7),
        if (dash) list("line-dasharray" = list(2, 1))
      )
    )),
    legend = list(list(
      swatch = list(fill = colour),
      label = paste(label, "voyage")
    )),
    popup = list(
      click = paste0(rule, "-line"),
      title = "vessel",
      fields = list(
        list(field = "voyage", label = "voyage"),
        list(field = "start", label = "start"),
        list(field = "stop", label = "stop"),
        list(field = "pings", label = "pings", format = "count")
      )
    )
  )
}

pings_layer <- rb_map_layer(
  "pings",
  "vector",
  label = "Pings",
  info = "Each ping of the six vessels, coloured by its recorded speed.",
  tiles = rb_map_tiles_table(
    dplyr::tbl(con, "italy_pings"),
    lon = "lon",
    lat = "lat",
    properties = c("vessel", "time", "speed", "cnr", "iapesca")
  ),
  count = list(text = "{1} pings"),
  style = list(list(
    id = "pings-dot",
    type = "circle",
    "source-layer" = "features",
    paint = list(
      "circle-color" = list(
        "step",
        list("get", "speed"),
        "#8a959e",
        1,
        "#2e8b57",
        5,
        "#c0392b"
      ),
      "circle-radius" = list(
        "interpolate",
        list("linear"),
        list("zoom"),
        7,
        1.5,
        12,
        3.5,
        15,
        5
      ),
      "circle-stroke-color" = "#fff",
      "circle-stroke-width" = 0.4
    )
  )),
  legend = list(
    list(swatch = list(dot = "#8a959e"), label = "under 1 knot"),
    list(swatch = list(dot = "#2e8b57"), label = "1-5 knots"),
    list(swatch = list(dot = "#c0392b"), label = "over 5 knots")
  ),
  popup = list(
    click = "pings-dot",
    title = "time",
    fields = list(
      list(field = "vessel", label = "vessel"),
      list(field = "speed", label = "speed (knots)"),
      list(field = "cnr", label = "cnr voyage"),
      list(field = "iapesca", label = "iapesca voyage")
    )
  )
)

harbours_layer <- rb_map_layer(
  "harbours",
  "vector",
  label = "Harbours",
  visible = FALSE,
  tiles = rb_map_tiles_table(
    rb_read_example("wkssfgeo_cnr_med_harbours"),
    properties = c("harbour", "Country"),
    points = "harbour_points"
  ),
  count = list(text = "{1} harbours"),
  style = list(
    list(
      id = "harbours-fill",
      type = "fill",
      "source-layer" = "features",
      minzoom = 10,
      paint = list("fill-color" = "#000", "fill-opacity" = 0.2)
    ),
    list(
      id = "harbours-dot",
      type = "circle",
      "source-layer" = "harbour_points",
      maxzoom = 10,
      paint = list("circle-color" = "#000", "circle-radius" = 3)
    )
  ),
  legend = list(list(swatch = list(dot = "#000"), label = "harbour")),
  popup = list(
    click = c("harbours-fill", "harbours-dot"),
    title = "harbour",
    fields = list(list(field = "Country", label = "country"))
  )
)

m <- rb_map(
  rb_map_group(
    "track",
    "Tracks",
    list(
      pings_layer,
      voyage_layer("cnr", "CNR", "#2c5d87", FALSE, TRUE),
      voyage_layer("iapesca", "iapesca", "#d9822b", TRUE, FALSE)
    )
  ),
  rb_map_group("ref", "Reference", list(harbours_layer)),
  title = "Italian tracks (CNR-IRBIM)",
  view = list(
    center = c(mean(range(pings$lon)), mean(range(pings$lat))),
    zoom = 8
  )
)

rb_map_serve(m, port = 8800) # opens the page in RStudio's Viewer or a browser; it answers while the console is idle
# rb_map_stop()
