# A map of ramb's example data, with switches: the Danish pings (dansk) by cell, at a cell size chosen in the page, from
# every ping or one per 5 minutes, with each ping's time on the ping or spread along the track to the next (the
# interpolation); and the harbours (dansk_harbours). Run with
# source(system.file("examples", "map", "dansk_switches_map.R", package = "ramb")); the map opens at
# http://127.0.0.1:8797/
library(ramb)

con <- duckdbfs::cached_connection()
DBI::dbExecute(con, "LOAD spatial") # if it is not there: DBI::dbExecute(con, "INSTALL spatial")

pings <- dbplyr::sql_render(rb_read_example("dansk"))

# The points the grid counts, made once. Each ping stands for the time to the vessel's next ping (minutes), from every
# ping (sample "all") or from the first ping of each 5 minutes (sample "5min"); a gap over 30 minutes stands for no
# time, since where the vessel was is not known. Interpolated ("on"), a ping's minutes are spread over the line to its
# next ping in 1-minute steps, each step at the middle of its stretch. Not interpolated ("off"), they stay on the ping.
DBI::dbExecute(
  con,
  sprintf(
    "
  CREATE OR REPLACE TEMP TABLE dansk_points AS
  WITH s AS (
    SELECT 'all' AS sample, * FROM (%1$s)
    UNION ALL
    SELECT '5min' AS sample, * FROM (%1$s)
    QUALIFY row_number() OVER (PARTITION BY vessel_id, time_bucket(INTERVAL 5 MINUTE, time_stamp) ORDER BY time_stamp) = 1),
  l AS (
    SELECT sample, behaviour, lon, lat,
           lead(lon) OVER w AS lon2, lead(lat) OVER w AS lat2,
           epoch(lead(time_stamp) OVER w - time_stamp) / 60 AS minutes
    FROM s WINDOW w AS (PARTITION BY sample, vessel_id ORDER BY time_stamp)),
  k AS (SELECT * REPLACE (CASE WHEN minutes <= 30 THEN minutes ELSE 0 END AS minutes),
               CASE WHEN minutes <= 30 THEN greatest(1, ceil(minutes)::INTEGER) ELSE 1 END AS steps
        FROM l WHERE minutes IS NOT NULL)
  SELECT sample, 'off' AS interp, behaviour, lon, lat, minutes FROM k
  UNION ALL
  SELECT sample, 'on' AS interp, behaviour,
         CASE WHEN steps > 1 THEN lon + (i + 0.5) / steps * (lon2 - lon) ELSE lon END AS lon,
         CASE WHEN steps > 1 THEN lat + (i + 0.5) / steps * (lat2 - lat) ELSE lat END AS lat,
         minutes / steps AS minutes
  FROM k, range(steps) AS r(i)",
    pings
  )
)

# A grid layer's tile function: the points' minutes and number summed by cell. The server has checked every value
# against the filters' options before it arrives here.
ping_tile <- function(z, x, y, params) {
  cell <- as.numeric(if (is.null(params$cell)) "0.01" else params$cell)
  where <- c(
    sprintf(
      "sample = %s",
      DBI::dbQuoteString(
        con,
        if (is.null(params$sample)) "all" else params$sample
      )
    ),
    sprintf(
      "interp = %s",
      DBI::dbQuoteString(
        con,
        if (is.null(params$interp)) "off" else params$interp
      )
    ),
    if (!is.null(params$behaviour)) {
      sprintf("behaviour = %s", DBI::dbQuoteString(con, params$behaviour))
    }
  )
  DBI::dbGetQuery(
    con,
    sprintf(
      "WITH c AS (SELECT floor(lon / %1$f) AS cx, floor(lat / %1$f) AS cy, sum(minutes) AS minutes, count(*) AS n
                FROM dansk_points WHERE %2$s GROUP BY ALL),
     m AS (SELECT minutes, n, ST_Transform(ST_MakeEnvelope(cx * %1$f, cy * %1$f, (cx + 1) * %1$f, (cy + 1) * %1$f),
                                           'EPSG:4326', 'EPSG:3857', always_xy := true) AS env FROM c)
     SELECT ST_AsMVT({'geometry': ST_AsMVTGeom(env, ST_Extent(ST_TileEnvelope(%3$d, %4$d, %5$d)), 4096, 64, true),
                      'minutes': minutes::FLOAT, 'n': n::INTEGER}, 'cells', 4096) AS tile FROM m",
      cell,
      paste(where, collapse = " AND "),
      z,
      x,
      y
    )
  )$tile[[1]]
}

pings_layer <- rb_map_layer(
  "pings",
  "grid",
  label = "Pings",
  info = "The example Danish vessels' time (dansk), summed by cell: each ping stands for the minutes to the vessel's next ping, on the ping or spread along the track to it.",
  tiles = ping_tile,
  meta = list(
    options = list(behaviour = data.frame(value = c("Fishing", "Other"))),
    v = 2
  ),
  filters = list(
    list(
      id = "behaviour",
      label = "Behaviour",
      options = list(list(value = "", label = "all")),
      meta = TRUE
    ),
    list(
      id = "cell",
      label = "Cells",
      default = "0.01",
      options = list(
        list(value = "0.05", label = "0.05°"),
        list(value = "0.02", label = "0.02°"),
        list(value = "0.01", label = "0.01°"),
        list(value = "0.005", label = "0.005°"),
        list(value = "0.002", label = "0.002°")
      )
    ),
    list(
      id = "sample",
      label = "Pings",
      options = list(
        list(value = "all", label = "every ping (10 s)"),
        list(value = "5min", label = "one per 5 minutes")
      )
    ),
    list(
      id = "interp",
      label = "Interpolate",
      options = list(
        list(value = "off", label = "off: on the ping"),
        list(value = "on", label = "on: along the track")
      )
    ),
    list(id = "metric", type = "metric", label = "Metric")
  ),
  metrics = list(
    list(field = "minutes", label = "minutes"),
    list(field = "n", label = "points")
  ),
  popup = list(
    fields = list(
      list(field = "minutes", label = "minutes", format = "value"),
      list(field = "n", label = "points", format = "count")
    )
  )
)

harbours_layer <- rb_map_layer(
  "harbours",
  "vector",
  label = "Harbours",
  visible = FALSE,
  tiles = rb_map_tiles_table(
    rb_read_example("dansk_harbours"),
    properties = "SI_HARB",
    points = "harbour_points"
  ),
  count = list(text = "{1} harbours"),
  style = list(
    list(
      id = "harbours-fill",
      type = "fill",
      "source-layer" = "features",
      minzoom = 9,
      paint = list("fill-color" = "#d9822b", "fill-opacity" = 0.4)
    ),
    list(
      id = "harbours-dot",
      type = "circle",
      "source-layer" = "harbour_points",
      maxzoom = 9,
      paint = list("circle-color" = "#d9822b", "circle-radius" = 3)
    )
  ),
  legend = list(list(swatch = list(dot = "#d9822b"), label = "harbour")),
  popup = list(
    click = c("harbours-fill", "harbours-dot"),
    title = "SI_HARB",
    fields = list(list(field = "kind", label = "kind"))
  )
)

m <- rb_map(
  rb_map_group("data", "Data", list(pings_layer)),
  rb_map_group("ref", "Reference", list(harbours_layer)),
  title = "Danish example pings",
  view = list(center = c(12.5, 56), zoom = 7),
  texts = list(
    Pings = list(en = "Pings", da = "Positioner"),
    Harbours = list(en = "Harbours", da = "Havne")
  ),
  languages = c("en", "da")
)

rb_map_serve(m, port = 8797) # opens the page in RStudio's Viewer or a browser; it answers while the console is idle
# rb_map_stop()
