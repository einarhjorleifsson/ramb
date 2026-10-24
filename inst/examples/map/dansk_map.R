# A map of ramb's example data: the Danish pings (dansk) counted by cell, and the harbours (dansk_harbours). Run with
# source(system.file("examples", "map", "dansk_map.R", package = "ramb")); the map opens at http://127.0.0.1:8797/
library(ramb)

con <- duckdbfs::cached_connection()
DBI::dbExecute(con, "LOAD spatial") # if it is not there: DBI::dbExecute(con, "INSTALL spatial")

pings <- dbplyr::sql_render(rb_read_example("dansk"))

# A grid layer's tile function: pings counted by 0.01-degree cell, with the behaviour filter's value if one is chosen.
# The server has checked the value against the filter's options before it arrives here.
ping_tile <- function(z, x, y, params) {
  where <- if (is.null(params$behaviour)) {
    ""
  } else {
    sprintf("WHERE behaviour = %s", DBI::dbQuoteString(con, params$behaviour))
  }
  DBI::dbGetQuery(
    con,
    sprintf(
      "WITH c AS (SELECT floor(lon * 100) AS cx, floor(lat * 100) AS cy, count(*) AS n FROM (%s) %s GROUP BY ALL),
     m AS (SELECT n, ST_Transform(ST_MakeEnvelope(cx / 100, cy / 100, (cx + 1) / 100, (cy + 1) / 100),
                                  'EPSG:4326', 'EPSG:3857', always_xy := true) AS env FROM c)
     SELECT ST_AsMVT({'geometry': ST_AsMVTGeom(env, ST_Extent(ST_TileEnvelope(%d, %d, %d)), 4096, 64, true),
                      'n': n::INTEGER}, 'cells', 4096) AS tile FROM m",
      pings,
      where,
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
  info = "Pings of the example Danish vessels (dansk), counted by 0.01-degree cell.",
  tiles = ping_tile,
  meta = list(
    options = list(behaviour = data.frame(value = c("Fishing", "Other"))),
    v = 1
  ),
  filters = list(
    list(
      id = "behaviour",
      label = "Behaviour",
      options = list(list(value = "", label = "all")),
      meta = TRUE
    ),
    list(id = "metric", type = "metric", label = "Metric")
  ),
  metrics = list(list(field = "n", label = "pings")),
  popup = list(
    fields = list(list(field = "n", label = "pings", format = "count"))
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
