# The map viewer (map.R): the config, the server's routes called as httpuv calls them (no network), the checks on a
# grid's parameters, the table tiles, and a real server started and stopped. All on ramb's example data.

skip_if_no_spatial <- function() {
  con <- duckdbfs::cached_connection()
  ok <- !inherits(
    try(DBI::dbExecute(con, "LOAD spatial"), silent = TRUE),
    "try-error"
  ) ||
    !inherits(
      try(DBI::dbExecute(con, "INSTALL spatial; LOAD spatial"), silent = TRUE),
      "try-error"
    )
  skip_if_not(ok, "DuckDB's spatial extension is not available")
}

get <- function(app, path, query = "") {
  app$call(list(REQUEST_METHOD = "GET", PATH_INFO = path, QUERY_STRING = query))
}

# the tile holding a point
tile_of <- function(lon, lat, z) {
  n <- 2^z
  c(
    z = z,
    x = floor((lon + 180) / 360 * n),
    y = floor(
      (1 - log(tan(lat * pi / 180) + 1 / cos(lat * pi / 180)) / pi) / 2 * n
    )
  )
}

# a map of the example pings by cell (a grid, filtered by behaviour) and the harbours (as tiles and as GeoJSON)
example_map <- function() {
  con <- duckdbfs::cached_connection()
  pings <- dbplyr::sql_render(rb_read_example("dansk"))
  grid_tile <- function(z, x, y, params) {
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
  grid <- rb_map_layer(
    "pings",
    "grid",
    label = "Pings",
    tiles = grid_tile,
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
  h <- rb_read_example("dansk_harbours")
  harbours <- rb_map_layer(
    "harbours",
    "vector",
    label = "Harbours",
    visible = FALSE,
    tiles = rb_map_tiles_table(
      h,
      properties = "SI_HARB",
      points = "harbour_points"
    ),
    style = list(list(
      id = "harbours-fill",
      type = "fill",
      "source-layer" = "features",
      paint = list("fill-color" = "#d9822b")
    )),
    popup = list(
      click = "harbours-fill",
      title = "SI_HARB",
      fields = list(list(field = "kind", label = "kind"))
    )
  )
  x <- dplyr::collect(h)
  sfh <- sf::st_sf(
    x["SI_HARB"],
    geometry = sf::st_as_sfc(structure(x$geometry, class = "WKB"), crs = 4326)
  )
  outline <- rb_map_layer(
    "outline",
    "geojson",
    label = "Outlines",
    data = sfh[1:5, ],
    style = list(list(
      id = "outline-line",
      type = "line",
      paint = list("line-color" = "#333")
    ))
  )
  rb_map(
    rb_map_group("data", "Data", list(grid)),
    rb_map_group("ref", "Reference", list(harbours, outline)),
    title = "Example",
    view = list(center = c(12.5, 56), zoom = 7),
    texts = list(Pings = list(en = "Pings", da = "Positioner")),
    languages = c("da", "en"),
    language = "en"
  )
}

test_that("a layer and a map are checked as they are made", {
  noop <- function(z, x, y) raw(0)
  st <- list(list(id = "a", type = "line"))
  expect_error(
    rb_map_layer("Bad-id", "vector", tiles = noop, style = st),
    "lower case"
  )
  expect_error(
    rb_map_layer("lang", "vector", tiles = noop, style = st),
    "address"
  )
  expect_error(rb_map_layer("a", "vector", style = st), "needs `tiles`")
  expect_error(rb_map_layer("a", "grid", tiles = noop), "needs `metrics`")
  expect_error(rb_map_layer("a", "geojson", style = st), "needs `data`")
  expect_error(rb_map_layer("a", "vector", tiles = noop), "needs `style`")
  a <- rb_map_layer("a", "vector", tiles = noop, style = st)
  expect_error(rb_map(rb_map_group("g", layers = list(a, a))), "used twice")
  g <- function(id) {
    rb_map_layer(
      id,
      "grid",
      tiles = noop,
      metrics = list(list(field = "n", label = "n"))
    )
  }
  expect_error(
    rb_map(rb_map_group("g", layers = list(g("a"), g("b")))),
    "one grid"
  )
  expect_error(
    rb_map(rb_map_group("g", layers = list(a)), palette = "nope"),
    "palette"
  )
  expect_error(rb_map(list()), "rb_map_group")
})

test_that("the config keeps one-element arrays as arrays", {
  skip_if_not_installed("jsonlite")
  a <- rb_map_layer(
    "a",
    "vector",
    tiles = function(z, x, y) raw(0),
    popup = list(click = "a-line", title = "x"),
    style = list(list(
      id = "a-line",
      type = "line",
      filter = list("==", list("get", "k"), "v"),
      minzoom = 3,
      layout = list("text-field" = list("zoom"))
    ))
  )
  m <- rb_map(
    rb_map_group("g", "G", list(a)),
    languages = "en",
    about = "about1"
  )
  j <- as.character(jsonlite::toJSON(
    m$config,
    auto_unbox = TRUE,
    null = "null"
  ))
  expect_match(j, '"layers":["a"]', fixed = TRUE)
  expect_match(j, '"click":["a-line"]', fixed = TRUE)
  expect_match(j, '"languages":["en"]', fixed = TRUE)
  expect_match(j, '"about":["about1"]', fixed = TRUE)
  expect_match(j, '"text-field":["zoom"]', fixed = TRUE)
  expect_match(j, '"filter":["==",["get","k"],"v"]', fixed = TRUE)
  expect_length(m$config$palettes, 12)
  expect_equal(
    vapply(m$config$basemaps, `[[`, "", "id"),
    c("light", "voyager", "dark", "ocean", "satellite", "none")
  )
})

test_that("the server's routes: the page, its config, meta, tiles and data", {
  skip_if_not_installed("jsonlite")
  skip_if_no_spatial()
  m <- example_map()
  app <- .rb_map_app(m)
  page <- get(app, "/")
  expect_equal(page$status, 200L)
  expect_false(grepl("__CONFIG__", page$body, fixed = TRUE))
  expect_match(page$body, '"title":"Example"', fixed = TRUE)
  cfg <- jsonlite::fromJSON(
    get(app, "/config.json")$body,
    simplifyVector = FALSE
  )
  expect_equal(
    vapply(cfg$layers, `[[`, "", "id"),
    c("pings", "harbours", "outline")
  )
  meta <- jsonlite::fromJSON(get(app, "/meta")$body)
  expect_equal(meta$engine, .rb_map_engine)
  expect_equal(meta$layers$harbours$count, 942L)
  expect_true(nzchar(meta$layers$harbours$v)) # the server's start, in the tiles' address
  expect_equal(
    get(app, "/data/outline.geojson")$headers$`Cache-Control`,
    "no-store"
  )
  expect_equal(meta$layers$pings$options$behaviour$value, c("Fishing", "Other"))
  t <- tile_of(12.6, 56.0, 7)
  all <- get(app, sprintf("/tiles/pings/%d/%d/%d.pbf", t["z"], t["x"], t["y"]))
  fishing <- get(
    app,
    sprintf("/tiles/pings/%d/%d/%d.pbf", t["z"], t["x"], t["y"]),
    "behaviour=Fishing&v=1"
  )
  expect_equal(all$status, 200L)
  expect_gt(length(all$body), length(fishing$body))
  expect_gt(length(fishing$body), 0)
  # a harbour's own tile has features in both layers
  con <- duckdbfs::cached_connection()
  p <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT ST_X(ST_PointOnSurface(geometry)) AS x, ST_Y(ST_PointOnSurface(geometry)) AS y FROM (%s) LIMIT 1",
      dbplyr::sql_render(rb_read_example("dansk_harbours"))
    )
  )
  h <- tile_of(p$x, p$y, 10)
  ht <- get(
    app,
    sprintf("/tiles/harbours/%d/%d/%d.pbf", h["z"], h["x"], h["y"])
  )
  expect_equal(ht$status, 200L)
  text <- rawToChar(ht$body[ht$body >= as.raw(32) & ht$body < as.raw(127)]) # the layers' names, among the bytes
  expect_match(text, "features", fixed = TRUE)
  expect_match(text, "harbour_points", fixed = TRUE)
  expect_match(
    get(app, "/data/outline.geojson")$body,
    "FeatureCollection",
    fixed = TRUE
  )
})

test_that("a table's properties of any type reach its tiles, cast to what a tile holds", {
  skip_if_no_spatial()
  con <- duckdbfs::cached_connection()
  duckdb::duckdb_register(
    con,
    "rb_test_types",
    data.frame(
      lon = 12.6,
      lat = 56.0,
      gear = factor("gillnet"), # an ENUM in DuckDB
      day = as.Date("2026-10-24"),
      time = as.POSIXct("2026-10-24 21:00:00", tz = "UTC")
    )
  )
  on.exit(duckdb::duckdb_unregister(con, "rb_test_types"))
  x <- dplyr::tbl(
    con,
    dplyr::sql(
      "SELECT *, 3::TINYINT AS n, 2.5::DECIMAL(4, 1) AS kg, 7::UINTEGER AS id FROM rb_test_types"
    )
  )
  tiles <- rb_map_tiles_table(
    x,
    lon = "lon",
    lat = "lat",
    properties = c("gear", "day", "time", "n", "kg", "id")
  )
  t <- tile_of(12.6, 56.0, 9)
  b <- tiles(t["z"], t["x"], t["y"])
  expect_gt(length(b), 0)
  text <- rawToChar(b[b >= as.raw(32) & b < as.raw(127)])
  expect_match(text, "gillnet", fixed = TRUE)
  expect_match(text, "2026-10-24", fixed = TRUE)
  expect_equal(
    .rb_mvt_cast(
      c("a", "b", "c", "d", "e"),
      c("INTEGER", "SMALLINT", "UINTEGER", "DECIMAL(4,1)", "ENUM('x')")
    ),
    c(
      "a",
      "CAST(b AS INTEGER)",
      "CAST(c AS BIGINT)",
      "CAST(d AS DOUBLE)",
      "CAST(e AS VARCHAR)"
    )
  )
})

test_that("tables cut into tiles never share a name, after set.seed() or a reload", {
  skip_if_no_spatial()
  h <- rb_read_example("dansk_harbours")
  set.seed(1)
  x <- stats::runif(1)
  set.seed(1)
  a <- rb_map_tiles_table(h, properties = "SI_HARB")
  expect_equal(stats::runif(1), x) # the user's random numbers are as they were
  set.seed(1)
  b <- rb_map_tiles_table(h, properties = "SI_HARB")
  .rb_tiles$n <- 0L # as after the package is loaded again, the connection's tables still there
  d <- rb_map_tiles_table(h, properties = "SI_HARB")
  expect_equal(
    c(attr(a, "count"), attr(b, "count"), attr(d, "count")),
    rep(942L, 3)
  )
})

test_that("a tile function that fails replies with its reason", {
  skip_if_not_installed("jsonlite")
  boom <- rb_map_layer(
    "boom",
    "vector",
    label = "Boom",
    tiles = function(z, x, y) stop("no tile here\nsecond line"),
    style = list(list(
      id = "boom-line",
      type = "line",
      "source-layer" = "features",
      paint = list("line-color" = "#333")
    ))
  )
  app <- .rb_map_app(rb_map(rb_map_group("g", "G", list(boom)), title = "Boom"))
  r <- get(app, "/tiles/boom/1/0/0.pbf")
  expect_equal(r$status, 500L)
  expect_equal(r$body, "no tile here")
})

test_that("a grid's parameters are checked before its tile function sees them", {
  skip_if_not_installed("jsonlite")
  skip_if_no_spatial()
  app <- .rb_map_app(example_map())
  bad <- get(app, "/tiles/pings/7/68/39.pbf", "behaviour=Fishing%27%3B%20DROP")
  expect_equal(bad$status, 400L)
  expect_match(bad$body, "behaviour is Fishing, Other", fixed = TRUE)
  expect_equal(get(app, "/tiles/pings/30/0/0.pbf")$status, 400L)
  expect_equal(get(app, "/tiles/pings/2/9/0.pbf")$status, 400L)
  expect_equal(get(app, "/tiles/nope/2/1/1.pbf")$status, 404L)
  expect_equal(get(app, "/tiles/outline/2/1/1.pbf")$status, 404L)
  expect_equal(get(app, "/data/pings.geojson")$status, 404L)
  expect_equal(get(app, "/elsewhere")$status, 404L)
  expect_equal(
    app$call(list(
      REQUEST_METHOD = "POST",
      PATH_INFO = "/",
      QUERY_STRING = ""
    ))$status,
    405L
  )
})

test_that("a grid's range filter takes its bounds together and within its meta", {
  f <- list(list(id = "years", type = "range", params = list("y1", "y2")))
  meta <- list(ranges = list(years = c(2007, 2026)))
  expect_equal(
    .rb_map_params(f, meta, list(y1 = "2010", y2 = "2012")),
    list(y1 = "2010", y2 = "2012")
  )
  expect_match(.rb_map_params(f, meta, list(y1 = "2010")), "given together")
  expect_match(
    .rb_map_params(f, meta, list(y1 = "1990", y2 = "2012")),
    "lie in 2007 to 2026"
  )
  expect_equal(.rb_map_params(f, meta, list()), list())
})

test_that("a server starts in the background and stops", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  a <- rb_map_layer(
    "a",
    "vector",
    tiles = function(z, x, y) raw(0),
    style = list(list(id = "a-line", type = "line"))
  )
  port <- httpuv::randomPort()
  url <- rb_map_serve(
    rb_map(rb_map_group("g", layers = list(a))),
    port = port,
    open = FALSE,
    query = list(lang = "en")
  )
  expect_equal(url, sprintf("http://localhost:%d/?lang=en", port))
  expect_equal(rb_map_stop(port), port)
  expect_equal(rb_map_stop(port), integer(0))
})

test_that("a layer's variants: styles to choose between in the page, checked as they are made", {
  skip_if_not_installed("jsonlite")
  st <- list(list(id = "a-dot", type = "circle", "source-layer" = "features"))
  a <- rb_map_layer(
    "a",
    "vector",
    tiles = function(z, x, y) raw(0),
    style = st,
    variants = list(
      label = "Colour by",
      options = list(
        list(
          value = "x",
          paint = list("a-dot" = list("circle-color" = "#000000"))
        ),
        list(
          value = "y",
          label = "Y",
          paint = list("a-dot" = list("circle-color" = "#ffffff"))
        )
      )
    )
  )
  expect_equal(
    vapply(a$config$variants$options, `[[`, "", "label"),
    c("x", "Y")
  ) # a label defaults to the value
  cfg <- jsonlite::fromJSON(
    .rb_map_app(rb_map(rb_map_group("g", layers = list(a))))$call(
      list(
        REQUEST_METHOD = "GET",
        PATH_INFO = "/config.json",
        QUERY_STRING = ""
      )
    )$body,
    simplifyVector = FALSE
  )
  expect_equal(
    cfg$layers[[1]]$variants$options[[2]]$paint$`a-dot`$`circle-color`,
    "#ffffff"
  )
  bad <- function(v) {
    rb_map_layer(
      "a",
      "vector",
      tiles = function(z, x, y) raw(0),
      style = st,
      variants = v
    )
  }
  expect_error(
    bad(list(options = list(list(value = "x", paint = list(nope = list()))))),
    "style ids"
  )
  expect_error(bad(list(options = list())), "options")
  expect_error(
    bad(list(
      options = list(
        list(value = "x", paint = list("a-dot" = list())),
        list(value = "x", paint = list("a-dot" = list()))
      )
    )),
    "distinct"
  )
})

test_that("pings on a map, coloured by a column", {
  skip_if_no_spatial()
  d <- rb_read_example("dansk")
  m <- rb_map_pings(d, time = "time_stamp")
  expect_s3_class(m, "rb_map")
  expect_equal(vapply(m$config$layers, `[[`, "", "id"), "pings")
  expect_equal(m$config$basemaps[[1]]$id, "dark")
  # a numeric column: a continuous palette, grey where it is missing; five steps in the legend
  expect_equal(
    m$config$layers[[1]]$style[[1]]$paint[["circle-color"]][[1]],
    "case"
  )
  expect_length(m$config$layers[[1]]$legend, 5)
  expect_equal(attr(m$layers$pings$tiles, "count"), 395558L)
  # a column by name: a colour per value
  m2 <- rb_map_pings(d, behaviour, time = "time_stamp")
  expect_equal(
    m2$config$layers[[1]]$style[[1]]$paint[["circle-color"]][[1]],
    "match"
  )
  expect_setequal(
    vapply(m2$config$layers[[1]]$legend, `[[`, "", "label"),
    c("Fishing", "Other")
  )
  # a data frame and sf points give the same pings
  df <- dplyr::collect(utils::head(d, 500))
  expect_equal(
    attr(rb_map_pings(df, time = "time_stamp")$layers$pings$tiles, "count"),
    500L
  )
  s <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  expect_equal(
    attr(rb_map_pings(s, time = "time_stamp")$layers$pings$tiles, "count"),
    500L
  )
  expect_error(rb_map_pings(d, nope), "No column nope")
  expect_output(print(m), "<rb_map> Track", fixed = TRUE) # not at a console: a summary
})

test_that("pings coloured by several columns, chosen in the page", {
  skip_if_no_spatial()
  d <- rb_read_example("dansk")
  m <- rb_map_pings(d, c(speed, course, behaviour), time = "time_stamp")
  l <- m$config$layers[[1]]
  expect_equal(
    vapply(l$variants$options, `[[`, "", "value"),
    c("speed", "course", "behaviour")
  )
  # the page opens on the first: the style's paint is the first option's
  expect_identical(
    l$style[[1]]$paint[["circle-color"]],
    l$variants$options[[1]]$paint[["pings-dot"]][["circle-color"]]
  )
  expect_equal(
    vapply(l$variants$options, function(o) length(o$legend), 1),
    c(5, 5, 2)
  )
  expect_equal(
    l$variants$options[[3]]$paint[["pings-dot"]][["circle-color"]][[2]][[2]],
    "rb_class_3"
  )
  expect_equal(
    vapply(l$popup$fields, `[[`, "", "field"),
    c("speed", "course", "behaviour")
  )
  # a tile carries every column, so a choice fetches nothing
  t <- m$layers$pings$tiles(11, 1095, 641)
  text <- rawToChar(t[t >= as.raw(32) & t < as.raw(127)])
  for (f in c("speed", "course", "rb_class_3")) {
    expect_match(text, f, fixed = TRUE)
  }
  cols <- c("speed", "behaviour") # a variable holding column names
  expect_length(
    rb_map_pings(d, cols, time = "time_stamp")$config$layers[[
      1
    ]]$variants$options,
    2
  )
  expect_null(
    rb_map_pings(d, "speed", time = "time_stamp")$config$layers[[1]]$variants
  )
})

test_that("a track: a line per vessel broken at silences, or coloured by runs of a column, joined", {
  skip_if_no_spatial()
  d <- rb_read_example("dansk")
  one <- rb_map_track(d, time = "time_stamp", by = vessel_id, gap_h = Inf)
  expect_equal(attr(one$layers$path$tiles, "count"), 14L)
  plain <- rb_map_track(d, time = "time_stamp", by = vessel_id)
  expect_gt(attr(plain$layers$path$tiles, "count"), 14L)
  m <- rb_map_track(d, behaviour, time = "time_stamp", by = vessel_id)
  p <- m$config$layers[[1]]
  expect_equal(p$label, "Track by behaviour")
  expect_setequal(vapply(p$legend, `[[`, "", "label"), c("Fishing", "Other"))
  # a run ends where the next begins, except at a silence: as many unjoined ends as the plain line has stretches
  ends <- DBI::dbGetQuery(
    duckdbfs::cached_connection(),
    sprintf(
      "WITH r AS (SELECT ST_AsText(ST_StartPoint(g3857)) AS s, ST_AsText(ST_EndPoint(g3857)) AS e FROM %s)
       SELECT count(*) FILTER (WHERE e NOT IN (SELECT s FROM r)) AS n FROM r",
      attr(m$layers$path$tiles, "table")
    )
  )$n
  expect_equal(ends, attr(plain$layers$path$tiles, "count"))
  expect_error(rb_map_track(d, by = vessel_id), "time order")
  expect_error(rb_map_track(d, nope, time = "time_stamp"), "No column nope")
})

test_that("layers compose: each adds to the map piped in, from its pings and settings", {
  skip_if_no_spatial()
  d <- rb_read_example("dansk")
  m <- d |>
    rb_map_track(behaviour, time = "time_stamp", by = vessel_id) |>
    rb_map_pings(c(behaviour, speed))
  expect_equal(vapply(m$config$layers, `[[`, "", "id"), c("path", "pings")) # the later drawn over the earlier
  # the line's colours are the points' for the same column
  expect_identical(
    m$config$layers[[1]]$style[[1]]$paint[["line-color"]][-2],
    m$config$layers[[2]]$style[[1]]$paint[["circle-color"]][-2]
  )
  expect_equal(m$config$layers[[2]]$popup$title, "time_stamp") # the map's time, not given again
  # other pings on top, numbered
  h <- dplyr::mutate(dplyr::collect(utils::head(d, 1000)), kind = "sample")
  m2 <- rb_map_pings(m, kind, data = h)
  expect_equal(
    vapply(m2$config$layers, `[[`, "", "id"),
    c("path", "pings", "pings_2")
  )
  expect_equal(m2$config$layers[[3]]$label, "Pings 2")
  expect_length(attr(m2, "tables"), 3)
  expect_error(rb_map_pings(m, by = vessel_id), "give `data`")
  expect_error(
    rb_map(rb_map_group("g", layers = m$layers)) |> rb_map_pings(),
    "rb_map\\(\\)"
  )
})

test_that("a map is served when printed at a console; a replaced map's tables are dropped and made again", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  skip_if_no_spatial()
  con <- duckdbfs::cached_connection()
  exists <- function(m) {
    vapply(
      attr(m, "tables"),
      function(t) DBI::dbExistsTable(t$con, t$name),
      logical(1)
    )
  }
  df <- dplyr::collect(utils::head(rb_read_example("dansk"), 500))
  port <- httpuv::randomPort()
  m1 <- rb_map_track(df, time = "time_stamp", port = port) |> rb_map_pings()
  m2 <- rb_map_pings(df, behaviour, time = "time_stamp", port = port)
  rlang::with_interactive(print(m1))
  expect_true(all(exists(m1)))
  rlang::with_interactive(print(m2)) # on the same port: m1's tables go
  expect_false(any(exists(m1)))
  expect_gt(length(m1$layers$pings$tiles(10, 547, 320)), 0) # shown again, a table is made again
  expect_true(exists(m1)[2])
  expect_equal(rb_map_stop(port), port)
  expect_false(any(exists(m2)))
})

test_that("two tracks by different columns: each cut by its own runs, a layer of its own", {
  skip_if_no_spatial()
  d <- rb_read_example("dansk")
  m <- d |>
    rb_map_track(behaviour, time = "time_stamp", by = vessel_id) |>
    rb_map_track(speed) |>
    rb_map_pings()
  expect_equal(
    vapply(m$config$layers, `[[`, "", "id"),
    c("path", "path_2", "pings")
  )
  expect_equal(
    vapply(m$config$layers, `[[`, "", "label"),
    c("Track by behaviour", "Track by speed", "Pings")
  )
  one <- rb_map_track(d, behaviour, time = "time_stamp", by = vessel_id)
  expect_equal(
    attr(m$layers$path$tiles, "count"),
    attr(one$layers$path$tiles, "count")
  ) # its own runs
  expect_equal(
    m$config$layers[[2]]$style[[1]]$paint[["line-color"]][[1]],
    "case"
  ) # speed: a palette
  # a name two layers would share is numbered
  two <- d |>
    rb_map_track(time = "time_stamp", by = vessel_id) |>
    rb_map_track()
  expect_equal(
    vapply(two$config$layers, `[[`, "", "label"),
    c("Track", "Track 2")
  )
  expect_error(
    rb_map_track(d, c(behaviour, speed), time = "time_stamp"),
    "call rb_map_track() again",
    fixed = TRUE
  )
})
