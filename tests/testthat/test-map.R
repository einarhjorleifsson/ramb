# The map viewer (map.R): the config, the server's routes called as httpuv calls them (no network), the checks on a
# grid's parameters, the table tiles, and a real server started and stopped. All on ramb's example data.

skip_if_no_spatial <- function() {
  con <- .rb_connection()
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
  con <- .rb_connection()
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
  con <- .rb_connection()
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
  con <- .rb_connection()
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

test_that("a layer's state: values its style reads, checked as they are made", {
  skip_if_not_installed("jsonlite")
  gj <- '{"type": "FeatureCollection", "features": []}'
  st <- list(
    list(id = "n-area", type = "fill", filter = list("global-state", "n_areas")),
    list(id = "n-label", type = "symbol",
         filter = list("<=", list("-", list("get", "min_zoom"), list("global-state", "n_early")), list("zoom")),
         paint = list("text-color" = list("global-state", "n_colour")))
  )
  state <- list(
    list(id = "areas", label = "Areas", default = FALSE),
    list(id = "early", label = "Names appear",
         options = list(list(value = -1, label = "later"), list(value = 0, label = "as set"),
                        list(value = 1))),
    list(id = "colour", options = list(list(value = "#0b3954", label = "dark blue"),
                                       list(value = "#ffffff", label = "white")), default = "#ffffff")
  )
  n <- rb_map_layer("n", "geojson", data = gj, style = st, state = state)
  s <- n$config$state
  expect_equal(vapply(s, `[[`, "", "id"), c("areas", "early", "colour"))
  expect_equal(s[[2]]$default, -1) # the first option's value when not given
  expect_equal(s[[2]]$options[[3]]$label, "1") # a label defaults to the value
  expect_equal(s[[3]]$label, "colour") # and a control's to its id
  cfg <- jsonlite::fromJSON(
    .rb_map_app(rb_map(rb_map_group("g", layers = list(n))))$call(
      list(REQUEST_METHOD = "GET", PATH_INFO = "/config.json", QUERY_STRING = "")
    )$body,
    simplifyVector = FALSE
  )
  expect_identical(cfg$layers[[1]]$state[[1]]$default, FALSE)
  expect_identical(cfg$layers[[1]]$state[[2]]$options[[1]]$value, -1L) # a number stays a number in the page
  bad <- function(state, style = st, id = "n") {
    rb_map_layer(id, "geojson", data = gj, style = style, state = state)
  }
  expect_error(bad(list(list(id = "nope", default = TRUE))), "read by none")
  expect_error(bad(state[1], style = st[2]), "global-state")
  expect_error(bad(list(list(id = "areas"))), "TRUE or FALSE")
  expect_error(bad(list(list(id = "areas", default = "yes"))), "TRUE or FALSE")
  expect_error(bad(list(list(id = "Areas", default = TRUE))), "lower-case")
  expect_error(bad(list(list(id = "by", default = TRUE))), "own controls")
  expect_error(bad(list(state[[1]], state[[1]])), "once")
  expect_error(bad(list(list(id = "early", options = list(list(value = 1), list(value = 1))))), "distinct")
  expect_error(bad(list(list(id = "early", options = list(list(value = 1)), default = 2))), "one of its options")
  expect_error(bad(list(list(id = "early", options = list(list(value = c(1, 2)))))), "options")
  expect_error(bad("areas"), "list of controls")
  expect_error(bad(list()), "list of controls")
  expect_error(
    rb_map_layer("g", "grid", tiles = function(z, x, y, p) raw(0), metrics = list(list(field = "n")), state = state),
    "drawn by the page"
  )
  # a layer named as another's state would share its place in the address
  other <- rb_map_layer("n_areas", "geojson", data = gj, style = list(list(id = "o", type = "fill")))
  expect_error(rb_map(rb_map_group("g", layers = list(n, other))), "another layer's state")
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
  # by default one line a vessel, however long it falls silent; with max_gap_s, broken at the silences
  one <- rb_map_track(d, time = "time_stamp", by = vessel_id)
  expect_equal(attr(one$layers$path$tiles, "count"), 14L)
  expect_equal(
    one$config$layers[[1]]$info,
    "The pings of each vessel_id joined in time order."
  )
  plain <- rb_map_track(d, time = "time_stamp", by = vessel_id, max_gap_s = 3600)
  expect_gt(attr(plain$layers$path$tiles, "count"), 14L)
  expect_match(plain$config$layers[[1]]$info, "more than 1 hour apart.", fixed = TRUE)
  # a run ends where the next begins, except at a silence: as many unjoined ends as the plain line has stretches
  ends <- function(m) {
    DBI::dbGetQuery(
      .rb_connection(),
      sprintf(
        "WITH r AS (SELECT ST_AsText(ST_StartPoint(g3857)) AS s, ST_AsText(ST_EndPoint(g3857)) AS e FROM %s)
         SELECT count(*) FILTER (WHERE e NOT IN (SELECT s FROM r)) AS n FROM r",
        attr(m$layers$path$tiles, "table")
      )
    )$n
  }
  m <- rb_map_track(d, behaviour, time = "time_stamp", by = vessel_id)
  p <- m$config$layers[[1]]
  expect_equal(p$label, "Track by behaviour")
  expect_setequal(vapply(p$legend, `[[`, "", "label"), c("Fishing", "Other"))
  expect_equal(p$info, "The pings of each vessel_id joined in time order and coloured by behaviour.")
  expect_equal(ends(m), 14L)
  m1 <- rb_map_track(d, behaviour, time = "time_stamp", by = vessel_id, max_gap_s = 3600)
  expect_equal(ends(m1), attr(plain$layers$path$tiles, "count"))
  expect_match(m1$config$layers[[1]]$info, "1 hour apart, and coloured by behaviour.", fixed = TRUE)
  # the old argument, in hours, warns once and gives the same line
  .rb_reset_renamed()
  expect_warning(
    old <- rb_map_track(d, time = "time_stamp", by = vessel_id, gap_h = 1),
    "is now `max_gap_s = `"
  )
  expect_equal(
    attr(old$layers$path$tiles, "count"),
    attr(plain$layers$path$tiles, "count")
  )
  expect_error(
    rb_map_track(d, time = "time_stamp", max_gap_s = 0),
    "number of seconds"
  )
  expect_error(
    rb_map_track(d, time = "time_stamp", max_gap_s = "1 hour"),
    "number of seconds"
  )
  expect_error(rb_map_track(d, by = vessel_id), "time order")
  expect_error(rb_map_track(d, nope, time = "time_stamp"), "No column nope")
})

test_that("a gap in words: whole hours, minutes or seconds, else hours", {
  expect_equal(.rb_duration_text(3600), "1 hour")
  expect_equal(.rb_duration_text(7200), "2 hours")
  expect_equal(.rb_duration_text(5400), "1.5 hours")
  expect_equal(.rb_duration_text(1800), "30 minutes")
  expect_equal(.rb_duration_text(60), "1 minute")
  expect_equal(.rb_duration_text(90), "90 seconds")
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
  con <- .rb_connection()
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

test_that("grouped pings: a choice per group column, over every layer that has it", {
  skip_if_no_spatial()
  d <- rb_read_example("dansk")
  m <- d |>
    dplyr::group_by(vessel_id) |>
    rb_map_track(time = "time_stamp") |>
    rb_map_pings()
  ch <- m$config$choose
  expect_equal(vapply(ch$fields, `[[`, "", "field"), "vessel_id")
  expect_length(ch$fields[[1]]$options, 14)
  expect_equal(unlist(ch$fields[[1]]$layers), c("path", "pings"))
  expect_length(ch$fields[[1]]$options[[1]]$bbox, 4)
  expect_false(ch$fields[[1]]$multi) # 14 values: a list, one at a time
  # every ping counted once among the options
  counts <- as.integer(gsub(
    ",",
    "",
    sub(
      "[)]$",
      "",
      sub("^.*[(]", "", vapply(ch$fields[[1]]$options, `[[`, "", "label"))
    )
  ))
  expect_equal(sum(counts), 395558L)
  # the groups cut the track: each vessel's line its own, as with `by`
  expect_equal(
    attr(m$layers$path$tiles, "count"),
    attr(
      rb_map_track(d, time = "time_stamp", by = vessel_id)$layers$path$tiles,
      "count"
    )
  )
  # `choose` without grouping; two fields, and the extents of their combinations
  m2 <- rb_map_pings(d, choose = c(vessel_id, behaviour), time = "time_stamp")
  expect_equal(
    vapply(m2$config$choose$fields, `[[`, "", "field"),
    c("vessel_id", "behaviour")
  )
  expect_true(m2$config$choose$fields[[2]]$multi) # two values: chips, so both can show
  expect_gt(length(m2$config$choose$extents), 14)
  expect_match(m2$config$choose$extents[[1]]$key, intToUtf8(31), fixed = TRUE)
  # a number is matched as a number; other data without the field are left as they are
  m3 <- d |>
    dplyr::mutate(r = .rowid %% 3L) |>
    dplyr::group_by(r) |>
    rb_map_pings(time = "time_stamp") |>
    rb_map_pings(name, data = data.frame(lon = 12.6, lat = 56, name = "here"))
  expect_true(is.numeric(m3$config$choose$fields[[1]]$options[[1]]$match))
  expect_equal(unlist(m3$config$choose$fields[[1]]$layers), "pings")
  expect_null(rb_map_pings(d, time = "time_stamp")$config$choose)
  expect_warning(
    rb_map_pings(m, choose = behaviour),
    "set by the call that starts it"
  )
  expect_error(
    d |>
      dplyr::rename(base = vessel_id) |>
      dplyr::group_by(base) |>
      rb_map_pings(time = "time_stamp"),
    "named as the page's address"
  )
})

test_that("a dem layer: elevation images from an MBTiles file, served as images under their own extension", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")
  f <- tempfile(fileext = ".mbtiles")
  on.exit(unlink(f))
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  DBI::dbWriteTable(con, "metadata", data.frame(
    name = c("format", "encoding", "bounds", "minzoom", "maxzoom", "tile_size"),
    value = c("png", "terrarium", "-30,60,-5,70", "0", "1", "256")
  ))
  DBI::dbExecute(con, "CREATE TABLE tiles (zoom_level INTEGER, tile_column INTEGER, tile_row INTEGER, tile_data BLOB)")
  # zoom 1, the north-west tile: row 1 counted from the south
  DBI::dbExecute(con, "INSERT INTO tiles VALUES (1, 0, 1, ?)", params = list(list(as.raw(1:4))))
  DBI::dbDisconnect(con)

  tiles <- rb_map_tiles_mbtiles(f)
  expect_identical(tiles(1, 0, 0), as.raw(1:4))
  expect_null(tiles(1, 0, 1))
  expect_equal(attr(tiles, "bounds"), c(-30, 60, -5, 70))
  expect_error(rb_map_tiles_mbtiles(tempfile()), "path of an MBTiles file")

  shade <- list(list(id = "depth-shade", type = "hillshade"))
  l <- rb_map_layer("depth", "dem", tiles = tiles, style = shade)
  expect_equal(l$config[c("encoding", "tile_size", "format", "minzoom", "maxzoom")],
               list(encoding = "terrarium", tile_size = 256L, format = "png", minzoom = 0L, maxzoom = 1L))
  expect_equal(l$config$bounds, list(-30, 60, -5, 70)) # an array in the config's JSON
  # given arguments win over the file's
  expect_equal(rb_map_layer("depth", "dem", tiles = tiles, style = shade, tile_size = 512)$config$tile_size, 512L)
  expect_error(rb_map_layer("depth", "dem", style = shade), "needs `tiles`, a function returning an image tile")
  expect_error(rb_map_layer("depth", "dem", tiles = tiles, style = shade, encoding = "lerc"), "terrarium")
  expect_error(
    rb_map_layer("lines", "vector", tiles = function(z, x, y) raw(0), style = shade, bounds = c(0, 0, 1, 1)),
    "for a dem or contour layer"
  )

  app <- .rb_map_app(rb_map(rb_map_group("g", "G", list(l))))
  ok <- get(app, "/tiles/depth/1/0/0.png")
  expect_equal(ok$status, 200L)
  expect_equal(ok$headers$`Content-Type`, "image/png")
  expect_identical(ok$body, as.raw(1:4))
  expect_equal(get(app, "/tiles/depth/1/0/1.png")$status, 404L) # not in the file: no empty image
  expect_equal(get(app, "/tiles/depth/1/0/0.pbf")$status, 404L) # not its format
  meta <- jsonlite::fromJSON(get(app, "/meta")$body)
  expect_true(nzchar(meta$layers$depth$v))
})

test_that("a dem layer's relief controls: MapLibre's defaults filled in, each value checked, its address names taken", {
  dem <- function(z, x, y) NULL
  shade_style <- list(list(id = "relief-shade", type = "hillshade"))
  l <- rb_map_layer("relief", "dem", tiles = dem, style = shade_style, shade = TRUE)
  expect_equal(
    l$config$shade,
    list(method = "standard", direction = 335, altitude = 45, exaggeration = 0.5, opacity = 100,
         shadow = "#000000", highlight = "#ffffff", auto = TRUE)
  )
  given <- rb_map_layer("relief", "dem", tiles = dem, style = shade_style,
                        shade = list(method = "igor", direction = 315, highlight = "#FFF8E0"))$config$shade
  expect_equal(given[c("method", "direction", "altitude", "highlight")],
               list(method = "igor", direction = 315, altitude = 45, highlight = "#fff8e0"))
  expect_null(rb_map_layer("relief", "dem", tiles = dem, style = shade_style)$config$shade)

  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, shade = TRUE), "`hillshade` style")
  expect_error(
    rb_map_layer("lines", "vector", tiles = function(z, x, y) raw(0), style = shade_style, shade = TRUE),
    "`hillshade` style"
  )
  expect_error(rb_map_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(method = "sun")), "method")
  expect_error(rb_map_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(altitude = 95)), "altitude")
  expect_error(rb_map_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(shadow = "black")), "shadow")
  expect_error(rb_map_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(sun = 1)), "TRUE or a list")

  # a choice may not take a name the relief's controls write into the address
  expect_error(
    rb_map(
      rb_map_group("g", "G", list(l)),
      choose = list(fields = list(list(field = "relief_dir", options = list(list(value = "a", match = "a")),
                                       layers = "relief")))
    ),
    "relief_dir"
  )
})

test_that("a dem layer's colour controls: defaults filled in, each value checked, the palette the map's, names taken", {
  dem <- function(z, x, y) NULL
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  l <- rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = TRUE)
  expect_equal(
    l$config$colour[c("of", "floor", "cap", "scale", "mode", "cut", "cut_as", "opacity")],
    list(of = "depth", floor = 10, cap = 2000, scale = "log", mode = "ramp", cut = 0, cut_as = "clear", opacity = 100)
  )
  expect_null(l$config$colour$palette) # the map's own
  expect_equal(l$config$colour$bands, list(10, 20, 50, 100, 200, 500, 1000, 2000)) # an array in the config's JSON
  given <- rb_map_layer("depth", "dem", tiles = dem, style = colour_style,
                        colour = list(palette = "blues", cut = 20, cut_as = "grey", mode = "bands"))$config$colour
  expect_equal(given[c("palette", "cut", "cut_as", "mode")], list(palette = "blues", cut = 20, cut_as = "grey", mode = "bands"))

  shade_style <- list(list(id = "relief-shade", type = "hillshade"))
  expect_error(rb_map_layer("relief", "dem", tiles = dem, style = shade_style, colour = TRUE), "`color-relief` style")
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(cut = 15)), "cut")
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(bands = c(50, 20))), "bands")
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(scale = "sqrt")), "scale")
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(floor = 0)), "floor")
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(hue = 1)), "TRUE or a list")

  expect_error(
    rb_map(rb_map_group("g", "G", list(rb_map_layer("depth", "dem", tiles = dem, style = colour_style,
                                                    colour = list(palette = "rainbow"))))),
    "palette"
  )
  expect_error(
    rb_map(
      rb_map_group("g", "G", list(l)),
      choose = list(fields = list(list(field = "depth_cut", options = list(list(value = "a", match = "a")),
                                       layers = "depth")))
    ),
    "depth_cut"
  )
})

test_that("a contour layer: its intervals by zoom checked and kept as an object, its tiles served as images", {
  skip_if_not_installed("jsonlite")
  png1 <- as.raw(1:4)
  tiles <- structure(function(z, x, y) if (z == 1) png1 else NULL, format = "png", encoding = "terrarium", maxzoom = 1L)
  lines <- list(list(id = "contours-line", type = "line", "source-layer" = "contours"))
  l <- rb_map_layer("contours", "contour", tiles = tiles, style = lines,
                    contours = list(thresholds = list("9" = c(50, 200), "6" = c(500, 1000)), multiplier = -1))
  expect_equal(names(l$config$contours$thresholds), c("6", "9")) # in zoom order
  expect_equal(l$config$contours$multiplier, -1)
  expect_equal(l$config[c("format", "encoding", "maxzoom")], list(format = "png", encoding = "terrarium", maxzoom = 1L))
  cfg <- jsonlite::fromJSON(.rb_map_app(rb_map(rb_map_group("g", "G", list(l))))$call(
    list(REQUEST_METHOD = "GET", PATH_INFO = "/config.json", QUERY_STRING = "")
  )$body, simplifyVector = FALSE)
  expect_equal(cfg$layers[[1]]$contours$thresholds, list("6" = list(500, 1000), "9" = list(50, 200)))

  expect_error(rb_map_layer("contours", "contour", tiles = tiles, style = lines), "`contours` is a list of `thresholds`")
  expect_error(
    rb_map_layer("contours", "contour", tiles = tiles, style = lines, contours = list(thresholds = list("x" = 100))),
    "thresholds"
  )
  expect_error(
    rb_map_layer("contours", "contour", tiles = tiles, style = lines, contours = list(thresholds = list("6" = c(500, 100)))),
    "thresholds"
  )
  expect_error(
    rb_map_layer("contours", "contour", tiles = tiles, style = lines,
                 contours = list(thresholds = list("6" = 100), multiplier = 0)),
    "multiplier"
  )
  dem <- list(list(id = "d", type = "hillshade"))
  expect_error(rb_map_layer("d", "dem", tiles = tiles, style = dem, contours = list(thresholds = list("6" = 100))),
               "for a contour layer")

  app <- .rb_map_app(rb_map(rb_map_group("g", "G", list(l))))
  ok <- get(app, "/tiles/contours/1/0/0.png")
  expect_equal(ok$status, 200L)
  expect_equal(ok$headers$`Content-Type`, "image/png")
  expect_equal(get(app, "/tiles/contours/2/0/0.png")$status, 404L)
  expect_equal(get(app, "/tiles/contours/1/0/0.pbf")$status, 404L)
})

test_that("a dem layer's colours fitted to the view: the server asks the layer's fit for the view's box", {
  skip_if_not_installed("jsonlite")
  dem <- function(z, x, y) NULL
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  seen <- NULL
  fit <- function(west, south, east, north) {
    seen <<- c(west, south, east, north)
    if (west < -100) NULL else c(12.5, 480)
  }
  l <- rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(fit = fit))
  expect_true(l$config$colour$fit) # the page's button; the function stays on the server
  expect_true(is.function(l$fit))
  app <- .rb_map_app(rb_map(rb_map_group("g", "G", list(l))))
  r <- get(app, "/fit/depth", "bbox=-25.5,63.5,-20,65")
  expect_equal(r$status, 200L)
  expect_equal(jsonlite::fromJSON(r$body), list(floor = 12.5, cap = 480))
  expect_equal(seen, c(-25.5, 63.5, -20, 65))
  expect_equal(get(app, "/fit/depth", "bbox=-120,63.5,-110,65")$status, 404L) # no values there
  expect_equal(get(app, "/fit/depth", "bbox=-20,63.5,-25,65")$status, 400L) # west east of east
  expect_equal(get(app, "/fit/depth", "")$status, 400L)
  expect_equal(get(app, "/fit/other", "bbox=-25,63,-20,65")$status, 404L)
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(fit = 3)), "a function")
  expect_null(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = TRUE)$config$colour$fit)
})

test_that("a dem layer's colours follow the view by default, and its address name is taken", {
  dem <- function(z, x, y) NULL
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  expect_true(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = TRUE)$config$colour$auto)
  expect_false(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(auto = FALSE))$config$colour$auto)
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(auto = "yes")), "auto")
  l <- rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = TRUE)
  expect_error(
    rb_map(rb_map_group("g", "G", list(l)),
           choose = list(fields = list(list(field = "depth_auto", options = list(list(value = "a", match = "a")),
                                            layers = "depth")))),
    "depth_auto"
  )
})

test_that("a fit may give the values' quantiles in view, for colours in equal shares", {
  skip_if_not_installed("jsonlite")
  dem <- function(z, x, y) NULL
  colour_style <- list(list(id = "depth-colour", type = "color-relief"))
  fit <- function(west, south, east, north) {
    if (west < -100) list(floor = 1, cap = 9, quantiles = c(5, 3)) else list(floor = 3, cap = 80, quantiles = c(3, 20, 40, 80))
  }
  l <- rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(fit = fit, scale = "equal"))
  expect_equal(l$config$colour$scale, "equal")
  app <- .rb_map_app(rb_map(rb_map_group("g", "G", list(l))))
  r <- get(app, "/fit/depth", "bbox=-25,63,-20,65")
  expect_equal(r$status, 200L)
  expect_equal(jsonlite::fromJSON(r$body, simplifyVector = FALSE), list(floor = 3, cap = 80, q = list(3, 20, 40, 80)))
  expect_equal(get(app, "/fit/depth", "bbox=-120,63,-110,65")$status, 500L) # quantiles not rising
  expect_error(rb_map_layer("depth", "dem", tiles = dem, style = colour_style, colour = list(scale = "equal")),
               "needs `colour\\$fit`")
})

test_that("a contour layer's levels from the view: its fit kept on the server, following by default, its name taken", {
  skip_if_not_installed("jsonlite")
  tiles <- structure(function(z, x, y) NULL, format = "png")
  lines <- list(list(id = "contours-line", type = "line", "source-layer" = "contours"))
  fit <- function(west, south, east, north) list(floor = 3, cap = 80, quantiles = c(3, 20, 40, 80))
  l <- rb_map_layer("contours", "contour", tiles = tiles, style = lines,
                    contours = list(thresholds = list("5" = 100), multiplier = -1, fit = fit))
  expect_true(l$config$contours$fit)
  expect_true(l$config$contours$auto)
  expect_true(is.function(l$fit))
  app <- .rb_map_app(rb_map(rb_map_group("g", "G", list(l))))
  expect_equal(jsonlite::fromJSON(get(app, "/fit/contours", "bbox=-25,63,-20,65")$body)$q, c(3, 20, 40, 80))
  expect_false(rb_map_layer("contours", "contour", tiles = tiles, style = lines,
                            contours = list(thresholds = list("5" = 100), auto = FALSE))$config$contours$auto)
  expect_null(rb_map_layer("contours", "contour", tiles = tiles, style = lines,
                           contours = list(thresholds = list("5" = 100)))$config$contours$fit)
  expect_error(rb_map_layer("contours", "contour", tiles = tiles, style = lines,
                            contours = list(thresholds = list("5" = 100), fit = 1)), "a function")
  expect_error(
    rb_map(rb_map_group("g", "G", list(l)),
           choose = list(fields = list(list(field = "contours_auto", options = list(list(value = "a", match = "a")),
                                            layers = "contours")))),
    "contours_auto"
  )
})

test_that("a shading's intensity from the view: its fit answered by the route, one fit a layer", {
  skip_if_not_installed("jsonlite")
  dem <- function(z, x, y) NULL
  shade_style <- list(list(id = "relief-shade", type = "hillshade"))
  fit <- function(west, south, east, north) if (west < -100) list(exaggeration = 2) else list(exaggeration = 0.45)
  l <- rb_map_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(exaggeration = 0.6, fit = fit))
  expect_true(l$config$shade$fit)
  expect_true(l$config$shade$auto)
  expect_true(is.function(l$fit))
  app <- .rb_map_app(rb_map(rb_map_group("g", "G", list(l))))
  expect_equal(jsonlite::fromJSON(get(app, "/fit/relief", "bbox=-25,63,-20,65")$body), list(exaggeration = 0.45))
  expect_equal(get(app, "/fit/relief", "bbox=-120,63,-110,65")$status, 500L) # out of 0-1
  both <- list(list(id = "c", type = "color-relief"), list(id = "s", type = "hillshade"))
  expect_error(rb_map_layer("d", "dem", tiles = dem, style = both, shade = list(fit = fit), colour = list(fit = fit)),
               "one fit")
  expect_error(rb_map_layer("relief", "dem", tiles = dem, style = shade_style, shade = list(fit = "x")), "a function")
})

test_that("a bundle: the page, its data and tiles as scripts, the tiles once for layers on one file", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("jsonlite")
  f <- tempfile(fileext = ".mbtiles")
  d <- tempfile("bundle_")
  on.exit(unlink(c(f, d), recursive = TRUE))
  con <- DBI::dbConnect(RSQLite::SQLite(), f)
  DBI::dbWriteTable(con, "metadata", data.frame(
    name = c("format", "encoding", "bounds", "minzoom", "maxzoom"),
    value = c("png", "terrarium", "-30,60,-5,70", "0", "2")
  ))
  DBI::dbExecute(con, "CREATE TABLE tiles (zoom_level INTEGER, tile_column INTEGER, tile_row INTEGER, tile_data BLOB)")
  # the tiles inside the bounds: zoom 0 the world, zoom 1 its north-west, zoom 2 two (rows counted from the south)
  for (t in list(c(0, 0, 0), c(1, 0, 1), c(2, 1, 3), c(2, 1, 2))) {
    DBI::dbExecute(con, "INSERT INTO tiles VALUES (?, ?, ?, ?)", params = list(t[1], t[2], t[3], list(as.raw(c(t, 9)))))
  }
  DBI::dbDisconnect(con)
  expect_equal(attr(rb_map_tiles_mbtiles(f), "source"), normalizePath(f))

  # two layers on one file, each from its own reader, and a geojson layer
  depth <- rb_map_layer("depth", "dem", tiles = rb_map_tiles_mbtiles(f), style = list(list(id = "d", type = "hillshade")))
  relief <- rb_map_layer("relief", "dem", tiles = rb_map_tiles_mbtiles(f), style = list(list(id = "r", type = "hillshade")))
  gj <- '{"type":"FeatureCollection","features":[{"type":"Feature","properties":{"name":"Þórshöfn"},"geometry":{"type":"Point","coordinates":[-15.3,66.2]}}]}'
  places <- rb_map_layer("places", "geojson", data = gj, style = list(list(id = "p", type = "circle")))
  m <- rb_map(rb_map_group("g", "G", list(depth, relief, places)))
  out <- rb_map_bundle(m, d, maxzoom = 1)
  expect_equal(attr(out, "tiles"), 2L) # zooms 0 and 1, once for both layers
  expect_true(attr(out, "bytes") > 0)
  expect_false(dir.exists(file.path(d, "tiles", "relief")))
  expect_false(dir.exists(file.path(d, "tiles", "depth", "2"))) # past maxzoom
  js <- readLines(file.path(d, "tiles", "depth", "1", "0", "0.js"))
  expect_length(js, 1)
  expect_identical(
    jsonlite::base64_dec(sub('^RB_BUNDLE\\("tiles/depth/1/0/0","(.*)"\\);$', "\\1", js)),
    as.raw(c(1, 0, 1, 9))
  )
  data <- paste(readLines(file.path(d, "data", "places.js"), encoding = "UTF-8"), collapse = "\n")
  expect_match(data, '^RB_BUNDLE\\("data/places",\\{')
  expect_match(data, "Þórshöfn", fixed = TRUE)

  page <- paste(readLines(file.path(d, "index.html"), encoding = "UTF-8"), collapse = "\n")
  block <- function(id) {
    jsonlite::fromJSON(
      sub(sprintf('(?s).*<script id="%s" type="application/json">(.*?)</script>.*', id), "\\1", page, perl = TRUE),
      simplifyVector = FALSE
    )
  }
  config <- block("config")
  bundle <- block("bundle")
  expect_equal(vapply(config$layers, `[[`, "", "id"), c("depth", "relief", "places"))
  expect_equal(config$layers[[1]]$maxzoom, 1L) # the map draws zoom 1's tiles larger past it
  expect_equal(bundle$tiles, list(depth = "depth", relief = "depth"))
  expect_equal(bundle$fit$probs, list(0.02, 0.98))
  expect_equal(bundle$fit$slope$k, 2)
  expect_equal(names(bundle$meta$layers), c("depth", "relief"))
  expect_false(grepl("__BUNDLE__", page, fixed = TRUE))
  # the server's page has none
  expect_match(.rb_map_page("{}"), '<script id="bundle" type="application/json">null</script>', fixed = TRUE)

  # a bundle is written over only when asked, and then only its own files go
  writeLines("the caller's", file.path(d, "README.md"))
  expect_error(rb_map_bundle(m, d), "holds a bundle")
  rb_map_bundle(m, d, maxzoom = 2, overwrite = TRUE)
  expect_true(file.exists(file.path(d, "README.md")))
  expect_true(file.exists(file.path(d, "tiles", "depth", "2", "1", "0.js")))
  expect_true(file.exists(file.path(d, "tiles", "depth", "2", "1", "1.js")))

  # its arguments checked
  expect_error(rb_map_bundle(list(), d), "rb_map")
  expect_error(rb_map_bundle(m, d, maxzoom = 1.5, overwrite = TRUE), "whole number")
  expect_error(rb_map_bundle(m, d, overwrite = NA), "TRUE or FALSE")
  expect_error(rb_map_bundle(m, d, fit = list(probs = c(0.9, 0.1)), overwrite = TRUE), "probs out of range")
  expect_error(rb_map_bundle(m, d, fit = list(slope = list(q = 1)), overwrite = TRUE), "a list of any of")
  expect_error(rb_map_bundle(m, d, fit = list(n = 1), overwrite = TRUE), "n out of range")
  v <- .rb_map_bundle_fit(list(nodata = NULL, slope = list(k = 3)))
  expect_null(v$nodata) # no value left out
  expect_equal(v$slope, list(k = 3, p = 0.9, min = 0.3, max = 1))
  no_bounds <- rb_map_layer("depth", "dem", tiles = function(z, x, y) NULL, maxzoom = 2,
                            style = list(list(id = "d", type = "hillshade")))
  expect_error(rb_map_bundle(rb_map(rb_map_group("g", "G", list(no_bounds))), tempfile()), "needs its `bounds`")
})

test_that("a bundle leaves out the layers a server draws, with a message, and keeps a table's tiles", {
  skip_if_not_installed("jsonlite")
  skip_if_no_spatial()
  d <- tempfile("bundle_")
  on.exit(unlink(d, recursive = TRUE))
  expect_message(rb_map_bundle(example_map(), d, maxzoom = 8), "Left out of the bundle, as their tiles are made by a server: pings[.]")
  page <- paste(readLines(file.path(d, "index.html"), encoding = "UTF-8"), collapse = "\n")
  config <- jsonlite::fromJSON(
    sub('(?s).*<script id="config" type="application/json">(.*?)</script>.*', "\\1", page, perl = TRUE),
    simplifyVector = FALSE
  )
  expect_equal(vapply(config$layers, `[[`, "", "id"), c("harbours", "outline"))
  expect_equal(vapply(config$groups, `[[`, "", "id"), "ref") # the grid's group, left empty, is gone
  expect_true(file.exists(file.path(d, "data", "outline.js")))
  # the harbours' tiles: only those that hold one, to zoom 8, the page drawing zoom 8's larger past it
  expect_equal(config$layers[[1]]$maxzoom, 8L)
  at <- attr(rb_read_example("dansk_harbours") |> rb_map_tiles_table(properties = "SI_HARB"), "tiles_at")(8)
  expect_true(nrow(at) > 0)
  written <- list.files(file.path(d, "tiles", "harbours", "8"), recursive = TRUE)
  expect_setequal(written, sprintf("%.0f/%.0f.js", at$x, at$y))
})

test_that("a bundle's grid: its rows summed by cell at each level, in blocks the page reads", {
  skip_if_not_installed("jsonlite")
  rows <- data.frame(
    x = c(0L, 1L, 2L, -1L, 300L), y = c(0L, 0L, 1L, -1L, 200L),
    behaviour = c("F", "F", "O", "F", NA), n = c(1, 2, 4, 8, 16), h = c(0.5, 0.5, 1, 1, 2)
  )
  grid <- rb_map_layer(
    "cells", "grid", label = "Cells", tiles = function(z, x, y, p) raw(0),
    meta = list(options = list(behaviour = data.frame(value = c("F", "O"))), v = 1),
    filters = list(
      list(id = "behaviour", label = "Behaviour", options = list(list(value = "", label = "all")), meta = TRUE),
      list(id = "speed", label = "Speed", options = list(list(value = "any"), list(value = "slow"))),
      list(id = "metric", type = "metric", label = "Metric")
    ),
    metrics = list(list(field = "n", label = "count"), list(field = "h_km2", label = "hours per km2"))
  )
  m <- rb_map(rb_map_group("g", "G", list(grid)))
  spec <- list(rows = rows, unit = 0.01, levels = c(1, 2), columns = c(behaviour = "behaviour"), sums = c("n", "h"),
               fields = list(n = "n", h_km2 = c("h", "km2")))
  d <- tempfile("bundle_")
  on.exit(unlink(d, recursive = TRUE))
  expect_message(rb_map_bundle(m, d, grids = list(cells = spec)), "left out of the bundle, as its rows have no column for them: speed")
  page <- paste(readLines(file.path(d, "index.html"), encoding = "UTF-8"), collapse = "\n")
  block <- function(id) {
    jsonlite::fromJSON(
      sub(sprintf('(?s).*<script id="%s" type="application/json">(.*?)</script>.*', id), "\\1", page, perl = TRUE),
      simplifyVector = FALSE
    )
  }
  config <- block("config")
  g <- block("bundle")$grids$cells
  expect_equal(vapply(config$layers[[1]]$filters, `[[`, "", "id"), c("behaviour", "metric"))
  expect_equal(unlist(g$levels), c(1, 2))
  expect_equal(g$block, 128)
  expect_equal(g$columns[[1]][c("id", "range", "bytes")], list(id = "behaviour", range = FALSE, bytes = 1))
  expect_equal(g$columns[[1]]$values, list("F", "O", NULL)) # NA last, as null
  expect_equal(g$fields, list(n = list("n"), h_km2 = list("h", "km2")))
  expect_setequal(unlist(g$blocks[["1"]]), c("0_0", "-1_-1", "2_1"))
  expect_setequal(unlist(g$blocks[["2"]]), c("0_0", "-1_-1", "1_0"))

  # a block read as the page reads it: rows, each sum (float32), the cell in the block (uint16), the codes (uint8)
  read_block <- function(key, sums = 2) {
    js <- readLines(file.path(d, "grids", "cells", paste0(key, ".js")))
    expect_match(js, sprintf('^RB_BUNDLE\\("grids/cells/%s",', key))
    bytes <- memDecompress(jsonlite::base64_dec(sub('^RB_BUNDLE\\("[^"]+","(.*)"\\);$', "\\1", js)), "gzip")
    con <- rawConnection(bytes)
    on.exit(close(con))
    n <- readBin(con, "integer", 1, size = 4, endian = "little")
    s <- lapply(seq_len(sums), function(i) readBin(con, "double", n, size = 4, endian = "little"))
    cell <- readBin(con, "integer", n, size = 2, signed = FALSE, endian = "little")
    code <- readBin(con, "integer", n, size = 1, signed = FALSE)
    data.frame(cell = cell, code = code, n = s[[1]], h = s[[2]])
  }
  b <- read_block("2/0_0")
  # level 2: x 0 and 1 in cell 0 (F, n 1 + 2), x 2 at y 1 in cell 1 (O, n 4)
  expect_equal(b[order(b$cell), ], data.frame(cell = c(0L, 1L), code = c(0L, 1L), n = c(3, 4), h = c(1, 1)),
               ignore_attr = TRUE)
  b <- read_block("1/-1_-1") # x -1, y -1: the block to the south-west, its north-east corner
  expect_equal(b$cell, 127L * 128L + 127L)
  b <- read_block("1/2_1") # x 300, y 200, no behaviour: block 2, 1, its code the last
  expect_equal(b[c("cell", "code", "n")], data.frame(cell = (200L - 128L) * 128L + (300L - 256L), code = 2L, n = 16))

  # its checks
  bad <- function(msg, ...) {
    g <- spec
    v <- list(...)
    g[names(v)] <- v
    expect_error(rb_map_bundle(m, tempfile(), grids = list(cells = g)), msg)
  }
  bad("rising", levels = c(2, 1))
  bad("metric h_km2", fields = list(n = "n"))
  bad("`fields`", fields = list(n = "x", h_km2 = c("h", "km2")))
  bad("names the layer's filters", columns = c(colour = "behaviour"))
  bad("`sums`", sums = "speed")
  expect_error(rb_map_bundle(m, tempfile(), grids = list(other = spec)), "names no grid layer")
  expect_message(rb_map_bundle(m, tempfile()), "made by a server: cells")
})

# The time panel (rb_map_time()) -------------------------------------------------------------------------------------

time_pings <- function() {
  d <- dplyr::collect(rb_read_example("dansk"))
  d <- d[
    d$vessel_id %in% c("EX_1", "EX_12"),
    c("vessel_id", "time_stamp", "lon", "lat", "speed", "course", "behaviour")
  ]
  names(d)[1:2] <- c("vid", "time")
  d$flag <- ifelse(d$speed > 5, "fast", NA)
  d
}

test_that("a time panel: each vessel and its spans in the config, a span's series from /time", {
  skip_if_no_spatial()
  d <- time_pings()
  m <- rb_map_time(
    d,
    c(speed, course),
    marks = flag,
    lanes = list(behaviour = "behaviour"),
    span_max = 10000
  )
  tp <- m$config$time
  expect_equal(vapply(tp$vessels, `[[`, "", "value"), c("EX_1", "EX_12"))
  expect_equal(vapply(tp$cols, `[[`, "", "field"), c("speed", "course"))
  # a mark's label: its column's, or the name it is given
  expect_equal(tp$marks, list(list(field = "flag", label = "flag")))
  d$fast <- d$speed > 8
  named <- rb_map_time(d, marks = c(slow = flag, quick = "fast"))$config$time$marks
  expect_equal(named, list(list(field = "flag", label = "slow"), list(field = "fast", label = "quick")))
  expect_equal(rb_map_time(d, marks = c(flag, quick = fast))$config$time$marks[[1]]$label, "flag")
  expect_equal(tp$lanes[[1]]$label, "behaviour")
  # a vessel with more than span_max pings is offered by month only, its latest month first
  ex12 <- tp$vessels[[2]]
  expect_equal(ex12$n, sum(d$vid == "EX_12"))
  expect_false("all" %in% vapply(ex12$spans, `[[`, "", "value"))
  expect_equal(ex12$span, "2014-12")
  expect_equal(tp$vessels[[1]]$span, "all")
  app <- .rb_map_app(m)
  r <- get(app, "/time", "v=EX_1&span=all")
  expect_equal(r$status, 200L)
  s <- jsonlite::fromJSON(r$body)
  e1 <- d[d$vid == "EX_1", ]
  e1 <- e1[order(e1$time), ]
  expect_equal(length(s$t), nrow(e1))
  expect_equal(s$t, as.numeric(e1$time))
  expect_equal(s$y$speed, signif(e1$speed, 6))
  expect_true(any(!is.na(e1$flag)))
  expect_equal(s$marks$flag$i, which(!is.na(e1$flag)) - 1L)
  expect_equal(unique(s$marks$flag$v), "fast")
  # the lane: runs of one value, broken by a silence of over an hour, each interval within the pings' times
  l <- s$lanes[["1"]]
  expect_true(length(l$t0) > 1)
  expect_true(
    all(l$t0 <= l$t1) && min(l$t0) >= min(s$t) && max(l$t1) <= max(s$t)
  )
  expect_setequal(unique(l$label), unique(e1$behaviour))
  # no gap breaks the panel's line or a lane's run by default; max_gap_s does, the panel's own or the track's
  expect_null(tp$max_gap_s)
  m2 <- rb_map_time(d, lanes = list(behaviour = "behaviour"), max_gap_s = 3600)
  expect_equal(m2$config$time$max_gap_s, 3600)
  l2 <- jsonlite::fromJSON(get(.rb_map_app(m2), "/time", "v=EX_1&span=all")$body)$lanes[["1"]]
  expect_gt(length(l2$t0), length(l$t0))
  t1 <- as.numeric(e1$time)
  inside <- function(a, b) max(c(0, diff(t1[t1 >= a & t1 <= b])))
  expect_true(all(mapply(inside, l2$t0, l2$t1) <= 3600))
  expect_gt(max(mapply(inside, l$t0, l$t1)), 3600)
  expect_equal(
    (d |> rb_map_track(max_gap_s = 1800) |> rb_map_time())$config$time$max_gap_s,
    1800
  )
  expect_equal(
    (d |> rb_map_track(max_gap_s = 1800) |> rb_map_time(max_gap_s = 60))$config$time$max_gap_s,
    60
  )
  expect_error(rb_map_time(d, max_gap_s = -1), "number of seconds")
  # a month's span, and the refusals
  r <- get(app, "/time", "v=EX_12&span=2014-12")
  expect_equal(r$status, 200L)
  expect_equal(
    length(jsonlite::fromJSON(r$body)$t),
    sum(d$vid == "EX_12" & format(d$time, "%Y-%m") == "2014-12")
  )
  expect_equal(get(app, "/time", "v=nope&span=all")$status, 400L)
  expect_match(
    get(app, "/time", "v=EX_12&span=all")$body,
    "span is one of 2014-12"
  )
  expect_equal(get(app, "/time", "")$status, 400L)
  expect_equal(
    get(.rb_map_app(rb_map_pings(d)), "/time", "v=EX_1&span=all")$status,
    404L
  )
})

test_that("a time panel pipes with the layers, either way round, one to a map", {
  skip_if_no_spatial()
  d <- time_pings()
  m <- d |> rb_map_track(behaviour) |> rb_map_time(speed) |> rb_map_pings(speed)
  expect_equal(vapply(m$config$layers, `[[`, "", "id"), c("path", "pings"))
  expect_false(is.null(m$config$time))
  expect_true(is.function(m$time$series))
  # the panel's tables go with the map's, dropped when it is replaced on its port
  expect_true(all(
    vapply(m$time$tables, `[[`, "", "name") %in%
      vapply(attr(m, "tables"), `[[`, "", "name")
  ))
  expect_error(rb_map_time(m, speed), "one time panel")
  expect_match(format(m)[3], "time panel: speed over time, 2 vessels")
  # vessels in natural order where their keys are not all numbers
  expect_equal(order(.rb_natural_key(c("EX_10", "EX_2", "EX_1"))), c(3L, 2L, 1L))
  # a map of the panel alone
  m0 <- rb_map_time(d)
  expect_length(m0$config$layers, 0)
  expect_equal(m0$config$time$cols[[1]]$field, "speed")
})

test_that("a time panel's lanes from interval tables, its vessel keys matched across number types", {
  skip_if_no_spatial()
  d <- time_pings()
  d$v <- as.numeric(factor(d$vid)) # 1 and 2, as doubles
  first <- c(min(d$time[d$v == 1]), min(d$time[d$v == 2]))
  calls <- data.frame(
    v = c(1L, 2L),
    T_in = first,
    T_out = first + 1800,
    port = c("a", "b")
  )
  voyages <- data.frame(
    v = 2L,
    start = as.POSIXct("2014-12-08", tz = "UTC"),
    end = as.POSIXct("2014-12-09", tz = "UTC")
  )
  m <- rb_map_time(
    d,
    by = v,
    lanes = list(calls = list(data = calls, label = "port"), voyages = voyages)
  )
  app <- .rb_map_app(m)
  s1 <- jsonlite::fromJSON(get(app, "/time", "v=1&span=all")$body)
  expect_equal(s1$lanes[["1"]]$label, "a")
  s2 <- jsonlite::fromJSON(get(app, "/time", "v=2&span=all")$body)
  expect_equal(s2$lanes[["2"]]$t0, as.numeric(voyages$start))
  expect_error(
    rb_map_time(d, lanes = list(x = data.frame(v = 1, a = Sys.time()))),
    "no start and end"
  )
  expect_error(rb_map_time(d, lanes = list(x = "nope")), "no column nope")
  expect_error(rb_map_time(d, lanes = list("behaviour")), "named list")
  expect_error(rb_map_time(d, behaviour), "numeric")
  expect_error(rb_map_time(d[, c("vid", "lon", "lat", "speed")]), "time column")
})

test_that("a time panel's spans: the whole series of raw pings, or a column's runs or a table's intervals", {
  skip_if_no_spatial()
  d <- time_pings()
  # raw pings, nothing but time and position: each vessel's whole series alone, no months, no list in the page
  tp <- rb_map_time(d)$config$time
  expect_equal(
    lapply(tp$vessels, function(v) vapply(v$spans, `[[`, "", "value")),
    list("all", "all")
  )
  expect_null(tp$span_label)
  # a column's runs of one value: numbered in time order, named by value and start, a span holding every ping of its
  # vessel from its start to its end; the column's name, or the name given, labels them
  d$haul <- ifelse(d$behaviour == "Fishing", paste0("h", cumsum(c(1, diff(d$behaviour == "Fishing") == 1))), NA)
  m <- rb_map_time(d, span = c(Haul = "haul"))
  tp <- m$config$time
  expect_equal(tp$span_label, "Haul")
  expect_equal(rb_map_time(d, span = "haul")$config$time$span_label, "haul")
  e1 <- d[d$vid == "EX_1", ]
  e1 <- e1[order(e1$time), ]
  runs <- rle(ifelse(is.na(e1$haul), "", e1$haul))
  n_runs <- sum(runs$values != "")
  sp <- tp$vessels[[1]]$spans
  expect_equal(vapply(sp, `[[`, "", "value"), c("all", sprintf("s%d", seq_len(n_runs))))
  first <- which(!is.na(e1$haul))[1]
  last <- first + runs$lengths[which(runs$values != "")[1]] - 1
  expect_equal(
    sp[[2]]$name,
    sprintf("%s · %s", e1$haul[first], format(e1$time[first], "%Y-%m-%d %H:%M"))
  )
  s <- jsonlite::fromJSON(get(.rb_map_app(m), "/time", "v=EX_1&span=s1")$body)
  expect_equal(s$t, as.numeric(e1$time[first:last]))
  expect_equal(sp[[2]]$n, length(s$t))
  expect_match(format(m)[3], "spans: Haul")
  # one span per value: an id missing on a ping inside its unit, as on one a speed filter removed, leaves the unit one
  # span, and that ping in it to be seen
  d2 <- d
  mid <- which(d2$vid == "EX_1" & d2$time == e1$time[first + 1])
  d2$haul[mid] <- NA
  sp2 <- rb_map_time(d2, span = "haul")$config$time$vessels[[1]]$spans
  expect_equal(length(sp2), length(sp))
  s2 <- jsonlite::fromJSON(get(.rb_map_app(rb_map_time(d2, span = "haul")), "/time", "v=EX_1&span=s1")$body)
  expect_equal(s2$t, as.numeric(e1$time[first:last]))
  # a state's values come and go: their spans overlap, which is said
  expect_message(rb_map_time(d, span = "behaviour"), "belongs in `lanes`")
  expect_no_message(rb_map_time(d, span = "haul"))
  # a table's intervals, with a label column; an interval that holds no ping is left out
  tab <- data.frame(
    vid = c("EX_1", "EX_1", "EX_12"),
    start = c(min(e1$time), as.POSIXct("2000-01-01", tz = "UTC"), min(d$time[d$vid == "EX_12"])),
    end = c(min(e1$time) + 3600, as.POSIXct("2000-01-02", tz = "UTC"), max(d$time[d$vid == "EX_12"])),
    trip = c("t1", "none", "t2")
  )
  tp <- rb_map_time(d, span = list(data = tab, label = "trip"))$config$time
  expect_equal(tp$span_label, "Span")
  # a table named as a column is: list(trips = <table or spec>)
  tn <- rb_map_time(d, span = list(trips = list(data = tab, label = "trip")))$config$time
  expect_equal(tn$span_label, "trips")
  expect_equal(tn$vessels[[1]]$spans, tp$vessels[[1]]$spans)
  expect_equal(rb_map_time(d, span = list(trips = tab))$config$time$span_label, "trips")
  expect_equal(vapply(tp$vessels[[1]]$spans, `[[`, "", "value"), c("all", "s1"))
  expect_equal(tp$vessels[[1]]$spans[[2]]$n, sum(e1$time <= min(e1$time) + 3600))
  # a vessel over span_max is offered by its spans alone, the latest first; with none, by month
  tp <- rb_map_time(d, span = list(data = tab, label = "trip"), span_max = 10000)$config$time
  expect_equal(vapply(tp$vessels[[2]]$spans, `[[`, "", "value"), "s1")
  expect_equal(tp$vessels[[2]]$span, "s1")
  expect_error(rb_map_time(d, span = "nope"), "`span`: no column nope")
  # the layers whose features carry the vessel and a time, which the page filters to the vessel and span
  ml <- d |> rb_map_track(behaviour) |> rb_map_pings(speed) |> rb_map_time()
  expect_equal(
    ml$config$time$layers,
    list(
      list(id = "path", by = "vid", t0 = "rb_t0", t1 = "rb_t1"),
      list(id = "pings", by = "vid", t = "rb_t")
    )
  )
  # their times are the panel's: a track's first and last, a ping's own
  lines <- DBI::dbGetQuery(
    .rb_connection(),
    sprintf(
      "SELECT vid, min(rb_t0) AS t0, max(rb_t1) AS t1 FROM %s GROUP BY vid ORDER BY vid",
      attr(ml$layers$path$tiles, "table")
    )
  )
  expect_equal(lines$t0, as.numeric(tapply(d$time, d$vid, min)))
  expect_equal(lines$t1, as.numeric(tapply(d$time, d$vid, max)))
})

test_that("a bundle carries the time panel's spans, a file each", {
  skip_if_no_spatial()
  d <- time_pings()
  m <- rb_map_time(d, speed, span_max = 10000)
  dir <- tempfile()
  rb_map_bundle(m, dir)
  f <- sort(list.files(file.path(dir, "time")))
  # EX_1 held whole, EX_12 over span_max by its one month
  expect_equal(f, c("0_all.js", "1_2014-12.js"))
  js <- readLines(file.path(dir, "time", "0_all.js"), warn = FALSE)
  s <- jsonlite::fromJSON(sub(
    '^RB_BUNDLE\\("time/0_all",(.*)\\);$',
    "\\1",
    paste(js, collapse = "")
  ))
  expect_equal(length(s$t), sum(d$vid == "EX_1"))
  page <- paste(
    readLines(file.path(dir, "index.html"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(page, '"time":\\{', perl = TRUE)
})

test_that("a label on hover: style ids of the layer's own, TRUE for the click's, with a title", {
  data <- '{"type": "FeatureCollection", "features": []}'
  st <- list(list(id = "a-fill", type = "fill"), list(id = "a-line", type = "line"))
  a <- rb_map_layer("a", "geojson", data = data, style = st,
                    popup = list(click = "a-fill", hover = TRUE, title = "name"))
  expect_equal(a$config$popup$hover, list("a-fill"))
  b <- rb_map_layer("b", "geojson", data = data, style = st,
                    popup = list(hover = c("a-fill", "a-line"), title = "name", hover_fields = TRUE))
  expect_equal(b$config$popup$hover, list("a-fill", "a-line"))
  expect_null(b$config$popup$click)
  # one id stays an array in the config's JSON
  j <- .rb_map_json(rb_map(rb_map_group("g", layers = list(a)))$config)
  expect_match(j, '"hover":["a-fill"]', fixed = TRUE)
  expect_error(rb_map_layer("c", "geojson", data = data, style = st, popup = list(hover = "nope", title = "name")),
               "style ids")
  expect_error(rb_map_layer("c", "geojson", data = data, style = st, popup = list(hover = "a-fill")), "title")
  expect_error(rb_map_layer("c", "geojson", data = data, style = st, popup = list(hover = TRUE, title = "name")),
               "click")
  expect_error(rb_map_layer("c", "geojson", data = data, style = st,
                            popup = list(hover = "a-fill", title = "name", hover_fields = "yes")), "TRUE or FALSE")
  expect_error(rb_map_layer("c", "geojson", data = data, style = st,
                            popup = list(click = "a-fill", title = "name", hover_fields = TRUE)), "goes with")
  expect_error(rb_map_layer("c", "dem", tiles = function(z, x, y) NULL,
                            style = list(list(id = "c-shade", type = "hillshade")),
                            popup = list(hover = "c-shade", title = "name")), "vector or geojson")
  # no hover, as before
  expect_null(rb_map_layer("d", "geojson", data = data, style = st, popup = list(click = "a-fill", title = "x"))$config$popup$hover)
})

# a request to a map's app, as httpuv makes it: a page opened as localhost, its header, and a body to read
notes_call <- function(app, method, path, body = NULL, host = "localhost:8811", page = TRUE, origin = NULL) {
  req <- list(REQUEST_METHOD = method, PATH_INFO = path, QUERY_STRING = "", HTTP_HOST = host)
  if (page) req$HTTP_X_RB_NOTES <- "1"
  req$HTTP_ORIGIN <- origin
  if (!is.null(body)) {
    req$rook.input <- list(read = function(...) charToRaw(enc2utf8(body)))
    req$CONTENT_LENGTH <- as.character(length(charToRaw(enc2utf8(body))))
  }
  app$call(req)
}

test_that("notes from R: a note, a reply at its place, a reply to a reply in the same thread, read back", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "notes are written in a UTF-8 locale")
  f <- tempfile(fileext = ".geojson")
  n0 <- rb_map_notes(f)
  expect_equal(nrow(n0), 0)
  expect_named(n0, c("id", "thread", "reply_to", "time", "author", "text", "lon", "lat", "zoom", "under", "address"))
  expect_equal(rb_map_notes_add(f, "Er brún Rósagarðs hér?", lon = -14.6, lat = 64.05, author = "reader"), 1L)
  expect_equal(rb_map_notes_add(f, "Já, við 200 m.", reply_to = 1), 2L)
  expect_equal(rb_map_notes_add(f, "Og þá?", reply_to = 2, author = "reader"), 3L)
  expect_equal(rb_map_notes_add(f, "Another place", lon = -20, lat = 65), 4L)
  n <- rb_map_notes(f)
  expect_equal(n$id, 1:4)
  expect_equal(n$thread, c(1L, 1L, 1L, 4L))
  expect_equal(n$reply_to, c(NA, 1L, 1L, NA))
  expect_equal(n$author, c("reader", "analyst", "reader", "analyst"))
  expect_equal(n$text[1:3], c("Er brún Rósagarðs hér?", "Já, við 200 m.", "Og þá?"))
  expect_equal(n$lon, c(-14.6, -14.6, -14.6, -20))
  expect_s3_class(n$time, "POSIXct")
  expect_equal(attr(n$time, "tzone"), "UTC")
  # plain GeoJSON, a note a line, its letters as written
  lines <- readLines(f, encoding = "UTF-8")
  expect_length(lines, 6)
  expect_match(lines[2], "Rósagarðs", fixed = TRUE)
  expect_equal(jsonlite::fromJSON(f, simplifyVector = FALSE)$type, "FeatureCollection")
  expect_error(rb_map_notes_add(f, "", lon = 1, lat = 1), "text")
  expect_error(rb_map_notes_add(f, "x"), "place")
  expect_error(rb_map_notes_add(f, "x", lon = 200, lat = 1), "place")
  expect_error(rb_map_notes_add(f, "x", reply_to = 99), "reply_to")
  expect_error(rb_map_notes_add(f, "x", lon = 1, lat = 1, author = ""), "author")
  expect_error(rb_map_notes_add("notes.csv", "x", lon = 1, lat = 1), "geojson")
  expect_error(rb_map_notes_add(file.path(tempfile(), "n.geojson"), "x", lon = 1, lat = 1), "does not exist")
  # a file that is not notes is never overwritten
  g <- tempfile(fileext = ".geojson")
  writeLines('{"type": "Feature"}', g)
  expect_error(rb_map_notes_add(g, "x", lon = 1, lat = 1), "not a notes file")
  expect_equal(readLines(g), '{"type": "Feature"}')
})

test_that("notes served: the page's notes, replies and deletions through /notes, from the page only", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "notes are written in a UTF-8 locale")
  f <- tempfile(fileext = ".geojson")
  a <- rb_map_layer("a", "geojson", data = '{"type": "FeatureCollection", "features": []}',
                    style = list(list(id = "a-fill", type = "fill")))
  m <- rb_map(rb_map_group("g", layers = list(a)))
  app <- .rb_map_app(m, notes = f)
  cfg <- jsonlite::fromJSON(get(app, "/config.json")$body, simplifyVector = FALSE)
  expect_equal(cfg$notes$file, basename(f))
  expect_null(m$config$notes) # the map itself is left as it was: a bundle of it has no notes
  r <- notes_call(app, "GET", "/notes")
  expect_equal(r$status, 200L)
  expect_equal(r$headers$`Cache-Control`, "no-store")
  expect_length(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$features, 0)
  expect_false(file.exists(f)) # made at the first note
  r <- notes_call(app, "POST", "/notes", '{"text": "Hér er grunnið", "lon": -20.5, "lat": 65.25, "zoom": 7.333,
                                            "under": ["Areas: Rósagarður"], "address": "?a=1#7.33/65.25/-20.5"}')
  expect_equal(r$status, 200L)
  fc <- jsonlite::fromJSON(r$body, simplifyVector = FALSE)
  expect_equal(fc$added, 1L)
  p <- fc$features[[1]]$properties
  expect_equal(p$author, "reader")
  expect_equal(p$text, "Hér er grunnið")
  expect_equal(p$zoom, 7.33)
  expect_equal(p$under, list("Areas: Rósagarður"))
  expect_equal(p$address, "?a=1#7.33/65.25/-20.5")
  expect_null(p$reply_to)
  r <- notes_call(app, "POST", "/notes", '{"text": "A reply", "reply_to": 1}')
  expect_equal(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$added, 2L)
  rb_map_notes_add(f, "From R", reply_to = 1, author = "claude") # R and the server write the same file
  n <- rb_map_notes(f)
  expect_equal(n$author, c("reader", "reader", "claude"))
  expect_equal(n$under[1], "Areas: Rósagarður")
  expect_equal(unique(n$lon), -20.5)
  # refused: no text, no place, a bad reply, not JSON, too big; nothing written
  before <- readLines(f, encoding = "UTF-8")
  expect_equal(notes_call(app, "POST", "/notes", '{"text": " ", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x"}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "reply_to": 9}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1, "under": [1]}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", "not json")$status, 400L)
  expect_equal(notes_call(app, "POST", "/notes", strrep("x", 70000))$status, 413L)
  # from elsewhere: no page header, another site's origin, a page opened under another host name
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}', page = FALSE)$status, 403L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}', origin = "http://evil.example")$status, 403L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}', host = "evil.example:8811")$status, 403L)
  expect_equal(notes_call(app, "GET", "/notes", host = "evil.example:8811")$status, 403L)
  expect_equal(notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}', origin = "http://localhost:8811")$status, 200L)
  expect_equal(nrow(rb_map_notes(f)), 4)
  # a reply deleted alone, a thread's first note with its replies
  expect_equal(notes_call(app, "DELETE", "/notes/2")$status, 200L)
  expect_equal(rb_map_notes(f)$id, c(1L, 3L, 4L))
  expect_equal(notes_call(app, "DELETE", "/notes/1")$status, 200L)
  expect_equal(rb_map_notes(f)$id, 4L)
  expect_equal(notes_call(app, "DELETE", "/notes/1")$status, 404L)
  # a number is never given twice: the last note deleted, the next takes the one after it
  expect_equal(notes_call(app, "DELETE", "/notes/4")$status, 200L)
  expect_equal(nrow(rb_map_notes(f)), 0)
  expect_equal(rb_map_notes_add(f, "after", lon = 1, lat = 1), 5L)
  expect_equal(jsonlite::fromJSON(f, simplifyVector = FALSE)$last_id, 5L)
  expect_null(jsonlite::fromJSON(notes_call(app, "GET", "/notes")$body, simplifyVector = FALSE)$last_id)
  expect_equal(notes_call(app, "DELETE", "/notes/5", page = FALSE)$status, 403L)
  expect_equal(notes_call(app, "PUT", "/notes")$status, 405L)
  # a file broken by hand: an error to the page, and left as it is
  writeLines("{broken", f)
  r <- notes_call(app, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}')
  expect_equal(r$status, 500L)
  expect_match(r$body, "not a notes file")
  expect_equal(readLines(f), "{broken")
  # a map served without notes has no such route
  plain <- .rb_map_app(m)
  expect_equal(notes_call(plain, "GET", "/notes")$status, 404L)
  expect_equal(notes_call(plain, "POST", "/notes", '{"text": "x", "lon": 1, "lat": 1}')$status, 405L)
  expect_null(jsonlite::fromJSON(get(plain, "/config.json")$body, simplifyVector = FALSE)$notes)
})

# a layer whose features take a verdict
verdict_layer <- function(id = "a", ...) {
  rb_map_layer(id, "geojson", data = '{"type": "FeatureCollection", "features": []}',
               style = list(list(id = paste0(id, "-dot"), type = "circle")),
               popup = list(click = paste0(id, "-dot"), title = "name",
                            verdict = list(field = "key", text = TRUE,
                                           options = list(list(value = "right", label = "vRight"),
                                                          list(value = "place", label = "vPlace", colour = "#fdae61"),
                                                          list(value = "name", label = "vName")))), ...)
}

test_that("a popup's verdict: checked, its options an array in the config", {
  a <- verdict_layer()
  v <- a$config$popup$verdict
  expect_equal(v$field, "key")
  expect_true(v$text)
  expect_equal(v$questions[[1]]$id, "verdict") # one question given as `options`
  expect_equal(vapply(v$questions[[1]]$options, `[[`, "", "value"), c("right", "place", "name"))
  expect_equal(v$questions[[1]]$options[[2]]$colour, "#fdae61")
  expect_false(v$questions[[1]]$options[[1]]$other)
  cfg <- jsonlite::fromJSON(.rb_map_json(rb_map(rb_map_group("g", layers = list(a)))$config), simplifyVector = FALSE)
  expect_length(cfg$layers[[1]]$popup$verdict$questions[[1]]$options, 3)
  # a label defaults to its value; text defaults to FALSE
  b <- rb_map_layer("b", "geojson", data = "{}", style = list(list(id = "b-dot", type = "circle")),
                    popup = list(click = "b-dot", verdict = list(field = "k", options = list(list(value = "y"), list(value = "n")))))
  expect_equal(b$config$popup$verdict$questions[[1]]$options[[1]]$label, "y")
  expect_false(b$config$popup$verdict$text)
  bad <- function(verdict, click = "b-dot", kind = "geojson") {
    rb_map_layer("b", kind, data = if (kind == "geojson") "{}", tiles = if (kind == "vector") function(z, x, y) raw(0),
                 style = list(list(id = "b-dot", type = "circle")), popup = list(click = click, verdict = verdict))
  }
  ok <- list(list(value = "y"), list(value = "n"))
  expect_error(bad(list(field = "k", options = ok), click = NULL), "click")
  expect_error(bad(list(options = ok)), "field")
  expect_error(bad(list(field = "k", options = ok[1])), "two to six")
  expect_error(bad(list(field = "k", options = rep(ok, 4))), "two to six")
  expect_error(bad(list(field = "k", options = list(list(value = "a b"), list(value = "n")))), "letters")
  expect_error(bad(list(field = "k", options = list(list(value = "y"), list(value = "y")))), "of their own")
  expect_error(bad(list(field = "k", options = list(list(value = "y", colour = "red"), list(value = "n")))), "colour")
  expect_error(bad(list(field = "k", options = ok, text = "yes")), "TRUE or FALSE")
  expect_no_error(bad(list(field = "k", options = ok), kind = "vector"))
})

test_that("verdicts served: given, replaced, withdrawn, read back; from the page only; a file broken by hand left", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "verdicts are written in a UTF-8 locale")
  f <- tempfile(fileext = ".csv")
  plain <- rb_map_layer("p", "geojson", data = '{"type": "FeatureCollection", "features": []}',
                        style = list(list(id = "p-fill", type = "fill")))
  m <- rb_map(rb_map_group("g", layers = list(verdict_layer(), plain)))
  app <- .rb_map_app(m, verdicts = f)
  cfg <- jsonlite::fromJSON(get(app, "/config.json")$body, simplifyVector = FALSE)
  expect_equal(cfg$verdicts$file, basename(f))
  expect_null(m$config$verdicts) # the map itself is left as it was: a bundle of it has no verdicts
  r <- notes_call(app, "GET", "/verdicts")
  expect_equal(r$status, 200L)
  expect_equal(r$headers$`Cache-Control`, "no-store")
  expect_length(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$verdicts, 0)
  expect_false(file.exists(f)) # made at the first verdict
  r <- notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "eldeyjarbodi", "value": "place", "text": "Hér er það ekki",
                                              "title": "Eldeyjarboði", "lon": -23.7, "lat": 63.45, "zoom": 9.123}')
  expect_equal(r$status, 200L)
  v <- jsonlite::fromJSON(r$body, simplifyVector = FALSE)$verdicts
  expect_length(v, 1)
  expect_equal(v[[1]]$value, "place")
  expect_equal(v[[1]]$text, "Hér er það ekki")
  notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": 7, "value": "right", "lon": 1, "lat": 2}')
  # a later answer replaces the earlier; an empty one withdraws it; every answer kept in the file
  notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "eldeyjarbodi", "value": "name", "lon": -23.7, "lat": 63.45}')
  expect_equal(rb_map_verdicts(f)$value, c("right", "name"))
  expect_equal(rb_map_verdicts(f)$id, c("7", "eldeyjarbodi"))
  r <- notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "7", "value": "", "lon": 1, "lat": 2}')
  expect_equal(r$status, 200L)
  expect_length(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$verdicts, 1)
  all <- rb_map_verdicts(f, all = TRUE)
  expect_equal(all$value, c("place", "right", "name", ""))
  expect_equal(names(all), c("time", "layer", "id", "question", "value", "other", "text", "title", "lon", "lat", "zoom"))
  expect_equal(unique(all$question), "verdict") # a page of one question may leave it out
  expect_equal(all$title[1], "Eldeyjarboði")
  expect_equal(all$zoom[1], 9.12)
  expect_true(is.na(all$zoom[2]))
  expect_equal(all$text[2], "")
  # refused: another layer, a value not offered, no id, no place, too long, not JSON, too big; nothing written
  before <- readLines(f, encoding = "UTF-8")
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "p", "id": "x", "value": "right", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "maybe", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "", "value": "right", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right"}')$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", sprintf('{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1, "text": "%s"}',
                                                              strrep("x", 1001)))$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", "not json")$status, 400L)
  expect_equal(notes_call(app, "POST", "/verdicts", strrep("x", 20000))$status, 413L)
  # from elsewhere: no page header, another site's origin, a page opened under another host name
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1}', page = FALSE)$status, 403L)
  expect_equal(notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1}',
                          origin = "http://evil.example")$status, 403L)
  expect_equal(notes_call(app, "GET", "/verdicts", host = "evil.example:8811")$status, 403L)
  expect_equal(readLines(f, encoding = "UTF-8"), before)
  expect_equal(notes_call(app, "PUT", "/verdicts")$status, 405L)
  # a file broken by hand: an error to the page, and left as it is
  writeLines("a,b\n1,2", f)
  r <- notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1}')
  expect_equal(r$status, 500L)
  expect_match(r$body, "not a verdicts file")
  expect_equal(readLines(f), c("a,b", "1,2"))
  # a map served without verdicts has no such route
  expect_equal(notes_call(.rb_map_app(m), "GET", "/verdicts")$status, 404L)
  expect_null(jsonlite::fromJSON(get(.rb_map_app(m), "/config.json")$body, simplifyVector = FALSE)$verdicts)
})

# a layer whose features take two questions, one skipped by an answer to the first, an answer naming another, a note
two_questions <- function() {
  rb_map_layer("n", "geojson", data = '{"type": "FeatureCollection", "features": []}',
               style = list(list(id = "n-dot", type = "circle")),
               popup = list(click = "n-dot", title = "name", verdict = list(field = "key", text = TRUE, questions = list(
                 list(id = "name", label = "qName", options = list(
                   list(value = "right", label = "nRight"), list(value = "form", label = "nForm", other = TRUE),
                   list(value = "not", label = "nNot"), list(value = "unsure", label = "nUnsure", colour = "#999999"))),
                 list(id = "place", label = "qPlace", skip = list(name = "not"), options = list(
                   list(value = "right", label = "pRight"), list(value = "wrong", label = "pWrong")))))))
}

test_that("a verdict's questions: checked, an answer naming another, a question skipped by an earlier answer", {
  q <- two_questions()$config$popup$verdict$questions
  expect_equal(vapply(q, `[[`, "", "id"), c("name", "place"))
  expect_true(q[[1]]$options[[2]]$other)
  expect_equal(q[[2]]$skip, list(name = list("not")))
  cfg <- jsonlite::fromJSON(.rb_map_json(rb_map(rb_map_group("g", layers = list(two_questions())))$config), simplifyVector = FALSE)
  expect_equal(cfg$layers[[1]]$popup$verdict$questions[[2]]$skip$name, list("not")) # an array, one answer or several
  bad <- function(verdict) {
    rb_map_layer("b", "geojson", data = "{}", style = list(list(id = "b-dot", type = "circle")),
                 popup = list(click = "b-dot", verdict = verdict))
  }
  ok <- list(list(value = "y"), list(value = "n"))
  expect_error(bad(list(field = "k", options = ok, questions = list(list(id = "a", options = ok)))), "not both")
  expect_error(bad(list(field = "k", questions = rep(list(list(id = "a", options = ok)), 5))), "one to four")
  expect_error(bad(list(field = "k", questions = list(list(id = "a", options = ok), list(id = "a", options = ok)))), "ids of their own")
  expect_error(bad(list(field = "k", questions = list(list(id = "note", options = ok)))), "note")
  expect_error(bad(list(field = "k", questions = list(list(id = "a", options = ok), list(id = "b", options = ok, skip = list(c = "y"))))),
               "earlier questions")
  expect_error(bad(list(field = "k", questions = list(list(id = "a", options = ok), list(id = "b", options = ok, skip = list(a = "z"))))),
               "answers of the question")
  expect_error(bad(list(field = "k", questions = list(list(id = "b", options = ok, skip = list(b = "y"))))), "earlier questions")
  expect_error(bad(list(field = "k", options = list(list(value = "y", other = "yes"), list(value = "n")))), "other")
})

test_that("verdicts served by question: an answer naming another, a note, each question's latest; a file of one question read", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "verdicts are written in a UTF-8 locale")
  f <- tempfile(fileext = ".csv")
  m <- rb_map(rb_map_group("g", layers = list(two_questions())))
  app <- .rb_map_app(m, verdicts = f)
  post <- function(body) notes_call(app, "POST", "/verdicts", body)
  expect_equal(post('{"layer": "n", "id": "bodi", "question": "name", "value": "form", "other": "Eldeyjarboði", "lon": -23.7, "lat": 63.4}')$status, 200L)
  expect_equal(post('{"layer": "n", "id": "bodi", "question": "place", "value": "wrong", "lon": -23.7, "lat": 63.4}')$status, 200L)
  expect_equal(post('{"layer": "n", "id": "bodi", "question": "note", "text": "Austan við", "lon": -23.7, "lat": 63.4}')$status, 200L)
  expect_equal(post('{"layer": "n", "id": "sker", "question": "name", "value": "not", "lon": -23.1, "lat": 63.6}')$status, 200L)
  v <- rb_map_verdicts(f)
  expect_equal(paste(v$id, v$question, v$value, v$other), c("bodi name form Eldeyjarboði", "bodi place wrong ", "bodi note  ", "sker name not "))
  expect_equal(v$text[v$question == "note"], "Austan við")
  # each question's latest: the place withdrawn, the note cleared, the name kept
  post('{"layer": "n", "id": "bodi", "question": "place", "value": "", "lon": -23.7, "lat": 63.4}')
  post('{"layer": "n", "id": "bodi", "question": "note", "text": "", "lon": -23.7, "lat": 63.4}')
  v <- rb_map_verdicts(f)
  expect_equal(paste(v$id, v$question), c("bodi name", "sker name"))
  expect_equal(nrow(rb_map_verdicts(f, all = TRUE)), 6)
  r <- jsonlite::fromJSON(notes_call(app, "GET", "/verdicts")$body, simplifyVector = FALSE)$verdicts
  expect_equal(r[[1]]$other, "Eldeyjarboði")
  # refused: no name where the answer names one, a name where it does not, no question, a question not there,
  # a note with a value; nothing written
  before <- readLines(f, encoding = "UTF-8")
  expect_equal(post('{"layer": "n", "id": "x", "question": "name", "value": "form", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "question": "name", "value": "form", "other": " ", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "question": "name", "value": "right", "other": "B", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "value": "right", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "question": "depth", "value": "right", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(post('{"layer": "n", "id": "x", "question": "note", "value": "right", "text": "t", "lon": 1, "lat": 1}')$status, 400L)
  expect_equal(readLines(f, encoding = "UTF-8"), before)
  # a file of 2026.11.09, one question with no question column: read as "verdict", and written on in the new form
  g <- tempfile(fileext = ".csv")
  writeLines(c('"time","layer","id","value","text","title","lon","lat","zoom"',
               '"2026-11-09T09:19:03Z","a","x","place","Hér","X",-22.78,63.78,10'), g, useBytes = TRUE)
  old <- rb_map_verdicts(g)
  expect_equal(c(old$question, old$value, old$other, old$text), c("verdict", "place", "", "Hér"))
  app1 <- .rb_map_app(rb_map(rb_map_group("g", layers = list(verdict_layer()))), verdicts = g)
  expect_equal(notes_call(app1, "POST", "/verdicts", '{"layer": "a", "id": "y", "value": "name", "lon": 1, "lat": 1}')$status, 200L)
  expect_equal(names(utils::read.csv(g)), c("time", "layer", "id", "question", "value", "other", "text", "title", "lon", "lat", "zoom"))
  expect_equal(rb_map_verdicts(g)$id, c("x", "y"))
})

test_that("a verdict kept under another layer's answers: shared both ways, the same questions asked", {
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "verdicts are written in a UTF-8 locale")
  shared <- function(id, store = NULL, options = list(list(value = "right"), list(value = "not"))) {
    rb_map_layer(id, "geojson", data = '{"type": "FeatureCollection", "features": []}',
                 style = list(list(id = paste0(id, "-dot"), type = "circle")),
                 popup = list(click = paste0(id, "-dot"), title = "name",
                              verdict = list(field = "key", store = store, options = options)))
  }
  expect_equal(shared("b", "a")$config$popup$verdict$store, "a")
  expect_null(shared("a")$config$popup$verdict$store)
  expect_error(shared("b", "a b"), "store")
  f <- tempfile(fileext = ".csv")
  m <- rb_map(rb_map_group("g", layers = list(shared("a"), shared("b", "a"))))
  app <- .rb_map_app(m, verdicts = f)
  # an answer given through b is kept as a's, and read as a's
  r <- notes_call(app, "POST", "/verdicts", '{"layer": "b", "id": "x", "value": "not", "lon": 1, "lat": 1}')
  expect_equal(r$status, 200L)
  expect_equal(jsonlite::fromJSON(r$body, simplifyVector = FALSE)$verdicts[[1]]$layer, "a")
  notes_call(app, "POST", "/verdicts", '{"layer": "a", "id": "x", "value": "right", "lon": 1, "lat": 1}')
  v <- rb_map_verdicts(f)
  expect_equal(paste(v$layer, v$id, v$value), "a x right") # one feature, its latest answer, whichever layer gave it
  expect_equal(rb_map_verdicts(f, all = TRUE)$layer, c("a", "a"))
  # layers sharing answers ask the same questions, or the map is refused
  odd <- rb_map(rb_map_group("g", layers = list(shared("a"), shared("b", "a", list(list(value = "yes"), list(value = "no"))))))
  expect_error(.rb_map_app(odd, verdicts = f), "different questions")
  expect_error(rb_map_serve(odd, port = httpuv::randomPort(), open = FALSE, verdicts = tempfile(fileext = ".csv")), "different questions")
})

test_that("a server with verdicts: the file and the map checked as it starts", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "verdicts are written in a UTF-8 locale")
  m <- rb_map(rb_map_group("g", layers = list(verdict_layer())))
  g <- tempfile(fileext = ".csv")
  writeLines("x,y", g)
  expect_error(rb_map_serve(m, port = httpuv::randomPort(), open = FALSE, verdicts = g), "not a verdicts file")
  expect_error(rb_map_serve(m, port = httpuv::randomPort(), open = FALSE, verdicts = tempfile(fileext = ".json")), "csv")
  expect_error(rb_map_serve(m, port = httpuv::randomPort(), open = FALSE, verdicts = file.path(tempfile(), "v.csv")),
               "does not exist")
  a <- rb_map_layer("a", "vector", tiles = function(z, x, y) raw(0), style = list(list(id = "a-line", type = "line")))
  expect_error(rb_map_serve(rb_map(rb_map_group("g", layers = list(a))), port = httpuv::randomPort(), open = FALSE,
                            verdicts = tempfile(fileext = ".csv")), "has none")
  port <- httpuv::randomPort()
  rb_map_serve(m, port = port, open = FALSE, verdicts = tempfile(fileext = ".csv"))
  expect_equal(rb_map_stop(port), port)
})

test_that("a server with notes: the file checked as it starts", {
  skip_if_not_installed("httpuv")
  skip_if_not_installed("jsonlite")
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "notes are written in a UTF-8 locale")
  a <- rb_map_layer("a", "vector", tiles = function(z, x, y) raw(0), style = list(list(id = "a-line", type = "line")))
  m <- rb_map(rb_map_group("g", layers = list(a)))
  g <- tempfile(fileext = ".geojson")
  writeLines("[]", g)
  expect_error(rb_map_serve(m, port = httpuv::randomPort(), open = FALSE, notes = g), "not a notes file")
  expect_error(rb_map_serve(m, port = httpuv::randomPort(), open = FALSE, notes = file.path(tempfile(), "n.geojson")),
               "does not exist")
  port <- httpuv::randomPort()
  rb_map_serve(m, port = port, open = FALSE, notes = tempfile(fileext = ".geojson"))
  expect_equal(rb_map_stop(port), port)
})
