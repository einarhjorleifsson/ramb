# map.R — a map viewer for track and effort data: layers declared in R, a page (inst/map/index.html, the engine) that
# shows them from a config, and a small httpuv server with fixed routes. The data stay in DuckDB and reach the browser
# as vector tiles, so colours, filters and the legend work in the page.
#
# Routes of rb_map_serve():
#   /                                the page, with the map's config written in
#   /config.json                     the config
#   /meta                            the engine's version and each layer's meta: a grid's options and ranges, a
#                                    vector layer's count
#   /tiles/<id>/{z}/{x}/{y}.pbf      a grid or vector layer's tile; a grid's query parameters are checked against
#                                    its filters before its tile function sees them
#   /data/<id>.geojson               a geojson layer's data

.rb_map_engine <- 1L # the page's ENGINE: changed together
.rb_maps <- new.env(parent = emptyenv()) # the servers rb_map_serve() started, by port

#' A layer of a map
#'
#' One layer of an [rb_map()]: what it shows, how it is drawn, and where its data come from. Three kinds:
#'
#' * `"grid"`: vector tiles of cells, coloured in the page by a numeric field on a log scale, with the palette,
#'   stretch, floor, cap and histogram there too. Its `filters` become controls whose values go into the tile address
#'   (one grid per map).
#' * `"vector"`: vector tiles of features, drawn by `style`, such as a table of geometries served by
#'   [rb_map_tiles_table()].
#' * `"geojson"`: features sent whole as GeoJSON, drawn by `style`.
#'
#' Texts (`label`, `info`, a filter's `label`, a legend's `label`, ...) are keys of the map's `texts` ([rb_map()]), or
#' the text itself where no key matches. MapLibre styles and expressions are lists, so that a one-element array stays
#' an array in JSON: `list("get", "kind")`, `list("zoom")`; read from JSON with `jsonlite::fromJSON(simplifyVector =
#' FALSE)` they already are.
#'
#' @param id The layer's name, lower case: its checkbox, its source and its key in the page's address.
#' @param kind `"grid"`, `"vector"` or `"geojson"`.
#' @param label,info Its name in the sidebar and the text behind its ⓘ.
#' @param visible Whether it is on when the page opens; the address turns it off with `id=0` or on with `id=1`.
#' @param tiles The tile function. For a grid, `function(z, x, y, params)`, `params` a named list of the filters'
#'   values as text (absent when not given); for a vector layer, `function(z, x, y)`. It returns a Mapbox vector tile
#'   as raw bytes (DuckDB's `ST_AsMVT()`), or `raw(0)`.
#' @param meta For a grid, a list or a function returning one, read when the server starts: `options` (per filter
#'   with `meta = TRUE`, a data frame of `value` and, optionally, `name` and `code`), `ranges` (per range filter, its
#'   first and last value) and `v` (a version, put into the tile address so a rebuilt source is not hidden by the
#'   browser's cache).
#' @param data For a geojson layer: GeoJSON text, an sf object, or a function returning either, read when the server
#'   starts.
#' @param style A list of MapLibre layer specifications, without `source` (the layer's own).
#' @param popup For a vector or geojson layer, what a click shows: `click` (the style ids that answer), `title` (a
#'   field), `fields` (each a list of `field`, `label`, and optionally `values`, a value-to-text lookup whose text may
#'   hold `{field}` placeholders; `template`, a text with `{field}` placeholders shown in place of the value when there
#'   is one; and `format`, `"value"` or `"count"`), `maxWidth`. For a grid, `fields` only, the first in bold.
#' @param legend A list of swatches, each `swatch` (`fill` and `stroke`, or `dot`) and `label`.
#' @param count The text beside its label: `text` (a key whose `{1}` is the count) and, for geojson, `where` (a field
#'   and the value of the features to count). A vector layer's count is its tile function's `count` attribute.
#' @param missing The text shown when the server has none of it.
#' @param filters For a grid: a list of controls, each a list with `id`, `label` and one of: `options` (a list of
#'   `value`, `label` and optionally `tip`) with an optional `default`; `meta = TRUE` (options from `meta`, with an
#'   optional `unnamed` text for an option with no name; several values, comma separated, may come from the address);
#'   `type = "range"` with `params` (two names, such as `c("y1", "y2")`, its bounds from `meta`); `type = "metric"`
#'   (the choice among `metrics`).
#' @param metrics For a grid: a list of the numeric fields it can be coloured by, each `field` and `label`.
#' @param source_layer For a grid, the tile layer its cells are in (`"cells"`).
#' @param minzoom,maxzoom The zooms its tiles are made for.
#' @param opacity For a grid, its opacity when the page opens, 0-100.
#' @return An `rb_map_layer`, for [rb_map_group()].
#' @seealso [rb_map()], [rb_map_tiles_table()]
#' @export
rb_map_layer <- function(
  id,
  kind = c("grid", "vector", "geojson"),
  label = id,
  info = NULL,
  visible = TRUE,
  tiles = NULL,
  meta = NULL,
  data = NULL,
  style = NULL,
  popup = NULL,
  legend = NULL,
  count = NULL,
  missing = NULL,
  filters = NULL,
  metrics = NULL,
  source_layer = NULL,
  minzoom = NULL,
  maxzoom = NULL,
  opacity = NULL
) {
  kind <- match.arg(kind)
  if (!is.character(id) || length(id) != 1 || !grepl("^[a-z][a-z0-9_]*$", id)) {
    stop(
      "`id` is one name in lower case: letters, digits and _, starting with a letter.",
      call. = FALSE
    )
  }
  if (
    id %in%
      c(
        "metric",
        "palette",
        "stretch",
        "floor",
        "cap",
        "opacity",
        "base",
        "lang",
        "v"
      )
  ) {
    stop(
      sprintf("`id` \"%s\" is a name the page's address uses for itself.", id),
      call. = FALSE
    )
  }
  if (kind %in% c("grid", "vector") && !is.function(tiles)) {
    stop(
      sprintf(
        "A %s layer needs `tiles`, a function returning a vector tile.",
        kind
      ),
      call. = FALSE
    )
  }
  if (kind == "grid" && !length(metrics)) {
    stop(
      "A grid layer needs `metrics`: the fields it can be coloured by.",
      call. = FALSE
    )
  }
  if (kind == "geojson" && is.null(data)) {
    stop("A geojson layer needs `data`.", call. = FALSE)
  }
  if (kind != "grid" && !length(style)) {
    stop(
      sprintf("A %s layer needs `style`: MapLibre layer specifications.", kind),
      call. = FALSE
    )
  }
  # one-element arrays stay arrays in the config's JSON
  if (!is.null(popup$click)) {
    popup$click <- as.list(popup$click)
  }
  if (!is.null(count$where)) {
    count$where <- as.list(count$where)
  }
  filters <- lapply(filters, function(f) {
    if (identical(f$type, "range")) {
      f$params <- as.list(f$params)
    }
    f
  })
  config <- list(
    id = id,
    kind = kind,
    label = label,
    info = info,
    visible = isTRUE(visible),
    style = style,
    popup = popup,
    legend = legend,
    count = count,
    missing = missing,
    filters = filters,
    metrics = metrics,
    source_layer = source_layer,
    minzoom = minzoom,
    maxzoom = maxzoom,
    opacity = opacity
  )
  structure(
    list(
      config = Filter(Negate(is.null), config),
      tiles = tiles,
      meta = meta,
      data = data
    ),
    class = "rb_map_layer"
  )
}

#' A group of layers in a map's sidebar
#'
#' @param id The group's name.
#' @param title Its title in the sidebar: a key of the map's `texts`, or the text itself.
#' @param layers A list of [rb_map_layer()]s, in the order they are listed and drawn (the later over the earlier).
#' @param open Whether the group is unfolded when the page opens.
#' @return An `rb_map_group`, for [rb_map()].
#' @export
rb_map_group <- function(id, title = id, layers, open = TRUE) {
  if (inherits(layers, "rb_map_layer")) {
    layers <- list(layers)
  }
  if (!all(vapply(layers, inherits, logical(1), "rb_map_layer"))) {
    stop("`layers` is a list of rb_map_layer()s.", call. = FALSE)
  }
  structure(
    list(id = id, title = title, open = isTRUE(open), layers = layers),
    class = "rb_map_group"
  )
}

#' A map: layers in groups, texts, base maps and palettes
#'
#' The map is a page with a sidebar of groups that fold, each layer with its info and, when ticked, its controls and
#' legend under it; base maps to choose from; the whole view in the page's address, so a link gives the same view;
#' and on a phone the sidebar behind a menu button. [rb_map_serve()] serves it.
#'
#' Four whole maps on the example data are scripts in `system.file("examples", "map", package = "ramb")`: the Danish
#' pings by cell (`dansk_map.R`), the same with a cell size, a sample and the interpolation to choose in the page
#' (`dansk_switches_map.R`), the Italian tracks' voyages by two rules (`cnr_map.R`), and iapesca's example vessel
#' (`iapesca_map.R`, which needs iapesca).
#'
#' @param ... [rb_map_group()]s.
#' @param title The page's title: a key of `texts`, or the text itself.
#' @param view The first view: `center` (longitude, latitude) and `zoom`.
#' @param texts The texts of the layers and groups, and of the page in other languages: a named list whose elements
#'   are a text, or a list of texts by language (`list(en = "Ports", is = "Hafnir")`). The page's own texts are
#'   English; give them in another language under the same keys, a base map's label under `b_<id>`.
#' @param languages The languages of the page, in the order of its language buttons (none for one language).
#' @param language The language the page opens in.
#' @param locales A list by language of the locales its numbers are written in, the first a browser has
#'   (`list(is = c("is-IS", "de-DE"))`, German standing in where a browser lacks Icelandic).
#' @param about Keys of `texts` for the paragraphs of the About group (none, no group).
#' @param palette The grid's palette when the page opens: an `id` of `palettes`.
#' @param palettes,basemaps The palettes and base maps to choose from: [rb_map_palettes()], [rb_map_basemaps()].
#' @return An `rb_map`: `config` (what the page is sent) and `layers`.
#' @export
#' @examples
#' harbours <- rb_map_layer(
#'   "harbours", "vector", label = "Harbours",
#'   tiles = function(z, x, y) raw(0),
#'   style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
#'                     paint = list("fill-color" = "#d9822b")))
#' )
#' m <- rb_map(rb_map_group("ref", "Reference", list(harbours)), title = "Example",
#'             view = list(center = c(12.5, 56), zoom = 7))
#' names(m$config)
rb_map <- function(
  ...,
  title = "Map",
  view = list(center = c(0, 0), zoom = 2),
  texts = list(),
  languages = "en",
  language = languages[1],
  locales = NULL,
  about = NULL,
  palette = NULL,
  palettes = rb_map_palettes(),
  basemaps = rb_map_basemaps()
) {
  groups <- list(...)
  if (
    !length(groups) ||
      !all(vapply(groups, inherits, logical(1), "rb_map_group"))
  ) {
    stop("`...` is one or more rb_map_group()s.", call. = FALSE)
  }
  layers <- unlist(lapply(groups, `[[`, "layers"), recursive = FALSE)
  ids <- vapply(layers, function(l) l$config$id, "")
  if (anyDuplicated(ids)) {
    stop(
      sprintf(
        "Layer ids are used twice: %s.",
        paste(unique(ids[duplicated(ids)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (
    sum(vapply(layers, function(l) l$config$kind == "grid", logical(1))) > 1
  ) {
    stop("A map has one grid layer at most.", call. = FALSE)
  }
  if (!is.null(palette) && !palette %in% vapply(palettes, `[[`, "", "id")) {
    stop("`palette` is an id of `palettes`.", call. = FALSE)
  }
  names(layers) <- ids
  config <- list(
    title = title,
    view = list(center = as.list(view$center), zoom = view$zoom),
    languages = as.list(languages),
    language = language,
    locales = if (length(locales)) lapply(locales, as.list),
    about = if (length(about)) as.list(about),
    palette = palette,
    groups = lapply(groups, function(g) {
      list(
        id = g$id,
        title = g$title,
        open = g$open,
        layers = as.list(vapply(g$layers, function(l) l$config$id, ""))
      )
    }),
    layers = unname(lapply(layers, `[[`, "config")),
    basemaps = basemaps,
    palettes = lapply(palettes, function(p) {
      p$colors <- as.list(p$colors)
      p
    }),
    texts = texts
  )
  structure(
    list(config = Filter(Negate(is.null), config), layers = layers),
    class = "rb_map"
  )
}

#' The palettes and base maps of a map
#'
#' `rb_map_palettes()`: twelve palettes of nine colours, light (low) to dark (high). The first ten change in lightness
#' steadily (OKLab), from viridisLite (magma, viridis, plasma) and tidyterra (deep, scotland, population_dens,
#' etopo1_bathy, colombia_bathy, gn_yl, blues); the last two, terrain palettes, rise and fall in lightness.
#' `rb_map_basemaps()`: Carto's Positron, Voyager and Dark Matter (vector), Esri's ocean and satellite tiles
#' (raster), and none.
#'
#' @return A list, each element with `id` and `label`: a palette with `source` and `colors`; a base map with `style`
#'   (a MapLibre style's address) or `tiles` (a raster address) with `maxzoom` and `attribution`, or neither (none).
#' @export
#' @examples
#' vapply(rb_map_palettes(), `[[`, "", "id")
#' vapply(rb_map_basemaps(), `[[`, "", "id")
rb_map_palettes <- function() {
  p <- function(id, source, colors, label = id) {
    list(id = id, label = label, source = source, colors = colors)
  }
  list(
    p(
      "magma",
      "viridisLite",
      c(
        "#FCFDBF",
        "#FEC287",
        "#FB8861",
        "#E65164",
        "#B63679",
        "#822681",
        "#51127C",
        "#1D1147",
        "#000004"
      )
    ),
    p(
      "viridis",
      "viridisLite",
      c(
        "#FDE725",
        "#AADC32",
        "#5DC863",
        "#27AD81",
        "#21908C",
        "#2C728E",
        "#3B528B",
        "#472D7B",
        "#440154"
      )
    ),
    p(
      "plasma",
      "viridisLite",
      c(
        "#F0F921",
        "#FDC328",
        "#F89441",
        "#E56B5D",
        "#CC4678",
        "#A92395",
        "#7E03A8",
        "#4C02A1",
        "#0D0887"
      )
    ),
    p(
      "deep",
      "tidyterra whitebox",
      c(
        "#FEFED7",
        "#EBF7B1",
        "#C5E7B4",
        "#7ECCBB",
        "#40B5C3",
        "#1D90BF",
        "#215DA7",
        "#243493",
        "#081C57"
      )
    ),
    p(
      "scotland",
      "tidyterra princess",
      c(
        "#E6CCAD",
        "#A98066",
        "#6C3B27",
        "#493835",
        "#173442",
        "#0F2531",
        "#071720",
        "#051017",
        "#01060B"
      )
    ),
    p(
      "population_dens",
      "tidyterra grass",
      c(
        "#FFFFFF",
        "#AF6419",
        "#8B4010",
        "#83370E",
        "#7A2E0C",
        "#72250B",
        "#6A1C08",
        "#621104",
        "#5A0400"
      )
    ),
    p(
      "etopo1_bathy",
      "tidyterra hypso",
      c(
        "#BCE6FF",
        "#55D7FF",
        "#34BFFF",
        "#17A0FC",
        "#176EF2",
        "#0026DE",
        "#0010CD",
        "#1505AF",
        "#1A0089"
      )
    ),
    p(
      "colombia_bathy",
      "tidyterra hypso",
      c(
        "#C6ECFF",
        "#8FD5FF",
        "#53BCF2",
        "#1B9FD3",
        "#017FB3",
        "#015F92",
        "#013F72",
        "#002B5E",
        "#001E50"
      )
    ),
    p(
      "gn_yl",
      "tidyterra whitebox",
      c(
        "#FFFFCC",
        "#E7F6B2",
        "#CEEB9E",
        "#B3DF91",
        "#93D283",
        "#70C274",
        "#46AC5D",
        "#218C49",
        "#006837"
      )
    ),
    p(
      "blues",
      "tidyterra grass",
      c(
        "#FFFFFF",
        "#EEE2FF",
        "#DCC4FF",
        "#C8A8FF",
        "#B38BFF",
        "#9B6FFF",
        "#7E52FF",
        "#5932FF",
        "#0000FF"
      )
    ),
    p(
      "high_relief",
      "tidyterra whitebox",
      c(
        "#488737",
        "#96B170",
        "#E2DBAB",
        "#E4C793",
        "#E4B47B",
        "#CDA886",
        "#B69C90",
        "#DBCCC6",
        "#FFFFFF"
      ),
      label = "high_relief (lightness not steady)"
    ),
    p(
      "bcyr",
      "tidyterra grass",
      c(
        "#0000FF",
        "#5377FF",
        "#4FC8FF",
        "#6CFFE7",
        "#C4FF9E",
        "#F3FF47",
        "#FFD100",
        "#FF8800",
        "#FF0000"
      ),
      label = "bcyr (lightness not steady)"
    )
  )
}

#' @rdname rb_map_palettes
#' @export
rb_map_basemaps <- function() {
  esri <- "Tiles \u00a9 Esri and its sources"
  list(
    list(
      id = "light",
      label = "Light (Carto Positron)",
      style = "https://basemaps.cartocdn.com/gl/positron-gl-style/style.json"
    ),
    list(
      id = "voyager",
      label = "Colour (Carto Voyager)",
      style = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json"
    ),
    list(
      id = "dark",
      label = "Dark (Carto Dark Matter)",
      style = "https://basemaps.cartocdn.com/gl/dark-matter-gl-style/style.json"
    ),
    list(
      id = "ocean",
      label = "Ocean (Esri)",
      maxzoom = 13,
      attribution = esri,
      tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}"
    ),
    list(
      id = "satellite",
      label = "Satellite (Esri)",
      maxzoom = 19,
      attribution = esri,
      tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
    ),
    list(id = "none", label = "None")
  )
}

#' Serve a map
#'
#' Starts a server for an [rb_map()] in the background of this R session, on `host` only (`127.0.0.1`: this computer),
#' and opens the page. It answers while the console is idle: a map waits while R runs something else.
#' `rb_map_stop()` stops it. A grid's `meta` and a geojson layer's `data` are read once, when the server starts.
#'
#' @param map An [rb_map()].
#' @param port,host Where the server listens.
#' @param open Whether to open the page: RStudio's Viewer when there is one, else a browser.
#' @param query Settings for the first view, in the page's address (`list(lang = "is", palette = "deep")`).
#' @return The page's address, invisibly.
#' @export
#' @examples
#' \dontrun{
#' rb_map_serve(m)
#' rb_map_stop()
#' }
rb_map_serve <- function(
  map,
  port = 8790L,
  host = "127.0.0.1",
  open = interactive(),
  query = NULL
) {
  .rb_need("httpuv")
  .rb_need("jsonlite")
  if (!inherits(map, "rb_map")) {
    stop("`map` is an rb_map().", call. = FALSE)
  }
  port <- as.integer(port)
  rb_map_stop(port)
  app <- .rb_map_app(map)
  .rb_maps[[as.character(port)]] <- tryCatch(
    httpuv::startServer(host, port, app),
    error = function(e) {
      stop(sprintf("Port %d is taken: give another port.", port), call. = FALSE)
    }
  )
  url <- sprintf(
    "http://%s:%d/",
    if (host == "127.0.0.1") "localhost" else host,
    port
  )
  if (length(query)) {
    url <- paste0(
      url,
      "?",
      paste(
        names(query),
        vapply(
          query,
          function(v) utils::URLencode(as.character(v), reserved = TRUE),
          ""
        ),
        sep = "=",
        collapse = "&"
      )
    )
  }
  if (open) {
    if (
      requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()
    ) {
      rstudioapi::viewer(url, height = "maximize")
    } else {
      utils::browseURL(url)
    }
  }
  invisible(url)
}

#' @rdname rb_map_serve
#' @export
rb_map_stop <- function(port = NULL) {
  ports <- if (is.null(port)) {
    ls(.rb_maps)
  } else {
    intersect(as.character(port), ls(.rb_maps))
  }
  for (p in ports) {
    httpuv::stopServer(.rb_maps[[p]])
    rm(list = p, envir = .rb_maps)
  }
  invisible(as.integer(ports))
}

#' A table of geometries as vector tiles
#'
#' Makes a tile function for a `"vector"` [rb_map_layer()] from a table of points, lines or areas: the table is copied
#' once into a temporary DuckDB table in web mercator, with each feature's bounds, and each tile is cut from it by
#' DuckDB (`ST_AsMVT()`). Every feature carries `kind` (`"point"`, `"line"` or `"area"`) and the `properties`.
#'
#' @param x A lazy DuckDB table or a data frame: a geometry column (DuckDB's GEOMETRY, or WKB), or `lon` and `lat`.
#' @param geometry The geometry column; or `lon`, `lat` for points from two columns.
#' @param lon,lat The position columns, for points.
#' @param properties The columns each feature carries. A vector tile holds text, numbers and logicals only, so a
#'   factor, date or time is carried as text, a small integer as an integer and a decimal as a double.
#' @param layer The tile layer of the features.
#' @param points The tile layer of a point on every feature (`ST_PointOnSurface()`), for the zooms where an area is
#'   too small to see; `NULL` for none.
#' @return A function of `z`, `x`, `y` returning a tile (raw bytes), with the number of features as its `count`
#'   attribute.
#' @export
#' @examples
#' \donttest{
#' tiles <- rb_map_tiles_table(rb_read_example("dansk_harbours"),
#'   properties = "SI_HARB", points = "harbour_points")
#' attr(tiles, "count")
#' length(tiles(7, 68, 39))
#' }
rb_map_tiles_table <- function(
  x,
  geometry = "geometry",
  lon = NULL,
  lat = NULL,
  properties = NULL,
  layer = "features",
  points = NULL
) {
  con <- .rb_con_for(x)
  tryCatch(DBI::dbExecute(con, "LOAD spatial"), error = function(e) {
    DBI::dbExecute(con, "INSTALL spatial; LOAD spatial")
  })
  src <- .rb_sql(.rb_as_lazy(x, con))
  q <- function(s) DBI::dbQuoteIdentifier(con, s)
  g <- if (!is.null(lon)) {
    sprintf("ST_Point(%s, %s)", q(lon), q(lat))
  } else {
    type <- DBI::dbGetQuery(
      con,
      sprintf("SELECT typeof(%s) AS t FROM (%s) LIMIT 1", q(geometry), src)
    )$t
    # DuckDB's GEOMETRY may carry its CRS in the type, GEOMETRY('OGC:CRS84'); WKB is a BLOB
    if (startsWith(type, "GEOMETRY")) {
      q(geometry)
    } else {
      sprintf("ST_GeomFromWKB(%s)", q(geometry))
    }
  }
  props <- vapply(properties, function(p) as.character(q(p)), "")
  nm <- paste0(
    "rb_tiles_",
    paste(sample(c(letters, 0:9), 10, TRUE), collapse = "")
  )
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE TEMP TABLE %s AS
     WITH p AS (SELECT %s %s AS g FROM (%s)),
     m AS (
       SELECT * EXCLUDE (g),
              CASE WHEN ST_GeometryType(g) IN ('POINT', 'MULTIPOINT') THEN 'point'
                   WHEN ST_GeometryType(g) IN ('LINESTRING', 'MULTILINESTRING') THEN 'line' ELSE 'area' END AS kind,
              ST_Transform(g, 'EPSG:4326', 'EPSG:3857', always_xy := true) AS g3857,
              ST_Transform(ST_PointOnSurface(g), 'EPSG:4326', 'EPSG:3857', always_xy := true) AS p3857
       FROM p WHERE g IS NOT NULL)
     SELECT *, ST_XMin(g3857) AS xmin, ST_XMax(g3857) AS xmax, ST_YMin(g3857) AS ymin, ST_YMax(g3857) AS ymax FROM m",
      nm,
      if (length(props)) paste0(paste(props, collapse = ", "), ",") else "",
      g,
      src
    )
  )
  n <- DBI::dbGetQuery(
    con,
    sprintf("SELECT count(*)::INTEGER AS n FROM %s", nm)
  )$n
  types <- DBI::dbGetQuery(con, sprintf("DESCRIBE %s", nm))
  fields <- paste(
    c(sprintf(
      "%s: %s",
      DBI::dbQuoteString(con, c(properties, "kind")),
      c(
        .rb_mvt_cast(
          props,
          types$column_type[match(properties, types$column_name)]
        ),
        "kind"
      )
    )),
    collapse = ", "
  )
  # the layers are written apart and joined: a tile's layers are protobuf messages, so their bytes concatenate
  tile <- function(z, x, y) {
    one <- function(geom, name, where) {
      sprintf(
        "(SELECT ST_AsMVT({'geometry': ST_AsMVTGeom(%s, ST_Extent(env), 4096, 64, true), %s}, %s, 4096) FROM f, e %s)",
        geom,
        fields,
        DBI::dbQuoteString(con, name),
        where
      )
    }
    sql <- sprintf(
      "WITH e AS (SELECT ST_TileEnvelope(%d, %d, %d) AS env),
       f AS (SELECT t.* FROM %s t, e
             WHERE t.xmax >= ST_XMin(e.env) AND t.xmin <= ST_XMax(e.env) AND t.ymax >= ST_YMin(e.env) AND t.ymin <= ST_YMax(e.env))
       SELECT %s AS a%s",
      as.integer(z),
      as.integer(x),
      as.integer(y),
      nm,
      one("g3857", layer, if (is.null(points)) "" else "WHERE kind <> 'point'"),
      if (is.null(points)) {
        ""
      } else {
        paste0(", ", one("p3857", points, ""), " AS b")
      }
    )
    r <- DBI::dbGetQuery(con, sql)
    out <- c(r$a[[1]], if (!is.null(points)) r$b[[1]])
    if (is.null(out)) raw(0) else out
  }
  attr(tile, "count") <- n
  tile
}

# ST_AsMVT() takes VARCHAR, FLOAT, DOUBLE, INTEGER, BIGINT and BOOLEAN properties and refuses every other type: an R
# factor reaches DuckDB as an ENUM, a date as a DATE
.rb_mvt_cast <- function(expr, type) {
  to <- ifelse(
    type %in% c("VARCHAR", "FLOAT", "DOUBLE", "INTEGER", "BIGINT", "BOOLEAN"),
    NA,
    ifelse(
      type %in% c("TINYINT", "SMALLINT", "UTINYINT", "USMALLINT"),
      "INTEGER",
      ifelse(
        type == "UINTEGER",
        "BIGINT",
        ifelse(startsWith(type, "DECIMAL"), "DOUBLE", "VARCHAR")
      )
    )
  )
  ifelse(is.na(to), expr, sprintf("CAST(%s AS %s)", expr, to))
}

# The server -----------------------------------------------------------------------------------------------------------

# The app behind rb_map_serve(), a list with `call(req)` for httpuv: its routes in the header of this file
.rb_map_app <- function(map) {
  page <- paste(
    readLines(
      system.file("map", "index.html", package = "ramb"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  json <- function(x) {
    as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    ))
  }
  config <- json(map$config)
  # "</" escaped, so the config cannot close its script tag
  page <- sub(
    "__CONFIG__",
    gsub("</", "<\\/", config, fixed = TRUE),
    page,
    fixed = TRUE
  )
  layers <- map$layers
  # a vector layer's tiles carry the server's start in their address, so a server started on other data on the same
  # port is not hidden by the browser's cache (a grid carries its meta's `v`)
  started <- format(as.numeric(Sys.time()), digits = 15)
  metas <- lapply(layers, function(l) {
    switch(
      l$config$kind,
      grid = if (is.function(l$meta)) l$meta() else l$meta,
      vector = list(
        count = if (is.null(attr(l$tiles, "count"))) {
          1L
        } else {
          attr(l$tiles, "count")
        },
        v = started
      ),
      NULL
    )
  })
  geojson <- lapply(layers, function(l) {
    if (l$config$kind != "geojson") {
      return(NULL)
    }
    d <- if (is.function(l$data)) l$data() else l$data
    if (inherits(d, "sf")) {
      f <- tempfile(fileext = ".geojson")
      sf::st_write(
        d,
        f,
        driver = "GeoJSON",
        quiet = TRUE,
        layer_options = "COORDINATE_PRECISION=6"
      )
      d <- paste(
        readLines(f, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )
    }
    d
  })
  meta <- json(list(
    engine = .rb_map_engine,
    layers = Filter(Negate(is.null), metas)
  ))
  reply <- function(
    status,
    body,
    type = "text/plain; charset=utf-8",
    headers = list()
  ) {
    list(
      status = status,
      headers = c(list(`Content-Type` = type), headers),
      body = body
    )
  }
  list(call = function(req) {
    path <- req$PATH_INFO
    if (req$REQUEST_METHOD != "GET") {
      return(reply(405L, "GET only"))
    }
    if (path == "/") {
      return(reply(
        200L,
        page,
        "text/html; charset=utf-8",
        list(`Cache-Control` = "no-store")
      ))
    }
    if (path == "/config.json") {
      return(reply(200L, config, "application/json; charset=utf-8"))
    }
    if (path == "/meta") {
      return(reply(
        200L,
        meta,
        "application/json; charset=utf-8",
        list(`Cache-Control` = "no-store")
      ))
    }
    d <- regmatches(
      path,
      regexec("^/data/([a-z][a-z0-9_]*)[.]geojson$", path)
    )[[1]]
    if (length(d) == 2) {
      g <- geojson[[d[2]]]
      if (is.null(g)) {
        return(reply(404L, "no such data"))
      }
      return(reply(
        200L,
        g,
        "application/geo+json; charset=utf-8",
        list(`Cache-Control` = "no-store")
      ))
    }
    m <- regmatches(
      path,
      regexec(
        "^/tiles/([a-z][a-z0-9_]*)/([0-9]+)/([0-9]+)/([0-9]+)[.]pbf$",
        path
      )
    )[[1]]
    if (
      length(m) != 5 ||
        is.null(layers[[m[2]]]) ||
        layers[[m[2]]]$config$kind == "geojson"
    ) {
      return(reply(404L, "not found"))
    }
    z <- .rb_map_whole(m[3], 0L, 22L)
    if (is.null(z)) {
      return(reply(400L, "z is 0-22"))
    }
    x <- .rb_map_whole(m[4], 0L, 2L^z - 1L)
    y <- .rb_map_whole(m[5], 0L, 2L^z - 1L)
    if (is.null(x) || is.null(y)) {
      return(reply(400L, "x and y lie in the zoom's range"))
    }
    l <- layers[[m[2]]]
    if (l$config$kind == "vector") {
      return(.rb_map_tile_reply(function() l$tiles(z, x, y), reply))
    }
    p <- .rb_map_params(
      l$config$filters,
      metas[[m[2]]],
      .rb_map_query(req$QUERY_STRING)
    )
    if (is.character(p)) {
      return(reply(400L, p))
    }
    .rb_map_tile_reply(function() l$tiles(z, x, y, p), reply)
  })
}

# A tile, or the first line of why its function failed: the page shows the reason, where the map would stay blank
.rb_map_tile_reply <- function(tile, reply) {
  body <- tryCatch(tile(), error = function(e) e)
  if (inherits(body, "error")) {
    return(reply(500L, sub("\n.*", "", conditionMessage(body))))
  }
  reply(
    200L,
    if (is.null(body)) raw(0) else body,
    "application/vnd.mapbox-vector-tile",
    list(`Cache-Control` = "max-age=600")
  )
}

.rb_map_whole <- function(x, lo, hi) {
  if (is.null(x) || !grepl("^[0-9]+$", x)) {
    return(NULL)
  }
  v <- as.numeric(x)
  if (v < lo || v > hi) NULL else as.integer(v)
}

.rb_map_query <- function(q) {
  q <- sub("^[?]", "", if (is.null(q)) "" else q)
  if (!nzchar(q)) {
    return(list())
  }
  kv <- strsplit(strsplit(q, "&", fixed = TRUE)[[1]], "=", fixed = TRUE)
  stats::setNames(
    lapply(kv, function(x) utils::URLdecode(if (length(x) > 1) x[2] else "")),
    vapply(kv, `[`, "", 1)
  )
}

# A grid's query against its filters: a named list of the values given, or the reason it is refused. A value must be
# one of the filter's options (several of its meta options, comma separated), or a range's bounds within its meta.
.rb_map_params <- function(filters, meta, query) {
  out <- list()
  for (f in filters) {
    if (identical(f$type, "metric")) {
      next
    }
    if (identical(f$type, "range")) {
      ps <- unlist(f$params)
      v <- query[ps]
      given <- !vapply(v, is.null, logical(1))
      if (!any(given)) {
        next
      }
      if (!all(given)) {
        return(sprintf("%s are given together", paste(ps, collapse = " and ")))
      }
      r <- unlist(meta$ranges[[f$id]])
      n <- suppressWarnings(as.numeric(unlist(v)))
      if (anyNA(n) || any(n < r[1] | n > r[2])) {
        return(sprintf(
          "%s lie in %s to %s",
          paste(ps, collapse = " and "),
          r[1],
          r[2]
        ))
      }
      out[ps] <- unlist(v)
      next
    }
    v <- query[[f$id]]
    if (is.null(v) || !nzchar(v)) {
      next
    }
    allowed <- if (isTRUE(f$meta)) {
      as.character(meta$options[[f$id]]$value)
    } else {
      vapply(f$options, function(o) as.character(o$value), "")
    }
    vs <- if (isTRUE(f$meta)) strsplit(v, ",", fixed = TRUE)[[1]] else v
    if (!length(vs) || !all(vs %in% allowed)) {
      return(sprintf(
        "%s is %s",
        f$id,
        if (length(allowed) <= 12) {
          paste(allowed, collapse = ", ")
        } else {
          "one of its options"
        }
      ))
    }
    out[[f$id]] <- v
  }
  out
}
