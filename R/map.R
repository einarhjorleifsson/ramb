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
.rb_tiles <- new.env(parent = emptyenv()) # the number of rb_map_tiles_table()'s last temporary table
.rb_tiles$n <- 0L
.rb_tiles$served <- list() # by port, the temporary tables (connection and name) of the map served there

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
#' @param variants For a vector or geojson layer, styles to choose between in the page, such as the column the
#'   points are coloured by: `label` (the control's text) and `options`, each a `value`, a `label`, `paint` (by the
#'   layer's style ids, the paint properties it sets) and optionally `legend` (in place of the layer's). The first
#'   option is shown when the page opens; a choice goes into the address as `<id>_by`, and nothing is fetched again.
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
#' @examples
#' # A vector layer: the harbours as areas, a click shows the harbour's id
#' \donttest{
#' harbours <- rb_map_layer(
#'   "harbours", "vector", label = "Harbours",
#'   tiles = rb_map_tiles_table(rb_read_example("dansk_harbours"), properties = "SI_HARB"),
#'   style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
#'                     paint = list("fill-color" = "#d9822b"))),
#'   popup = list(click = "harbours-fill", title = "SI_HARB")
#' )
#' }
#'
#' # A grid: its tile function is given the filters' values in `params`. This one returns an
#' # empty tile; a real one returns DuckDB's ST_AsMVT() of the cells (see the "Maps in RStudio"
#' # article).
#' cells <- rb_map_layer(
#'   "cells", "grid", label = "Pings by cell",
#'   tiles = function(z, x, y, params) raw(0),
#'   filters = list(
#'     list(id = "behaviour", label = "Behaviour",
#'          options = list(list(value = "", label = "all"),
#'                         list(value = "Fishing", label = "fishing"))),
#'     list(id = "metric", type = "metric", label = "Metric")
#'   ),
#'   metrics = list(list(field = "n", label = "pings"))
#' )
#'
#' # A geojson layer: features sent whole, here one point
#' point <- rb_map_layer(
#'   "point", "geojson", label = "A point",
#'   data = '{"type": "FeatureCollection", "features": [{"type": "Feature", "properties": {},
#'            "geometry": {"type": "Point", "coordinates": [12.6, 56.0]}}]}',
#'   style = list(list(id = "point-dot", type = "circle", paint = list("circle-color" = "#c0392b")))
#' )
#' names(cells$config)
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
  variants = NULL,
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
  if (!is.null(variants)) {
    if (kind == "grid") {
      stop(
        "`variants` are for a vector or geojson layer; a grid is coloured by its `metrics`.",
        call. = FALSE
      )
    }
    style_ids <- vapply(style, function(s) if (is.null(s$id)) "" else s$id, "")
    one <- function(o) {
      is.character(o$value) &&
        length(o$value) == 1 &&
        is.list(o$paint) &&
        length(o$paint) > 0 &&
        all(names(o$paint) %in% style_ids)
    }
    if (
      !length(variants$options) ||
        !all(vapply(variants$options, one, logical(1)))
    ) {
      stop(
        "`variants` is a list of `label` and `options`, each option a `value`, a `label` and `paint`: by the layer's style ids, the paint properties it sets.",
        call. = FALSE
      )
    }
    if (anyDuplicated(vapply(variants$options, `[[`, "", "value"))) {
      stop("The `options` of `variants` need distinct values.", call. = FALSE)
    }
    variants$options <- lapply(variants$options, function(o) {
      if (is.null(o$label)) {
        o$label <- o$value
      }
      o
    })
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
    variants = variants,
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
#' @examples
#' point <- rb_map_layer(
#'   "point", "geojson", label = "A point",
#'   data = '{"type": "Point", "coordinates": [12.6, 56.0]}',
#'   style = list(list(id = "point-dot", type = "circle"))
#' )
#' ref <- rb_map_group("ref", "Reference", list(point), open = FALSE)
#' ref$title
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
#' @param choose Fields whose values the page offers to show, over every layer that has the field (each a control
#'   above the groups, with buttons that step through the values): a list with `fields`, each a `field`, a `label`,
#'   `multi` (`TRUE`: each value a chip ticked on and off, so several show at once; else a list, "all" first, to show
#'   one at a time), its `options` (each a `value` for the control and the address, a `label`, `match`, the value the
#'   layers' features carry, `NULL` for none, and optionally `bbox`, the extent the view is fitted to) and `layers`
#'   (the ids it filters); and optionally `extents`, each a `key` (the values of every field joined by the character 31)
#'   and a `bbox`, for the view where every field has one value chosen. [rb_map_pings()] and [rb_map_track()] make it
#'   from the data's groups. A choice goes into the address under the field's name, several values separated by
#'   commas, and nothing is fetched again.
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
  choose = NULL,
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
  if (!is.null(choose)) {
    one <- function(f) {
      is.character(f$field) && length(f$field) == 1 && length(f$options) > 0
    }
    if (
      !length(choose$fields) || !all(vapply(choose$fields, one, logical(1)))
    ) {
      stop(
        "`choose` is a list of `fields`, each a `field`, its `options` (each a `value` and a `match`) and its `layers`.",
        call. = FALSE
      )
    }
    taken <- c(
      ids,
      paste0(ids, "_by"),
      "metric",
      "palette",
      "stretch",
      "floor",
      "cap",
      "opacity",
      "base",
      "lang",
      "v",
      unlist(lapply(layers, function(l) {
        unlist(lapply(l$config$filters, function(f) c(f$id, unlist(f$params))))
      }))
    )
    clash <- intersect(vapply(choose$fields, `[[`, "", "field"), taken)
    if (length(clash)) {
      stop(
        sprintf(
          "`choose` has a field, %s, named as the page's address names a layer or a control.",
          paste(clash, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    # one-element arrays stay arrays in the config's JSON
    choose$fields <- lapply(choose$fields, function(f) {
      f$layers <- as.list(f$layers)
      f$options <- lapply(f$options, function(o) {
        if (!is.null(o$bbox)) {
          o$bbox <- as.list(o$bbox)
        }
        o
      })
      f
    })
    choose$extents <- lapply(choose$extents, function(e) {
      e$bbox <- as.list(e$bbox)
      e
    })
  }
  names(layers) <- ids
  config <- list(
    title = title,
    view = list(center = as.list(view$center), zoom = view$zoom),
    languages = as.list(languages),
    language = language,
    locales = if (length(locales)) lapply(locales, as.list),
    about = if (length(about)) as.list(about),
    choose = choose,
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
#' # Opens a map in RStudio's Viewer (or a browser) when run by hand, e.g. by example(rb_map_serve)
#' if (interactive()) {
#'   harbours <- rb_map_layer(
#'     "harbours", "vector", label = "Harbours",
#'     tiles = rb_map_tiles_table(rb_read_example("dansk_harbours"), properties = "SI_HARB"),
#'     style = list(list(id = "harbours-fill", type = "fill", "source-layer" = "features",
#'                       paint = list("fill-color" = "#d9822b"))),
#'     popup = list(click = "harbours-fill", title = "SI_HARB")
#'   )
#'   m <- rb_map(rb_map_group("ref", "Reference", list(harbours)),
#'               title = "Danish harbours", view = list(center = c(10.5, 56), zoom = 7))
#'   rb_map_serve(m)
#'   # again, opening on the ocean base map (it replaces the first):
#'   # rb_map_serve(m, query = list(base = "ocean"))
#'   # rb_map_stop() # when done
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
  .rb_tiles$served[[as.character(port)]] <- attr(map, "tables")
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
    .rb_map_drop(p)
  }
  invisible(as.integer(ports))
}

# The temporary tables of the map served on a port, dropped when it stops or is replaced, unless a map on another
# port uses them; a map shown again makes them again (rb_map_tiles_table())
.rb_map_drop <- function(port) {
  old <- .rb_tiles$served[[as.character(port)]]
  .rb_tiles$served[[as.character(port)]] <- NULL
  in_use <- unlist(lapply(.rb_tiles$served, function(ts) {
    vapply(ts, `[[`, "", "name")
  }))
  for (t in old) {
    if (!t$name %in% in_use) {
      try(
        DBI::dbExecute(t$con, sprintf("DROP TABLE IF EXISTS %s", t$name)),
        silent = TRUE
      )
    }
  }
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
#'   attribute and the temporary table's name as its `table` attribute.
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
  # numbered, not named from R's random numbers: those repeat after set.seed(), and drawing them moves the user's
  # stream; a name a table already has (after the package is loaded again) is skipped
  repeat {
    .rb_tiles$n <- .rb_tiles$n + 1L
    nm <- paste0("rb_tiles_", .rb_tiles$n)
    if (!DBI::dbExistsTable(con, nm)) break
  }
  make <- function() {
    DBI::dbExecute(con, create)
  }
  create <- sprintf(
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
  make()
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
    # a table dropped when its map was replaced on its port (rb_map_serve()) is made again
    r <- tryCatch(DBI::dbGetQuery(con, sql), error = function(e) {
      if (DBI::dbExistsTable(con, nm)) {
        stop(e)
      }
      make()
      DBI::dbGetQuery(con, sql)
    })
    out <- c(r$a[[1]], if (!is.null(points)) r$b[[1]])
    if (is.null(out)) raw(0) else out
  }
  attr(tile, "count") <- n
  attr(tile, "table") <- nm
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

#' Pings on a map, layer by layer
#'
#' The quick look at pings. `rb_map_pings()` draws them as points coloured by a column, `rb_map_track()` joins them
#' into each vessel's track, and each adds its layer to the map piped into it, so they compose:
#' `pings |> rb_map_track() |> rb_map_pings()`.
#'
#' Given pings, each starts a map; given a map from either, it adds a layer drawn over the earlier ones, from the map's
#' pings and settings (`lon`, `lat`, `time`, `by`) unless `data` gives others. The result is an [rb_map()], shown when
#' printed: at the console in RStudio's Viewer (or a browser), a map printed on the same port replacing the last and
#' dropping its temporary tables. [rb_map_serve()] serves it on a port of one's choice.
#'
#' Points: a numeric column is coloured on a continuous palette spread over its 1st to 99th percentile; any other
#' column, or with `discrete = TRUE`, takes a colour per value (the 12 most frequent; the rest grey). Several columns,
#' `c(speed, behaviour)`, are offered under "Colour by" in the page, the first shown. A point's radius is in metres,
#' so it grows as the map zooms in, from 1 pixel to `radius_max` pixels.
#'
#' Track: each vessel's pings in time order, broken where they are more than `gap_h` hours apart. With a column, such
#' as `fishing_flow`, it breaks where the value changes too, each run coloured by its value (as the points are) and
#' ending at the next run's first fix, so the line stays joined. A second call adds a second track, by another column,
#' each one turned on and off in the page: `pings |> rb_map_track(fishing_flow) |> rb_map_track(provider)`.
#'
#' Grouped pings, `group_by(trip)` or `group_by(trip, gear)`, give the page a control per group column above the
#' layers, each value with its number of pings: for a column of 12 values or fewer, chips that tick on and off, so
#' several show at once (setting and hauling); for one of more, a list to show one at a time; and buttons that step to
#' the previous and next. A choice shows those groups' pings and lines on every layer that has the column, fits the
#' view to them, and goes into the address. The groups cut the track too, so each group's line is its own.
#'
#' The map opens on a dark base map. Each layer copies its pings into a temporary DuckDB table, as
#' [rb_map_tiles_table()] does.
#'
#' @param x Pings: a data frame or lazy DuckDB table with `lon` and `lat` (WGS 84), or an sf object of points; or a
#'   map made by either function.
#' @param col For the points, the column they are coloured by, a name or a string, or several,
#'   `c(fishing_flow, speed)`, to choose between in the page ("Colour by"), the first shown. For the track, one column,
#'   whose runs of equal value cut and colour the line (a numeric one cuts it at every ping, each step coloured by its
#'   first ping's value), or `NULL` for one colour; for another column, call `rb_map_track()` again.
#' @param popup Further columns a click on a point shows.
#' @param discrete Whether a column of `col` takes a colour per value; by default each that is not numeric.
#' @param radius,radius_max The point radius in metres, and its largest size in pixels.
#' @param palette A palette of [grDevices::hcl.colors()] for a numeric column, light at its high end on the dark base
#'   map.
#' @param gap_h A silence of more than `gap_h` hours between a vessel's pings breaks its line; `Inf` for none.
#' @param data Pings for this layer, in place of the map's.
#' @param lon,lat,time The position and time columns: by default the map's, else `"lon"`, `"lat"` and `"time"`.
#'   The track needs the time; a point's popup shows it first.
#' @param by The vessel column, a name or a string: one track each. By default the map's, else `vid` where the data
#'   have it, else one track through all the pings.
#' @param choose Columns whose values the page offers to show, a name, a string or `c()` of them: by default
#'   the groups of the pings (`dplyr::group_by()`). Set by the call that starts the map.
#' @param title,port The page's title, and where the map is served when printed: set by the call that starts it.
#' @return An [rb_map()], with the layer added.
#' @seealso [rb_map()] for a map of other layers, groups and languages.
#' @export
#' @examples
#' \donttest{
#' m <- rb_read_example("dansk") |>
#'   rb_map_track(time = "time_stamp", by = vessel_id) |>
#'   rb_map_pings(c(speed, behaviour))
#' m
#' }
#' # At the console a map opens in RStudio's Viewer when printed:
#' if (interactive()) {
#'   rb_read_example("dansk") |> rb_map_pings(time = "time_stamp")
#'   rb_read_example("dansk") |>
#'     rb_map_track(behaviour, time = "time_stamp", by = vessel_id) |>
#'     rb_map_pings(c(behaviour, speed))
#' }
rb_map_pings <- function(
  x,
  col = "speed",
  popup = NULL,
  discrete = NULL,
  radius = 200,
  radius_max = 8,
  palette = "Inferno",
  data = NULL,
  lon = NULL,
  lat = NULL,
  time = NULL,
  by = NULL,
  choose = NULL,
  title = "Track",
  port = 8790L
) {
  s <- .rb_quick(
    x,
    data,
    lon,
    lat,
    time,
    rlang::enquo(by),
    title,
    port,
    rlang::enquo(choose)
  )
  ctx <- s$ctx_layer
  col <- .rb_track_cols(rlang::enquo(col), ctx$cols)
  .rb_quick_need(ctx, c(col, popup))
  id <- .rb_quick_id(s$state, "pings")
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  numeric <- vapply(
    col,
    function(cc) {
      is.numeric(dplyr::collect(utils::head(
        dplyr::select(ctx$x, dplyr::all_of(cc)),
        1
      ))[[1]])
    },
    logical(1)
  )
  is_discrete <- if (is.null(discrete)) {
    !numeric
  } else {
    rep(isTRUE(discrete), length(col))
  }
  colours <- lapply(seq_along(col), function(i) {
    .rb_quick_colours(
      ctx,
      col[i],
      is_discrete[i],
      paste0("rb_class_", i),
      palette
    )
  })
  dot <- paste0(id, "-dot")
  # several columns: a choice in the page, each column's colours and legend a variant of the points' style
  variants <- if (length(col) > 1) {
    list(
      label = "Colour by",
      options = lapply(seq_along(col), function(i) {
        list(
          value = col[i],
          label = col[i],
          paint = stats::setNames(
            list(list("circle-color" = colours[[i]]$colour)),
            dot
          ),
          legend = colours[[i]]$legend
        )
      })
    )
  }
  # the radius in metres: pixels at zoom 0 at the data's middle latitude, doubling with each zoom, from 1 to radius_max
  r0 <- radius /
    (156543.03392 * cos((ctx$box$lat0 + ctx$box$lat1) / 2 * pi / 180))
  za <- min(max(log2(1 / r0), 0), 23)
  zb <- min(max(log2(radius_max / r0), za + 0.01), 24)
  size <- list(
    "interpolate",
    list("exponential", 2),
    list("zoom"),
    za,
    max(1, r0 * 2^za),
    zb,
    min(radius_max, r0 * 2^zb)
  )
  classes <- vapply(
    which(is_discrete),
    function(i) sprintf(", CAST(%s AS VARCHAR) AS rb_class_%d", q(col[i]), i),
    ""
  )
  pts <- dplyr::tbl(
    con,
    dplyr::sql(sprintf(
      "SELECT *%s FROM (%s)",
      paste(classes, collapse = ""),
      ctx$src
    ))
  )
  props <- unique(c(
    col,
    ctx$time,
    popup,
    ctx$by,
    intersect(.rb_or(s$state[["choose_fields"]], character()), ctx$cols),
    if (any(is_discrete)) paste0("rb_class_", which(is_discrete))
  ))
  tiles <- rb_map_tiles_table(
    pts,
    lon = ctx$lon,
    lat = ctx$lat,
    properties = props
  )
  fields <- c(
    lapply(seq_along(col), function(i) {
      Filter(
        Negate(is.null),
        list(
          field = col[i],
          label = col[i],
          format = if (is_discrete[i]) NULL else "value"
        )
      )
    }),
    lapply(setdiff(unique(c(popup, ctx$by)), col), function(f) {
      list(field = f, label = f)
    })
  )
  layer <- rb_map_layer(
    id,
    "vector",
    label = .rb_quick_label(s$state, "Pings"),
    info = if (length(col) > 1) {
      "Each ping, coloured by the column chosen below; a numeric one over its 1st to 99th percentile."
    } else {
      sprintf(
        "Each ping, coloured by %s%s.",
        col,
        if (is_discrete[1]) {
          ""
        } else {
          ", its 1st to 99th percentile across the palette"
        }
      )
    },
    tiles = tiles,
    count = list(text = "{1} pings"),
    style = list(list(
      id = dot,
      type = "circle",
      "source-layer" = "features",
      paint = list(
        "circle-color" = colours[[1]]$colour,
        "circle-radius" = size,
        "circle-opacity" = 0.85
      )
    )),
    legend = colours[[1]]$legend,
    variants = variants,
    popup = list(click = dot, title = .rb_or(ctx$time, col[1]), fields = fields)
  )
  .rb_quick_map(s, layer, list(con = con, name = attr(tiles, "table")))
}

#' @rdname rb_map_pings
#' @export
rb_map_track <- function(
  x,
  col = NULL,
  gap_h = 1,
  discrete = NULL,
  palette = "Inferno",
  data = NULL,
  lon = NULL,
  lat = NULL,
  time = NULL,
  by = NULL,
  choose = NULL,
  title = "Track",
  port = 8790L
) {
  s <- .rb_quick(
    x,
    data,
    lon,
    lat,
    time,
    rlang::enquo(by),
    title,
    port,
    rlang::enquo(choose)
  )
  ctx <- s$ctx_layer
  col_q <- rlang::enquo(col)
  col <- if (rlang::quo_is_null(col_q)) {
    NULL
  } else {
    .rb_track_cols(col_q, ctx$cols)
  }
  if (length(col) > 1) {
    stop(
      "The track is coloured by one column; for another, call rb_map_track() again: each call adds a track to turn on and off.",
      call. = FALSE
    )
  }
  if (is.null(ctx$time)) {
    stop(
      "The track joins the pings in time order: name their time column in `time`.",
      call. = FALSE
    )
  }
  .rb_quick_need(ctx, col)
  id <- .rb_quick_id(s$state, "path")
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  by <- ctx$by
  # the line is cut by the fields to choose by as well as by the vessel, so a choice shows whole lines
  keys <- unique(c(
    by,
    intersect(.rb_or(s$state[["choose_fields"]], character()), ctx$cols)
  ))
  k <- length(col)
  numeric <- vapply(
    col,
    function(cc) {
      is.numeric(dplyr::collect(utils::head(
        dplyr::select(ctx$x, dplyr::all_of(cc)),
        1
      ))[[1]])
    },
    logical(1)
  )
  is_discrete <- if (is.null(discrete)) !numeric else rep(isTRUE(discrete), k)
  # each column's value on the run, as rb_path_<i>: text for a colour per value, a number for a palette
  vals <- vapply(
    seq_len(k),
    function(i) {
      sprintf(
        ", CAST(%s AS %s) AS v%d",
        q(col[i]),
        if (is_discrete[i]) "VARCHAR" else "DOUBLE",
        i
      )
    },
    ""
  )
  changed <- vapply(
    seq_len(k),
    function(i) sprintf(" OR v%1$d IS DISTINCT FROM lag(v%1$d) OVER o", i),
    ""
  )
  kept <- vapply(
    seq_len(k),
    function(i) sprintf("any_value(v%1$d) AS rb_path_%1$d, ", i),
    ""
  )
  part <- if (!length(keys)) {
    ""
  } else {
    paste("PARTITION BY", paste(q(keys), collapse = ", "))
  }
  gap <- if (is.finite(gap_h)) {
    format(gap_h * 3600, scientific = FALSE)
  } else {
    "'infinity'::DOUBLE"
  }
  # a line per vessel and stretch: a silence of more than gap_h hours starts a new one, and so does a change of any
  # column's value, so each run carries one value of each; a run ends at the next run's first fix, so the line stays
  # joined across a change
  lines <- dplyr::tbl(
    con,
    dplyr::sql(sprintf(
      "WITH s AS (
         SELECT %1$s %2$s AS lon, %3$s AS lat, %4$s AS t %8$s FROM (%7$s)
         WHERE %2$s IS NOT NULL AND %3$s IS NOT NULL AND %4$s IS NOT NULL),
       w AS (
         SELECT *, CASE WHEN epoch(t) - epoch(lag(t) OVER o) > %6$s %9$s THEN 1 ELSE 0 END AS brk,
                lead(lon) OVER o AS lon2, lead(lat) OVER o AS lat2, epoch(lead(t) OVER o) - epoch(t) AS dt_next
         FROM s WINDOW o AS (%5$s ORDER BY t)),
       g AS (SELECT *, sum(brk) OVER (%5$s ORDER BY t ROWS UNBOUNDED PRECEDING) AS stretch FROM w),
       a AS (
         SELECT %1$s %10$s count(*) AS pings, strftime(min(t), '%%Y-%%m-%%d %%H:%%M') AS start,
                strftime(max(t), '%%Y-%%m-%%d %%H:%%M') AS stop, list(ST_Point(lon, lat) ORDER BY t) AS pts,
                last(CASE WHEN dt_next <= %6$s THEN lon2 END ORDER BY t) AS lon2,
                last(CASE WHEN dt_next <= %6$s THEN lat2 END ORDER BY t) AS lat2
         FROM g GROUP BY %1$s stretch),
       l AS (SELECT * EXCLUDE (pts, lon2, lat2),
                    CASE WHEN lon2 IS NULL THEN pts ELSE list_append(pts, ST_Point(lon2, lat2)) END AS pts FROM a)
       SELECT * EXCLUDE (pts), ST_MakeLine(pts) AS geometry FROM l WHERE len(pts) > 1",
      if (!length(keys)) "" else paste0(paste(q(keys), collapse = ", "), ","),
      q(ctx$lon),
      q(ctx$lat),
      q(ctx$time),
      part,
      gap,
      ctx$src,
      paste(vals, collapse = ""),
      paste(changed, collapse = ""),
      paste(kept, collapse = "")
    ))
  )
  props <- c(
    keys,
    if (k) paste0("rb_path_", seq_len(k)),
    "pings",
    "start",
    "stop"
  )
  tiles <- rb_map_tiles_table(lines, properties = props)
  line <- paste0(id, "-line")
  colours <- lapply(seq_len(k), function(i) {
    p <- paste0("rb_path_", i)
    v <- .rb_quick_colours(ctx, col[i], is_discrete[i], p, palette, get = p)
    v$legend <- lapply(v$legend, function(x) {
      list(swatch = list(fill = x$swatch$dot), label = x$label)
    })
    v
  })
  layer <- rb_map_layer(
    id,
    "vector",
    label = .rb_quick_label(
      s$state,
      if (k == 1) paste("Track by", col) else "Track"
    ),
    info = sprintf(
      "%s pings joined in time order, broken where they are more than %s hours apart%s.",
      if (is.null(by)) "The" else sprintf("Each %s's", by),
      format(gap_h),
      if (k == 0) {
        ""
      } else {
        sprintf(", and coloured by %s", col)
      }
    ),
    tiles = tiles,
    style = list(list(
      id = line,
      type = "line",
      "source-layer" = "features",
      paint = if (k == 0) {
        list("line-color" = "#00e5ff", "line-width" = 1.5, "line-opacity" = 0.5)
      } else {
        list(
          "line-color" = colours[[1]]$colour,
          "line-width" = 3,
          "line-opacity" = 0.85
        )
      }
    )),
    legend = if (k == 0) {
      list(list(swatch = list(fill = "#00e5ff"), label = "track"))
    } else {
      colours[[1]]$legend
    },
    popup = list(
      click = line,
      title = .rb_or(by, "start"),
      fields = c(
        lapply(seq_len(k), function(i) {
          Filter(
            Negate(is.null),
            list(
              field = paste0("rb_path_", i),
              label = col[i],
              format = if (is_discrete[i]) NULL else "value"
            )
          )
        }),
        list(
          list(field = "start", label = "from"),
          list(field = "stop", label = "to"),
          list(field = "pings", label = "pings", format = "count")
        )
      )
    )
  )
  .rb_quick_map(s, layer, list(con = con, name = attr(tiles, "table")))
}

#' Show a map
#'
#' At an interactive console a map is served when printed ([rb_map_serve()], on the port its first layer function set,
#' else 8790) and opens in RStudio's Viewer or a browser; elsewhere, as when a document is knitted, its title and
#' layers are printed.
#'
#' @param x An [rb_map()].
#' @param ... Not used.
#' @return `x`, invisibly.
#' @export
print.rb_map <- function(x, ...) {
  if (rlang::is_interactive()) {
    rb_map_serve(x, port = .rb_or(attr(x, "port"), 8790L))
  } else {
    cat(format(x), sep = "\n")
  }
  invisible(x)
}

#' @rdname print.rb_map
#' @export
format.rb_map <- function(x, ...) {
  layers <- vapply(
    x$config$layers,
    function(l) sprintf("%s (%s)", l$id, l$kind),
    ""
  )
  c(
    sprintf("<rb_map> %s", .rb_or(x$config$title, "")),
    sprintf("  layers: %s", paste(layers, collapse = ", "))
  )
}

.rb_or <- function(a, b) if (is.null(a)) b else a

# The map piped in, if any, and the pings and settings of the layer being added
.rb_quick <- function(
  x,
  data,
  lon,
  lat,
  time,
  by_q,
  title,
  port,
  choose_q = rlang::quo(NULL)
) {
  state <- if (inherits(x, "rb_map")) attr(x, "quick")
  if (inherits(x, "rb_map") && is.null(state)) {
    stop(
      "This map was not made by rb_map_pings() or rb_map_track(); give its layers to rb_map().",
      call. = FALSE
    )
  }
  base <- state$ctx
  pings <- if (!is.null(data)) {
    data
  } else if (is.null(state)) {
    x
  }
  ctx <- if (is.null(pings)) {
    if (
      !rlang::quo_is_null(by_q) ||
        !is.null(lon) ||
        !is.null(lat) ||
        !is.null(time)
    ) {
      stop(
        "`lon`, `lat`, `time` and `by` are the map's; give `data` to set them for other pings.",
        call. = FALSE
      )
    }
    base
  } else {
    .rb_quick_data(pings, lon, lat, time, by_q, base)
  }
  if (is.null(state)) {
    state <- list(
      ctx = ctx,
      layers = list(),
      tables = list(),
      boxes = list(),
      title = title,
      port = as.integer(port)
    )
  }
  # the map's choices: the groups of the pings that start it, and the columns `choose` names
  wanted <- unique(c(
    ctx$groups,
    if (!rlang::quo_is_null(choose_q)) .rb_track_cols(choose_q, ctx$cols)
  ))
  if (length(state$layers) == 0) {
    .rb_quick_need(ctx, wanted)
    state[["choose_fields"]] <- wanted
    state[["choose"]] <- .rb_quick_choose(ctx, wanted)
  } else if (length(setdiff(wanted, state[["choose_fields"]]))) {
    warning(
      "The map's choices are set by the call that starts it; ",
      paste(setdiff(wanted, state[["choose_fields"]]), collapse = ", "),
      " is not added.",
      call. = FALSE
    )
  }
  list(state = state, ctx_layer = ctx)
}

# The values of each field to choose by, with the pings' count and extent of each, and the extent of each combination
# that occurs where there are several fields. A number or a logical is matched as it is, any other type as the text
# the tiles carry it as
.rb_quick_choose <- function(ctx, fields) {
  if (!length(fields)) {
    return(NULL)
  }
  q <- function(v) as.character(DBI::dbQuoteIdentifier(ctx$con, v))
  as_is <- vapply(
    fields,
    function(f) {
      v <- dplyr::collect(utils::head(
        dplyr::select(ctx$x, dplyr::all_of(f)),
        1
      ))[[1]]
      is.numeric(v) || is.logical(v)
    },
    logical(1)
  )
  value_sql <- function(i) {
    if (as_is[i]) q(fields[i]) else sprintf("CAST(%s AS VARCHAR)", q(fields[i]))
  }
  box_sql <- sprintf(
    "count(*) AS n, min(%1$s) AS lon0, min(%2$s) AS lat0, max(%1$s) AS lon1, max(%2$s) AS lat1",
    q(ctx$lon),
    q(ctx$lat)
  )
  where <- sprintf("%s IS NOT NULL AND %s IS NOT NULL", q(ctx$lon), q(ctx$lat))
  key <- function(v, i) {
    if (is.na(v)) {
      "<NA>"
    } else if (is.numeric(v)) {
      format(v, scientific = FALSE, trim = TRUE, digits = 15)
    } else {
      as.character(v)
    }
  }
  box <- function(r, j) c(r$lon0[j], r$lat0[j], r$lon1[j], r$lat1[j])
  out <- list(
    fields = lapply(seq_along(fields), function(i) {
      r <- DBI::dbGetQuery(
        ctx$con,
        sprintf(
          "SELECT %s AS v, %s FROM (%s) WHERE %s GROUP BY 1 ORDER BY v NULLS LAST",
          value_sql(i),
          box_sql,
          ctx$src,
          where
        )
      )
      list(
        field = fields[i],
        label = fields[i],
        multi = nrow(r) <= 12, # few values: chips that tick on and off, so several show at once
        options = lapply(seq_len(nrow(r)), function(j) {
          v <- r$v[j]
          list(
            value = key(v, i),
            label = sprintf(
              "%s (%s)",
              if (is.na(v)) "none" else key(v, i),
              format(r$n[j], big.mark = ",", trim = TRUE)
            ),
            match = if (is.na(v)) NULL else v,
            bbox = box(r, j)
          )
        })
      )
    })
  )
  if (length(fields) > 1) {
    r <- DBI::dbGetQuery(
      ctx$con,
      sprintf(
        "SELECT %s, %s FROM (%s) WHERE %s GROUP BY ALL",
        paste(
          vapply(
            seq_along(fields),
            function(i) sprintf("%s AS v%d", value_sql(i), i),
            ""
          ),
          collapse = ", "
        ),
        box_sql,
        ctx$src,
        where
      )
    )
    out$extents <- lapply(seq_len(nrow(r)), function(j) {
      list(
        key = paste(
          vapply(
            seq_along(fields),
            function(i) key(r[[paste0("v", i)]][j], i),
            ""
          ),
          collapse = intToUtf8(31)
        ),
        bbox = box(r, j)
      )
    })
  }
  out
}

# Pings read for a layer: the lazy table, its columns, the settings (from the arguments, else the map's, else the
# usual names) and its bounds
.rb_quick_data <- function(pings, lon, lat, time, by_q, base) {
  if (inherits(pings, "sf")) {
    xy <- sf::st_coordinates(pings)
    pings <- sf::st_drop_geometry(pings)
    pings[[.rb_or(lon, "lon")]] <- xy[, 1]
    pings[[.rb_or(lat, "lat")]] <- xy[, 2]
  }
  groups <- dplyr::group_vars(pings)
  con <- .rb_con_for(pings)
  x <- dplyr::ungroup(.rb_as_lazy(pings, con))
  cols <- colnames(x)
  pick <- function(given, from_map, usual) {
    if (!is.null(given)) {
      given
    } else if (!is.null(from_map) && from_map %in% cols) {
      from_map
    } else {
      usual
    }
  }
  lon <- pick(lon, base$lon, "lon")
  lat <- pick(lat, base$lat, "lat")
  time <- pick(time, base$time, "time")
  if (!time %in% cols) {
    time <- NULL
  }
  by <- if (rlang::quo_is_null(by_q)) {
    if (!is.null(base$by) && base$by %in% cols) {
      base$by
    } else if ("vid" %in% cols) {
      "vid"
    }
  } else {
    rlang::as_name(by_q)
  }
  ctx <- list(
    x = x,
    con = con,
    groups = groups,
    src = .rb_sql(x),
    cols = cols,
    lon = lon,
    lat = lat,
    time = time,
    by = by
  )
  .rb_quick_need(ctx, c(lon, lat, by))
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  box <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT count(*) AS n, min(%1$s) AS lon0, max(%1$s) AS lon1, min(%2$s) AS lat0, max(%2$s) AS lat1
       FROM (%3$s) WHERE %1$s IS NOT NULL AND %2$s IS NOT NULL",
      q(lon),
      q(lat),
      ctx$src
    )
  )
  if (box$n == 0) {
    stop("No ping has a position.", call. = FALSE)
  }
  ctx$box <- box
  ctx
}

.rb_quick_need <- function(ctx, need) {
  missing <- setdiff(unique(need), ctx$cols)
  if (length(missing)) {
    stop(
      sprintf(
        "No column %s in the data; it has %s.",
        paste(missing, collapse = ", "),
        paste(ctx$cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# A layer's id: the kind's name, numbered from the second of its kind on the map
.rb_quick_id <- function(state, kind) {
  ids <- vapply(state$layers, function(l) l$config$id, "")
  n <- sum(ids == kind | startsWith(ids, paste0(kind, "_")))
  if (n == 0) kind else paste0(kind, "_", n + 1)
}

# A layer's name, numbered only where an earlier layer has the same
.rb_quick_label <- function(state, label) {
  taken <- vapply(state$layers, function(l) .rb_or(l$config$label, ""), "")
  if (!label %in% taken) {
    return(label)
  }
  n <- 2
  while (paste(label, n) %in% taken) {
    n <- n + 1
  }
  paste(label, n)
}

# A column's colours and legend: a colour per value (the 12 most frequent; the rest grey), matched on the text
# property `prop`, or a continuous palette over its 1st to 99th percentile, read from the property `get`
.rb_quick_colours <- function(
  ctx,
  cc,
  discrete,
  prop,
  palette = "Inferno",
  get = cc
) {
  grey <- "#808080"
  q <- function(v) as.character(DBI::dbQuoteIdentifier(ctx$con, v))
  if (discrete) {
    vals <- DBI::dbGetQuery(
      ctx$con,
      sprintf(
        "SELECT CAST(%1$s AS VARCHAR) AS v, count(*) AS n FROM (%2$s) WHERE %1$s IS NOT NULL GROUP BY 1 ORDER BY n DESC, v",
        q(cc),
        ctx$src
      )
    )
    k <- min(nrow(vals), 12L)
    pal <- grDevices::hcl.colors(max(k, 2L), "Dynamic")[seq_len(k)]
    legend <- lapply(seq_len(k), function(j) {
      list(swatch = list(dot = pal[j]), label = vals$v[j])
    })
    if (nrow(vals) > k) {
      legend <- c(
        legend,
        list(list(swatch = list(dot = grey), label = "other"))
      )
    }
    return(list(
      colour = c(
        list("match", list("get", prop)),
        unlist(
          lapply(seq_len(k), function(j) list(vals$v[j], pal[j])),
          recursive = FALSE
        ),
        list(grey)
      ),
      legend = legend
    ))
  }
  qs <- DBI::dbGetQuery(
    ctx$con,
    sprintf(
      "SELECT quantile_cont(%1$s, 0.01) AS lo, quantile_cont(%1$s, 0.99) AS hi FROM (%2$s) WHERE %1$s IS NOT NULL",
      q(cc),
      ctx$src
    )
  )
  pal <- grDevices::hcl.colors(9, palette)
  lo <- qs$lo
  hi <- if (isTRUE(qs$hi > qs$lo)) qs$hi else lo + 1
  stops <- seq(lo, hi, length.out = 9)
  fmt <- function(v) format(signif(v, 3), big.mark = ",", trim = TRUE)
  list(
    colour = list(
      "case",
      list("has", get),
      c(
        list("interpolate", list("linear"), list("get", get)),
        unlist(
          lapply(1:9, function(j) list(stops[j], pal[j])),
          recursive = FALSE
        )
      ),
      grey
    ),
    legend = lapply(c(1, 3, 5, 7, 9), function(j) {
      list(
        swatch = list(dot = pal[j]),
        # less than or equal, greater than or equal: an escape would be rewritten by the formatter
        label = paste0(
          if (j == 1) {
            paste0(intToUtf8(8804), " ")
          } else if (j == 9) {
            paste0(intToUtf8(8805), " ")
          } else {
            ""
          },
          fmt(stops[j])
        )
      )
    })
  )
}

# The map with the layer added over the others: the first view covers every layer's pings, on a dark base map
.rb_quick_map <- function(s, layer, table) {
  state <- s$state
  state$layer_fields[[layer$config$id]] <- intersect(
    .rb_or(state[["choose_fields"]], character()),
    s$ctx_layer$cols
  )
  state$layers <- c(state$layers, list(layer))
  state$tables <- c(state$tables, list(table))
  state$boxes <- c(state$boxes, list(s$ctx_layer$box))
  lon0 <- min(vapply(state$boxes, `[[`, 1, "lon0"))
  lon1 <- max(vapply(state$boxes, `[[`, 1, "lon1"))
  lat0 <- min(vapply(state$boxes, `[[`, 1, "lat0"))
  lat1 <- max(vapply(state$boxes, `[[`, 1, "lat1"))
  span <- max(lon1 - lon0, 2 * (lat1 - lat0)) # a degree of latitude draws taller
  bases <- rb_map_basemaps()
  ids <- vapply(bases, `[[`, "", "id")
  m <- rb_map(
    rb_map_group("track", "Track", state$layers),
    title = state$title,
    view = list(
      center = c((lon0 + lon1) / 2, (lat0 + lat1) / 2),
      zoom = if (span > 0) max(1, min(15, floor(log2(360 / span)) - 1)) else 12
    ),
    choose = if (!is.null(state[["choose"]])) {
      cfg <- state[["choose"]]
      cfg$fields <- lapply(cfg$fields, function(f) {
        f$layers <- names(Filter(
          function(fl) f$field %in% fl,
          state$layer_fields
        ))
        f
      })
      cfg
    },
    basemaps = bases[c(which(ids == "dark"), which(ids != "dark"))]
  )
  attr(m, "quick") <- state
  attr(m, "tables") <- state$tables
  attr(m, "port") <- state$port
  m
}

# The columns `col` names: a name, a string, c() of them, or a variable holding strings
.rb_track_cols <- function(q, cols) {
  e <- rlang::quo_get_expr(q)
  if (rlang::is_call(e, "c")) {
    return(vapply(
      rlang::call_args(e),
      function(a) if (is.character(a)) a else rlang::as_name(a),
      ""
    ))
  }
  if (is.character(e)) {
    return(e)
  }
  if (rlang::is_symbol(e)) {
    # a column, else a variable holding names, else the name as given, for the column check to report
    nm <- rlang::as_name(e)
    if (nm %in% cols) {
      return(nm)
    }
    v <- tryCatch(rlang::eval_tidy(q), error = function(err) NULL)
    return(if (is.character(v)) v else nm)
  }
  v <- rlang::eval_tidy(q)
  if (is.character(v)) {
    return(v)
  }
  stop(
    "`col` is a column, by name or as a string, or c() of them.",
    call. = FALSE
  )
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
