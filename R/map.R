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
#   /tiles/<id>/{z}/{x}/{y}.png      a dem or contour layer's tile, an image of elevations (.webp for a WebP one)
#   /data/<id>.geojson               a geojson layer's data
#   /fit/<id>?bbox=w,s,e,n           a dem layer's floor and cap for the values in a box, from its colour's `fit`
#   /time?v=<vessel>&span=<span>     the time panel's series of one vessel's span (rb_map_time()), as JSON; the vessel
#                                    and the span are checked against the panel's before R sees them
#   /notes                           with rb_map_serve(notes = ): the notes (GET), a note or a reply added (POST),
#   /notes/<id>                      a note deleted (DELETE), each answered with the notes; only for a page opened
#                                    from this computer, a write only from the page itself
#   /verdicts                        with rb_map_serve(verdicts = ): the features' current verdicts (GET), a verdict
#                                    given or withdrawn (POST), answered with them; as the notes, from this computer
#                                    and the page only

.rb_map_engine <- 5L # the page's ENGINE: changed together
.rb_maps <- new.env(parent = emptyenv()) # the servers rb_map_serve() started, by port
.rb_tiles <- new.env(parent = emptyenv())
.rb_tiles$served <- list() # by port, the temporary tables (connection and name) of the map served there

#' A layer of a map
#'
#' One layer of an [rb_map()]: what it shows, how it is drawn, and where its data come from. Five kinds:
#'
#' * `"grid"`: vector tiles of cells, coloured in the page by a numeric field on a log scale, with the palette,
#'   stretch, floor, cap and histogram there too. Its `filters` become controls whose values go into the tile address
#'   (one grid per map).
#' * `"vector"`: vector tiles of features, drawn by `style`, such as a table of geometries served by
#'   [rb_map_tiles_table()].
#' * `"geojson"`: features sent whole as GeoJSON, drawn by `style`.
#' * `"dem"`: image tiles of elevations (Terrarium or Mapbox Terrain-RGB), such as an MBTiles file read by
#'   [rb_map_tiles_mbtiles()], drawn by `style` in the page under the base map's labels: MapLibre's `hillshade` and
#'   `color-relief` layers, so the shading and the colours are made in the browser.
#' * `"contour"`: contour lines made in the browser from the same image tiles of elevations, by `maplibre-contour`
#'   (loaded by the page only for a map that has one), at the intervals of `contours`, drawn by `style` (a `line`, and
#'   a `symbol` for the labels, from the source layer `"contours"`, its features' `ele` and `level`) under the base
#'   map's labels.
#'
#' Texts (`label`, `info`, a filter's `label`, a legend's `label`, ...) are keys of the map's `texts` ([rb_map()]), or
#' the text itself where no key matches. MapLibre styles and expressions are lists, so that a one-element array stays
#' an array in JSON: `list("get", "kind")`, `list("zoom")`; read from JSON with `jsonlite::fromJSON(simplifyVector =
#' FALSE)` they already are.
#'
#' @param id The layer's name, lower case: its checkbox, its source and its key in the page's address.
#' @param kind `"grid"`, `"vector"`, `"geojson"`, `"dem"` or `"contour"`.
#' @param label,info Its name in the sidebar and the text behind its ⓘ.
#' @param visible Whether it is on when the page opens; the address turns it off with `id=0` or on with `id=1`.
#' @param tiles The tile function. For a grid, `function(z, x, y, params)`, `params` a named list of the filters'
#'   values as text (absent when not given); for a vector layer, `function(z, x, y)`. It returns a Mapbox vector tile
#'   as raw bytes (DuckDB's `ST_AsMVT()`), or `raw(0)`. A dem layer's, `function(z, x, y)`, returns an image of
#'   elevations (PNG or WebP) as raw bytes, or `NULL` where there is none; a contour layer's the same.
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
#'   is one; and `format`, `"value"` or `"count"`), `maxWidth`. For a grid, `fields` only, the first in bold. Opt-in,
#'   a label on hover: `hover`, the style ids whose features show their `title` in a light label beside the pointer
#'   while it is over them (`TRUE` for those of `click`), and `hover_fields`, `TRUE` to show the `fields` there too.
#'   Over features of several layers, the topmost speaks, as for a click; a click hides the label until the pointer is
#'   over another feature. Opt-in, a verdict on each feature, kept by a server started with `rb_map_serve(verdicts = )`:
#'   `verdict`, a list of `field` (the field naming a feature: a verdict belongs to its value), `questions` (one to
#'   four, each an `id`, a `label`, two to six `options` and optionally `skip`), and `text` (`TRUE` for a note on the
#'   feature, in the reader's words). An option is a `value` (letters, digits, `_` or `-`), a `label` (a text or a key
#'   of the map's `texts`), optionally a `colour` for the feature's mark (the first green by default, the others red),
#'   and `other = TRUE` for an answer that names another feature: a box for its name, the names in view offered.
#'   `skip` names earlier questions and the answers that leave this one out (`list(name = "not")`). One question may be
#'   given as `options` alone. The click's popup then holds the questions, a button for each answer; each answer is
#'   written to the server's file with the feature's place, and a mark drawn on the feature (the first question's
#'   colour its fill, the second's its ring). A later answer replaces the earlier; [rb_map_verdicts()] reads them.
#' @param legend A list of swatches, each `swatch` (`fill` and `stroke`, or `dot`) and `label`.
#' @param variants For a vector, geojson or dem layer, styles to choose between in the page, such as the column the
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
#' @param encoding,tile_size,bounds For a dem or contour layer: how its images hold the elevation (`"terrarium"` or `"mapbox"`),
#'   their side in pixels, and the extent that has tiles (west, south, east, north), so the page asks for no other.
#'   Taken, with `minzoom` and `maxzoom`, from the attributes of `tiles` where not given ([rb_map_tiles_mbtiles()]
#'   sets them); else `"terrarium"` and 512. The image format (`"png"` or `"webp"`) is the `format` attribute's.
#' @param shade For a dem layer with a `hillshade` style: the relief's controls in the page, and their values when it
#'   opens, a list of any of `method` (`"standard"`, `"basic"`, `"combined"`, `"igor"` or `"multidirectional"`, three
#'   lights 45 degrees apart), `direction` (the light's azimuth, 0-359), `altitude` (0-90), `exaggeration` (the
#'   intensity, 0-1), `opacity` (0-100: the alpha of the shading's colours, which has no opacity of its own),
#'   `shadow` and `highlight` (colours as `"#rrggbb"`); MapLibre's own defaults for those not given. `TRUE` takes
#'   every default. Each control applies as it moves, and goes into the page's address as `<id>_method`, `<id>_dir`,
#'   `<id>_alt`, `<id>_ex`, `<id>_op`, `<id>_shadow` and `<id>_light`. With `fit`, a function of the view's box as a
#'   colour's `fit` returning a list with `exaggeration` (0-1), the intensity can follow the view (`auto`, `TRUE` by
#'   default; `<id>_auto` in the address, which leaves out `<id>_ex` while it follows), until it is set by hand.
#' @param colour For a dem layer with a `color-relief` style: its values in colour, with their controls in the page, a
#'   list of any of `of` (`"depth"`, the elevation's negative, or `"elevation"`; what is 0 or less is clear), `palette`
#'   (an id of the map's palettes; the map's own when not given), `floor` and `cap` (the values the palette's ends
#'   stand at, 1-10,000), `scale` (`"log"` or `"linear"`), `mode` (`"ramp"` or `"bands"`), `bands` (the bands' breaks,
#'   rising), `cuts` (the cuts offered, 0 for none) and `cut` (one of them: values under it are drawn as `cut_as`,
#'   `"clear"` or `"grey"`), `opacity` (0-100). Defaults: depth, floor 10, cap 2,000, log, a ramp, bands at 10, 20, 50,
#'   100, 200, 500, 1,000 and 2,000, cuts 0, 10, 20 and 50, no cut, clear, 100. `TRUE` takes them all. Each control
#'   applies as it moves, and goes into the page's address as `<id>_pal`, `<id>_lo`, `<id>_hi`, `<id>_scale`,
#'   `<id>_mode`, `<id>_cut`, `<id>_cutas` and `<id>_cop`. With `fit`, a function of the view's `west`, `south`, `east`
#'   and `north` returning the floor and the cap for the values there (or `NULL` where there are none; or a list of
#'   `floor`, `cap` and `quantiles`, the values' quantiles rising, which also offers `scale` `"equal"`, each colour an
#'   equal share of the values in view between the floor and the cap), the page has a
#'   "Fit to view" button, answered by the server at `/fit/<id>?bbox=w,s,e,n`, and follows the view: it fits again each
#'   time the map settles, until a floor or cap is set by hand (`auto`, `TRUE` by default; `<id>_auto` in the address,
#'   where the floor and cap are left out while it follows).
#' @param contours For a contour layer: `thresholds`, a named list by zoom of the minor and the major interval from that
#'   zoom on (`list("6" = c(500, 1000), "9" = c(50, 200))`; a major line has `level` 1), and `multiplier`, by which the
#'   elevations are multiplied before the lines are drawn (-1 for depths, 1 by default). With `fit`, a function as a
#'   colour's `fit` returning a list with the `quantiles` of the values in view, the lines can follow the view (`auto`,
#'   `TRUE` by default; `<id>_auto` in the address): about a dozen levels at round values near the view's quantiles,
#'   so the lines fall where the area is, drawn at a step of 1, 2 or 5 times a power of ten that makes some 40 of the
#'   view's range; unticked, the `thresholds` and the style's filters.
#' @param state For a vector, geojson, dem or contour layer: values in the reader's hands that its `style` reads, in
#'   a filter, a paint or a layout property, as `list("global-state", "<id>_<name>")` (MapLibre's global state), such
#'   as whether a style is drawn, its colour, or a shift of the zoom it starts at. A list of controls, each with `id`
#'   (`<name>` above: lower-case letters and digits) and `label`, and either `options` (a list of `value`, a single
#'   number, text or `TRUE`/`FALSE`, and `label`), a list to choose from, with `default` one of the values (the first
#'   when not given); or no `options` and `default` `TRUE` or `FALSE`, a checkbox. Each applies as it changes, nothing
#'   fetched again, and goes into the page's address as `<id>_<name>`. Every one must be read by the style.
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
  kind = c("grid", "vector", "geojson", "dem", "contour"),
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
  opacity = NULL,
  encoding = NULL,
  tile_size = NULL,
  bounds = NULL,
  shade = NULL,
  colour = NULL,
  contours = NULL,
  state = NULL
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
  if (kind %in% c("grid", "vector", "dem", "contour") && !is.function(tiles)) {
    stop(
      sprintf(
        "A %s layer needs `tiles`, a function returning %s.",
        kind,
        if (kind %in% c("dem", "contour")) "an image tile" else "a vector tile"
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
        "`variants` are for a vector, geojson or dem layer; a grid is coloured by its `metrics`.",
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
  if (!is.null(popup[["hover"]]) || !is.null(popup[["hover_fields"]])) {
    popup <- .rb_map_hover(popup, kind, style)
  }
  if (!is.null(popup[["verdict"]])) {
    popup <- .rb_map_verdict_spec(popup, kind)
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
  format <- NULL
  if (kind %in% c("dem", "contour")) {
    given_or_attr <- function(given, name, usual = NULL) {
      if (!is.null(given)) {
        given
      } else if (!is.null(attr(tiles, name))) {
        attr(tiles, name)
      } else {
        usual
      }
    }
    encoding <- given_or_attr(encoding, "encoding", "terrarium")
    format <- given_or_attr(NULL, "format", "png")
    if (!encoding %in% c("terrarium", "mapbox") || !format %in% c("png", "webp")) {
      stop(
        "A dem or contour layer's `encoding` is \"terrarium\" or \"mapbox\", its tiles' format \"png\" or \"webp\".",
        call. = FALSE
      )
    }
    tile_size <- as.integer(given_or_attr(tile_size, "tile_size", 512L))
    bounds <- given_or_attr(bounds, "bounds")
    if (!is.null(bounds)) {
      bounds <- as.list(as.numeric(bounds))
    }
    minzoom <- given_or_attr(minzoom, "minzoom")
    maxzoom <- given_or_attr(maxzoom, "maxzoom")
  } else if (!is.null(encoding) || !is.null(tile_size) || !is.null(bounds)) {
    stop("`encoding`, `tile_size` and `bounds` are for a dem or contour layer.", call. = FALSE)
  }
  fit <- NULL # a colour's or the contours' fit: kept on the server, the page told only that there is one
  if (kind == "contour") {
    if (is.list(contours) && !is.null(contours$fit)) {
      if (!is.function(contours$fit)) {
        stop("`contours$fit` is a function of west, south, east and north.", call. = FALSE)
      }
      fit <- contours$fit
      contours$fit <- NULL
    }
    contours <- .rb_map_contours(contours)
    if (!is.null(fit)) {
      contours$fit <- TRUE
    }
  } else if (!is.null(contours)) {
    stop("`contours` is for a contour layer.", call. = FALSE)
  }
  if (!is.null(shade)) {
    if (is.list(shade) && !is.null(shade$fit)) {
      if (!is.function(shade$fit)) {
        stop("`shade$fit` is a function of west, south, east and north.", call. = FALSE)
      }
      fit <- shade$fit
      shade$fit <- NULL
    }
    shade <- .rb_map_shade(shade, kind, style)
    if (!is.null(fit)) {
      shade$fit <- TRUE
    }
  }
  if (!is.null(colour)) {
    if (is.list(colour) && !is.null(colour$fit)) {
      if (!is.function(colour$fit)) {
        stop("`colour$fit` is a function of west, south, east and north.", call. = FALSE)
      }
      if (!is.null(fit)) {
        stop("A layer has one fit: give the colour's and the shading's to two layers on the same tiles.", call. = FALSE)
      }
      fit <- colour$fit
      colour$fit <- NULL
    }
    colour <- .rb_map_colour(colour, kind, style)
    if (!is.null(fit)) {
      colour$fit <- TRUE
    } else if (identical(colour$scale, "equal")) {
      stop("`colour$scale` \"equal\" needs `colour$fit`, which gives the values' quantiles in view.", call. = FALSE)
    }
  }
  if (!is.null(state)) {
    state <- .rb_map_state(state, id, kind, style)
  }
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
    opacity = opacity,
    encoding = encoding,
    tile_size = tile_size,
    bounds = bounds,
    format = format,
    shade = shade,
    colour = colour,
    contours = contours,
    state = state
  )
  structure(
    list(
      config = Filter(Negate(is.null), config),
      tiles = tiles,
      meta = meta,
      data = data,
      fit = fit
    ),
    class = "rb_map_layer"
  )
}

# A popup's verdict, checked: a field naming the feature; one to four questions, each an id, a label, two to six
# options (a value, a label, a colour, and `other` for an answer that names another feature) and the answers of
# earlier questions that skip it; and whether a note goes with the feature. The single question of 2026.11.09
# (`options` with no `questions`) is the question "verdict". Lists stay arrays in the config's JSON
.rb_map_verdict_spec <- function(popup, kind) {
  if (!kind %in% c("vector", "geojson")) {
    stop("`popup$verdict` is for a vector or geojson layer.", call. = FALSE)
  }
  if (is.null(popup[["click"]])) {
    stop("`popup$verdict` is given in a click's popup: give `popup$click`.", call. = FALSE)
  }
  v <- popup[["verdict"]]
  if (!is.list(v) || !is.character(v$field) || length(v$field) != 1 || !nzchar(v$field)) {
    stop("`popup$verdict$field` is the field that names a feature.", call. = FALSE)
  }
  if (!is.null(v$questions) && !is.null(v$options)) {
    stop("`popup$verdict` has `options` (one question) or `questions`, not both.", call. = FALSE)
  }
  qs <- if (is.null(v$questions)) list(list(id = "verdict", options = v$options)) else v$questions
  if (!is.list(qs) || length(qs) < 1 || length(qs) > 4) {
    stop("`popup$verdict$questions` is one to four questions, each a list of `id`, `label` and `options`.", call. = FALSE)
  }
  ok_id <- function(x) is.character(x) && length(x) == 1 && grepl("^[A-Za-z0-9_-]{1,40}$", x)
  seen <- list()
  qs <- lapply(qs, function(q) {
    if (!is.list(q) || !ok_id(q$id) || q$id == "note") {
      stop("A verdict's question has an `id`: 1-40 letters, digits, `_` or `-`, not \"note\" (the note's).", call. = FALSE)
    }
    if (q$id %in% names(seen)) {
      stop("A verdict's questions have ids of their own.", call. = FALSE)
    }
    if (!is.null(q$label) && !(is.character(q$label) && length(q$label) == 1)) {
      stop("A verdict's question's `label` is one text (or a key of the map's texts).", call. = FALSE)
    }
    o <- q$options
    if (!is.list(o) || length(o) < 2 || length(o) > 6) {
      stop("A verdict's question has two to six `options`, each a list of `value` and `label`.", call. = FALSE)
    }
    o <- lapply(o, function(x) {
      if (!is.list(x) || !ok_id(x$value)) {
        stop("A verdict's option has a `value`: 1-40 letters, digits, `_` or `-`.", call. = FALSE)
      }
      if (is.null(x$label)) {
        x$label <- x$value
      }
      if (!is.character(x$label) || length(x$label) != 1) {
        stop("A verdict's option's `label` is one text (or a key of the map's texts).", call. = FALSE)
      }
      if (!is.null(x$colour) && !(is.character(x$colour) && length(x$colour) == 1 && grepl("^#[0-9A-Fa-f]{6}$", x$colour))) {
        stop("A verdict's option's `colour` is one colour, as \"#1a9850\".", call. = FALSE)
      }
      if (!is.null(x$other) && !(is.logical(x$other) && length(x$other) == 1 && !is.na(x$other))) {
        stop("A verdict's option's `other` is TRUE or FALSE: whether the answer names another feature.", call. = FALSE)
      }
      x$other <- isTRUE(x$other)
      x
    })
    values <- vapply(o, `[[`, "", "value")
    if (anyDuplicated(values)) {
      stop("A verdict's options have values of their own.", call. = FALSE)
    }
    skip <- q$skip
    if (!is.null(skip)) {
      if (!is.list(skip) || is.null(names(skip)) || any(!names(skip) %in% names(seen))) {
        stop("A verdict's question's `skip` names earlier questions, each with the answers that skip it.", call. = FALSE)
      }
      for (k in names(skip)) {
        if (!is.character(unlist(skip[[k]])) || !all(unlist(skip[[k]]) %in% seen[[k]])) {
          stop(sprintf("A verdict's question's `skip` gives answers of the question \"%s\": %s.", k,
                       paste(seen[[k]], collapse = ", ")), call. = FALSE)
        }
      }
      skip <- lapply(skip, function(x) as.list(unlist(x)))
    }
    seen[[q$id]] <<- values
    Filter(Negate(is.null), list(id = q$id, label = q$label, options = o, skip = skip))
  })
  if (!is.null(v$text) && !(is.logical(v$text) && length(v$text) == 1 && !is.na(v$text))) {
    stop("`popup$verdict$text` is TRUE or FALSE.", call. = FALSE)
  }
  popup[["verdict"]] <- list(field = v$field, questions = qs, text = isTRUE(v$text))
  popup
}

# A popup's hover, checked: style ids of the layer's own (TRUE for the click's), a title to show, hover_fields a flag
.rb_map_hover <- function(popup, kind, style) {
  if (!kind %in% c("vector", "geojson")) {
    stop("`popup$hover` is for a vector or geojson layer.", call. = FALSE)
  }
  if (is.null(popup[["hover"]])) {
    stop("`popup$hover_fields` goes with `popup$hover`, the style ids that show a label on hover.", call. = FALSE)
  }
  if (isTRUE(popup[["hover"]])) {
    if (is.null(popup[["click"]])) {
      stop("`popup$hover = TRUE` takes the style ids of `popup$click`: give `click`, or the ids to `hover`.", call. = FALSE)
    }
    popup[["hover"]] <- popup[["click"]]
  }
  ids <- unlist(popup[["hover"]])
  style_ids <- vapply(style, function(s) if (is.null(s$id)) "" else s$id, "")
  if (!is.character(ids) || !length(ids) || anyNA(ids) || !all(ids %in% style_ids)) {
    stop(
      "`popup$hover` is some of the layer's style ids (or TRUE for those of `click`): their features show a label on hover.",
      call. = FALSE
    )
  }
  if (!is.character(popup[["title"]]) || length(popup[["title"]]) != 1) {
    stop("`popup$hover` shows a feature's `title`: give `popup$title`, a field.", call. = FALSE)
  }
  hf <- popup[["hover_fields"]]
  if (!is.null(hf) && !(is.logical(hf) && length(hf) == 1 && !is.na(hf))) {
    stop("`popup$hover_fields` is TRUE or FALSE.", call. = FALSE)
  }
  popup[["hover"]] <- as.list(unique(ids))
  popup
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
#' Five whole maps on the example data are scripts in `system.file("examples", "map", package = "ramb")`: the Danish
#' pings by cell (`dansk_map.R`), the same with a cell size, a sample and the interpolation to choose in the page
#' (`dansk_switches_map.R`), the Danish pings with a time panel under the map (`dansk_time_map.R`, [rb_map_time()]),
#' the Italian tracks' voyages by two rules (`cnr_map.R`), and iapesca's example vessel (`iapesca_map.R`, which needs
#' iapesca).
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
  # a layer's state goes into the address and MapLibre's global state as <id>_<name>
  state_keys <- unlist(lapply(layers, function(l) {
    if (length(l$config$state)) paste0(l$config$id, "_", vapply(l$config$state, `[[`, "", "id"))
  }))
  if (length(clash <- intersect(state_keys, ids))) {
    stop(sprintf("A layer is named as another layer's state, %s: rename one.", paste(clash, collapse = ", ")),
         call. = FALSE)
  }
  if (!is.null(palette) && !palette %in% vapply(palettes, `[[`, "", "id")) {
    stop("`palette` is an id of `palettes`.", call. = FALSE)
  }
  for (l in layers) {
    p <- l$config$colour$palette
    if (!is.null(p) && !p %in% vapply(palettes, `[[`, "", "id")) {
      stop(sprintf("Layer %s: its colour's `palette` is an id of `palettes`.", l$config$id), call. = FALSE)
    }
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
      state_keys,
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
      })),
      unlist(lapply(layers, function(l) {
        c(
          if (!is.null(l$config$shade)) {
            paste0(l$config$id, "_", c("method", "dir", "alt", "ex", "op", "shadow", "light", "auto"))
          },
          if (!is.null(l$config$colour)) {
            paste0(l$config$id, "_", c("pal", "lo", "hi", "scale", "mode", "cut", "cutas", "cop", "auto"))
          },
          if (isTRUE(l$config$contours$fit)) {
            paste0(l$config$id, "_auto")
          }
        )
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
#' @param notes Opt-in, notes pinned on the map: the path of a `.geojson` file (in a folder that exists) where the
#'   server keeps them, made at the first note; `NULL`, none. The page then has a Notes group: "Add a note", a click
#'   on the map, a text, and the note is written to the file with the features under it; a reply joins a note's
#'   thread. [rb_map_notes()] reads them in R and [rb_map_notes_add()] answers. They are answered only for a page
#'   opened from this computer (as `localhost`, `127.0.0.1` or `host`), and never in a bundle.
#' @param verdicts Opt-in, the readers' verdicts on features: the path of a `.csv` file (in a folder that exists) where
#'   the server keeps them, made at the first verdict; `NULL`, none. A layer whose popup has a `verdict`
#'   ([rb_map_layer()]) then shows a button for each option in a feature's popup. Each answer is a row added to the
#'   file (when, the layer, the feature's value of the verdict's `field`, the option, the reader's line, the feature's
#'   title and place); a later one for the feature replaces it, and an empty answer withdraws it. [rb_map_verdicts()]
#'   reads them. As the notes, answered only for a page opened from this computer, and never in a bundle.
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
#'   # with notes pinned on the map, kept in a file:
#'   # rb_map_serve(m, notes = "harbour_notes.geojson"); rb_map_notes("harbour_notes.geojson")
#'   # rb_map_stop() # when done
#' }
rb_map_serve <- function(
  map,
  port = 8790L,
  host = "127.0.0.1",
  open = interactive(),
  query = NULL,
  notes = NULL,
  verdicts = NULL
) {
  .rb_need("httpuv")
  .rb_need("jsonlite")
  if (!inherits(map, "rb_map")) {
    stop("`map` is an rb_map().", call. = FALSE)
  }
  if (!is.null(notes)) {
    notes <- .rb_notes_path(notes)
    .rb_notes_utf8()
    .rb_notes_read(notes) # a file that is there must be notes: it is never overwritten otherwise
  }
  if (!is.null(verdicts)) {
    if (!length(.rb_map_verdict_layers(map))) {
      stop("`verdicts` serves the verdicts of a layer whose popup has a `verdict`: the map has none.", call. = FALSE)
    }
    verdicts <- .rb_verdicts_path(verdicts)
    .rb_notes_utf8()
    .rb_verdicts_read(verdicts) # a file that is there must be verdicts: it is never overwritten otherwise
  }
  port <- as.integer(port)
  rb_map_stop(port)
  app <- .rb_map_app(map, notes = notes, host = host, verdicts = verdicts)
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

#' Notes pinned on a served map
#'
#' A map served with `rb_map_serve(notes = )` takes notes: in its Notes group the reader presses "Add a note", clicks
#' the map and writes, and the server writes the note to the file, a GeoJSON FeatureCollection with a point for each
#' note. A note keeps its `id`, its `time` (UTC), its `author` (`"reader"` from the page), its `text`, the map's `zoom`
#' and the page's `address` (its query and position: the view and layers as the reader saw them), and `under`, the
#' features under it: each "layer: title", from the layers whose popup has a `title`. A reply joins a note's thread
#' (`reply_to`, the first note's `id`) at its place. The page shows a pin for each thread, red while its last word is
#' the reader's and blue once answered, with the thread and a box to reply in on a click, and asks for the notes again
#' when it regains the focus and every 30 s.
#'
#' `rb_map_notes()` reads the notes, `rb_map_notes_add()` adds a note or a reply from R: a dialogue about places on a
#' map, between a reader in the page and someone at R (or an assistant reading the file). Both may run while the map is
#' served, in this R session or another: each write replaces the file whole, by a rename. The file is plain GeoJSON, a
#' note a line, and opens in any GIS. Notes are written only by R in a UTF-8 locale, where letters outside ASCII are
#' kept as they are.
#'
#' @param path The notes' file (`.geojson`).
#' @param text The note's text.
#' @param lon,lat Where it is pinned. A reply's place is its thread's.
#' @param author Who wrote it.
#' @param reply_to The `id` of the note it answers; a reply to a reply joins the same thread.
#' @return `rb_map_notes()`: a data frame, a row for each note in the order written: `id`, `thread` (the `id` of its
#'   thread's first note), `reply_to`, `time` (POSIXct, UTC), `author`, `text`, `lon`, `lat`, `zoom`, `under` (the
#'   features under it, separated by "; ") and `address`; no rows when there is no file. `rb_map_notes_add()`: the
#'   new note's `id`, invisibly.
#' @seealso [rb_map_serve()]
#' @export
#' @examples
#' f <- tempfile(fileext = ".geojson")
#' rb_map_notes_add(f, "Is the bank's edge here?", lon = -14.6, lat = 64.0, author = "reader")
#' rb_map_notes_add(f, "Yes, at the 200 m contour.", reply_to = 1, author = "analyst")
#' rb_map_notes(f)[, c("id", "thread", "author", "text")]
#' # a map served with them: rb_map_serve(m, notes = f)
rb_map_notes <- function(path) {
  path <- .rb_notes_path(path, folder = FALSE)
  fs <- .rb_notes_read(path)
  p <- lapply(fs, `[[`, "properties")
  chr <- function(k) vapply(p, function(x) if (is.null(x[[k]])) NA_character_ else as.character(x[[k]]), "")
  num <- function(k) vapply(p, function(x) if (is.null(x[[k]])) NA_real_ else as.numeric(x[[k]]), 0)
  xy <- function(i) vapply(fs, function(f) as.numeric(.rb_or(f$geometry$coordinates[[i]], NA)), 0)
  id <- as.integer(num("id"))
  reply_to <- as.integer(num("reply_to"))
  thread <- id
  thread[!is.na(reply_to)] <- reply_to[!is.na(reply_to)]
  data.frame(
    id = id,
    thread = thread,
    reply_to = reply_to,
    time = as.POSIXct(chr("time"), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    author = chr("author"),
    text = chr("text"),
    lon = xy(1),
    lat = xy(2),
    zoom = num("zoom"),
    under = vapply(p, function(x) if (length(x$under)) paste(unlist(x$under), collapse = "; ") else NA_character_, ""),
    address = chr("address"),
    stringsAsFactors = FALSE
  )
}

#' @rdname rb_map_notes
#' @export
rb_map_notes_add <- function(path, text, lon = NULL, lat = NULL, author = "analyst", reply_to = NULL) {
  path <- .rb_notes_path(path)
  .rb_notes_utf8()
  if (!is.character(author) || length(author) != 1 || is.na(author) || !nzchar(trimws(author))) {
    stop("`author` is a name.", call. = FALSE)
  }
  why <- .rb_notes_check(list(text = text, lon = lon, lat = lat, reply_to = reply_to), .rb_notes_read(path))
  if (!is.null(why)) {
    stop(why, call. = FALSE)
  }
  invisible(.rb_notes_add(path, list(text = text, lon = lon, lat = lat, reply_to = reply_to), trimws(author)))
}

# Notes are written only by an R in a UTF-8 locale: in another (C, as Rscript may start), R's strings and jsonlite write
# a letter outside ASCII as "<c3><b3>", which would stay in the file
.rb_notes_utf8 <- function() {
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    stop(
      "Notes are written by R in a UTF-8 locale (this one is ", Sys.getlocale("LC_CTYPE"), "): start R with ",
      "LANG=en_US.UTF-8, or Sys.setlocale(\"LC_ALL\", \"en_US.UTF-8\").",
      call. = FALSE
    )
  }
}

# The notes' file: one path, a .geojson or .json file whose folder exists (when it is to be written)
.rb_notes_path <- function(path, folder = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !grepl("[.](geo)?json$", path, ignore.case = TRUE)) {
    stop("The notes' file is one path to a .geojson file.", call. = FALSE)
  }
  path <- normalizePath(path, mustWork = FALSE)
  if (folder && !dir.exists(dirname(path))) {
    stop(sprintf("The notes' folder %s does not exist.", dirname(path)), call. = FALSE)
  }
  path
}

# The notes as features, none when there is no file; a file that is not a FeatureCollection of numbered notes stops
# whatever would read or write it, so it is never overwritten
.rb_notes_read <- function(path) {
  if (!file.exists(path)) {
    return(list())
  }
  x <- tryCatch(
    jsonlite::fromJSON(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                       simplifyVector = FALSE),
    error = function(e) NULL
  )
  ok <- is.list(x) && identical(x$type, "FeatureCollection") && is.list(x$features) &&
    all(vapply(x$features, function(f) is.numeric(f$properties$id) && length(f$properties$id) == 1, logical(1)))
  if (!ok) {
    stop(sprintf("%s is not a notes file (a GeoJSON FeatureCollection, each feature with an id): left as it is.", path),
         call. = FALSE)
  }
  structure(x$features, last_id = if (is.numeric(x$last_id)) x$last_id)
}

# The highest id given, deleted notes' included: a number is never given to two notes
.rb_notes_last_id <- function(features) {
  max(c(0, attr(features, "last_id"), vapply(features, function(f) as.numeric(f$properties$id), 0)))
}

# Each note on a line of its own, and the last id given (a foreign member, which GeoJSON readers pass over), written to
# a file beside it and renamed over it
.rb_notes_write <- function(path, features, last_id) {
  one <- function(f) as.character(jsonlite::toJSON(f, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
  body <- if (length(features)) paste0("\n", paste(vapply(features, one, ""), collapse = ",\n"), "\n") else ""
  tmp <- paste0(path, ".tmp")
  writeBin(charToRaw(enc2utf8(paste0('{"type": "FeatureCollection", "last_id": ', last_id, ', "features": [', body,
                                     "]}\n"))), tmp)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop(sprintf("The notes could not be written to %s.", path), call. = FALSE)
  }
  invisible(path)
}

# A note's fields checked against the notes there: NULL, or why it is refused
.rb_notes_check <- function(n, features) {
  one_num <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  if (!is.character(n$text) || length(n$text) != 1 || is.na(n$text) || !nzchar(trimws(n$text))) {
    return("A note has a `text`.")
  }
  if (nchar(n$text) > 4000) {
    return("A note's text is at most 4,000 characters.")
  }
  if (!is.null(n$reply_to)) {
    ids <- vapply(features, function(f) as.numeric(f$properties$id), 0)
    if (!one_num(n$reply_to) || !n$reply_to %in% ids) {
      return("`reply_to` is the id of a note there.")
    }
  } else if (!one_num(n$lon) || !one_num(n$lat) || abs(n$lon) > 180 || abs(n$lat) > 90) {
    return("A note has a place, `lon` and `lat` in degrees (a reply takes its thread's).")
  }
  if (!is.null(n$zoom) && !(one_num(n$zoom) && n$zoom >= 0 && n$zoom <= 24)) {
    return("A note's `zoom` is a number from 0 to 24.")
  }
  if (!is.null(n$under) && !(is.list(n$under) || is.character(n$under))) {
    return("A note's `under` is a list of texts.")
  }
  if (!is.null(n$under) && (length(n$under) > 50 ||
                              !all(vapply(n$under, function(u) is.character(u) && length(u) == 1, logical(1))))) {
    return("A note's `under` is at most 50 texts.")
  }
  if (!is.null(n$address) && !(is.character(n$address) && length(n$address) == 1 && nchar(n$address) <= 4000)) {
    return("A note's `address` is a text of at most 4,000 characters.")
  }
  NULL
}

# A note added, checked before: a reply at its thread's first note, which it names; its id, one more than the last
.rb_notes_add <- function(path, n, author) {
  fs <- .rb_notes_read(path)
  ids <- vapply(fs, function(f) as.numeric(f$properties$id), 0)
  reply_to <- NULL
  if (!is.null(n$reply_to)) {
    root <- fs[[match(n$reply_to, ids)]]
    if (!is.null(root$properties$reply_to)) {
      root <- fs[[match(root$properties$reply_to, ids)]]
    }
    reply_to <- as.integer(root$properties$id)
    n$lon <- root$geometry$coordinates[[1]]
    n$lat <- root$geometry$coordinates[[2]]
  }
  id <- as.integer(.rb_notes_last_id(fs) + 1)
  fs[[length(fs) + 1]] <- list(
    type = "Feature",
    geometry = list(type = "Point", coordinates = list(round(n$lon, 6), round(n$lat, 6))),
    properties = list(
      id = id,
      reply_to = reply_to,
      time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      author = author,
      text = trimws(n$text),
      zoom = if (!is.null(n$zoom)) round(n$zoom, 2),
      under = as.list(vapply(n$under, function(u) substr(u, 1, 300), "")),
      address = n$address
    )
  )
  .rb_notes_write(path, fs, id)
  id
}

# A note deleted, and with the first note of a thread its replies
.rb_notes_delete <- function(path, id) {
  fs <- .rb_notes_read(path)
  ids <- vapply(fs, function(f) as.numeric(f$properties$id), 0)
  to <- vapply(fs, function(f) as.numeric(.rb_or(f$properties$reply_to, NA)), 0)
  if (!id %in% ids) {
    return(FALSE)
  }
  .rb_notes_write(path, fs[!(ids == id | to %in% id)], .rb_notes_last_id(fs))
  TRUE
}

# The routes of the notes: GET /notes, POST /notes (a note or a reply), DELETE /notes/<id>, each answered with the notes
# (a new note's id in `added`). Only for a page opened from this computer (its Host, against DNS rebinding); a write
# only from the page itself (its header, which a page elsewhere may not send without the server's leave, and its Origin)
.rb_map_notes_reply <- function(req, notes, hosts, reply, json) {
  refused <- .rb_map_local_only(req, hosts, reply, "the notes", "a note")
  if (!is.null(refused)) {
    return(refused)
  }
  method <- req$REQUEST_METHOD
  answer <- function(added = NULL) {
    force(added) # a note added before the notes are read
    fc <- list(type = "FeatureCollection", features = .rb_notes_read(notes))
    fc$added <- added
    reply(200L, json(fc), "application/geo+json; charset=utf-8", list(`Cache-Control` = "no-store"))
  }
  path <- req$PATH_INFO
  tryCatch(
    {
      if (path == "/notes" && method == "GET") {
        return(answer())
      }
      if (path == "/notes" && method == "POST") {
        if (suppressWarnings(as.numeric(.rb_or(req$CONTENT_LENGTH, "0"))) > 65536) {
          return(reply(413L, "a note is at most 64 kB"))
        }
        b <- req$rook.input$read()
        if (length(b) > 65536) {
          return(reply(413L, "a note is at most 64 kB"))
        }
        b <- rawToChar(b)
        Encoding(b) <- "UTF-8"
        n <- tryCatch(jsonlite::fromJSON(b, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.list(n) || is.null(names(n))) {
          return(reply(400L, "a note is a JSON object"))
        }
        if (is.list(n$under)) {
          n$under <- lapply(n$under, function(u) if (is.character(u)) u else NA)
        }
        why <- .rb_notes_check(n, .rb_notes_read(notes))
        if (!is.null(why)) {
          return(reply(400L, why))
        }
        return(answer(.rb_notes_add(notes, n, "reader")))
      }
      d <- regmatches(path, regexec("^/notes/([0-9]+)$", path))[[1]]
      if (length(d) == 2 && method == "DELETE") {
        if (!.rb_notes_delete(notes, as.numeric(d[2]))) {
          return(reply(404L, "no such note"))
        }
        return(answer())
      }
      reply(405L, "GET /notes, POST /notes or DELETE /notes/<id>")
    },
    error = function(e) reply(500L, sub("\n.*", "", conditionMessage(e)))
  )
}

# A request for what the server keeps (the notes, the verdicts), checked: only from a page opened from this computer
# (its Host, against DNS rebinding), and a write only from the page itself (its header, which a page elsewhere may not
# send without the server's leave, and its Origin). NULL, or the refusal
.rb_map_local_only <- function(req, hosts, reply, what, one) {
  host <- tolower(sub(":[0-9]+$", "", .rb_or(req$HTTP_HOST, "")))
  if (!host %in% hosts) {
    return(reply(403L, sprintf("%s are kept for a page opened from this computer", what)))
  }
  if (req$REQUEST_METHOD != "GET" && (!identical(req$HTTP_X_RB_NOTES, "1") ||
                                        (!is.null(req$HTTP_ORIGIN) &&
                                           !identical(req$HTTP_ORIGIN, paste0("http://", req$HTTP_HOST))))) {
    return(reply(403L, sprintf("%s comes from the map's page", one)))
  }
  NULL
}

#' Verdicts given on a served map
#'
#' A map served with `rb_map_serve(verdicts = )` takes the reader's verdict on the features of a layer whose popup has
#' a `verdict` ([rb_map_layer()]): its questions in a feature's popup, a button for each answer, a box for the name an
#' answer points at (`other`, with the names in view offered), and the feature's note where the verdict asks for one.
#' Each answer is a row added to a `.csv` file, never a row changed: when, the layer, the feature (its value of the
#' verdict's `field`), the question, the answer, the other feature's name, the note, the feature's title, its place
#' (the feature's own for a point, else where it was clicked) and the zoom. The note is a question of its own, "note",
#' its text the answer. A later answer to a question replaces the earlier, and an empty one withdraws it. The page draws
#' a mark on each feature judged: the first question's answer its fill, the second's its ring. A review written down
#' where it is made, and read back as data: what a map shows can be checked by hand, and the checks counted. A file of
#' 2026.11.09, before the questions, reads as the question "verdict" with no other names.
#'
#' @param path The verdicts' file (`.csv`).
#' @param all `FALSE`, each feature's current answer to each question (the last, none where it was withdrawn; a note
#'   where it has a text); `TRUE`, every answer given, in the order given.
#' @return A data frame: `time` (UTC, as text), `layer`, `id`, `question`, `value`, `other`, `text`, `title`, `lon`,
#'   `lat`, `zoom`; no rows when there is no file.
#' @export
#' @examples
#' # a map whose harbours take a verdict each:
#' # popup = list(click = "harbours-fill", title = "SI_HARB",
#' #              verdict = list(field = "SI_HARB", text = TRUE,
#' #                             options = list(list(value = "right", label = "right"),
#' #                                            list(value = "wrong", label = "wrong"))))
#' # rb_map_serve(m, verdicts = "harbour_verdicts.csv")
#' f <- tempfile(fileext = ".csv")
#' rb_map_verdicts(f) # none yet
rb_map_verdicts <- function(path, all = FALSE) {
  v <- .rb_verdicts_read(.rb_verdicts_path(path, folder = FALSE))
  if (isTRUE(all) || !nrow(v)) {
    return(v)
  }
  last <- !duplicated(paste(v$layer, v$id, v$question, sep = "\r"), fromLast = TRUE)
  given <- ifelse(v$question == "note", nzchar(v$text), nzchar(v$value))
  v <- v[last & given, , drop = FALSE]
  rownames(v) <- NULL
  v
}

.rb_verdict_cols <- c("time", "layer", "id", "question", "value", "other", "text", "title", "lon", "lat", "zoom")
.rb_verdict_cols_v1 <- c("time", "layer", "id", "value", "text", "title", "lon", "lat", "zoom") # 2026.11.09

# The verdicts' file: one path, a .csv file whose folder exists (when it is to be written)
.rb_verdicts_path <- function(path, folder = TRUE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) || !grepl("[.]csv$", path, ignore.case = TRUE)) {
    stop("The verdicts' file is one path to a .csv file.", call. = FALSE)
  }
  path <- normalizePath(path, mustWork = FALSE)
  if (folder && !dir.exists(dirname(path))) {
    stop(sprintf("The verdicts' folder %s does not exist.", dirname(path)), call. = FALSE)
  }
  path
}

# The verdicts as rows, none when there is no file; a file without the verdicts' columns stops whatever would read or
# write it, so it is never overwritten
.rb_verdicts_read <- function(path) {
  empty <- as.data.frame(stats::setNames(replicate(length(.rb_verdict_cols), character(), simplify = FALSE),
                                         .rb_verdict_cols), stringsAsFactors = FALSE)
  num <- function(v) {
    for (k in c("lon", "lat", "zoom")) v[[k]] <- suppressWarnings(as.numeric(v[[k]]))
    v
  }
  if (!file.exists(path)) {
    return(num(empty))
  }
  v <- tryCatch(utils::read.csv(path, colClasses = "character", na.strings = character(), encoding = "UTF-8",
                                check.names = FALSE),
                error = function(e) NULL)
  if (is.data.frame(v) && identical(names(v), .rb_verdict_cols_v1)) { # one question, before the questions
    v$question <- rep("verdict", nrow(v))
    v$other <- rep("", nrow(v))
    v <- v[, .rb_verdict_cols]
  }
  if (!is.data.frame(v) || !identical(names(v), .rb_verdict_cols)) {
    stop(sprintf("%s is not a verdicts file (a .csv with the columns %s): left as it is.", path,
                 paste(.rb_verdict_cols, collapse = ", ")), call. = FALSE)
  }
  num(v)
}

# A verdict's fields checked against the layers that take one: NULL, or why it is refused. An answer to a question is
# one of its options, with the other feature's name where the option names one and none where it does not, or empty
# to withdraw it; a note ("note") has a text, empty to clear it
.rb_verdicts_check <- function(n, specs) {
  one <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
  one_num <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x)
  if (!one(n$layer) || is.null(specs[[n$layer]])) {
    return(sprintf("`layer` is one of the layers that take a verdict: %s.", paste(names(specs), collapse = ", ")))
  }
  if (!(one(n$id) || one_num(n$id)) || !nzchar(as.character(n$id)) || nchar(as.character(n$id)) > 300) {
    return("A verdict's `id` names its feature: a text of 1-300 characters.")
  }
  qs <- specs[[n$layer]]$questions
  ids <- vapply(qs, `[[`, "", "id")
  question <- .rb_or(n$question, if (length(qs) == 1) ids[1])
  if (!one(question) || !question %in% c(ids, if (specs[[n$layer]]$text) "note")) {
    return(sprintf("A verdict's `question` is one of %s.", paste(c(ids, if (specs[[n$layer]]$text) "note"), collapse = ", ")))
  }
  if (!is.null(n$text) && !(one(n$text) && nchar(n$text) <= 1000)) {
    return("A verdict's `text` is at most 1,000 characters.")
  }
  if (question == "note") {
    if (!is.null(n$value) && !identical(n$value, "")) {
      return("A note has a `text`, no `value`.")
    }
  } else {
    o <- qs[[match(question, ids)]]$options
    values <- vapply(o, `[[`, "", "value")
    if (!one(n$value) || !(n$value %in% c(values, ""))) {
      return(sprintf("An answer to \"%s\" is one of %s, or empty to withdraw it.", question, paste(values, collapse = ", ")))
    }
    asks <- nzchar(n$value) && isTRUE(o[[match(n$value, values)]]$other)
    if (asks && !(one(n$other) && nzchar(trimws(n$other)) && nchar(n$other) <= 200)) {
      return(sprintf("The answer \"%s\" names another feature: `other`, a name of 1-200 characters.", n$value))
    }
    if (!asks && !is.null(n$other) && !identical(n$other, "")) {
      return(sprintf("The answer \"%s\" names no other feature: no `other`.", n$value))
    }
  }
  if (!is.null(n$title) && !(one(n$title) && nchar(n$title) <= 300)) {
    return("A verdict's `title` is at most 300 characters.")
  }
  if (!one_num(n$lon) || !one_num(n$lat) || abs(n$lon) > 180 || abs(n$lat) > 90) {
    return("A verdict has its feature's place, `lon` and `lat` in degrees.")
  }
  if (!is.null(n$zoom) && !(one_num(n$zoom) && n$zoom >= 0 && n$zoom <= 24)) {
    return("A verdict's `zoom` is a number from 0 to 24.")
  }
  NULL
}

# A verdict added, checked before: the file written beside itself and renamed over it
.rb_verdicts_add <- function(path, n) {
  v <- .rb_verdicts_read(path)
  row <- data.frame(
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), layer = n$layer, id = as.character(n$id),
    question = n$question, value = .rb_or(n$value, ""), other = trimws(.rb_or(n$other, "")),
    text = trimws(.rb_or(n$text, "")), title = .rb_or(n$title, ""),
    lon = round(n$lon, 6), lat = round(n$lat, 6), zoom = if (is.null(n$zoom)) NA_real_ else round(n$zoom, 2),
    stringsAsFactors = FALSE
  )
  v <- rbind(v, row)
  tmp <- paste0(path, ".tmp")
  con <- file(tmp, open = "w", encoding = "UTF-8")
  utils::write.csv(v, con, row.names = FALSE, na = "")
  close(con)
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    stop(sprintf("The verdicts could not be written to %s.", path), call. = FALSE)
  }
  invisible(path)
}

# The layers of a map that take a verdict, by id: their verdicts' specs
.rb_map_verdict_layers <- function(map) {
  specs <- lapply(map$layers, function(l) l$config$popup$verdict)
  names(specs) <- vapply(map$layers, function(l) l$config$id, "")
  Filter(Negate(is.null), specs)
}

# The routes of the verdicts: GET /verdicts, POST /verdicts (given or withdrawn), each answered with the current ones
.rb_map_verdicts_reply <- function(req, verdicts, specs, hosts, reply, json) {
  refused <- .rb_map_local_only(req, hosts, reply, "the verdicts", "a verdict")
  if (!is.null(refused)) {
    return(refused)
  }
  answer <- function() {
    v <- rb_map_verdicts(verdicts)
    rows <- lapply(seq_len(nrow(v)), function(i) {
      as.list(v[i, c("layer", "id", "question", "value", "other", "text", "lon", "lat", "time")])
    })
    reply(200L, json(list(verdicts = rows)), "application/json; charset=utf-8", list(`Cache-Control` = "no-store"))
  }
  method <- req$REQUEST_METHOD
  tryCatch(
    {
      if (req$PATH_INFO != "/verdicts") {
        return(reply(404L, "no such route"))
      }
      if (method == "GET") {
        return(answer())
      }
      if (method == "POST") {
        if (suppressWarnings(as.numeric(.rb_or(req$CONTENT_LENGTH, "0"))) > 16384) {
          return(reply(413L, "a verdict is at most 16 kB"))
        }
        b <- req$rook.input$read()
        if (length(b) > 16384) {
          return(reply(413L, "a verdict is at most 16 kB"))
        }
        b <- rawToChar(b)
        Encoding(b) <- "UTF-8"
        n <- tryCatch(jsonlite::fromJSON(b, simplifyVector = FALSE), error = function(e) NULL)
        if (!is.list(n) || is.null(names(n))) {
          return(reply(400L, "a verdict is a JSON object"))
        }
        why <- .rb_verdicts_check(n, specs)
        if (!is.null(why)) {
          return(reply(400L, why))
        }
        if (is.null(n$question)) {
          n$question <- specs[[n$layer]]$questions[[1]]$id # a page of one question may leave it out
        }
        .rb_verdicts_read(verdicts) # a file broken by hand stops here, left as it is
        .rb_verdicts_add(verdicts, n)
        return(answer())
      }
      reply(405L, "GET /verdicts or POST /verdicts")
    },
    error = function(e) reply(500L, sub("\n.*", "", conditionMessage(e)))
  )
}

#' Write a map as a folder that opens with no R and no server
#'
#' `rb_map_bundle()` writes an [rb_map()] as a folder whose `index.html` opens in a browser with a double click, on any
#' system, with no R and no server: the page with the map's config, and beside it the layers' tiles and data as script
#' files (`tiles/`, `data/`), which a page opened as a file (`file://`) may load where it may not fetch them. It needs
#' the internet only for MapLibre and maplibre-contour (from jsdelivr) and for the base maps.
#'
#' What a bundle carries:
#' * a geojson layer, its data;
#' * a dem or contour layer, its tiles from its `minzoom` to its `maxzoom` inside its `bounds`, written once for the
#'   layers that read one file (the `source` of [rb_map_tiles_mbtiles()]). Past the deepest zoom written, the map draws
#'   that zoom's tiles larger. A view's fit (a colour's, the contours' or a shading's `fit`) is made in the browser from
#'   the layer's tiles in view, by the rules in `fit`, not by the layer's R function, which needs a server;
#' * a vector layer from [rb_map_tiles_table()], the tiles that hold a feature, to the deepest zoom written;
#' * a time panel ([rb_map_time()]), each vessel's spans, the whole and each month, a file each (`time/`);
#' * a grid layer named in `grids`, its rows: summed by cell at a few cell sizes and written in blocks of cells
#'   (`grids/`), from which the page makes each tile, its filters applied and its cells summed in the browser, with the
#'   fields its server would send. Its cells hold one to two screen pixels (the filter `px` times that), from the
#'   cell sizes written. A filter the rows have no column for is left out, with a message.
#'
#' Another vector layer, or a grid layer not in `grids`, is left out, with a message: their tiles are made by a server.
#'
#' @param map An [rb_map()].
#' @param dir The folder to write. If it already holds a bundle (`index.html`, `tiles/`, `data/`, `grids/` or `time/`),
#'   `overwrite` replaces those and leaves the rest of the folder as it is.
#' @param maxzoom The deepest zoom of the tiles written (dem, contour and vector layers), at most each layer's own;
#'   `NULL` for each layer's own.
#' @param fit A view's fit in the browser: `probs`, the percentiles (0-1) of the floor and the cap, with `n` quantiles
#'   from one to the other (for colours in equal shares and contour levels); `nodata`, an elevation that means no value
#'   (`NULL` for none); `slope`, a shading's intensity: `k` over the `p` percentile of the slope in degrees, kept
#'   between `min` and `max`. Any left out keeps its default: those of a depth map whose tiles hold 0 on land.
#' @param grids A grid layer's rows for the browser, by the layer's id, each a list of:
#'   * `rows`: a data frame or a lazy table ([dplyr::tbl()] on DuckDB), a row for each cell and combination of the
#'     filters' values, with the cell's south-west corner as whole numbers `x` and `y` of `unit` degrees;
#'   * `unit`: the degrees of one step of `x` and `y`;
#'   * `levels`: the cell sizes written, in steps of `unit`, rising, each a multiple of the rows' own cell;
#'   * `columns`: a named character vector, a filter's id to the column it selects on (a range filter's column holds
#'     the values its two parameters bound);
#'   * `sums`: the columns added up within a cell;
#'   * `fields`: a named list, each of the tile's fields a sum's column, or `c(<column>, "km2")` for that sum per
#'     square kilometre of the cell;
#'   * `x`, `y`: the corner's columns, `"x"` and `"y"` unless given;
#'   * `px`: the filter that scales the cells to that many screen pixels, if any.
#' @param overwrite Whether to replace a bundle already in `dir`.
#' @return `dir`, invisibly, with the number of tiles written (`tiles`) and the bundle's size in bytes (`bytes`) as
#'   attributes.
#' @seealso [rb_map_serve()], which serves the map from this R session instead.
#' @export
#' @examples
#' places <- sf::st_as_sf(
#'   data.frame(name = c("Skagen", "Hirtshals"), lon = c(10.59, 9.96), lat = c(57.72, 57.59)),
#'   coords = c("lon", "lat"), crs = 4326
#' )
#' layer <- rb_map_layer(
#'   "places", "geojson", label = "Places", data = places,
#'   style = list(list(id = "places-dot", type = "circle", paint = list("circle-color" = "#d9822b"))),
#'   popup = list(click = "places-dot", title = "name")
#' )
#' m <- rb_map(rb_map_group("ref", "Reference", list(layer)),
#'             title = "Two harbours", view = list(center = c(10.3, 57.6), zoom = 8))
#' d <- rb_map_bundle(m, file.path(tempdir(), "two_harbours"), overwrite = TRUE)
#' list.files(d, recursive = TRUE)
#' # utils::browseURL(file.path(d, "index.html"))  # opens it in a browser
rb_map_bundle <- function(
  map,
  dir,
  maxzoom = NULL,
  fit = list(probs = c(0.02, 0.98), n = 25, nodata = 0, slope = list(k = 2, p = 0.9, min = 0.3, max = 1)),
  grids = list(),
  overwrite = FALSE
) {
  if (!inherits(map, "rb_map")) {
    stop("`map` is an rb_map().", call. = FALSE)
  }
  if (!is.character(dir) || length(dir) != 1 || is.na(dir) || !nzchar(dir)) {
    stop("`dir` is the path of a folder.", call. = FALSE)
  }
  if (
    !is.null(maxzoom) &&
      !(is.numeric(maxzoom) && length(maxzoom) == 1 && !is.na(maxzoom) && maxzoom >= 0 && maxzoom <= 22 &&
        maxzoom == round(maxzoom))
  ) {
    stop("`maxzoom` is a whole number from 0 to 22, or NULL.", call. = FALSE)
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` is TRUE or FALSE.", call. = FALSE)
  }
  fit <- .rb_map_bundle_fit(fit)
  if (!is.list(grids) || (length(grids) && (is.null(names(grids)) || any(!nzchar(names(grids)))))) {
    stop("`grids` is a list of a grid layer's rows, by the layer's id.", call. = FALSE)
  }
  parts <- file.path(dir, c("index.html", "tiles", "data", "grids", "time"))
  if (any(file.exists(parts))) {
    if (!overwrite) {
      stop(sprintf("%s holds a bundle: `overwrite = TRUE` replaces it.", dir), call. = FALSE)
    }
    unlink(parts, recursive = TRUE)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir)) {
    stop(sprintf("Could not make the folder %s.", dir), call. = FALSE)
  }

  # the layers a bundle carries, and the config without the others
  layers <- map$layers
  kind <- vapply(layers, function(l) l$config$kind, "")
  unknown <- setdiff(names(grids), names(layers)[kind == "grid"])
  if (length(unknown)) {
    stop(sprintf("`grids` names no grid layer of the map: %s.", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  listed <- vapply(layers, function(l) is.function(attr(l$tiles, "tiles_at")), logical(1))
  out <- names(layers)[(kind == "vector" & !listed) | (kind == "grid" & !names(layers) %in% names(grids))]
  if (length(out)) {
    message(sprintf(
      "Left out of the bundle, as their tiles are made by a server: %s.",
      paste(out, collapse = ", ")
    ))
  }
  keep <- setdiff(names(layers), out)
  config <- map$config
  config$layers <- Filter(function(l) l$id %in% keep, config$layers)
  config$groups <- Filter(
    function(g) length(g$layers) > 0,
    lapply(config$groups, function(g) {
      g$layers <- as.list(intersect(unlist(g$layers), keep))
      g
    })
  )
  if (!is.null(config$choose$fields)) {
    config$choose$fields <- Filter(
      function(f) length(f$layers) > 0,
      lapply(config$choose$fields, function(f) {
        f$layers <- as.list(intersect(unlist(f$layers), keep))
        f
      })
    )
    if (!length(config$choose$fields)) {
      config$choose <- NULL
    }
  }

  # a grid's rows, summed by cell and written in blocks; its filters without a column left out
  grid_specs <- list()
  for (id in names(grids)) {
    i <- which(vapply(config$layers, `[[`, "", "id") == id)
    spec <- .rb_map_bundle_grid_spec(grids[[id]], config$layers[[i]])
    dropped <- setdiff(vapply(config$layers[[i]]$filters, `[[`, "", "id"), spec$filters)
    if (length(dropped)) {
      message(sprintf("Layer %s: left out of the bundle, as its rows have no column for them: %s.", id,
                      paste(dropped, collapse = ", ")))
    }
    config$layers[[i]]$filters <- Filter(function(f) f$id %in% spec$filters, config$layers[[i]]$filters)
    grid_specs[[id]] <- .rb_map_bundle_grid(spec, config$layers[[i]], dir, id)
  }

  # the tiles: once for the layers on one source, from the least of their zooms to the most; a dem or contour layer's
  # inside its bounds, a vector layer's those that hold a feature (its tiles' `tiles_at`)
  ids <- vapply(config$layers, `[[`, "", "id")
  dem <- which(vapply(config$layers, function(l) l$kind %in% c("dem", "contour", "vector"), logical(1)))
  for (i in dem) {
    lc <- config$layers[[i]]
    z1 <- c(unlist(lc$maxzoom), maxzoom)
    if (!length(z1)) {
      stop(sprintf("Layer %s: its tiles' deepest zoom is unknown: give `maxzoom`.", lc$id), call. = FALSE)
    }
    z1 <- min(z1)
    if (lc$kind != "vector" && is.null(lc$bounds)) {
      stop(sprintf("Layer %s: a dem or contour layer in a bundle needs its `bounds`.", lc$id), call. = FALSE)
    }
    config$layers[[i]]$maxzoom <- as.integer(z1)
  }
  source <- vapply(ids[dem], function(id) .rb_or(attr(layers[[id]]$tiles, "source"), paste0("layer:", id)), "")
  folder <- stats::setNames(list(), character())
  n_tiles <- 0L
  for (s in unique(source)) {
    on <- dem[source == s]
    lcs <- config$layers[on]
    b <- if (lcs[[1]]$kind != "vector") vapply(lcs, function(lc) as.numeric(unlist(lc$bounds)), numeric(4))
    n_tiles <- n_tiles + .rb_map_bundle_tiles(
      layers[[ids[on[1]]]]$tiles,
      dir,
      ids[on[1]],
      min(vapply(lcs, function(lc) as.integer(.rb_or(lc$minzoom, 0L)), integer(1))),
      max(vapply(lcs, function(lc) lc$maxzoom, integer(1))),
      if (!is.null(b)) c(min(b[1, ]), min(b[2, ]), max(b[3, ]), max(b[4, ]))
    )
    for (i in on) {
      folder[[ids[i]]] <- ids[on[1]]
    }
  }

  # the time panel's series (rb_map_time()): each vessel's spans, the whole and each month, a file each, named by the
  # vessel's place in the panel's list
  if (!is.null(map$time)) {
    dir.create(file.path(dir, "time"), showWarnings = FALSE)
    for (i in seq_along(map$time$config$vessels)) {
      v <- map$time$config$vessels[[i]]
      for (sp in v$spans) {
        key <- sprintf("time/%d_%s", i - 1L, sp$value)
        writeLines(
          enc2utf8(sprintf(
            'RB_BUNDLE("%s",%s);',
            key,
            .rb_map_json(map$time$series(v$value, sp$value))
          )),
          file.path(dir, paste0(key, ".js")),
          useBytes = TRUE
        )
      }
    }
  }

  # a geojson layer's data
  for (lc in config$layers) {
    if (lc$kind != "geojson") {
      next
    }
    d <- .rb_map_geojson(layers[[lc$id]])
    if (is.null(d)) {
      next
    }
    dir.create(file.path(dir, "data"), showWarnings = FALSE)
    writeLines(
      enc2utf8(sprintf('RB_BUNDLE("data/%s",%s);', lc$id, paste(d, collapse = "\n"))),
      file.path(dir, "data", paste0(lc$id, ".js")),
      useBytes = TRUE
    )
  }

  bundle <- list(
    meta = list(engine = .rb_map_engine, layers = Filter(Negate(is.null), .rb_map_metas(layers[keep]))),
    tiles = folder,
    fit = fit,
    grids = if (length(grid_specs)) grid_specs else stats::setNames(list(), character())
  )
  writeLines(
    enc2utf8(.rb_map_page(.rb_map_json(config), .rb_map_json(bundle))),
    file.path(dir, "index.html"),
    useBytes = TRUE
  )
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  invisible(structure(dir, tiles = n_tiles, bytes = sum(file.size(files))))
}

# A bundle's rules of fit: the defaults where `fit` leaves one out, each checked
.rb_map_bundle_fit <- function(fit) {
  d <- list(probs = c(0.02, 0.98), n = 25, nodata = 0, slope = list(k = 2, p = 0.9, min = 0.3, max = 1))
  if (!is.list(fit) || (length(fit) && is.null(names(fit))) || length(setdiff(names(fit), names(d))) ||
        (!is.null(fit$slope) && (!is.list(fit$slope) || length(setdiff(names(fit$slope), names(d$slope)))))) {
    stop("`fit` is a list of any of probs, n, nodata and slope (a list of any of k, p, min and max).", call. = FALSE)
  }
  if ("slope" %in% names(fit)) {
    fit$slope <- utils::modifyList(d$slope, fit$slope)
  }
  v <- utils::modifyList(d, fit)
  num <- function(x, n = 1) is.numeric(x) && length(x) == n && !anyNA(x)
  s <- v$slope
  ok <- c(
    probs = num(v$probs, 2) && v$probs[1] >= 0 && v$probs[1] < v$probs[2] && v$probs[2] <= 1,
    n = num(v$n) && v$n >= 2 && v$n == round(v$n),
    nodata = is.null(v$nodata) || num(v$nodata),
    slope = num(s$k) && s$k > 0 && num(s$p) && s$p > 0 && s$p < 1 && num(s$min) && num(s$max) && s$min >= 0 &&
      s$min <= s$max
  )
  if (!all(ok)) {
    stop(
      sprintf(
        "`fit`: %s out of range (probs two rising numbers from 0 to 1; n a whole number of 2 or more; nodata a number or NULL; slope's k above 0, p between 0 and 1, min and max from 0 with min no more than max).",
        paste(names(ok)[!ok], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  v$probs <- as.list(v$probs)
  v$n <- as.integer(v$n)
  v
}

# A source's tiles from zoom z0 to z1, inside `bounds` (west, south, east, north), or with no bounds those its
# `tiles_at` lists, each a script handing its bytes in base64 to RB_BUNDLE(): tiles/<key>/<z>/<x>/<y>.js. Its count
.rb_map_bundle_tiles <- function(tiles, dir, key, z0, z1, bounds = NULL) {
  col <- function(lon, z) min(2^z - 1, max(0, floor((lon + 180) / 360 * 2^z)))
  row <- function(lat, z) {
    r <- max(-85.0511, min(85.0511, lat)) * pi / 180
    min(2^z - 1, max(0, floor((1 - log(tan(r) + 1 / cos(r)) / pi) / 2 * 2^z)))
  }
  n <- 0L
  failed <- character()
  for (z in seq(z0, z1)) {
    at <- if (is.null(bounds)) {
      attr(tiles, "tiles_at")(z)
    } else {
      expand.grid(y = seq(row(bounds[4], z), row(bounds[2], z)), x = seq(col(bounds[1], z), col(bounds[3], z)))
    }
    for (k in seq_len(nrow(at))) {
      x <- at$x[k]
      y <- at$y[k]
      # a tile its function cannot make is left out, as its server would refuse it
      t <- tryCatch(tiles(z, x, y), error = function(e) {
        failed <<- c(failed, sprintf("%d/%.0f/%.0f (%s)", z, x, y, sub("\n.*", "", trimws(conditionMessage(e)))))
        NULL
      })
      if (is.null(t) || !length(t)) {
        next
      }
      f <- file.path(dir, "tiles", key, z, x, paste0(y, ".js"))
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      writeLines(
        sprintf('RB_BUNDLE("tiles/%s/%d/%d/%d","%s");', key, z, x, y, gsub("\n", "", jsonlite::base64_enc(t), fixed = TRUE)),
        f
      )
      n <- n + 1L
    }
  }
  if (length(failed)) {
    message(sprintf("%s: %d tile(s) its function could not make, left out: %s.", key, length(failed),
                    paste(utils::head(failed, 5), collapse = "; ")))
  }
  n
}

# A grid's rows for a bundle, checked against its layer: the filters it can apply (those with a column, the metric
# and the cell scale) in `filters`
.rb_map_bundle_grid_spec <- function(g, lc) {
  bad <- function(what) stop(sprintf("`grids$%s`: %s", lc$id, what), call. = FALSE)
  allowed <- c("rows", "unit", "levels", "columns", "sums", "fields", "x", "y", "px")
  if (!is.list(g) || is.null(names(g)) || length(setdiff(names(g), allowed))) {
    bad(sprintf("a list of any of %s.", paste(allowed, collapse = ", ")))
  }
  lazy <- inherits(g$rows, "tbl_sql")
  if (!lazy && !is.data.frame(g$rows)) {
    bad("`rows` is a data frame or a lazy table.")
  }
  have <- colnames(g$rows)
  x <- .rb_or(g$x, "x")
  y <- .rb_or(g$y, "y")
  if (!all(c(x, y) %in% have)) {
    bad(sprintf("`rows` has no column %s.", paste(setdiff(c(x, y), have), collapse = " or ")))
  }
  if (!is.numeric(g$unit) || length(g$unit) != 1 || is.na(g$unit) || g$unit <= 0) {
    bad("`unit` is the degrees of one step of x and y, above 0.")
  }
  lv <- g$levels
  if (!is.numeric(lv) || !length(lv) || anyNA(lv) || any(lv < 1) || any(lv != round(lv)) || is.unsorted(lv, strictly = TRUE)) {
    bad("`levels` are whole numbers of steps, rising.")
  }
  filters <- lc$filters
  fid <- vapply(filters, `[[`, "", "id")
  ftype <- vapply(filters, function(f) .rb_or(f$type, "options"), "")
  cl <- g$columns
  if (length(cl) && (!is.character(cl) || is.null(names(cl)) || !all(names(cl) %in% fid[ftype %in% c("options", "range")]))) {
    bad("`columns` names the layer's filters (not its metric), each its column.")
  }
  if (!all(cl %in% have)) {
    bad(sprintf("`rows` has no column %s.", paste(setdiff(cl, have), collapse = ", ")))
  }
  if (!is.character(g$sums) || !length(g$sums) || !all(g$sums %in% have)) {
    bad("`sums` are columns of `rows`, to be added up within a cell.")
  }
  fl <- g$fields
  ok <- is.list(fl) && length(fl) && !is.null(names(fl)) && all(vapply(fl, function(f) {
    is.character(f) && length(f) %in% 1:2 && f[1] %in% g$sums && (length(f) == 1 || identical(f[2], "km2"))
  }, logical(1)))
  if (!ok) {
    bad("`fields` are the tile's fields by name, each a sum's column, or c(<column>, \"km2\") per square kilometre.")
  }
  metrics <- vapply(lc$metrics, `[[`, "", "field")
  if (!all(metrics %in% names(fl))) {
    bad(sprintf("the layer's metric %s is not among `fields`.", paste(setdiff(metrics, names(fl)), collapse = ", ")))
  }
  if (!is.null(g$px) && !(is.character(g$px) && length(g$px) == 1 && g$px %in% fid)) {
    bad("`px` is the id of the layer's filter that scales its cells.")
  }
  list(
    rows = g$rows, lazy = lazy, unit = g$unit, levels = as.integer(lv), columns = cl, sums = g$sums, fields = fl,
    x = x, y = y, px = g$px, range = names(cl) %in% fid[ftype == "range"],
    filters = c(names(cl), fid[ftype == "metric"], g$px)
  )
}

# A grid's rows summed by cell at each level, from the finest level that divides it, written in blocks of 128 x 128
# cells: grids/<id>/<level>/<bx>_<by>.js, each handing RB_BUNDLE() its rows compressed (zlib) and in base64: their
# number (uint32), each sum (float32), the cell within the block (uint16, row by row from the south-west), and each
# filter's column as codes into its values (uint16, then uint8), all little-endian. The page's account of them
.rb_map_bundle_grid <- function(spec, lc, dir, id) {
  B <- 128L
  if (spec$lazy) {
    con <- dbplyr::remote_con(spec$rows)
    src <- as.character(dbplyr::sql_render(spec$rows))
  } else {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    duckdb::duckdb_register(con, "rb_grid_rows", spec$rows)
    src <- "SELECT * FROM rb_grid_rows"
  }
  qi <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  get <- function(...) DBI::dbGetQuery(con, sprintf(...))
  run <- function(...) invisible(DBI::dbExecute(con, sprintf(...)))
  cols <- unname(spec$columns)
  values <- lapply(cols, function(cn) get("SELECT DISTINCT %s AS v FROM (%s) ORDER BY 1 NULLS LAST", qi(cn), src)$v)
  bytes <- vapply(values, function(v) if (length(v) <= 256) 1L else if (length(v) <= 65536) 2L else NA_integer_, 1L)
  if (anyNA(bytes)) {
    stop(sprintf("`grids$%s`: a filter's column holds more than 65,536 values.", id), call. = FALSE)
  }
  keys <- paste(c(vapply(cols, qi, ""), vapply(spec$sums, qi, "")), collapse = ", ")
  dims <- paste(vapply(cols, qi, ""), collapse = ", ")
  sums <- paste(sprintf("sum(%1$s) AS %1$s", vapply(spec$sums, qi, "")), collapse = ", ")
  stem <- sprintf("rb_grid_%s_%d", gsub("[^A-Za-z0-9]", "_", id), as.integer(stats::runif(1, 1, 1e9)))
  made <- integer()
  on.exit(for (L in made) DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s_%d", stem, L)), add = TRUE, after = FALSE)
  blocks <- list()
  for (L in spec$levels) {
    from <- rev(made[L %% made == 0])[1]
    sel <- if (is.na(from)) {
      sprintf("SELECT CAST(floor(%s / %d) AS BIGINT) AS cx, CAST(floor(%s / %d) AS BIGINT) AS cy%s, %s FROM (%s) GROUP BY ALL",
              qi(spec$x), L, qi(spec$y), L, if (nzchar(dims)) paste0(", ", dims) else "", sums, src)
    } else {
      sprintf("SELECT CAST(floor(cx / %d) AS BIGINT) AS cx, CAST(floor(cy / %d) AS BIGINT) AS cy%s, %s FROM %s_%d GROUP BY ALL",
              L %/% from, L %/% from, if (nzchar(dims)) paste0(", ", dims) else "", sums, stem, from)
    }
    run("CREATE TEMP TABLE %s_%d AS SELECT *, CAST(floor(cx / %d) AS BIGINT) AS blx, CAST(floor(cy / %d) AS BIGINT) AS bly
         FROM (%s) ORDER BY blx, bly, cx, cy", stem, L, B, B, sel)
    made <- c(made, L)
    bl <- get("SELECT DISTINCT blx, bly FROM %s_%d ORDER BY blx, bly", stem, L)
    folder <- file.path(dir, "grids", id, L)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    for (k in seq_len(nrow(bl))) {
      r <- get("SELECT cx, cy, %s FROM %s_%d WHERE blx = %.0f AND bly = %.0f ORDER BY cx, cy%s",
               keys, stem, L, bl$blx[k], bl$bly[k], if (nzchar(dims)) paste0(", ", dims) else "")
      cell <- as.integer((r$cy - bl$bly[k] * B) * B + (r$cx - bl$blx[k] * B))
      code <- lapply(seq_along(cols), function(j) match(r[[cols[j]]], values[[j]]) - 1L)
      f32 <- function(v) {
        v <- as.double(v)
        v[is.na(v)] <- 0
        writeBin(v, raw(), size = 4, endian = "little")
      }
      u16 <- function(v) writeBin(ifelse(v > 32767L, v - 65536L, v), raw(), size = 2, endian = "little")
      payload <- c(
        writeBin(nrow(r), raw(), size = 4, endian = "little"),
        unlist(lapply(spec$sums, function(s) f32(r[[s]]))),
        u16(cell),
        unlist(lapply(which(bytes == 2L), function(j) u16(code[[j]]))),
        unlist(lapply(which(bytes == 1L), function(j) as.raw(code[[j]])))
      )
      key <- sprintf("grids/%s/%d/%.0f_%.0f", id, L, bl$blx[k], bl$bly[k])
      writeLines(
        sprintf('RB_BUNDLE("%s","%s");', key, gsub("\n", "", jsonlite::base64_enc(memCompress(payload, "gzip")), fixed = TRUE)),
        file.path(dir, paste0(key, ".js"))
      )
    }
    blocks[[as.character(L)]] <- as.list(sprintf("%.0f_%.0f", bl$blx, bl$bly))
  }
  list(
    unit = spec$unit, levels = as.list(spec$levels), block = B, px = spec$px, layer = .rb_or(lc$source_layer, "cells"),
    sums = as.list(spec$sums), fields = lapply(spec$fields, as.list),
    columns = lapply(seq_along(cols), function(j) {
      list(id = names(spec$columns)[j], range = spec$range[j], bytes = bytes[j],
           values = as.list(ifelse(is.na(values[[j]]), NA, as.character(values[[j]]))))
    }),
    blocks = blocks
  )
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
#'   attribute, the temporary table's name as its `table` attribute, and as `tiles_at` a function of a zoom giving the
#'   tiles that hold a feature (`x`, `y`), by which [rb_map_bundle()] writes only those.
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
  # a data frame registered here is the tile function's for the session (it remakes its table from it)
  .rb_scope()
  .rb_extension(con, "spatial")
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
  nm <- .rb_name(con, "rb_tiles_")
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
  # the tiles at zoom z that hold a feature, x and y (y from the north), for a bundle (rb_map_bundle())
  attr(tile, "tiles_at") <- function(z) {
    if (!DBI::dbExistsTable(con, nm)) {
      make()
    }
    w <- 20037508.342789244
    k <- function(v) sprintf("CAST(floor(%s) AS BIGINT)", v)
    cl <- function(v) sprintf("greatest(0, least(%.0f, %s))", 2^z - 1, v)
    DBI::dbGetQuery(con, sprintf(
      "WITH r AS (SELECT %s AS x0, %s AS x1, %s AS y0, %s AS y1 FROM %s),
            a AS (SELECT unnest(range(x0, x1 + 1)) AS x, y0, y1 FROM r)
       SELECT DISTINCT x, y FROM (SELECT x, unnest(range(y0, y1 + 1)) AS y FROM a) ORDER BY x, y",
      cl(k(sprintf("(xmin + %.17e) / %.17e * %.0f", w, 2 * w, 2^z))),
      cl(k(sprintf("(xmax + %.17e) / %.17e * %.0f", w, 2 * w, 2^z))),
      cl(k(sprintf("(%.17e - ymax) / %.17e * %.0f", w, 2 * w, 2^z))),
      cl(k(sprintf("(%.17e - ymin) / %.17e * %.0f", w, 2 * w, 2^z))),
      nm
    ))
  }
  attr(tile, "count") <- n
  attr(tile, "table") <- nm
  .rb_hold(tile)
}

# A layer's state: controls whose values its style reads as MapLibre's global state, `<id>_<name>`; each checked, and
# each read by the style, so a control that changes nothing is not offered
.rb_map_state <- function(state, id, kind, style) {
  if (kind == "grid") {
    stop("`state` is for a vector, geojson, dem or contour layer: a grid is drawn by the page.", call. = FALSE)
  }
  shape <- "`state` is a list of controls, each a list with `id`, `label`, and `options` (each a `value` and a `label`) or a `default` TRUE or FALSE."
  if (!is.list(state) || !length(state) || !all(vapply(state, is.list, logical(1)))) {
    stop(shape, call. = FALSE)
  }
  # the names the page's address gives a layer's own controls
  own <- c("by", "method", "dir", "alt", "ex", "op", "shadow", "light", "pal", "lo", "hi", "scale", "mode", "cut",
           "cutas", "cop", "auto")
  scalar <- function(v) length(v) == 1 && !is.na(v) && (is.character(v) || is.numeric(v) || is.logical(v))
  read <- as.character(jsonlite::toJSON(style, auto_unbox = TRUE))
  state <- lapply(state, function(s) {
    if (!is.character(s$id) || length(s$id) != 1 || !grepl("^[a-z][a-z0-9]*$", s$id)) {
      stop("A state's `id` is lower-case letters and digits, starting with a letter.", call. = FALSE)
    }
    if (s$id %in% own) {
      stop(sprintf("A state's `id` \"%s\" is a name the page's address uses for a layer's own controls.", s$id),
           call. = FALSE)
    }
    key <- paste0(id, "_", s$id)
    if (!grepl(sprintf("[\"global-state\",\"%s\"]", key), read, fixed = TRUE)) {
      stop(sprintf("The state \"%s\" is read by none of the layer's styles: read it as list(\"global-state\", \"%s\").",
                   s$id, key), call. = FALSE)
    }
    if (is.null(s$label)) {
      s$label <- s$id
    }
    if (is.null(s$options)) {
      if (!isTRUE(s$default) && !isFALSE(s$default)) {
        stop(shape, call. = FALSE)
      }
      return(s)
    }
    if (!is.list(s$options) || !length(s$options) ||
          !all(vapply(s$options, function(o) is.list(o) && scalar(o$value), logical(1)))) {
      stop(shape, call. = FALSE)
    }
    values <- vapply(s$options, function(o) as.character(o$value), "")
    if (anyDuplicated(values)) {
      stop(sprintf("The options of the state \"%s\" need distinct values.", s$id), call. = FALSE)
    }
    if (is.null(s$default)) {
      s$default <- s$options[[1]]$value
    } else if (!scalar(s$default) || !as.character(s$default) %in% values) {
      stop(sprintf("The `default` of the state \"%s\" is one of its options' values.", s$id), call. = FALSE)
    }
    s$options <- lapply(s$options, function(o) {
      if (is.null(o$label)) {
        o$label <- as.character(o$value)
      }
      o
    })
    s
  })
  if (anyDuplicated(vapply(state, `[[`, "", "id"))) {
    stop("A layer's state ids are used once each.", call. = FALSE)
  }
  state
}

# A dem layer's relief controls: MapLibre's hillshade defaults for what is not given, each value checked
.rb_map_shade <- function(shade, kind, style) {
  if (kind != "dem" || !any(vapply(style, function(s) identical(s$type, "hillshade"), logical(1)))) {
    stop("`shade` is for a dem layer with a `hillshade` style.", call. = FALSE)
  }
  given <- if (isTRUE(shade)) list() else shade
  known <- c("method", "direction", "altitude", "exaggeration", "opacity", "shadow", "highlight", "auto")
  if (!is.list(given) || (length(given) && (is.null(names(given)) || !all(names(given) %in% known)))) {
    stop(sprintf("`shade` is TRUE or a list of any of %s.", paste(known, collapse = ", ")), call. = FALSE)
  }
  v <- utils::modifyList(
    list(method = "standard", direction = 335, altitude = 45, exaggeration = 0.5, opacity = 100,
         shadow = "#000000", highlight = "#ffffff", auto = TRUE),
    given
  )
  num <- function(x, lo, hi) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= lo && x <= hi
  hex <- function(x) is.character(x) && length(x) == 1 && grepl("^#[0-9A-Fa-f]{6}$", x)
  ok <- c(
    method = is.character(v$method) && length(v$method) == 1 &&
      v$method %in% c("standard", "basic", "combined", "igor", "multidirectional"),
    direction = num(v$direction, 0, 359),
    altitude = num(v$altitude, 0, 90),
    exaggeration = num(v$exaggeration, 0, 1),
    opacity = num(v$opacity, 0, 100),
    shadow = hex(v$shadow),
    highlight = hex(v$highlight),
    auto = is.logical(v$auto) && length(v$auto) == 1 && !is.na(v$auto)
  )
  if (!all(ok)) {
    stop(
      sprintf(
        "`shade`: %s out of range (method one of MapLibre's five; direction 0-359, altitude 0-90, exaggeration 0-1, opacity 0-100; colours \"#rrggbb\").",
        paste(names(ok)[!ok], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  v$shadow <- tolower(v$shadow)
  v$highlight <- tolower(v$highlight)
  v
}

# A contour layer's intervals by zoom, as an object of the zoom's name, and its multiplier, each checked
.rb_map_contours <- function(contours) {
  t <- contours$thresholds
  z <- suppressWarnings(as.numeric(names(t)))
  one <- function(x) is.numeric(x) && length(x) %in% 1:2 && !anyNA(x) && all(x > 0) && !is.unsorted(x)
  if (
    !is.list(contours) || !all(names(contours) %in% c("thresholds", "multiplier", "auto")) || !is.list(t) || !length(t) ||
      anyNA(z) || any(z < 0 | z > 24 | z != round(z)) || anyDuplicated(z) || !all(vapply(t, one, logical(1)))
  ) {
    stop(
      "A contour layer's `contours` is a list of `thresholds`, by zoom (0-24, as names) the minor and major interval from that zoom on, both above 0, and optionally `multiplier`.",
      call. = FALSE
    )
  }
  m <- if (is.null(contours$multiplier)) 1 else contours$multiplier
  if (!is.numeric(m) || length(m) != 1 || is.na(m) || m == 0) {
    stop("`contours$multiplier` is a number other than 0.", call. = FALSE)
  }
  a <- if (is.null(contours$auto)) TRUE else contours$auto
  if (!is.logical(a) || length(a) != 1 || is.na(a)) {
    stop("`contours$auto` is TRUE or FALSE.", call. = FALSE)
  }
  list(thresholds = lapply(t[order(z)], as.list), multiplier = m, auto = a)
}

# A dem layer's colour controls: the defaults for what is not given, each value checked
.rb_map_colour <- function(colour, kind, style) {
  if (kind != "dem" || !any(vapply(style, function(s) identical(s$type, "color-relief"), logical(1)))) {
    stop("`colour` is for a dem layer with a `color-relief` style.", call. = FALSE)
  }
  given <- if (isTRUE(colour)) list() else colour
  known <- c("of", "palette", "floor", "cap", "scale", "mode", "bands", "cuts", "cut", "cut_as", "opacity", "auto")
  if (!is.list(given) || (length(given) && (is.null(names(given)) || !all(names(given) %in% known)))) {
    stop(sprintf("`colour` is TRUE or a list of any of %s.", paste(known, collapse = ", ")), call. = FALSE)
  }
  v <- utils::modifyList(
    list(of = "depth", palette = NULL, floor = 10, cap = 2000, scale = "log", mode = "ramp",
         bands = c(10, 20, 50, 100, 200, 500, 1000, 2000), cuts = c(0, 10, 20, 50), cut = 0, cut_as = "clear",
         opacity = 100, auto = TRUE),
    given
  )
  one_of <- function(x, ok) is.character(x) && length(x) == 1 && x %in% ok
  num <- function(x, lo, hi) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= lo && x <= hi
  rising <- function(x) is.numeric(x) && length(x) > 0 && !anyNA(x) && all(x >= 0) && !is.unsorted(x, strictly = TRUE)
  ok <- c(
    of = one_of(v$of, c("depth", "elevation")),
    palette = is.null(v$palette) || (is.character(v$palette) && length(v$palette) == 1),
    floor = num(v$floor, 1, 10000),
    cap = num(v$cap, 1, 10000),
    scale = one_of(v$scale, c("log", "linear", "equal")),
    mode = one_of(v$mode, c("ramp", "bands")),
    bands = rising(v$bands) && all(v$bands > 0),
    cuts = rising(v$cuts),
    cut = num(v$cut, 0, Inf) && v$cut %in% v$cuts,
    cut_as = one_of(v$cut_as, c("clear", "grey")),
    opacity = num(v$opacity, 0, 100),
    auto = is.logical(v$auto) && length(v$auto) == 1 && !is.na(v$auto)
  )
  if (!all(ok)) {
    stop(
      sprintf(
        "`colour`: %s out of range (of depth or elevation; floor and cap 1-10,000; scale log, linear or equal; mode ramp or bands; bands and cuts rising, the cut one of the cuts; cut_as clear or grey; opacity 0-100).",
        paste(names(ok)[!ok], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  v$bands <- as.list(v$bands)
  v$cuts <- as.list(v$cuts)
  Filter(Negate(is.null), v)
}

#' Tiles from an MBTiles file
#'
#' A tile function for an [rb_map_layer()] that reads its tiles from an MBTiles file (SQLite), such as the image tiles
#' of elevations a dem layer draws. The file is opened once, read-only, and stays open while the function lives.
#'
#' @param file An MBTiles file: a `tiles` table (`zoom_level`, `tile_column`, `tile_row` in the TMS order, from the
#'   south, and `tile_data`) and a `metadata` table of `name` and `value`.
#' @return A function `function(z, x, y)` (`y` from the north, as the page asks) returning a tile's bytes, or `NULL`
#'   where the file has none; with the file's `format`, `bounds`, `minzoom`, `maxzoom`, `encoding` and `tile_size` as
#'   attributes where its metadata give them, which [rb_map_layer()] takes for a dem layer, and `source`, the file's
#'   path, by which [rb_map_bundle()] writes the tiles of layers on one file once.
#' @seealso [rb_map_layer()]
#' @export
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".mbtiles")
#'   con <- DBI::dbConnect(RSQLite::SQLite(), f)
#'   DBI::dbWriteTable(con, "metadata", data.frame(
#'     name = c("format", "encoding", "bounds", "minzoom", "maxzoom"),
#'     value = c("png", "terrarium", "-30,60,-5,70", "0", "1")
#'   ))
#'   DBI::dbExecute(con, "CREATE TABLE tiles (zoom_level INTEGER, tile_column INTEGER, tile_row INTEGER,
#'                        tile_data BLOB)")
#'   DBI::dbExecute(con, "INSERT INTO tiles VALUES (1, 0, 1, ?)", params = list(list(as.raw(1:4))))
#'   DBI::dbDisconnect(con)
#'   tiles <- rb_map_tiles_mbtiles(f)
#'   tiles(1, 0, 0) # the north-west tile at zoom 1, row 1 counted from the south
#'   attributes(tiles)[c("format", "encoding", "bounds")]
#' }
rb_map_tiles_mbtiles <- function(file) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("rb_map_tiles_mbtiles() needs the RSQLite package.", call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    stop("`file` is the path of an MBTiles file.", call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file, flags = RSQLite::SQLITE_RO)
  md <- DBI::dbGetQuery(con, "SELECT name, value FROM metadata")
  m <- stats::setNames(as.list(md$value), md$name)
  int <- function(v) if (is.null(v)) NULL else as.integer(v)
  tile <- function(z, x, y) {
    r <- DBI::dbGetQuery(
      con,
      "SELECT tile_data FROM tiles WHERE zoom_level = ? AND tile_column = ? AND tile_row = ?",
      params = list(as.integer(z), as.integer(x), as.integer(2^z - 1 - y))
    )
    if (!nrow(r)) NULL else as.raw(r$tile_data[[1]])
  }
  structure(
    tile,
    format = if (is.null(m$format)) "png" else m$format,
    encoding = m$encoding,
    tile_size = int(m$tile_size),
    bounds = if (!is.null(m$bounds)) as.numeric(strsplit(m$bounds, ",", fixed = TRUE)[[1]]),
    minzoom = int(m$minzoom),
    maxzoom = int(m$maxzoom),
    source = normalizePath(file)
  )
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
#' Track: each vessel's pings joined in time order, one line however long the vessel falls silent; with `max_gap_s`,
#' broken where two pings are more than that many seconds apart (`max_gap_s = 3600` at an hour). With a column, such
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
#' @param max_gap_s A silence between two of a vessel's pings longer than this many seconds breaks its line, as
#'   `3600` for an hour; `Inf`, the default, for none, so no gap is hidden.
#' @param gap_h Now `max_gap_s`, in seconds: the old name warns once a session and will be removed in the next
#'   release.
#' @param data Pings for this layer, in place of the map's.
#' @param lon,lat,time The position and time columns: by default the map's, else `"lon"`, `"lat"` and `"time"`.
#'   The track needs the time; a point's popup shows it first.
#' @param by The vessel column, a name or a string: one track each. By default the map's, else `vid` where the data
#'   have it, else one track through all the pings.
#' @param choose Columns whose values the page offers to show, a name, a string or `c()` of them: by default
#'   the groups of the pings (`dplyr::group_by()`). Set by the call that starts the map.
#' @param title,port The page's title, and where the map is served when printed: set by the call that starts it.
#' @return An [rb_map()], with the layer added.
#' @seealso [rb_map_time()] for a panel of a vessel's pings over time under the map; [rb_map()] for a map of other
#'   layers, groups and languages.
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
  # a data frame registered for the map stays with the map; one registered by a call that fails is released
  .rb_scope()
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
  # each ping's time in seconds on the time panel's clock, so a panel shows only its vessel's span (rb_map_time())
  t_s <- if (!is.null(ctx$time)) .rb_time_s_sql(con, ctx$src, ctx$time)
  pts <- dplyr::tbl(
    con,
    dplyr::sql(sprintf(
      "SELECT *%s%s FROM (%s)",
      paste(classes, collapse = ""),
      if (is.null(t_s)) "" else sprintf(", %s AS rb_t", t_s),
      ctx$src
    ))
  )
  props <- unique(c(
    col,
    ctx$time,
    popup,
    ctx$by,
    intersect(.rb_or(s$state[["choose_fields"]], character()), ctx$cols),
    if (any(is_discrete)) paste0("rb_class_", which(is_discrete)),
    if (!is.null(t_s)) "rb_t"
  ))
  if (!is.null(t_s)) {
    s$state$panel_fields[[id]] <- list(by = ctx$by, t = "rb_t")
  }
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
  .rb_hold(.rb_quick_map(
    s,
    layer,
    list(con = con, name = attr(tiles, "table"))
  ))
}

#' @rdname rb_map_pings
#' @export
rb_map_track <- function(
  x,
  col = NULL,
  max_gap_s = Inf,
  discrete = NULL,
  palette = "Inferno",
  data = NULL,
  lon = NULL,
  lat = NULL,
  time = NULL,
  by = NULL,
  choose = NULL,
  title = "Track",
  port = 8790L,
  gap_h = NULL
) {
  # as rb_map_pings(): the frames of a call that fails are released
  .rb_scope()
  if (!is.null(gap_h)) {
    .rb_renamed_arg("rb_map_track", "gap_h", "max_gap_s", "in seconds")
    max_gap_s <- if (is.numeric(gap_h)) gap_h * 3600 else gap_h
  }
  .rb_check_gap(max_gap_s)
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
  gap <- .rb_gap_sql(max_gap_s)
  # each line's first and last time in seconds on the time panel's clock, so a panel shows only its vessel's span
  t_s <- .rb_time_s_sql(con, ctx$src, ctx$time)
  # a line per vessel and stretch: a silence of more than max_gap_s starts a new one, and so does a change of any
  # column's value, so each run carries one value of each; a run ends at the next run's first fix, so the line stays
  # joined across a change
  .rb_extension(con, "spatial")
  lines <- dplyr::tbl(
    con,
    dplyr::sql(sprintf(
      "WITH s AS (
         SELECT %1$s %2$s AS lon, %3$s AS lat, %4$s AS t, %11$s AS rb_ts %8$s FROM (%7$s)
         WHERE %2$s IS NOT NULL AND %3$s IS NOT NULL AND %4$s IS NOT NULL),
       w AS (
         SELECT *, CASE WHEN epoch(t) - epoch(lag(t) OVER o) > %6$s %9$s THEN 1 ELSE 0 END AS brk,
                lead(lon) OVER o AS lon2, lead(lat) OVER o AS lat2, epoch(lead(t) OVER o) - epoch(t) AS dt_next
         FROM s WINDOW o AS (%5$s ORDER BY t)),
       g AS (SELECT *, sum(brk) OVER (%5$s ORDER BY t ROWS UNBOUNDED PRECEDING) AS stretch FROM w),
       a AS (
         SELECT %1$s %10$s count(*) AS pings, strftime(min(t), '%%Y-%%m-%%d %%H:%%M') AS start,
                strftime(max(t), '%%Y-%%m-%%d %%H:%%M') AS stop, min(rb_ts) AS rb_t0, max(rb_ts) AS rb_t1,
                list(ST_Point(lon, lat) ORDER BY t) AS pts,
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
      paste(kept, collapse = ""),
      .rb_or(t_s, "CAST(NULL AS DOUBLE)")
    ))
  )
  props <- c(
    keys,
    if (k) paste0("rb_path_", seq_len(k)),
    "pings",
    "start",
    "stop",
    if (!is.null(t_s)) c("rb_t0", "rb_t1")
  )
  if (!is.null(t_s)) {
    s$state$panel_fields[[id]] <- list(by = by, t0 = "rb_t0", t1 = "rb_t1")
  }
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
    info = paste0(
      if (is.null(by)) "The pings" else sprintf("The pings of each %s", by),
      " joined in time order",
      if (is.finite(max_gap_s)) {
        sprintf(
          ", broken where two are more than %s apart",
          .rb_duration_text(max_gap_s)
        )
      },
      if (k > 0) {
        sprintf(
          "%s coloured by %s",
          if (is.finite(max_gap_s)) ", and" else " and",
          col
        )
      },
      "."
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
  # the first track's gap, which a time panel added after it takes by default
  s$state$max_gap_s <- .rb_or(s$state$max_gap_s, max_gap_s)
  .rb_hold(.rb_quick_map(
    s,
    layer,
    list(con = con, name = attr(tiles, "table"))
  ))
}

#' A time panel under the map: one vessel's pings over time, linked to the map
#'
#' `rb_map_time()` adds a panel under a map from [rb_map_pings()] or [rb_map_track()], or starts a map with it, that
#' shows one vessel's pings over time:
#' * a column or several as lines (speed by default), the first shown and the others ticked on in the panel;
#' * marks where a column says so (a flag), on the chart and on the map;
#' * lanes of intervals under the chart (port calls, voyages, trips, records' windows), a click on one setting the
#'   window to it.
#'
#' The panel and the map are linked. A drag across the chart sets the time window, and a double click goes back to
#' the whole span. The map draws the window's track and pings, and fits to them. A hover on the chart marks that ping on
#' the map, and a click on a ping of the window marks its time on the chart. The vessel, the span and the window go into
#' the page's address.
#'
#' The page holds one vessel's span at a time: by default its whole series; with `span`, also each of its trips or other
#' intervals, chosen under *Span*. A vessel with more than `span_max` pings is offered by its spans only, or by month
#' where there are none. The map's other layers show only the chosen vessel and span, so the pings of the vessel's other
#' times and of other vessels are out of the way; a track's line that runs over a span's start or end is shown whole.
#' The window's track is drawn whole, its pings thinned to about 5,000 (every k-th, a marked one always), so the map
#' stays quick.
#'
#' Times are shown as the data hold them, in no time zone, labelled by `time_label`.
#'
#' @inheritParams rb_map_pings
#' @param col The numeric columns drawn over time, a name, a string or `c()` of them; the first shown.
#' @param marks Columns that mark pings, as `col`: a logical column marks its `TRUE` pings, any other its values that
#'   are not missing (such as a flag's label). Each column is a row of ticks along the chart's top and a ring on the
#'   map in a colour of its own, named in the sidebar; `c(truth = "wrong", clean = "flag")` names them.
#' @param lanes Intervals drawn as lanes under the chart: a named list (the names are the lanes' labels), each
#'   * a column of the pings, as a string: its runs of one value (a trip, a voyage, a phase), missing values left out
#'     and a run broken by a silence longer than `max_gap_s`;
#'   * a table of intervals with the vessel column (`by`), and its start and end in `T_in` and `T_out` (port calls,
#'     [rb_create_port_calls()]), `T1` and `T2` (voyages, [rb_create_voyages()]), `w_start` and `w_end` (records'
#'     windows) or `start` and `end`;
#'   * or `list(data = , start = , end = , label = )`, naming the columns, `label` a column that names each interval.
#' @param max_gap_s A silence between two pings longer than this many seconds breaks the panel's line, on the chart
#'   and on the map, and a lane's run of one value. By default the track's ([rb_map_track()]'s `max_gap_s`) when the
#'   map has a track already, else `Inf`: nothing broken, so no gap is hidden.
#' @param span Units the page holds one at a time, chosen besides the whole series: a column of the pings that names
#'   them, as a string (a trip, a voyage, a haul: `"trip_id"`, or `c(trip = "trip_id")` to name the list), one span per
#'   value from its first ping to its last; or a table of intervals, or `list(data = , start = , end = , label = )`, as
#'   for `lanes`, named as a column is with `list(trips = <table>)`. A span holds every ping of its vessel from its start
#'   to its end, those whose value is missing too, such
#'   as a ping a filter removed. Spans that overlap are reported: a state that comes and goes, such as behaviour,
#'   belongs in `lanes`. `NULL`, the default, offers the whole series alone.
#' @param span_max The most pings the page holds at once: a vessel with more is offered by its spans, else by month.
#'   The browser holds far more than the default is likely to meet: a series of 2.2 million pings loaded in about
#'   8 seconds, most of it the transfer.
#' @param time_label The clock the times are on, shown beside them.
#' @return An [rb_map()] with the time panel.
#' @seealso [rb_map_pings()] and [rb_map_track()], the layers it pairs with; [rb_map_bundle()], which writes the
#'   panel's series beside the page.
#' @export
#' @examples
#' \donttest{
#' d <- dplyr::filter(dplyr::collect(rb_read_example("dansk")), vessel_id %in% c("EX_1", "EX_12"))
#' m <- d |>
#'   rb_map_track(behaviour, time = "time_stamp", by = vessel_id) |>
#'   rb_map_time(speed, lanes = list(behaviour = "behaviour"))
#' format(m)
#' # m  # printed at the console, the map opens with the panel under it
#' }
rb_map_time <- function(
  x,
  col = "speed",
  marks = NULL,
  lanes = NULL,
  span = NULL,
  max_gap_s = NULL,
  span_max = 3000000,
  time_label = "UTC",
  data = NULL,
  lon = NULL,
  lat = NULL,
  time = NULL,
  by = NULL,
  title = "Track",
  port = 8790L
) {
  # as rb_map_pings(): the frames of a call that fails are released
  .rb_scope()
  s <- .rb_quick(
    x,
    data,
    lon,
    lat,
    time,
    rlang::enquo(by),
    title,
    port
  )
  ctx <- s$ctx_layer
  if (!is.null(s$state$time)) {
    stop("A map has one time panel.", call. = FALSE)
  }
  max_gap_s <- .rb_or(max_gap_s, .rb_or(s$state$max_gap_s, Inf))
  .rb_check_gap(max_gap_s)
  if (is.null(ctx$time)) {
    stop(
      "The time panel draws the pings over time: name their time column in `time`.",
      call. = FALSE
    )
  }
  # unnamed: the page's lists are arrays
  col <- unname(.rb_track_cols(rlang::enquo(col), ctx$cols))
  marks_q <- rlang::enquo(marks)
  marks <- if (rlang::quo_is_null(marks_q)) {
    character()
  } else {
    .rb_track_cols(marks_q, ctx$cols)
  }
  # a mark's label: the name it is given, c(truth = "wrong"), else its column's
  mark_names <- names(marks)
  if (is.null(mark_names)) {
    mark_names <- rep("", length(marks))
  }
  mark_labels <- ifelse(is.na(mark_names) | mark_names == "", marks, mark_names)
  marks <- unname(marks)
  mark_labels <- unname(mark_labels)
  .rb_quick_need(ctx, c(col, marks))
  if (
    !is.numeric(span_max) ||
      length(span_max) != 1 ||
      is.na(span_max) ||
      span_max < 1
  ) {
    stop("`span_max` is a number of pings.", call. = FALSE)
  }
  if (
    !is.character(time_label) || length(time_label) != 1 || is.na(time_label)
  ) {
    stop("`time_label` is a text.", call. = FALSE)
  }
  if (
    !is.null(lanes) &&
      (!is.list(lanes) || is.null(names(lanes)) || any(!nzchar(names(lanes))))
  ) {
    stop(
      "`lanes` is a named list: a column's name, a table or a list(data = ) each.",
      call. = FALSE
    )
  }
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  types <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM (%s)", ctx$src))
  type_of <- function(cc) types$column_type[match(cc, types$column_name)]
  is_num <- function(t) {
    grepl("^(U?(TINY|SMALL|BIG|HUGE)?INT(EGER)?|FLOAT|DOUBLE|REAL|DECIMAL)", t)
  }
  is_time <- function(t) grepl("^(TIMESTAMP|DATE)", t)
  not_num <- col[!is_num(type_of(col))]
  if (length(not_num)) {
    stop(
      sprintf(
        "`col` is numeric columns; %s is not.",
        paste(not_num, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is_time(type_of(ctx$time))) {
    stop(
      sprintf("The time column, %s, is not a date-time.", ctx$time),
      call. = FALSE
    )
  }
  # a time as microseconds since 1970 on its own clock: a date at its midnight, a time with a zone as its instant
  us <- function(v, t) {
    if (startsWith(t, "DATE")) {
      sprintf("epoch_us(CAST(%s AS TIMESTAMP))", v)
    } else {
      sprintf("epoch_us(%s)", v)
    }
  }
  vessel <- if (is.null(ctx$by)) {
    "'all'"
  } else {
    .rb_time_key(q(ctx$by), type_of(ctx$by))
  }
  mark_sql <- vapply(
    seq_along(marks),
    function(i) {
      if (type_of(marks[i]) == "BOOLEAN") {
        sprintf(
          ", CASE WHEN %s THEN %s END AS rb_k%d",
          q(marks[i]),
          DBI::dbQuoteString(con, marks[i]),
          i
        )
      } else {
        sprintf(", CAST(%s AS VARCHAR) AS rb_k%d", q(marks[i]), i)
      }
    },
    ""
  )
  nm <- .rb_name(con, "rb_time_")
  create <- sprintf(
    "CREATE TEMP TABLE %1$s AS
     SELECT %2$s AS rb_v, %3$s / 1e6 AS rb_t, strftime(make_timestamp(%3$s), '%%Y-%%m') AS rb_m,
            round(%4$s, 6) AS rb_lon, round(%5$s, 6) AS rb_lat %6$s %7$s
     FROM (%8$s) WHERE %9$s IS NOT NULL AND %4$s IS NOT NULL AND %5$s IS NOT NULL
     ORDER BY rb_v, rb_t",
    nm,
    vessel,
    us(q(ctx$time), type_of(ctx$time)),
    q(ctx$lon),
    q(ctx$lat),
    paste(
      sprintf(
        ", CAST(%s AS DOUBLE) AS rb_c%d",
        vapply(col, q, ""),
        seq_along(col)
      ),
      collapse = ""
    ),
    paste(mark_sql, collapse = ""),
    ctx$src,
    q(ctx$time)
  )
  # the lanes' intervals, each vessel's, in one table: the runs of a column, or a table's rows
  lane_sql <- unlist(lapply(seq_along(lanes), function(k) {
    .rb_time_lane(
      lanes[[k]],
      names(lanes)[k],
      k,
      ctx,
      vessel,
      us,
      type_of,
      max_gap_s
    )
  }))
  nm_l <- if (length(lane_sql)) .rb_name(con, "rb_lanes_")
  create_l <- if (length(lane_sql)) {
    sprintf(
      "CREATE TEMP TABLE %s AS %s",
      nm_l,
      paste(lane_sql, collapse = " UNION ALL ")
    )
  }
  # a table dropped when its map was replaced on its port (rb_map_serve()) is made again
  make <- function() {
    DBI::dbExecute(con, create)
    if (!is.null(create_l)) DBI::dbExecute(con, create_l)
  }
  make()
  # what the spans are called in the page: a column's name, or the name given it, c(trip = "trip_id")
  # a table of intervals, or list(data = ), named as a column is: list(fault = <table>)
  named_table <- is.list(span) && !is.data.frame(span) && !inherits(span, "tbl_lazy") && length(span) == 1 &&
    !is.null(names(span)) && nzchar(names(span)) && names(span) != "data" &&
    (is.data.frame(span[[1]]) || inherits(span[[1]], "tbl_lazy") || (is.list(span[[1]]) && !is.null(span[[1]]$data)))
  span_label <- if (is.character(span)) {
    if (!is.null(names(span)) && nzchar(names(span)[1])) names(span)[1] else span[1]
  } else if (named_table) {
    names(span)
  } else if (!is.null(span)) {
    "Span"
  }
  if (is.character(span)) {
    span <- unname(span)
  }
  if (named_table) {
    span <- span[[1]]
  }
  # the spans (`span`): each vessel's intervals in time order, numbered from 1, with the pings each holds; an interval
  # that holds none is left out. Kept here, where a span's series finds its start and end. A column gives one span per
  # value, from its first ping to its last: a trip whose id is missing on a ping, as on one a speed filter removed, stays
  # one span, and that ping is in it to be seen
  span_tab <- if (!is.null(span)) {
    sql_s <- if (is.character(span)) {
      if (length(span) != 1 || !span %in% ctx$cols) {
        stop(
          sprintf("`span`: no column %s in the pings.", paste(span, collapse = ", ")),
          call. = FALSE
        )
      }
      sprintf(
        "(SELECT %1$s AS rb_v, min(%2$s) / 1e6 AS rb_t0, max(%2$s) / 1e6 AS rb_t1, CAST(%3$s AS VARCHAR) AS rb_label
          FROM (%4$s) WHERE %5$s IS NOT NULL AND %3$s IS NOT NULL GROUP BY 1, 4)",
        vessel,
        us(q(ctx$time), type_of(ctx$time)),
        q(span),
        ctx$src,
        q(ctx$time)
      )
    } else {
      .rb_time_lane(
        span,
        "span",
        0L,
        ctx,
        vessel,
        us,
        type_of,
        Inf,
        what = "`span`"
      )
    }
    sp <- DBI::dbGetQuery(
      con,
      sprintf(
        "WITH s AS (SELECT rb_v, rb_t0, rb_t1, rb_label FROM %s WHERE rb_t1 >= rb_t0)
         SELECT s.rb_v, s.rb_t0, s.rb_t1, any_value(s.rb_label) AS rb_label, count(*) AS rb_n
         FROM s JOIN %s p ON p.rb_v = s.rb_v AND p.rb_t BETWEEN s.rb_t0 AND s.rb_t1
         GROUP BY s.rb_v, s.rb_t0, s.rb_t1 ORDER BY s.rb_v, s.rb_t0, s.rb_t1",
        sql_s,
        nm
      )
    )
    sp$rb_k <- stats::ave(seq_len(nrow(sp)), sp$rb_v, FUN = seq_along)
    # a unit's spans do not overlap; a state's do (Fishing from its first ping to its last, over Other), and so do two
    # trips that claim the same hours: said, not refused
    over <- sum(unlist(lapply(split(sp, sp$rb_v), function(x) {
      if (nrow(x) < 2) {
        return(0L)
      }
      ends <- cummax(x$rb_t1)
      sum(x$rb_t0[-1] < ends[-nrow(x)])
    })))
    if (over > 0) {
      message(sprintf(
        "%s of %s spans of %s begin before an earlier one has ended. A span is a unit such as a trip or a voyage; a state that comes and goes, such as behaviour, belongs in `lanes`.",
        format(over, big.mark = ","),
        format(nrow(sp), big.mark = ","),
        span_label
      ))
    }
    sp
  }
  vs <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT rb_v AS v, count(*) AS n, min(rb_t) AS t0, max(rb_t) AS t1 FROM %s GROUP BY 1",
      nm
    )
  )
  if (!nrow(vs)) {
    stop("No ping has a time and a position.", call. = FALSE)
  }
  # vessels in their order as numbers where every one is a number, else in natural order (EX_2 before EX_10)
  num_v <- suppressWarnings(as.numeric(vs$v))
  vs <- vs[if (anyNA(num_v)) order(.rb_natural_key(vs$v)) else order(num_v), ]
  months <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT rb_v AS v, rb_m AS m, count(*) AS n FROM %s GROUP BY ALL ORDER BY 1, 2",
      nm
    )
  )
  fmt_n <- function(n) format(n, big.mark = ",", trim = TRUE)
  # a span's name in the list: its label where it has one, and its start on the data's own clock
  span_name <- function(label, t0) {
    start <- format(
      as.POSIXct(t0, origin = "1970-01-01", tz = "UTC"),
      "%Y-%m-%d %H:%M"
    )
    if (is.na(label)) start else sprintf("%s · %s", label, start)
  }
  # each vessel's spans: the whole series where it holds no more than span_max, then its own spans (`span`), else, for
  # a vessel too long to hold whole, its months
  vessels <- lapply(seq_len(nrow(vs)), function(j) {
    whole <- vs$n[j] <= span_max
    sj <- if (!is.null(span_tab)) span_tab[span_tab$rb_v == vs$v[j], ]
    own <- if (!is.null(sj) && nrow(sj)) {
      lapply(seq_len(nrow(sj)), function(i) {
        nmi <- span_name(sj$rb_label[i], sj$rb_t0[i])
        list(
          value = sprintf("s%d", sj$rb_k[i]),
          name = nmi,
          label = sprintf("%s (%s)", nmi, fmt_n(sj$rb_n[i])),
          n = sj$rb_n[i]
        )
      })
    }
    by_month <- if (!whole && !length(own)) {
      mj <- months[months$v == vs$v[j], ]
      lapply(seq_len(nrow(mj)), function(i) {
        list(
          value = mj$m[i],
          label = sprintf("%s (%s)", mj$m[i], fmt_n(mj$n[i])),
          n = mj$n[i]
        )
      })
    }
    spans <- c(
      if (whole) {
        list(list(
          value = "all",
          label = sprintf("whole (%s)", fmt_n(vs$n[j])),
          n = vs$n[j]
        ))
      },
      own,
      by_month
    )
    list(
      value = vs$v[j],
      label = sprintf("%s (%s)", vs$v[j], fmt_n(vs$n[j])),
      n = vs$n[j],
      t0 = vs$t0[j],
      t1 = vs$t1[j],
      # the whole series where it is held, else the latest span
      span = if (whole) "all" else spans[[length(spans)]]$value,
      spans = spans
    )
  })
  series <- function(v, span) {
    if (!DBI::dbExistsTable(con, nm)) {
      make()
    }
    where <- sprintf(
      "rb_v = %s%s",
      DBI::dbQuoteString(con, v),
      if (identical(span, "all")) {
        ""
      } else if (grepl("^s[0-9]+$", span) && !is.null(span_tab)) {
        # one of the vessel's own spans: its pings from its start to its end, as numbers DuckDB reads as DOUBLE
        sk <- span_tab[
          span_tab$rb_v == v & span_tab$rb_k == as.integer(substring(span, 2)),
        ]
        sprintf(
          " AND rb_t BETWEEN %s AND %s",
          sprintf("%.17e", sk$rb_t0[1]),
          sprintf("%.17e", sk$rb_t1[1])
        )
      } else {
        sprintf(" AND rb_m = %s", DBI::dbQuoteString(con, span))
      }
    )
    r <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT rb_t, rb_lon, rb_lat%s%s FROM %s WHERE %s ORDER BY rb_t",
        paste(sprintf(", rb_c%d", seq_along(col)), collapse = ""),
        paste(sprintf(", rb_k%d", seq_along(marks)), collapse = ""),
        nm,
        where
      )
    )
    # I() keeps a span of one ping an array in JSON
    out <- list(
      t = I(r$rb_t),
      lon = I(r$rb_lon),
      lat = I(r$rb_lat),
      y = stats::setNames(
        lapply(seq_along(col), function(i) {
          I(signif(r[[paste0("rb_c", i)]], 6))
        }),
        col
      ),
      marks = stats::setNames(
        lapply(seq_along(marks), function(i) {
          k <- r[[paste0("rb_k", i)]]
          w <- which(!is.na(k))
          list(i = I(w - 1L), v = I(k[w]))
        }),
        marks
      )
    )
    if (length(marks) == 0) {
      out$marks <- stats::setNames(list(), character())
    }
    out$lanes <- stats::setNames(list(), character())
    if (!is.null(nm_l) && nrow(r)) {
      l <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT rb_lane, rb_t0, rb_t1, rb_label FROM %s WHERE rb_v = %s AND rb_t1 >= %s AND rb_t0 <= %s ORDER BY rb_lane, rb_t0",
          nm_l,
          DBI::dbQuoteString(con, v),
          format(min(r$rb_t), digits = 15),
          format(max(r$rb_t), digits = 15)
        )
      )
      out$lanes <- stats::setNames(
        lapply(seq_along(lanes), function(k) {
          lk <- l[l$rb_lane == k, ]
          list(t0 = I(lk$rb_t0), t1 = I(lk$rb_t1), label = I(lk$rb_label))
        }),
        as.character(seq_along(lanes))
      )
    }
    out
  }
  s$state$time <- list(
    config = list(
      by = ctx$by,
      time_label = time_label,
      span_max = span_max,
      span_label = span_label,
      # none for Inf, which JSON cannot hold: the page breaks the line nowhere
      max_gap_s = if (is.finite(max_gap_s)) max_gap_s,
      cols = lapply(col, function(cc) list(field = cc, label = cc)),
      marks = lapply(seq_along(marks), function(k) list(field = marks[k], label = mark_labels[k])),
      lanes = lapply(seq_along(lanes), function(k) {
        list(id = as.character(k), label = names(lanes)[k])
      }),
      vessels = vessels
    ),
    series = series,
    tables = c(
      list(list(con = con, name = nm)),
      if (!is.null(nm_l)) list(list(con = con, name = nm_l))
    )
  )
  .rb_hold(.rb_quick_map(s))
}

# A time column as seconds since 1970 on its own clock, as the time panel holds its pings (rb_map_time()'s `us`): a
# date at its midnight, a time with a zone as its instant; NULL for a column that is not a date-time
.rb_time_s_sql <- function(con, src, col) {
  qc <- as.character(DBI::dbQuoteIdentifier(con, col))
  ty <- DBI::dbGetQuery(
    con,
    sprintf("DESCRIBE SELECT %s FROM (%s)", qc, src)
  )$column_type[1]
  if (!grepl("^(TIMESTAMP|DATE)", ty)) {
    return(NULL)
  }
  if (startsWith(ty, "DATE")) {
    sprintf("epoch_us(CAST(%s AS TIMESTAMP)) / 1e6", qc)
  } else {
    sprintf("epoch_us(%s) / 1e6", qc)
  }
}

# The gap that breaks a line, in seconds: a positive number, Inf for none
.rb_check_gap <- function(max_gap_s) {
  if (
    !is.numeric(max_gap_s) ||
      length(max_gap_s) != 1 ||
      is.na(max_gap_s) ||
      max_gap_s <= 0
  ) {
    stop(
      "`max_gap_s` is a number of seconds, as 3600 for an hour, or Inf for no break.",
      call. = FALSE
    )
  }
  invisible(max_gap_s)
}

# The gap as an SQL number of seconds; Inf as DuckDB's infinity, which no silence exceeds
.rb_gap_sql <- function(max_gap_s) {
  if (is.finite(max_gap_s)) {
    format(max_gap_s, scientific = FALSE)
  } else {
    "'infinity'::DOUBLE"
  }
}

# A number of seconds in words for a page's text: whole hours, minutes or seconds where it is one, else hours
.rb_duration_text <- function(s) {
  unit <- function(n, word) {
    sprintf("%s %s%s", format(n), word, if (n == 1) "" else "s")
  }
  if (s %% 3600 == 0) {
    unit(s / 3600, "hour")
  } else if (s >= 3600) {
    unit(signif(s / 3600, 3), "hour")
  } else if (s %% 60 == 0) {
    unit(s / 60, "minute")
  } else {
    unit(signif(s, 3), "second")
  }
}

# A text whose runs of digits are padded with zeros, so it sorts in natural order: EX_2 before EX_10
.rb_natural_key <- function(x) {
  m <- gregexpr("[0-9]+", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(d) formatC(d, width = 20, flag = "0"))
  x
}

# A vessel's key as text, the same from the pings and from a lane's table: a whole number written without decimals,
# whether it is stored as an integer or a double
.rb_time_key <- function(v, type) {
  if (grepl("^(FLOAT|DOUBLE|REAL|DECIMAL)", type)) {
    sprintf(
      "CASE WHEN %1$s = trunc(%1$s) THEN CAST(CAST(%1$s AS HUGEINT) AS VARCHAR) ELSE CAST(%1$s AS VARCHAR) END",
      v
    )
  } else {
    sprintf("CAST(%s AS VARCHAR)", v)
  }
}

# One lane's intervals as SQL: rb_v, rb_lane (its number), rb_t0 and rb_t1 (seconds on the pings' clock), rb_label
.rb_time_lane <- function(
  lane,
  name,
  k,
  ctx,
  vessel,
  us,
  type_of,
  max_gap_s,
  what = sprintf("Lane %s", name)
) {
  con <- ctx$con
  q <- function(v) as.character(DBI::dbQuoteIdentifier(con, v))
  if (is.character(lane)) {
    if (length(lane) != 1 || !lane %in% ctx$cols) {
      stop(
        sprintf(
          "%s: no column %s in the pings.",
          what,
          paste(lane, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    # the runs of one value, each vessel's in time order, broken by a silence longer than max_gap_s as the page's
    # line is
    return(sprintf(
      "(WITH s AS (SELECT %1$s AS rb_v, %2$s / 1e6 AS t, CAST(%3$s AS VARCHAR) AS val FROM (%4$s) WHERE %5$s IS NOT NULL),
             w AS (SELECT *, CASE WHEN val IS DISTINCT FROM lag(val) OVER o OR t - lag(t) OVER o > %7$s THEN 1 ELSE 0 END AS chg
                   FROM s WINDOW o AS (PARTITION BY rb_v ORDER BY t)),
             g AS (SELECT *, sum(chg) OVER (PARTITION BY rb_v ORDER BY t ROWS UNBOUNDED PRECEDING) AS run FROM w)
        SELECT rb_v, %6$d AS rb_lane, min(t) AS rb_t0, max(t) AS rb_t1, any_value(val) AS rb_label
        FROM g WHERE val IS NOT NULL GROUP BY rb_v, run)",
      vessel,
      us(q(ctx$time), type_of(ctx$time)),
      q(lane),
      ctx$src,
      q(ctx$time),
      k,
      .rb_gap_sql(max_gap_s)
    ))
  }
  spec <- if (is.data.frame(lane) || inherits(lane, "tbl_lazy")) {
    list(data = lane)
  } else {
    lane
  }
  if (!is.list(spec) || is.null(spec$data)) {
    stop(
      sprintf(
        "%s: a column's name, a table, or list(data = , start = , end = ).",
        what
      ),
      call. = FALSE
    )
  }
  x <- dplyr::ungroup(.rb_as_lazy(spec$data, con))
  cols <- colnames(x)
  pairs <- list(
    c("T_in", "T_out"),
    c("T1", "T2"),
    c("w_start", "w_end"),
    c("start", "end")
  )
  se <- if (!is.null(spec$start) && !is.null(spec$end)) {
    c(spec$start, spec$end)
  } else {
    hit <- Filter(function(p) all(p %in% cols), pairs)
    if (!length(hit)) {
      stop(
        sprintf(
          "%s: no start and end; name them (`start`, `end`) or use T_in and T_out, T1 and T2, w_start and w_end, or start and end.",
          what
        ),
        call. = FALSE
      )
    }
    hit[[1]]
  }
  need <- c(se, ctx$by, spec$label)
  if (length(setdiff(need, cols))) {
    stop(
      sprintf(
        "%s: no column %s in its table.",
        what,
        paste(setdiff(need, cols), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  src <- .rb_sql(x)
  types <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM (%s)", src))
  ty <- function(cc) types$column_type[match(cc, types$column_name)]
  if (!all(grepl("^(TIMESTAMP|DATE)", ty(se)))) {
    stop(
      sprintf("%s: %s and %s are date-times.", what, se[1], se[2]),
      call. = FALSE
    )
  }
  sprintf(
    "(SELECT %s AS rb_v, %d AS rb_lane, %s / 1e6 AS rb_t0, %s / 1e6 AS rb_t1, %s AS rb_label
      FROM (%s) WHERE %s IS NOT NULL AND %s IS NOT NULL)",
    if (is.null(ctx$by)) "'all'" else .rb_time_key(q(ctx$by), ty(ctx$by)),
    k,
    us(q(se[1]), ty(se[1])),
    us(q(se[2]), ty(se[2])),
    if (is.null(spec$label)) {
      "CAST(NULL AS VARCHAR)"
    } else {
      sprintf("CAST(%s AS VARCHAR)", q(spec$label))
    },
    src,
    q(se[1]),
    q(se[2])
  )
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
  tp <- x$config$time
  c(
    sprintf("<rb_map> %s", .rb_or(x$config$title, "")),
    sprintf("  layers: %s", paste(layers, collapse = ", ")),
    if (!is.null(tp)) {
      sprintf(
        "  time panel: %s over time, %d vessel%s%s%s",
        paste(vapply(tp$cols, `[[`, "", "field"), collapse = ", "),
        length(tp$vessels),
        if (length(tp$vessels) == 1) "" else "s",
        if (length(tp$lanes)) {
          sprintf("; lanes: %s", paste(vapply(tp$lanes, `[[`, "", "label"), collapse = ", "))
        } else {
          ""
        },
        if (!is.null(tp$span_label)) {
          sprintf(
            "; spans: %s (%s)",
            tp$span_label,
            format(
              sum(vapply(tp$vessels, function(v) sum(vapply(v$spans, `[[`, "", "value") != "all"), 1)),
              big.mark = ","
            )
          )
        } else {
          ""
        }
      )
    }
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
.rb_quick_map <- function(s, layer = NULL, table = NULL) {
  state <- s$state
  if (!is.null(layer)) {
    state$layer_fields[[layer$config$id]] <- intersect(
      .rb_or(state[["choose_fields"]], character()),
      s$ctx_layer$cols
    )
    state$layers <- c(state$layers, list(layer))
    state$tables <- c(state$tables, list(table))
  }
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
  # the time panel (rb_map_time()): its config for the page, its series for the server and its tables
  if (!is.null(state$time)) {
    m$config$time <- state$time$config
    # the layers whose features carry a vessel and a time: the page shows on them only the panel's vessel and span
    m$config$time$layers <- unname(lapply(names(state$panel_fields), function(id) {
      c(list(id = id), Filter(Negate(is.null), state$panel_fields[[id]]))
    }))
    m$time <- state$time
  }
  attr(m, "quick") <- state
  attr(m, "tables") <- c(state$tables, state$time$tables)
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

.rb_map_json <- function(x) {
  as.character(jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  ))
}

# The page with its config, and a bundle's own (rb_map_bundle()) or none; "</" escaped, so neither can close its tag
.rb_map_page <- function(config, bundle = "null") {
  page <- paste(
    readLines(
      system.file("map", "index.html", package = "ramb"),
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  page <- sub("__CONFIG__", gsub("</", "<\\/", config, fixed = TRUE), page, fixed = TRUE)
  sub("__BUNDLE__", gsub("</", "<\\/", bundle, fixed = TRUE), page, fixed = TRUE)
}

# Each layer's meta. A vector layer's tiles carry the server's start in their address, so a server started on other
# data on the same port is not hidden by the browser's cache (a grid carries its meta's `v`)
.rb_map_metas <- function(layers) {
  started <- format(as.numeric(Sys.time()), digits = 15)
  lapply(layers, function(l) {
    switch(
      l$config$kind,
      grid = if (is.function(l$meta)) l$meta() else l$meta,
      dem = list(count = 1L, v = started),
      contour = list(count = 1L, v = started),
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
}

# A geojson layer's data as GeoJSON text
.rb_map_geojson <- function(l) {
  d <- if (is.function(l$data)) l$data() else l$data
  if (inherits(d, "sf")) {
    f <- tempfile(fileext = ".geojson")
    on.exit(unlink(f))
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
}

# The app behind rb_map_serve(), a list with `call(req)` for httpuv: its routes in the header of this file
.rb_map_app <- function(map, notes = NULL, host = "127.0.0.1", verdicts = NULL) {
  json <- .rb_map_json
  if (!is.null(notes)) {
    map$config$notes <- list(file = basename(notes))
  }
  specs <- .rb_map_verdict_layers(map)
  if (!is.null(verdicts)) {
    map$config$verdicts <- list(file = basename(verdicts))
  }
  hosts <- unique(tolower(c("localhost", "127.0.0.1", "[::1]", host)))
  config <- json(map$config)
  page <- .rb_map_page(config)
  layers <- map$layers
  metas <- .rb_map_metas(layers)
  geojson <- lapply(layers, function(l) {
    if (l$config$kind != "geojson") {
      return(NULL)
    }
    .rb_map_geojson(l)
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
    if (!is.null(notes) && (path == "/notes" || startsWith(path, "/notes/"))) {
      return(.rb_map_notes_reply(req, notes, hosts, reply, json))
    }
    if (!is.null(verdicts) && path == "/verdicts") {
      return(.rb_map_verdicts_reply(req, verdicts, specs, hosts, reply, json))
    }
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
    if (path == "/time") {
      return(.rb_map_time_reply(
        map$time,
        .rb_map_query(req$QUERY_STRING),
        reply,
        json
      ))
    }
    if (path == "/meta") {
      return(reply(
        200L,
        meta,
        "application/json; charset=utf-8",
        list(`Cache-Control` = "no-store")
      ))
    }
    f <- regmatches(path, regexec("^/fit/([a-z][a-z0-9_]*)$", path))[[1]]
    if (length(f) == 2) {
      fit <- layers[[f[2]]]$fit
      if (!is.function(fit)) {
        return(reply(404L, "no fit for this layer"))
      }
      b <- suppressWarnings(as.numeric(strsplit(
        .rb_or(.rb_map_query(req$QUERY_STRING)$bbox, ""),
        ",",
        fixed = TRUE
      )[[1]]))
      if (length(b) != 4 || anyNA(b) || b[1] >= b[3] || b[2] >= b[4] || any(abs(b[c(2, 4)]) > 90)) {
        return(reply(400L, "bbox is west,south,east,north"))
      }
      v <- tryCatch(fit(b[1], b[2], b[3], b[4]), error = function(e) e)
      if (inherits(v, "error")) {
        return(reply(500L, sub("\n.*", "", conditionMessage(v))))
      }
      # c(floor, cap), or a list of any of `floor` and `cap`, `quantiles` (rising, for colours in equal shares and
      # contour levels) and `exaggeration` (0-1, a shading's intensity)
      if (is.null(v)) {
        return(reply(404L, "no values in view"))
      }
      if (!is.list(v)) {
        v <- if (length(v) == 2) list(floor = v[[1]], cap = v[[2]]) else list()
      }
      fc <- c(v$floor, v$cap)
      q <- v$quantiles
      ex <- v$exaggeration
      if ((length(fc) && (length(fc) != 2 || anyNA(fc))) || (!length(fc) && is.null(q) && is.null(ex))) {
        return(reply(404L, "no values in view"))
      }
      if (!is.null(q) && (!is.numeric(q) || anyNA(q) || is.unsorted(q))) {
        return(reply(500L, "the fit's quantiles are not rising numbers"))
      }
      if (!is.null(ex) && (!is.numeric(ex) || length(ex) != 1 || is.na(ex) || ex < 0 || ex > 1)) {
        return(reply(500L, "the fit's exaggeration is not a number from 0 to 1"))
      }
      return(reply(
        200L,
        json(Filter(Negate(is.null), list(floor = v$floor, cap = v$cap, q = if (!is.null(q)) as.list(q), exaggeration = ex))),
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
        "^/tiles/([a-z][a-z0-9_]*)/([0-9]+)/([0-9]+)/([0-9]+)[.](pbf|png|webp)$",
        path
      )
    )[[1]]
    if (
      length(m) != 6 ||
        is.null(layers[[m[2]]]) ||
        layers[[m[2]]]$config$kind == "geojson" ||
        m[6] !=
          (if (layers[[m[2]]]$config$kind %in% c("dem", "contour")) {
            layers[[m[2]]]$config$format
          } else {
            "pbf"
          })
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
    if (l$config$kind %in% c("dem", "contour")) {
      return(.rb_map_tile_reply(
        function() l$tiles(z, x, y),
        reply,
        paste0("image/", l$config$format)
      ))
    }
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

# The time panel's series of one vessel's span, its vessel and span checked against the panel's first
.rb_map_time_reply <- function(tp, qs, reply, json) {
  if (is.null(tp)) {
    return(reply(404L, "this map has no time panel"))
  }
  vs <- tp$config$vessels
  i <- match(.rb_or(qs$v, NA_character_), vapply(vs, `[[`, "", "value"))
  if (is.na(i)) {
    return(reply(400L, "v is one of the panel's vessels"))
  }
  spans <- vapply(vs[[i]]$spans, `[[`, "", "value")
  if (!.rb_or(qs$span, "") %in% spans) {
    return(reply(
      400L,
      sprintf("span is one of %s", paste(spans, collapse = ", "))
    ))
  }
  body <- tryCatch(
    json(tp$series(vs[[i]]$value, qs$span)),
    error = function(e) e
  )
  if (inherits(body, "error")) {
    return(reply(500L, sub("\n.*", "", conditionMessage(body))))
  }
  reply(
    200L,
    body,
    "application/json; charset=utf-8",
    list(`Cache-Control` = "no-store")
  )
}

# A tile, or the first line of why its function failed: the page shows the reason, where the map would stay blank
.rb_map_tile_reply <- function(
  tile,
  reply,
  type = "application/vnd.mapbox-vector-tile"
) {
  body <- tryCatch(tile(), error = function(e) e)
  if (inherits(body, "error")) {
    return(reply(500L, sub("\n.*", "", conditionMessage(body))))
  }
  # an image that is not there: an empty one is no image the page can decode
  if (is.null(body) && startsWith(type, "image/")) {
    return(reply(404L, "no tile"))
  }
  reply(
    200L,
    if (is.null(body)) raw(0) else body,
    type,
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
