# flag_ping.R — step 0 of the fishing-activity flow: flag pings that are not usable positions.
#
# The three flaggers share one label column, `ping_flag`: NA for a usable ping, otherwise the reason
# ("invalid", "duplicate", or the impossible-position stage that caught it). Each tests only rows not yet
# flagged, so a chain applies them in order:
#
#   pings |> rb_flag_ping_invalid() |> rb_flag_ping_duplicate(priority = ...) |> rb_flag_ping_impossible()
#
# No row is dropped; the caller decides what to set aside (fishycode plan 013).

# Column arguments ---------------------------------------------------------------
# The functions work on the grammar's names (vid, time, lon, lat). A caller with other names passes them
# (`vid = vessel_id`); they are renamed in, and back out on the result.
.rb_cols <- function(...) {
  q <- rlang::enquos(...)
  vapply(q, rlang::as_name, character(1))
}
.rb_std_in <- function(x, map) {
  ren <- map[map != names(map)]
  if (!length(ren)) return(x)
  clash <- intersect(names(ren), colnames(x))
  if (length(clash)) stop("Column(s) ", paste(clash, collapse = ", "), " already exist; rename them first.",
                          call. = FALSE)
  dplyr::rename(x, !!!rlang::syms(ren))
}
.rb_std_out <- function(x, map) {
  ren <- map[map != names(map)]
  if (!length(ren)) return(x)
  dplyr::rename(x, !!!stats::setNames(rlang::syms(names(ren)), ren))
}
.rb_flag_init <- function(x) {
  if ("ping_flag" %in% colnames(x)) x else dplyr::mutate(x, ping_flag = NA_character_)
}

# Lazy tables: label in batches of whole vessels, join the labels back -------------------
# For rules that are sequential along a track and so cannot run in DuckDB itself. Each batch is
# collected, labelled in R by `fun` (a data-frame function that updates `ping_flag`), and its labels are
# written to a temporary parquet file; the result is the input joined to those files, still lazy. A row is
# found again by its number within its vessel, in `order_by` order, numbered the same way in DuckDB and
# in the batch. The files live until the R session ends.
.rb_label_lazy <- function(x, fun, cols, order_by, batch_pings = 5e6, what = "label") {
  con <- dbplyr::remote_con(x)
  ob <- rlang::syms(order_by)
  keyed <- x |>
    dplyr::group_by(vid) |>
    dbplyr::window_order(!!!ob) |>
    dplyr::mutate(.rb_rn = dplyr::row_number()) |>
    dplyr::ungroup()
  vn <- x |> dplyr::count(vid) |> dplyr::collect() |> dplyr::arrange(dplyr::desc(n))
  if (!nrow(vn)) stop(what, ": the table has no vessels.", call. = FALSE)
  batches <- split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  dir <- tempfile("rb_labels_")
  dir.create(dir)
  for (i in seq_along(batches)) {
    g <- batches[[i]]
    d <- x |>
      dplyr::filter(vid %in% g) |>
      dplyr::select(dplyr::all_of(unique(c("vid", cols, "ping_flag")))) |>
      dplyr::group_by(vid) |>
      dbplyr::window_order(!!!ob) |>
      dplyr::mutate(.rb_rn = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      dplyr::arrange(vid, !!!ob)
    lab <- fun(d)[, c("vid", ".rb_rn", "ping_flag")]
    lab$.rb_rn <- as.numeric(lab$.rb_rn)
    # Register, write with COPY on the same connection, release: a frame left registered in a loop
    # accumulates (fishycode R/write_parquet.R: 48 GB spilled before that was found).
    nm <- paste0("rb_lab_", i)
    duckdb::duckdb_register(con, nm, lab)
    DBI::dbExecute(con, sprintf("COPY %s TO '%s' (FORMAT parquet)", nm, file.path(dir, sprintf("b%05d.parquet", i))))
    duckdb::duckdb_unregister(con, nm)
  }
  labels <- dplyr::tbl(con, dplyr::sql(sprintf("SELECT * FROM read_parquet('%s/*.parquet')", dir)))
  keyed |>
    dplyr::select(-"ping_flag") |>
    dplyr::left_join(labels, by = c("vid", ".rb_rn")) |>
    dplyr::select(-".rb_rn") |>
    dbplyr::window_order()
}

#' Flag invalid ping positions
#'
#' Step 0 of the fishing-activity flow. A position that is a placeholder, not a fix, is flagged
#' `"invalid"` in `ping_flag`, unless the row already carries a flag. No row is dropped.
#'
#' @param x A data frame or a lazy DuckDB table with `lon` and `lat`.
#' @param rules Which tests to apply: `"lon_eq_lat"` (longitude equal to latitude, which also catches
#'   0, 0; a feed that wrote the latitude into the longitude) and `"out_of_range"` (|lon| > 180 or
#'   |lat| > 90).
#' @param lon,lat The position columns, if not named `lon` and `lat`.
#'
#' @return `x` with `ping_flag` added or updated: the same type, the same rows.
#' @family flag pings
#' @export
#'
#' @examples
#' d <- data.frame(lon = c(-20, 64.1, 0, 200), lat = c(64, 64.1, 0, 64))
#' rb_flag_ping_invalid(d)
rb_flag_ping_invalid <- function(x, rules = c("lon_eq_lat", "out_of_range"), lon = lon, lat = lat) {
  rules <- match.arg(rules, several.ok = TRUE)
  map <- .rb_cols(lon = {{ lon }}, lat = {{ lat }})
  x <- .rb_flag_init(.rb_std_in(x, map))
  eq <- "lon_eq_lat" %in% rules
  oor <- "out_of_range" %in% rules
  x <- dplyr::mutate(x, ping_flag = dplyr::case_when(
    !is.na(ping_flag) ~ ping_flag,
    !!eq & lon == lat ~ "invalid",
    !!oor & (abs(lon) > 180 | abs(lat) > 90) ~ "invalid",
    TRUE ~ NA_character_))
  .rb_std_out(x, map)
}

#' Flag a second feed's copy of a ping
#'
#' Step 0 of the fishing-activity flow. When several feeds (e.g. a national AIS network and a commercial
#' one) report the same fix, the lower-priority feed's copy is flagged `"duplicate"` in `ping_flag`: a
#' ping of the same vessel from a higher-priority feed lies within `max_dt_s` seconds and the distance
#' `kn_max` covers in that time, among the `n` neighbours on each side in time order. Only rows not yet
#' flagged are tested, and they are not used as anchors. No row is dropped.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon`, `lat` and the feed column.
#' @param priority The feeds, highest priority first. A feed not listed ranks last. No default: the
#'   order is a judgement about the feeds.
#' @param feed The column naming the feed.
#' @param max_dt_s,kn_max,n Seconds apart, speed in knots that sets the distance, neighbours compared.
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` added or updated: the same type, the same rows.
#' @family flag pings
#' @export
rb_flag_ping_duplicate <- function(x, priority, feed = provider, max_dt_s = 10, kn_max = 25, n = 3,
                                   vid = vid, time = time, lon = lon, lat = lat, batch_pings = 5e6) {
  if (missing(priority)) stop("`priority` is required: the feeds, highest priority first.", call. = FALSE)
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }}, lon = {{ lon }}, lat = {{ lat }}, feed = {{ feed }})
  x <- .rb_flag_init(.rb_std_in(x, map))
  fun <- function(d) {
    i <- which(is.na(d$ping_flag))
    if (length(i)) {
      dup <- rb_whack_duplicates(d$vid[i], d$time[i], d$lon[i], d$lat[i], d$feed[i], priority = priority,
                                 max_dt_s = max_dt_s, kn_max = kn_max, n = n)
      d$ping_flag[i[dup]] <- "duplicate"
    }
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(x, fun, cols = c("time", "lon", "lat", "feed"), order_by = c("time", "lon", "lat", "feed"),
                   batch_pings = batch_pings, what = "rb_flag_ping_duplicate()")
  } else {
    fun(x)
  }
  .rb_std_out(x, map)
}

#' Flag positions no vessel could have reached
#'
#' Step 0 of the fishing-activity flow. A position or jump that implies an impossible speed is flagged
#' in `ping_flag` with the stage that caught it. Only rows not yet flagged are tested, so invalid and
#' duplicate pings never serve as anchors. No row is dropped.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon` and `lat`.
#' @param method The rule:
#'   * `"clean"` (default): the recommended two-stage filter: speed-distance-angle ("vmask", "spike"),
#'     then a forward scan against the last kept ping ("forward"). See [rb_whack_clean()].
#'   * `"sda"`: the speed-distance-angle stage alone ("vmask", "spike"). See [rb_whack_sda()].
#'   * `"forward"`: the forward scan alone ("forward"). See [rb_whack_forward()].
#'   * `"fwdbwd"`: a ping too fast both from the previous and to the next ("fwdbwd"). See [rb_whack_fwdbwd()].
#'   * `"sequential"`: the cluster-aware sequential filter ("sequential"). See [rb_whack_sequential_fast()].
#' @param kn_max Speed threshold, knots.
#' @param min_dt_s A time step shorter than this counts as this many seconds.
#' @param max_gap_h For `"clean"` and `"forward"`: after a gap longer than this the scan restarts.
#' @param ... Passed to the method (e.g. `ang`, `distlim` for `"sda"`).
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` added or updated: the same type, the same rows.
#' @family flag pings
#' @export
rb_flag_ping_impossible <- function(x, method = c("clean", "sda", "forward", "fwdbwd", "sequential"),
                                    kn_max = 25, min_dt_s = 10, max_gap_h = 4, ...,
                                    vid = vid, time = time, lon = lon, lat = lat, batch_pings = 5e6) {
  method <- match.arg(method)
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }}, lon = {{ lon }}, lat = {{ lat }})
  x <- .rb_flag_init(.rb_std_in(x, map))
  dots <- list(...)
  fun <- function(d) {
    i <- which(is.na(d$ping_flag))
    if (!length(i)) return(d)
    s <- dplyr::ungroup(d[i, setdiff(names(d), "ping_flag"), drop = FALSE])
    s$.rb_i <- i
    stage <- switch(method,
      clean = {
        o <- do.call(.whack_clean_df, c(list(s, kn_max = kn_max, max_gap_h = max_gap_h, min_dt_s = min_dt_s), dots))
        stats::setNames(o$whack_stage, o$.rb_i)
      },
      sda = {
        o <- do.call(rb_whack_sda, c(list(s, kn_max = kn_max, min_dt_s = min_dt_s), dots))
        stats::setNames(o$whack_sda, o$.rb_i)
      },
      forward = {
        o <- rb_whack_forward(s, kn_max = kn_max, max_gap_h = max_gap_h, min_dt_s = min_dt_s)
        stats::setNames(ifelse(o$whack2, "forward", NA_character_), o$.rb_i)
      },
      fwdbwd = {
        o <- rb_whack_fwdbwd(s, kn_max = kn_max, min_dt_s = min_dt_s)
        stats::setNames(ifelse(o$whack, "fwdbwd", NA_character_), o$.rb_i)
      },
      sequential = {
        o <- s |>
          dplyr::arrange(vid, time) |>
          dplyr::group_by(vid) |>
          dplyr::mutate(.w = rb_whack_sequential_fast(lon, lat, time, kn_max = kn_max, min_dt_s = min_dt_s)) |>
          dplyr::ungroup()
        stats::setNames(ifelse(o$.w, "sequential", NA_character_), o$.rb_i)
      })
    d$ping_flag[as.integer(names(stage))] <- unname(stage)
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(x, fun, cols = c("time", "lon", "lat"), order_by = c("time", "lon", "lat"),
                   batch_pings = batch_pings, what = "rb_flag_ping_impossible()")
  } else {
    fun(x)
  }
  .rb_std_out(x, map)
}

#' Tag pings with the port polygon they lie in
#'
#' Step 0 of the fishing-activity flow: each ping gets the id of the port polygon that holds it
#' (`port_id`, NA at sea). Port pings are labelled and stay; nothing is dropped. A ping inside two
#' polygons gets the smaller one (the more specific port), so there is one row per ping. Runs in DuckDB
#' (spatial extension): a data frame is registered and collected, a lazy table stays lazy.
#'
#' @param pings A data frame or lazy DuckDB table with `lon`, `lat`.
#' @param ports An sf object of port polygons with `port_id`.
#' @param keep Other columns of `ports` to add to the pings (e.g. a second code).
#' @param lon,lat The position columns, if named otherwise.
#'
#' @return `pings` with `port_id` and the `keep` columns: the same type, the same rows.
#' @family flag pings
#' @export
rb_flag_ping_port <- function(pings, ports, keep = NULL, lon = lon, lat = lat) {
  map <- .rb_cols(lon = {{ lon }}, lat = {{ lat }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  tryCatch(DBI::dbExecute(con, "LOAD spatial"), error = function(e) DBI::dbExecute(con, "INSTALL spatial; LOAD spatial"))
  p <- .rb_as_lazy(.rb_std_in(pings, map), con)
  hb <- sf::st_transform(ports, 4326)
  # WKB keeps the coordinates bit for bit; area (in degrees) only ranks two polygons that hold one ping
  hw <- data.frame(sf::st_drop_geometry(hb)[, c("port_id", keep), drop = FALSE],
                   rb_area = as.numeric(sf::st_area(sf::st_set_crs(sf::st_geometry(hb), NA))))
  hw$rb_wkb <- lapply(sf::st_as_binary(sf::st_geometry(hb)), as.raw)
  nm <- .rb_register(con, hw, "rb_harb_")
  kp <- if (length(keep)) paste0(", ", paste(sprintf('h."%s"', keep), collapse = ", ")) else ""
  sql <- sprintf("
WITH p AS (SELECT *, row_number() OVER () AS rb_rid FROM (%s)),
h AS (SELECT * EXCLUDE (rb_wkb), ST_GeomFromWKB(rb_wkb) AS rb_g FROM %s),
j AS (SELECT p.*, h.port_id%s FROM p LEFT JOIN h ON ST_Intersects(h.rb_g, ST_Point(p.lon, p.lat))
      QUALIFY row_number() OVER (PARTITION BY p.rb_rid ORDER BY h.rb_area NULLS LAST, h.port_id) = 1)
SELECT * EXCLUDE (rb_rid) FROM j", .rb_sql(p), nm, kp)
  out <- .rb_std_out(dplyr::tbl(con, dplyr::sql(sql)), map)
  if (lazy_in) return(out)
  out <- as.data.frame(dplyr::collect(out))
  .rb_unregister(con)
  out
}
