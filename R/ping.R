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
  if (!length(ren)) {
    return(x)
  }
  clash <- intersect(names(ren), colnames(x))
  if (length(clash)) {
    stop(
      "Column(s) ",
      paste(clash, collapse = ", "),
      " already exist; rename them first.",
      call. = FALSE
    )
  }
  dplyr::rename(x, !!!rlang::syms(ren))
}
.rb_std_out <- function(x, map) {
  ren <- map[map != names(map)]
  if (!length(ren)) {
    return(x)
  }
  dplyr::rename(x, !!!stats::setNames(rlang::syms(names(ren)), ren))
}
.rb_flag_init <- function(x) {
  if ("ping_flag" %in% colnames(x)) {
    x
  } else {
    dplyr::mutate(x, ping_flag = NA_character_)
  }
}

# Lazy tables: label in batches of whole vessels, join the labels back -------------------
# For rules that are sequential along a track and so cannot run in DuckDB itself. Each batch is
# collected, labelled in R by `fun` (a data-frame function that updates `ping_flag`), and its labels are
# written to a temporary parquet file; the result is the input joined to those files, still lazy. A row is
# found again by its number within its vessel, in `order_by` order, numbered the same way in DuckDB and
# in the batch. The files live until the R session ends.
.rb_label_lazy <- function(
  x,
  fun,
  cols,
  order_by,
  batch_pings = 5e6,
  what = "label"
) {
  con <- dbplyr::remote_con(x)
  ob <- rlang::syms(order_by)
  keyed <- x |>
    dplyr::group_by(vid) |>
    dbplyr::window_order(!!!ob) |>
    dplyr::mutate(.rb_rn = dplyr::row_number()) |>
    dplyr::ungroup()
  vn <- x |>
    dplyr::count(vid) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(n))
  if (!nrow(vn)) {
    stop(what, ": the table has no vessels.", call. = FALSE)
  }
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
    DBI::dbExecute(
      con,
      sprintf(
        "COPY %s TO '%s' (FORMAT parquet)",
        nm,
        file.path(dir, sprintf("b%05d.parquet", i))
      )
    )
    duckdb::duckdb_unregister(con, nm)
  }
  labels <- dplyr::tbl(
    con,
    dplyr::sql(sprintf("SELECT * FROM read_parquet('%s/*.parquet')", dir))
  )
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
rb_flag_ping_invalid <- function(
  x,
  rules = c("lon_eq_lat", "out_of_range"),
  lon = lon,
  lat = lat
) {
  .rb_check_input(
    x,
    "rb_flag_ping_invalid",
    cols = c(
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    types = c(lon = "numeric", lat = "numeric")
  )
  rules <- match.arg(rules, several.ok = TRUE)
  map <- .rb_cols(lon = {{ lon }}, lat = {{ lat }})
  x <- .rb_flag_init(.rb_std_in(x, map))
  eq <- "lon_eq_lat" %in% rules
  oor <- "out_of_range" %in% rules
  x <- dplyr::mutate(
    x,
    ping_flag = dplyr::case_when(
      !is.na(ping_flag) ~ ping_flag,
      !!eq & lon == lat ~ "invalid",
      !!oor & (abs(lon) > 180 | abs(lat) > 90) ~ "invalid",
      TRUE ~ NA_character_
    )
  )
  .rb_std_out(x, map)
}

#' Flag a second feed's copy of a ping
#'
#' Step 0 of the fishing-activity flow. When several feeds (e.g. a national AIS network and a commercial
#' one) report the same fix, the lower-priority feed's copy is flagged `"duplicate"` in `ping_flag`: a
#' ping of the same vessel from a higher-priority feed lies within `max_dt_s` seconds and the distance
#' `kn_max` covers in that time, among the `n_neighbours` neighbours on each side in time order. Only rows not yet
#' flagged are tested, and they are not used as anchors. No row is dropped.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon`, `lat` and the feed column.
#' @param priority The feeds, highest priority first. A feed not listed ranks last. No default: the
#'   order is a judgement about the feeds.
#' @param feed The column naming the feed.
#' @param max_dt_s,kn_max,n_neighbours Seconds apart, speed in knots that sets the distance, neighbours compared.
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` added or updated: the same type, the same rows.
#' @family flag pings
#' @export
rb_flag_ping_duplicate <- function(
  x,
  priority,
  feed = provider,
  max_dt_s = 10,
  kn_max = 25,
  n_neighbours = 3,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  batch_pings = 5e6
) {
  .rb_check_input(
    x,
    "rb_flag_ping_duplicate",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = c(feed = rlang::as_name(rlang::enquo(feed))),
    types = c(time = "time", lon = "numeric", lat = "numeric")
  )
  if (missing(priority)) {
    stop(
      "`priority` is required: the feeds, highest priority first.",
      call. = FALSE
    )
  }
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }},
    feed = {{ feed }}
  )
  x <- .rb_flag_init(.rb_std_in(x, map))
  fun <- function(d) {
    i <- which(is.na(d$ping_flag))
    if (length(i)) {
      dup <- .rb_whack_duplicates(
        d$vid[i],
        d$time[i],
        d$lon[i],
        d$lat[i],
        d$feed[i],
        priority = priority,
        max_dt_s = max_dt_s,
        kn_max = kn_max,
        n = n_neighbours
      )
      d$ping_flag[i[dup]] <- "duplicate"
    }
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(
      x,
      fun,
      cols = c("time", "lon", "lat", "feed"),
      order_by = c("time", "lon", "lat", "feed"),
      batch_pings = batch_pings,
      what = "rb_flag_ping_duplicate()"
    )
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
#'     which judges each ping from both sides, then a forward scan against the last kept ping
#'     ("forward"), which resolves runs and interleaved streams.
#'   * `"sda"`: the speed-distance-angle stage alone ("vmask", "spike"); the algorithm of
#'     `argosfilter::sdafilter()` (Freitas et al. 2008), re-implemented in C++.
#'   * `"forward"`: the forward scan alone ("forward"): each ping is judged against the last
#'     *retained* ping, so consecutive clusters are caught.
#'   * `"fwdbwd"`: a ping too fast both from the previous and to the next ("fwdbwd"): isolated
#'     spikes only; consecutive clusters pass.
#'   * `"sequential"`: the cluster-aware sequential filter ("sequential"): iteratively removes
#'     the first too-fast segment and recomputes, in-memory only.
#' @param kn_max Speed threshold, knots.
#' @param min_dt_s A time step shorter than this counts as this many seconds.
#' @param max_gap_h For `"clean"` and `"forward"`: after a gap longer than this the scan restarts.
#' @param ... Passed to the method (e.g. `ang`, `distlim` for `"sda"`).
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` added or updated: the same type, the same rows.
#'
#' @references For `"sda"`: Freitas, C., Lydersen, C., Fedak, M.A. and Kovacs,
#'   K.M. (2008). "A simple new algorithm to filter marine mammal Argos
#'   locations". Marine Mammal Science, 24(2), 315–325.
#'   \doi{10.1111/j.1748-7692.2007.00180.x}; the algorithm of
#'   [argosfilter::sdafilter()], re-implemented in C++.
#' @family flag pings
#' @export
rb_flag_ping_impossible <- function(
  x,
  method = c("clean", "sda", "forward", "fwdbwd", "sequential"),
  kn_max = 25,
  min_dt_s = 10,
  max_gap_h = 4,
  ...,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  batch_pings = 5e6
) {
  .rb_check_input(
    x,
    "rb_flag_ping_impossible",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    types = c(time = "time", lon = "numeric", lat = "numeric")
  )
  method <- match.arg(method)
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  x <- .rb_flag_init(.rb_std_in(x, map))
  dots <- list(...)
  fun <- function(d) {
    i <- which(is.na(d$ping_flag))
    if (!length(i)) {
      return(d)
    }
    s <- dplyr::ungroup(d[i, setdiff(names(d), "ping_flag"), drop = FALSE])
    s$.rb_i <- i
    stage <- switch(
      method,
      clean = {
        o <- do.call(
          .whack_clean_df,
          c(
            list(
              s,
              kn_max = kn_max,
              max_gap_h = max_gap_h,
              min_dt_s = min_dt_s
            ),
            dots
          )
        )
        stats::setNames(o$whack_stage, o$.rb_i)
      },
      sda = {
        o <- do.call(
          .rb_whack_sda,
          c(list(s, kn_max = kn_max, min_dt_s = min_dt_s), dots)
        )
        stats::setNames(o$whack_sda, o$.rb_i)
      },
      forward = {
        o <- .rb_whack_forward(
          s,
          kn_max = kn_max,
          max_gap_h = max_gap_h,
          min_dt_s = min_dt_s
        )
        stats::setNames(ifelse(o$whack2, "forward", NA_character_), o$.rb_i)
      },
      fwdbwd = {
        o <- .rb_whack_fwdbwd(s, kn_max = kn_max, min_dt_s = min_dt_s)
        stats::setNames(ifelse(o$whack, "fwdbwd", NA_character_), o$.rb_i)
      },
      sequential = {
        o <- s |>
          dplyr::arrange(vid, time, lon, lat) |>
          dplyr::group_by(vid) |>
          dplyr::mutate(
            .w = .rb_whack_sequential_fast(
              lon,
              lat,
              time,
              kn_max = kn_max,
              min_dt_s = min_dt_s
            )
          ) |>
          dplyr::ungroup()
        stats::setNames(ifelse(o$.w, "sequential", NA_character_), o$.rb_i)
      }
    )
    d$ping_flag[as.integer(names(stage))] <- unname(stage)
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(
      x,
      fun,
      cols = c("time", "lon", "lat"),
      order_by = c("time", "lon", "lat"),
      batch_pings = batch_pings,
      what = "rb_flag_ping_impossible()"
    )
  } else {
    fun(x)
  }
  .rb_std_out(x, map)
}

#' Tag pings with the port polygon they lie in
#'
#' Step 0 of the fishing-activity flow: each ping gets the id of the port polygon that holds it
#' (`port_id`, NA at sea). Port pings are labelled and stay; nothing is dropped. A ping inside two
#' polygons gets the smaller one (the more specific port), then the lowest `port_id`, so there is one row per ping.
#' Runs in DuckDB (spatial extension): a data frame is registered and collected, a lazy table stays lazy. A ping's port
#' depends on its position alone, so each distinct position is joined to the polygons once and the pings take its port
#' by an equality join: no window runs over the pings, and the memory grows with the positions in port, not with the
#' pings' columns.
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
  .rb_check_input(
    pings,
    "rb_flag_ping_port",
    cols = c(
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    types = c(lon = "numeric", lat = "numeric")
  )
  map <- .rb_cols(lon = {{ lon }}, lat = {{ lat }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  .rb_scope()
  .rb_extension(con, "spatial")
  given <- pings
  if (!lazy_in) {
    pings <- .rb_with_rid(pings)
  }
  p <- .rb_as_lazy(.rb_std_in(pings, map), con)
  hb <- sf::st_transform(ports, 4326)
  # WKB keeps the coordinates bit for bit; area (in degrees) only ranks two polygons that hold one ping
  hw <- data.frame(
    sf::st_drop_geometry(hb)[, c("port_id", keep), drop = FALSE],
    rb_area = as.numeric(sf::st_area(sf::st_set_crs(sf::st_geometry(hb), NA)))
  )
  hw$rb_wkb <- lapply(sf::st_as_binary(sf::st_geometry(hb)), as.raw)
  nm <- .rb_register(con, hw, "rb_harb_")
  kp <- paste0(
    sprintf(
      ', first(h."%s" ORDER BY h.rb_area, h.port_id) AS "%s"',
      keep,
      keep
    ),
    collapse = ""
  )
  # each position once against the polygons (the smallest, then the lowest port_id), the pings joined back on it; until
  # 2026.10.28.3 a row number and a window over every ping and column did this, which took 15 GB for a year of 37
  # million pings under DuckDB 2.0 (fishycode plan 049)
  sql <- sprintf(
    "
WITH p AS NOT MATERIALIZED (%s),
h AS (SELECT * EXCLUDE (rb_wkb), ST_GeomFromWKB(rb_wkb) AS rb_g FROM %s),
q AS (SELECT DISTINCT lon, lat FROM p WHERE lon IS NOT NULL AND lat IS NOT NULL),
m AS (SELECT q.lon AS rb_lon, q.lat AS rb_lat, first(h.port_id ORDER BY h.rb_area, h.port_id) AS port_id%s
      FROM q JOIN h ON ST_Intersects(h.rb_g, ST_Point(q.lon, q.lat)) GROUP BY q.lon, q.lat)
SELECT p.*, m.* EXCLUDE (rb_lon, rb_lat) FROM p LEFT JOIN m ON p.lon = m.rb_lon AND p.lat = m.rb_lat",
    .rb_sql(p),
    nm,
    kp
  )
  out <- .rb_std_out(dplyr::tbl(con, dplyr::sql(sql)), map)
  if (lazy_in) {
    return(.rb_hold(out))
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  .rb_as_given(out, given, ".rb_rid")
}
