# trip.R — step 1: port calls and voyages, the two halves of a track, and the trip each ping belongs to.
#
# The "dwell" rule of rb_create_port_calls() is ported from fishycode's curate/port_stay.R (plan 011, decisions
# 060-064; ramb plan 013 phase 2), where a port call was called a stay (renamed by fishycode plan 032). A port call
# is time spent at a port, not a ping inside its polygon: one port ping followed by silence is not a port call.
# Runs in DuckDB (tier 2): a data frame is registered and the result collected; a lazy table stays lazy.

.rb_con_for <- function(x, ...) {
  # the connection of the lazy inputs, which must share one; a data frame is registered in it
  lazy <- Filter(function(t) inherits(t, "tbl_lazy"), list(x, ...))
  if (length(lazy) == 0) {
    return(duckdbfs::cached_connection())
  }
  cons <- lapply(lazy, dbplyr::remote_con)
  if (!all(vapply(cons, identical, logical(1), cons[[1]]))) {
    stop(
      "The lazy tables are on different DuckDB connections; open them on one (duckdbfs::open_dataset() shares its cached connection).",
      call. = FALSE
    )
  }
  cons[[1]]
}
# Data frames registered for one call. A call on data frames releases them once it has collected its
# result: registered frames left behind in a loop accumulate (fishycode R/write_parquet.R).
.rb_reg <- new.env(parent = emptyenv())
.rb_register <- function(con, x, prefix = "rb_in_") {
  nm <- paste0(prefix, paste(sample(c(letters, 0:9), 10, TRUE), collapse = ""))
  x <- as.data.frame(x)
  # a date-time or date stored as integer (seq() makes them) registers as INTEGER, not as a time
  for (k in names(x)) {
    if (inherits(x[[k]], c("POSIXct", "Date")) && is.integer(x[[k]])) {
      storage.mode(x[[k]]) <- "double"
    }
  }
  duckdb::duckdb_register(con, nm, x)
  assign(nm, TRUE, envir = .rb_reg)
  nm
}
.rb_unregister <- function(con) {
  for (nm in ls(.rb_reg)) {
    try(duckdb::duckdb_unregister(con, nm), silent = TRUE)
    rm(list = nm, envir = .rb_reg)
  }
  for (nm in ls(.rb_tmp)) {
    try(
      DBI::dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", nm)),
      silent = TRUE
    )
    rm(list = nm, envir = .rb_tmp)
  }
}
# A query materialised once as a temporary table, where a function reads it twice and both reads must agree.
# Dropped with the registered frames when the function collects its result; kept while a lazy result needs it.
.rb_tmp <- new.env(parent = emptyenv())
.rb_temp_table <- function(con, sql, keep = FALSE) {
  nm <- paste0(
    "rb_tmp_",
    paste(sample(c(letters, 0:9), 10, TRUE), collapse = "")
  )
  DBI::dbExecute(con, sprintf("CREATE TEMP TABLE %s AS %s", nm, sql))
  if (!keep) {
    assign(nm, TRUE, envir = .rb_tmp)
  }
  nm
}
.rb_as_lazy <- function(x, con) {
  if (inherits(x, "tbl_lazy")) {
    return(x)
  }
  dplyr::tbl(con, .rb_register(con, x))
}
.rb_sql <- function(x) as.character(dbplyr::sql_render(dplyr::ungroup(x)))

#' Create port calls from vessel tracks
#'
#' A step-1 builder of the fishing-activity flow: the port calls, the times a vessel spends in port. Port calls
#' and voyages ([rb_create_voyages()]) are the two halves of a track, and the two functions share their methods.
#'
#' * `"dwell"`: a port call is elapsed time at a port, in port for at least a minimum time (`min_dwell_h`), from
#'   any of three rules on the pings and, optionally, from declared port events (below). [rb_create_voyages()]
#'   with `method = "dwell"` cuts the track at these port calls.
#' * `"runs"`, `"jepol"`, `"cnr"`, `"iapesca"`: a port call is the time in port between two voyages of the
#'   method ([rb_create_voyages()], whose arguments for these methods go in `...`), from the end of one voyage
#'   to the start of the next, at the port the first ended in, else the port the second began from. Where
#'   neither voyage reaches a port (a long voyage split, or a silence at sea), there is no port call between
#'   them, nor before a vessel's first voyage or after its last.
#'
#' The rules of `"dwell"`:
#'
#' * `tag_then_gap`: two consecutive pings at the same port, the first within `radius_in` of it, at least
#'   the minimum dwell apart, one of them inside the polygon;
#' * `gap_near_port`: the same, neither inside the polygon;
#' * `tag_span`: a run of pings inside the polygon spanning at least the minimum dwell;
#' * `event_pair` (with `events`): an arrival then a departure event at the same port, adjacent in the
#'   vessel's events, both judged good, at least the minimum dwell apart.
#'
#' A ping takes the port among those whose `radius_out` buffer holds it, preferring one whose
#' `radius_in` buffer also holds it, then the nearest. Seeds are extended over the contiguous inside pings
#' at either end and merged; port calls from pings and from events are merged in time. `rule` names the
#' strongest evidence: event_pair > tag_then_gap > tag_span > gap_near_port.
#'
#' @param pings A data frame or lazy DuckDB table of cleaned pings: `vid`, `time`, and `lon`, `lat` for
#'   `"dwell"`; for the other methods, the columns [rb_create_voyages()] needs.
#' @param ports An sf object of port polygons with `port_id`, and for `"dwell"` optionally `radius_out`
#'   (metres; the departure radius, at least `radius_in`). Required for `"dwell"` and `"iapesca"`, optional for
#'   `"cnr"`, not used by `"runs"` and `"jepol"` (they read `port_id` on the pings).
#' @param method `"dwell"`, `"runs"`, `"jepol"`, `"cnr"` or `"iapesca"`.
#' @param min_dwell_h For `"dwell"`: the minimum time in port, hours, where `min_dwell` has no row for the port.
#' @param min_dwell For `"dwell"`: optional data frame `port_id`, `min_dwell_h`: a minimum per port.
#' @param radius_in For `"dwell"`: the arrival radius, metres.
#' @param events For `"dwell"`: optional data frame or lazy table of port events: `vid`, `time`, `io` (`"I"`
#'   arrival, `"U"` departure), `port_id`, `good` (logical).
#' @param t_in_window Optional `c(from, to)` (POSIXct): keep only port calls that begin in `[from, to)` (for
#'   `"dwell"`, applied before the port calls from pings and from events are merged). For processing a long
#'   series in chunks; the port calls are then numbered within the window, so renumber them once the chunks
#'   are bound.
#' @param crs_m For `"dwell"`: a metric CRS for the buffers (EPSG code). The default, 3575, suits the North
#'   Atlantic.
#' @param ... For `"runs"`, `"jepol"`, `"cnr"` and `"iapesca"`: arguments of [rb_create_voyages()] (such as
#'   `min_pings`, `max_dur_h`, `coast`, `cnr`, `iapesca`, or the ping columns `port_id` and `speed`).
#' @param vid,time,lon,lat The ping columns, if named otherwise.
#'
#' @return One row per port call: `vid`, `year` (of `T_in`; a port call is not cut at New Year), `port_call_id`
#'   (numbered per vessel by `T_in`, so a ping in a port call over New Year finds it by `vid` and `port_call_id`
#'   alone), `port_id`, `T_in`, `T_out`, `dur_h`, `rule`, `evidence`, `T_in_ping`, `T_out_ping`, `T_in_ev`, `T_out_ev`,
#'   `basis` (`"reconstructed"`). For the methods other than `"dwell"`, `rule` and `evidence` are the method and
#'   the event times are NA. A data frame for data frame input, a lazy table for lazy input.
#' @family trip
#' @export
rb_create_port_calls <- function(
  pings,
  ports = NULL,
  method = c("dwell", "runs", "jepol", "cnr", "iapesca"),
  min_dwell_h = 4,
  min_dwell = NULL,
  radius_in = 2000,
  events = NULL,
  t_in_window = NULL,
  crs_m = 3575,
  ...,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat
) {
  method <- match.arg(method)
  if (method != "dwell") {
    v <- rb_create_voyages(
      pings,
      method = method,
      ports = ports,
      by_year = FALSE,
      ...,
      vid = {{ vid }},
      time = {{ time }},
      lon = {{ lon }},
      lat = {{ lat }}
    )
    return(.rb_calls_between(
      v,
      inherits(pings, "tbl_lazy"),
      rlang::as_name(rlang::enquo(vid)),
      method,
      t_in_window
    ))
  }
  if (...length() > 0) {
    stop(
      "The arguments in `...` are for the methods other than \"dwell\".",
      call. = FALSE
    )
  }
  .rb_check_input(
    pings,
    "rb_create_port_calls",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time))
    ),
    also = c(
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    types = c(time = "time", lon = "numeric", lat = "numeric")
  )
  if (is.null(ports)) {
    stop("Method \"dwell\" needs `ports`: the port polygons.", call. = FALSE)
  }
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  tryCatch(DBI::dbExecute(con, "LOAD spatial"), error = function(e) {
    DBI::dbExecute(con, "INSTALL spatial; LOAD spatial")
  })
  p <- .rb_as_lazy(.rb_std_in(pings, map), con) |>
    dplyr::select(vid, time, lon, lat)

  # ports: polygon and the two buffers, as WKT, registered on the connection
  hb <- sf::st_make_valid(ports)
  r_out <- if ("radius_out" %in% names(hb)) {
    hb$radius_out
  } else {
    rep(radius_in, nrow(hb))
  }
  r_out <- pmax(dplyr::coalesce(r_out, radius_in), radius_in)
  buf <- function(d) {
    sf::st_transform(sf::st_buffer(sf::st_transform(hb, crs_m), d), 4326)
  }
  hw <- data.frame(
    port_id = hb$port_id,
    poly = sf::st_as_text(sf::st_geometry(hb), digits = 15),
    near_in = sf::st_as_text(sf::st_geometry(buf(radius_in)), digits = 15),
    near_out = sf::st_as_text(sf::st_geometry(buf(r_out)), digits = 15)
  )
  hw_nm <- .rb_register(con, hw, "rb_hw_")
  hg <- if (is.null(min_dwell)) {
    data.frame(port_id = character(0), g_h = numeric(0))
  } else {
    data.frame(port_id = min_dwell$port_id, g_h = min_dwell$min_dwell_h)
  }
  hg_nm <- .rb_register(con, hg, "rb_hg_")

  win_p <- win_e <- "TRUE"
  if (!is.null(t_in_window)) {
    f <- format(t_in_window[1], "%Y-%m-%d %H:%M:%S", tz = "UTC")
    t <- format(t_in_window[2], "%Y-%m-%d %H:%M:%S", tz = "UTC")
    win_p <- sprintf("t_in >= TIMESTAMP '%s' AND t_in < TIMESTAMP '%s'", f, t)
    win_e <- sprintf("time >= TIMESTAMP '%s' AND time < TIMESTAMP '%s'", f, t)
  }
  ev_sql <- if (is.null(events)) {
    "SELECT NULL::INTEGER AS vid, NULL::TIMESTAMP AS time, NULL::VARCHAR AS io, NULL::VARCHAR AS port_id, NULL::BOOLEAN AS good WHERE false"
  } else {
    .rb_sql(
      .rb_as_lazy(events, con) |> dplyr::select(vid, time, io, port_id, good)
    )
  }
  G <- min_dwell_h
  sql <- sprintf(
    "
WITH seq AS (SELECT vid, time, lon, lat, row_number() OVER (PARTITION BY vid ORDER BY time) AS rn FROM (%s)),
harb AS (SELECT port_id AS pid, ST_GeomFromText(poly) AS g, ST_GeomFromText(near_in) AS gi, ST_GeomFromText(near_out) AS go FROM %s),
hg AS (SELECT port_id AS pid, g_h FROM %s),
c AS (SELECT s.vid, s.rn, s.time, h.pid, ST_Distance(ST_Point(s.lon, s.lat), h.g) AS d, ST_Intersects(h.gi, ST_Point(s.lon, s.lat)) AS near_in
      FROM seq s JOIN harb h ON ST_Intersects(h.go, ST_Point(s.lon, s.lat))),
tg AS (SELECT vid, rn, any_value(time) AS time,
              arg_min(pid, (CASE WHEN near_in THEN 0 ELSE 10 END) + d) AS h,
              min(d) = 0 AS inside,
              arg_min(near_in, (CASE WHEN near_in THEN 0 ELSE 10 END) + d) AS near_in
       FROM c GROUP BY vid, rn),
runs AS (SELECT vid, h, min(rn) AS s, max(rn) AS e, min(time) AS t_s, max(time) AS t_e
         FROM (SELECT *, rn - row_number() OVER (PARTITION BY vid, h ORDER BY rn) AS grp FROM tg WHERE inside)
         GROUP BY vid, h, grp),
a AS (SELECT *, lead(rn) OVER w AS nrn, lead(h) OVER w AS nh, lead(inside) OVER w AS nin, lead(time) OVER w AS nt
      FROM tg WINDOW w AS (PARTITION BY vid ORDER BY rn)),
seeds AS (SELECT vid, h, rn AS s, nrn AS e, time AS t_s, nt AS t_e,
                 CASE WHEN inside OR nin THEN 'tag_then_gap' ELSE 'gap_near_port' END AS rule
          FROM a LEFT JOIN hg ON hg.pid = a.h
          WHERE nrn = rn + 1 AND nh = h AND near_in AND epoch(nt - time) >= coalesce(hg.g_h, %f) * 3600
          UNION ALL
          SELECT vid, h, s, e, t_s, t_e, 'tag_span' FROM runs LEFT JOIN hg ON hg.pid = runs.h
          WHERE epoch(t_e - t_s) >= coalesce(hg.g_h, %f) * 3600),
ext AS (SELECT sd.vid, sd.h, least(sd.s, coalesce(r1.s, sd.s)) AS s, greatest(sd.e, coalesce(r2.e, sd.e)) AS e,
               least(sd.t_s, coalesce(r1.t_s, sd.t_s)) AS t_s, greatest(sd.t_e, coalesce(r2.t_e, sd.t_e)) AS t_e, sd.rule
        FROM seeds sd
        LEFT JOIN runs r1 ON r1.vid = sd.vid AND r1.h = sd.h AND sd.s BETWEEN r1.s AND r1.e
        LEFT JOIN runs r2 ON r2.vid = sd.vid AND r2.h = sd.h AND sd.e BETWEEN r2.s AND r2.e),
m AS (SELECT *, max(e) OVER (PARTITION BY vid, h ORDER BY s, e ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pe FROM ext),
g AS (SELECT *, sum(CASE WHEN pe IS NULL OR s > pe + 1 THEN 1 ELSE 0 END) OVER (PARTITION BY vid, h ORDER BY s, e) AS grp FROM m),
pcall AS (SELECT vid, h AS pid, min(t_s) AS t_in, max(t_e) AS t_out, list_distinct(list(rule)) AS rules FROM g GROUP BY vid, h, grp),
ev AS (SELECT vid, time, io, port_id AS hid_pid, good,
              lead(io) OVER w AS nio, lead(time) OVER w AS nt, lead(port_id) OVER w AS nh, lead(good) OVER w AS ngood
       FROM (%s) WINDOW w AS (PARTITION BY vid ORDER BY time, io)),
ecall AS (SELECT vid, hid_pid AS pid, time AS t_in, nt AS t_out, ['event_pair'] AS rules
          FROM ev LEFT JOIN hg ON hg.pid = ev.hid_pid
          WHERE io = 'I' AND nio = 'U' AND good AND ngood AND hid_pid IS NOT NULL AND nh = hid_pid
            AND epoch(nt - time) >= coalesce(hg.g_h, %f) * 3600 AND %s),
u AS (SELECT vid, pid, t_in, t_out, rules, 'ping' AS src FROM pcall WHERE %s
      UNION ALL SELECT vid, pid, t_in, t_out, rules, 'ev' FROM ecall),
m2 AS (SELECT *, max(t_out) OVER (PARTITION BY vid, pid ORDER BY t_in, t_out ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pe FROM u),
g2 AS (SELECT *, sum(CASE WHEN pe IS NULL OR t_in > pe THEN 1 ELSE 0 END) OVER (PARTITION BY vid, pid ORDER BY t_in, t_out) AS grp FROM m2),
st AS (SELECT vid, pid, min(t_in) AS T_in, max(t_out) AS T_out,
              min(t_in) FILTER (src = 'ping') AS T_in_ping, max(t_out) FILTER (src = 'ping') AS T_out_ping,
              min(t_in) FILTER (src = 'ev') AS T_in_ev, max(t_out) FILTER (src = 'ev') AS T_out_ev,
              array_to_string(list_sort(list_distinct(flatten(list(rules)))), '+') AS evidence
       FROM g2 GROUP BY vid, pid, grp)
SELECT vid, year(T_in) AS year, row_number() OVER (PARTITION BY vid ORDER BY T_in, T_out, pid) AS port_call_id,
       pid AS port_id, T_in, T_out, epoch(T_out - T_in) / 3600 AS dur_h,
       CASE WHEN evidence LIKE '%%event_pair%%' THEN 'event_pair'
            WHEN evidence LIKE '%%tag_then_gap%%' THEN 'tag_then_gap'
            WHEN evidence LIKE '%%tag_span%%' THEN 'tag_span'
            ELSE 'gap_near_port' END AS rule,
       evidence, T_in_ping, T_out_ping, T_in_ev, T_out_ev, 'reconstructed' AS basis
FROM st",
    .rb_sql(p),
    hw_nm,
    hg_nm,
    G,
    G,
    ev_sql,
    G,
    win_e,
    win_p
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(
    out,
    !!!stats::setNames(rlang::syms("vid"), map[["vid"]])
  )
  if (lazy_in) {
    return(out)
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  out[order(out[[map[["vid"]]]], out$T_in), ]
}

# The port calls of the methods other than "dwell": the time in port between two voyages of one vessel, from the
# end of one to the start of the next, at the port the first ended in, else the port the second began from. The
# voyages come uncut at New Year (by_year = FALSE), so a New Year cut at sea makes no port call; a split at sea or
# a silence with no port at either side makes none either, since neither voyage end has a port.
.rb_calls_between <- function(voyages, lazy_in, vid_name, method, t_in_window) {
  con <- .rb_con_for(voyages)
  v <- .rb_as_lazy(voyages, con) |>
    dplyr::rename(!!!stats::setNames(rlang::syms(vid_name), "vid"))
  win <- "TRUE"
  if (!is.null(t_in_window)) {
    win <- sprintf(
      "T_in >= TIMESTAMP '%s' AND T_in < TIMESTAMP '%s'",
      format(t_in_window[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      format(t_in_window[2], "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
  }
  sql <- sprintf(
    "
WITH v AS (SELECT vid, T1, T2, port_from, port_to FROM (%s)),
w AS (SELECT vid, T2 AS T_in, lead(T1) OVER o AS T_out, coalesce(port_to, lead(port_from) OVER o) AS port_id
      FROM v WINDOW o AS (PARTITION BY vid ORDER BY T1, T2)),
c AS (SELECT * FROM w WHERE T_out IS NOT NULL AND port_id IS NOT NULL AND %s)
SELECT vid, year(T_in) AS year, row_number() OVER (PARTITION BY vid ORDER BY T_in, T_out, port_id) AS port_call_id,
       port_id, T_in, T_out, epoch(T_out - T_in) / 3600 AS dur_h, '%s' AS rule, '%s' AS evidence,
       T_in AS T_in_ping, T_out AS T_out_ping, NULL::TIMESTAMP AS T_in_ev, NULL::TIMESTAMP AS T_out_ev,
       'reconstructed' AS basis
FROM c",
    .rb_sql(v),
    win,
    method,
    method
  )
  out <- dplyr::tbl(con, dplyr::sql(sql)) |>
    dplyr::rename(!!!stats::setNames(rlang::syms("vid"), vid_name))
  if (lazy_in) {
    return(out)
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  out[order(out[[vid_name]], out$T_in), ]
}

# A Suggests package, checked where a function needs it.
.rb_need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf(
        "This function needs the package '%s'. Install it with install.packages(\"%s\").",
        pkg,
        pkg
      ),
      call. = FALSE
    )
  }
}

# glue::glue() for SQL templates, in base R: each {expr} is evaluated in the caller's frame.
.rb_fill <- function(template, env = parent.frame()) {
  m <- gregexpr("\\{[^{}]+\\}", template)
  regmatches(template, m) <- list(vapply(
    regmatches(template, m)[[1]],
    function(e) {
      paste(
        as.character(eval(parse(text = substr(e, 2, nchar(e) - 1)), env)),
        collapse = ""
      )
    },
    character(1)
  ))
  template
}
# Voyages, and the voyage each ping belongs to. Ported from fishycode's curate/ais_trip.R (decision 061; ramb plan
# 013 phase 2). Tier 2: one DuckDB query each; a data frame is registered and the result collected, a lazy table
# stays lazy.

# A ping takes the port call it falls in; inside two overlapping port calls, the earlier one. Rows are numbered
# within the vessel (rb_rid), not over the whole table: an unpartitioned row_number() runs on one core, which made
# the step 5.5 times slower than the code it replaced (fishycode TODO 48).
# With `port_call_id` in the port calls, the ping also takes the port call's key, so a ping can carry the key and
# not the port call's values (fishycode plan 014).
.rb_ping_call_sql <- function(p_sql, s_sql, by_year, call_id = FALSE) {
  sprintf(
    "
WITH p AS (SELECT *, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid FROM (%s)),
s AS (SELECT vid, port_id, T_in, T_out, %s AS port_call_id FROM (%s)),
ps AS (SELECT p.*, s.port_id AS port_call_port_id, s.port_call_id FROM p LEFT JOIN s ON s.vid = p.vid AND p.time BETWEEN s.T_in AND s.T_out
       QUALIFY row_number() OVER (PARTITION BY p.vid, p.rb_rid ORDER BY s.T_in NULLS LAST, s.port_id) = 1)
SELECT *, %s AS rb_year FROM ps",
    p_sql,
    if (call_id) "port_call_id" else "NULL::BIGINT",
    s_sql,
    if (by_year) "year(time)" else "0"
  )
}

# "runs" is what the removed rb_trip() numbered, "jepol" the removed rb_trip_jepol(). The jepol cut was
# fixed on 2026-10-12 (the old function cut one ping before the longest gap); see the comment above
# .rb_jepol_one() below, and NEWS.md.

#' Create voyages from vessel tracks
#'
#' A step-1 builder of the fishing-activity flow. Five methods, one result: a table of voyages. Voyages and port
#' calls ([rb_create_port_calls()]) are the two halves of a track, and the two functions share their methods.
#'
#' * `"dwell"`: a voyage is the sea between two port calls (see [rb_create_port_calls()]). A ping takes the port
#'   call it falls in (the earlier of two that overlap), the track is cut into runs of port call and sea, and each
#'   sea run becomes a voyage, from the port of the port call before it to the port of the port call after it.
#' * `"runs"`: as `"dwell"`, but every run of pings tagged in a port ([rb_flag_ping_port()]) is a port call,
#'   however short.
#' * `"jepol"`: the ICES VMS datacall rule (`define_trips_pol`): a voyage runs from the
#'   first ping at sea to the first ping back in port; voyages of `min_dur_h` hours or less are dropped, and
#'   with `split` one over `max_dur_h` hours is cut at its longest gap between pings, repeatedly. A long
#'   voyage with no inside gap of three minutes or more is left whole.
#' * `"cnr"`: CNR-IRBIM's trip finder (`create_fishing_trip()`, Galdelli et al. 2019), recoded in C++. A
#'   vessel is in port after `n_in` (4) consecutive pings in a port at a mean recorded speed under `speed_kn`
#'   (6 knots). A voyage runs from the last such ping (or the next, if it is over `shift_h`, 1 hour, later) to
#'   the ping before the `n_in`-th port ping of the next such run; a silence of over `gap_h` (4) hours at sea
#'   also ends it, and the next starts at the ping after the silence. Two voyages split by a silence with no
#'   port at either side of it are joined, unless both ends lie in the `coast` zone and together they last
#'   `join_h` (24) hours or more. A voyage end with no port then gets one from `ports`: after a silence that
#'   was not joined, the closest port; otherwise the port at the voyage's other end if it is among the
#'   `n_closest` (5) closest within `reach_m` (50 km), else the closest of those; with none in reach, NA. The
#'   port of an end is the port whose polygon, grown by `buffer_deg` (0.001 degrees, planar, as the R code),
#'   holds its ping, read `n_in - 1` pings before a voyage's first ping and at its last. A vessel (with
#'   `by_year`, a vessel-year) of fewer than `min_n` (10) pings has no voyages, and a missing speed never
#'   counts as slow. The `cnr` argument sets these values. Distances to the ports are on the WGS84 ellipsoid
#'   (`distance = "geodesic"`, the default, as the R code measured them; needs lwgeom) or on ramb's sphere
#'   (`"sphere"`, needs s2), whatever [sf::sf_use_s2()] says; the port buffer and the coastal zone are
#'   tested on the plane, as in the R code.
#' * `"iapesca"`: Ifremer's `Detect_FishingTrips()` with harbours (iapesca, J. Rodriguez), recoded as one DuckDB
#'   query, so a lazy table (parquet) never comes into memory. Pings at the same time and place as earlier
#'   ones are dropped (each matched separately, as the R code does); then, up to `max_pass` (5) times, the first
#'   of each run of pings reached at `max_speed_kn` (25) or more. A ping is in port within a buffer of the
#'   `ports` polygons: `buf_start_m` (2,000 m), or 1 % of the track's bounding-box diagonal when that lies
#'   between `buf_start_m` and `buf_max_m` (2 nautical miles); with no ping in port the buffer grows by that 1 %,
#'   up to five times. A voyage is a run of pings outside the buffer; it ends at a run of more pings in port than
#'   the mean ping interval in hours (rounded down) plus one, but only if over `min_stop_s` (1 hour) have passed
#'   since the previous voyage end, so a short voyage merges with the next. Pings in port belong to no voyage.
#'   The geometry is planar in `crs_m` (default: the UTM zone of the pings' mean position), as the R code is with
#'   projected input; the buffer is GEOS's (30 segments a quarter circle), as sf's. Added here: `port_from` and
#'   `port_to`, the nearest port of the in-port ping before and after the voyage. The `iapesca` argument sets
#'   these values.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`, and `port_id` for `"runs"`,
#'   `"jepol"` and `"cnr"` (NA at sea). For `"cnr"` also `speed` (knots, recorded) and `lon`, `lat`; for
#'   `"iapesca"`, `lon` and `lat`.
#' @param port_calls For `"dwell"`: port calls as returned by [rb_create_port_calls()]: `vid`, `port_id`, `T_in`,
#'   `T_out`.
#' @param method `"dwell"`, `"runs"`, `"jepol"`, `"cnr"` or `"iapesca"`.
#' @param min_pings Voyages with fewer pings are dropped. They are numbered before they are dropped, so the
#'   numbers have gaps where a short voyage was. Default 10 for `"dwell"`, 1 (none dropped) otherwise.
#' @param by_year Cut voyages at New Year and number them per vessel and year.
#' @param min_dur_h,max_dur_h,split For `"jepol"`: the minimum and maximum voyage length in hours, and whether
#'   longer voyages are split.
#' @param ports For `"cnr"`: an sf object of port polygons with `port_id`, for the ports of the voyage
#'   ends (see above). Without it an end takes its ping's `port_id`, and an end with none stays NA. For
#'   `"iapesca"`, required: the harbours whose buffer marks the pings in port.
#' @param coast For `"cnr"`: an sf object of the coastal zone (CNR's: the 3 nautical-mile band closed to
#'   towed gears), for the joining of voyages split by a silence. Without it they are always joined.
#' @param cnr For `"cnr"`: a list of settings, any of `n_in`, `speed_kn`, `gap_h`, `shift_h`, `join_h`,
#'   `min_n`, `buffer_deg`, `reach_m`, `n_closest`, `distance`; those left out keep CNR's values (above).
#' @param iapesca For `"iapesca"`: a list of settings, any of `max_speed_kn`, `max_pass`, `buf_start_m`,
#'   `buf_max_m`, `min_stop_s`, `crs_m` (an EPSG code of a projected CRS in metres); those left out keep
#'   iapesca's values (above).
#' @param batch_pings For `"jepol"` and `"cnr"` on a lazy table: about this many pings are collected at a
#'   time, whole vessels.
#' @param vid,time,port_id,speed,lon,lat The ping columns, if named otherwise.
#'
#' @return One row per voyage: `vid`, `year` (with `by_year`), `voyage_id`, `T1`, `T2`, `port_from`,
#'   `port_to`, `n_pings`. A data frame for data frame input, a lazy table for lazy input.
#'
#' @references For `"jepol"`: the trip rule `define_trips_pol` of the ICES VMS
#'   data-call workflow, after `vmstools` (Hintzen, N.T., Bastardie, F.,
#'   Beare, D., et al. (2012). "VMStools: Open-source software for the
#'   processing, analysis and visualisation of fisheries logbook and VMS
#'   data". Fisheries Research, 115–116, 31–43.
#'   \doi{10.1016/j.fishres.2011.11.007}).
#'
#'   For `"cnr"`: Galdelli, A., Mancini, A., Tassetti, A.N., et al. (2019). "A cloud computing architecture to
#'   map trawling activities using positioning data". Proceedings of the ASME 2019 IDETC/CIE, Volume 9.
#'   \doi{10.1115/DETC2019-97779}; the R code (R4AIS v1.0.1, \doi{10.5281/zenodo.4757505}) by A. Galdelli,
#'   J. Pulcinella, A.N. Tassetti and E. Armelloni (CNR-IRBIM), GNU GPL 3, in the WKSSFGEO repository's
#'   `dev_branch`.
#'
#'   For `"iapesca"`: Rodriguez, J. iapesca: interpretation of fishing paths from vessels' geo-spatial data,
#'   R package version 0.1 (Ifremer, GPL), <https://github.com/JulRodrigu/r-packages_iapesca>.
#' @family trip
#' @export
rb_create_voyages <- function(
  pings,
  port_calls = NULL,
  method = c("dwell", "runs", "jepol", "cnr", "iapesca"),
  min_pings = NULL,
  by_year = TRUE,
  min_dur_h = 0.5,
  max_dur_h = 72,
  split = TRUE,
  ports = NULL,
  coast = NULL,
  cnr = list(),
  iapesca = list(),
  batch_pings = 5e7,
  vid = vid,
  time = time,
  port_id = port_id,
  speed = speed,
  lon = lon,
  lat = lat
) {
  method <- match.arg(method)
  .rb_check_input(
    pings,
    "rb_create_voyages",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time))
    ),
    also = switch(
      method,
      cnr = c(
        port_id = rlang::as_name(rlang::enquo(port_id)),
        speed = rlang::as_name(rlang::enquo(speed)),
        lon = rlang::as_name(rlang::enquo(lon)),
        lat = rlang::as_name(rlang::enquo(lat))
      ),
      iapesca = c(
        lon = rlang::as_name(rlang::enquo(lon)),
        lat = rlang::as_name(rlang::enquo(lat))
      ),
      character(0)
    ),
    types = c(time = "time")
  )
  if (is.null(min_pings)) {
    min_pings <- if (method == "dwell") 10 else 1
  }
  if (method == "dwell" && is.null(port_calls)) {
    stop("Method \"dwell\" needs `port_calls`.", call. = FALSE)
  }
  if (method == "iapesca" && is.null(ports)) {
    stop(
      "Method \"iapesca\" needs `ports`: the harbour polygons.",
      call. = FALSE
    )
  }
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    port_id = {{ port_id }},
    speed = {{ speed }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  use <- switch(
    method,
    dwell = c("vid", "time"),
    cnr = names(map),
    iapesca = c("vid", "time", "lon", "lat"),
    c("vid", "time", "port_id")
  )
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p <- .rb_as_lazy(.rb_std_in(pings, map[use]), con)
  if (method == "cnr") {
    return(.rb_cut_cnr(
      p,
      con,
      lazy_in,
      map,
      min_pings,
      by_year,
      ports,
      coast,
      cnr,
      batch_pings
    ))
  }
  if (method == "iapesca") {
    return(.rb_cut_iapesca(
      p,
      con,
      lazy_in,
      map,
      min_pings,
      by_year,
      ports,
      iapesca
    ))
  }
  if (method == "jepol") {
    return(.rb_cut_jepol(
      p,
      con,
      lazy_in,
      map,
      min_pings,
      by_year,
      min_dur_h,
      max_dur_h,
      split,
      batch_pings
    ))
  }
  ps <- if (method == "dwell") {
    .rb_ping_call_sql(
      .rb_sql(dplyr::select(p, vid, time)),
      .rb_sql(.rb_as_lazy(port_calls, con)),
      by_year
    )
  } else {
    sprintf(
      "SELECT *, port_id AS port_call_port_id, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid, %s AS rb_year FROM (%s)",
      if (by_year) "year(time)" else "0",
      .rb_sql(dplyr::select(p, vid, time, port_id))
    )
  }
  sql <- sprintf(
    "
WITH ps AS (%s),
f AS (SELECT *, port_call_port_id IS NOT NULL AS in_h,
             lag(port_call_port_id IS NOT NULL) OVER w AS p_in, lag(port_call_port_id) OVER w AS p_h
      FROM ps WINDOW w AS (PARTITION BY vid, rb_year ORDER BY time, rb_rid)),
r AS (SELECT *, sum(CASE WHEN p_in IS NULL OR in_h <> p_in OR (in_h AND port_call_port_id <> p_h) THEN 1 ELSE 0 END)
                  OVER (PARTITION BY vid, rb_year ORDER BY time, rb_rid) AS run_id FROM f),
runs AS (SELECT vid, rb_year, run_id, bool_or(in_h) AS in_h, min(port_call_port_id) AS h,
                min(time) AS T1, max(time) AS T2, count(*) AS n_pings
         FROM r GROUP BY vid, rb_year, run_id),
t AS (SELECT *, CASE WHEN NOT in_h THEN lag(h) OVER w END AS port_from,
                CASE WHEN NOT in_h THEN lead(h) OVER w END AS port_to
      FROM runs WINDOW w AS (PARTITION BY vid, rb_year ORDER BY T1)),
v AS (SELECT *, row_number() OVER (PARTITION BY vid, rb_year ORDER BY T1) AS voyage_id FROM t WHERE NOT in_h)
SELECT vid, %s voyage_id, T1, T2, port_from, port_to, n_pings FROM v WHERE n_pings >= %d",
    ps,
    if (by_year) "rb_year AS year," else "",
    as.integer(min_pings)
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(
    out,
    !!!stats::setNames(rlang::syms("vid"), map[["vid"]])
  )
  if (lazy_in) {
    return(out)
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  out[order(out[[map[["vid"]]]], out$T1), ]
}

#' Add the voyage to each ping
#'
#' Step 1 of the fishing-activity flow. Each ping gets the voyage it lies in (`voyage_id`, from
#' [rb_create_voyages()]) and, with `port_calls`, the port of the port call it lies in (`port_call_port_id`) and
#' the port call's key (`port_call_id`, where the port calls carry one). `trip_id` is
#' the voyage and `trip_basis` says where it came from: `"reconstructed"` (the track) or `"none"`. Declared
#' trips (logbook) and landings as sources, and their reconciliation with the voyages, are planned for a
#' later release. No ping is dropped.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`.
#' @param voyages Voyages from [rb_create_voyages()].
#' @param port_calls Optional port calls from [rb_create_port_calls()].
#' @param by_year As in [rb_create_voyages()]: voyages are numbered per vessel and year.
#' @param vid,time The ping columns, if named otherwise.
#'
#' @return `pings` with `port_call_port_id` and `port_call_id` (with `port_calls`; the key where the port calls
#'   carry one), `voyage_id`, `trip_id`, `trip_basis`: the same type, the same rows.
#' @family trip
#' @export
rb_add_voyages <- function(
  pings,
  voyages,
  port_calls = NULL,
  by_year = TRUE,
  vid = vid,
  time = time
) {
  .rb_check_input(
    pings,
    "rb_add_voyages",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time))
    ),
    types = c(time = "time")
  )
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p <- .rb_as_lazy(.rb_std_in(pings, map), con)
  v <- .rb_as_lazy(voyages, con)
  base <- if (is.null(port_calls)) {
    sprintf(
      "SELECT *, NULL::VARCHAR AS port_call_port_id, NULL::BIGINT AS port_call_id, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid FROM (%s)",
      .rb_sql(p)
    )
  } else {
    sprintf(
      "SELECT * EXCLUDE (rb_year) FROM (%s)",
      .rb_ping_call_sql(
        .rb_sql(p),
        .rb_sql(.rb_as_lazy(port_calls, con)),
        by_year,
        call_id = "port_call_id" %in% colnames(port_calls)
      )
    )
  }
  yr <- if (by_year) "AND v.year = year(b.time)" else ""
  drop <- if (is.null(port_calls)) {
    "rb_rid, port_call_port_id, port_call_id"
  } else if ("port_call_id" %in% colnames(port_calls)) {
    "rb_rid"
  } else {
    "rb_rid, port_call_id"
  }
  sql <- sprintf(
    "
WITH b AS (%s), v AS (%s),
j AS (SELECT b.*, v.voyage_id FROM b LEFT JOIN v ON v.vid = b.vid %s AND b.time BETWEEN v.T1 AND v.T2
      QUALIFY row_number() OVER (PARTITION BY b.vid, b.rb_rid ORDER BY v.T1) = 1)
SELECT * EXCLUDE (%s), voyage_id AS trip_id,
       CASE WHEN voyage_id IS NULL THEN 'none' ELSE 'reconstructed' END AS trip_basis
FROM j",
    base,
    .rb_sql(v),
    yr,
    drop
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- .rb_std_out(out, map)
  if (lazy_in) {
    return(out)
  }
  out <- as.data.frame(dplyr::collect(out))
  .rb_unregister(con)
  out
}

#' Link declared trips to voyages by overlap
#'
#' A step-1 link of the fishing-activity flow: each declared trip (e.g. a logbook trip) is linked to the voyage
#' ([rb_create_voyages()]) of the same vessel that overlaps it by the most seconds (ties: more pings).
#' `n_candidates` counts the voyages that overlap it at all; a link is a link, not a merge: one voyage may be
#' linked to several trips.
#'
#' @param trips A data frame or lazy DuckDB table of declared trips: `vid`, `T1`, `T2` and the trip's key
#'   columns (`keys`).
#' @param voyages Voyages from [rb_create_voyages()].
#' @param keys The columns that identify a trip, carried through (e.g. `c(".tid", "schema")`).
#' @param t2_fix `"end_of_t1_day"`: a trip window is extended to at least the end of its departure day, for
#'   logbooks that record `T2` as a date (midnight) or that put `T2` before `T1` on same-day trips; `"none"`.
#' @param vid The vessel column, if named otherwise.
#'
#' @return One row per linked trip: `keys`, `vid`, the voyage's `year`, `voyage_id`, `T1_voyage`, `T2_voyage`,
#'   `port_from`, `port_to`, `n_pings`, `overlap_s`, `trip_span_s`, `overlap_frac`, `n_candidates`, `basis`
#'   (`"ais_overlap"`). A data frame for data frame input, a lazy table for lazy input.
#' @family trip
#' @export
rb_link_trip_voyages <- function(
  trips,
  voyages,
  keys,
  t2_fix = c("end_of_t1_day", "none"),
  vid = vid
) {
  t2_fix <- match.arg(t2_fix)
  map <- .rb_cols(vid = {{ vid }})
  lazy_in <- inherits(trips, "tbl_lazy")
  con <- .rb_con_for(trips)
  tr <- .rb_as_lazy(.rb_std_in(trips, map), con)
  v <- .rb_as_lazy(voyages, con)
  kq <- paste(sprintf('"%s"', keys), collapse = ", ")
  kl <- paste(sprintf('lb."%s"', keys), collapse = ", ")
  fix <- if (t2_fix == "end_of_t1_day") {
    "greatest(T2, date_trunc('day', T1) + INTERVAL 1 DAY - INTERVAL 1 SECOND)"
  } else {
    "T2"
  }
  sql <- sprintf(
    "
WITH lb AS (SELECT %s, vid, T1, T2, %s AS T2_fix FROM (%s) WHERE vid IS NOT NULL AND T1 IS NOT NULL),
v AS (SELECT vid, year, voyage_id, T1, T2, port_from, port_to, n_pings FROM (%s)),
ov AS (SELECT %s, lb.vid, v.year, v.voyage_id, v.T1 AS T1_voyage, v.T2 AS T2_voyage, v.port_from, v.port_to, v.n_pings,
              date_diff('second', greatest(lb.T1, v.T1), least(lb.T2_fix, v.T2)) AS overlap_s,
              date_diff('second', lb.T1, lb.T2_fix) AS trip_span_s
       FROM lb JOIN v ON lb.vid = v.vid AND v.T1 <= lb.T2_fix AND v.T2 >= lb.T1),
ranked AS (SELECT *, row_number() OVER (PARTITION BY %s ORDER BY overlap_s DESC, n_pings DESC, T1_voyage, voyage_id) AS rn,
                  count(*) OVER (PARTITION BY %s) AS n_candidates
           FROM ov WHERE overlap_s > 0)
SELECT %s, vid, year, voyage_id, T1_voyage, T2_voyage, port_from, port_to, n_pings, overlap_s, trip_span_s,
       round(overlap_s::DOUBLE / nullif(trip_span_s, 0), 4) AS overlap_frac, n_candidates, 'ais_overlap' AS basis
FROM ranked WHERE rn = 1",
    kq,
    fix,
    .rb_sql(tr),
    .rb_sql(v),
    kl,
    kq,
    kq,
    kq
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(
    out,
    !!!stats::setNames(rlang::syms("vid"), map[["vid"]])
  )
  if (lazy_in) {
    return(out)
  }
  out <- as.data.frame(dplyr::collect(out))
  .rb_unregister(con)
  out
}

# The "jepol" method: whole vessels collected in batches, the rule applied in R (R/trip_jepol.R), one voyage table.
.rb_cut_jepol <- function(
  p,
  con,
  lazy_in,
  map,
  min_pings,
  by_year,
  min_dur_h,
  max_dur_h,
  split,
  batch_pings
) {
  vn <- p |> dplyr::count(vid) |> dplyr::collect()
  vn <- vn[order(-vn$n, vn$vid), ]
  batches <- if (nrow(vn)) {
    split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  } else {
    list()
  }
  out <- lapply(batches, function(g) {
    d <- p |>
      dplyr::filter(vid %in% g) |>
      dplyr::select(vid, time, port_id) |>
      dplyr::collect()
    d <- d[order(d$vid, d$time, d$port_id, na.last = TRUE), ]
    yr <- as.numeric(format(d$time, "%Y", tz = "UTC"))
    k <- if (by_year) paste(d$vid, yr) else as.character(d$vid)
    tr <- .rb_jepol(
      k,
      d$time,
      as.integer(!is.na(d$port_id)),
      min_dur_h,
      max_dur_h,
      split
    )$trip
    f <- which(!is.na(tr))
    if (!length(f)) {
      return(NULL)
    }
    id <- unique(tr[f])
    first <- f[match(id, tr[f])]
    last <- f[length(f) + 1 - match(id, rev(tr[f]))]
    prev <- pmax(first - 1, 1)
    v <- data.frame(
      vid = d$vid[first],
      year = yr[first],
      k = k[first],
      T1 = d$time[first],
      T2 = d$time[last],
      port_from = ifelse(first > 1 & k[prev] == k[first], d$port_id[prev], NA),
      port_to = d$port_id[last],
      n_pings = as.numeric(tabulate(match(tr[f], id), length(id)))
    )
    v <- v[order(v$k, v$T1), ]
    v$voyage_id <- as.numeric(stats::ave(
      seq_len(nrow(v)),
      v$k,
      FUN = seq_along
    ))
    v
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(
      vid = vector(class(vn$vid)[1], 0),
      year = numeric(0),
      k = character(0),
      T1 = as.POSIXct(character(0), tz = "UTC"),
      T2 = as.POSIXct(character(0), tz = "UTC"),
      port_from = character(0),
      port_to = character(0),
      n_pings = numeric(0),
      voyage_id = numeric(0)
    )
  }
  out <- out[
    out$n_pings >= min_pings,
    c(
      "vid",
      if (by_year) "year",
      "voyage_id",
      "T1",
      "T2",
      "port_from",
      "port_to",
      "n_pings"
    )
  ]
  out <- out[order(out$vid, out$T1), ]
  rownames(out) <- NULL
  names(out)[1] <- map[["vid"]]
  if (!lazy_in) {
    .rb_unregister(con)
    return(out)
  }
  dplyr::tbl(con, .rb_register(con, out, "rb_voy_"))
}
# trip_jepol.R — the ICES datacall's trip rule (define_trips_pol), recoded in base R from rb_trip_jepol()'s
# data.table body (plan 013: recoded, not copied). Sequential (the split loop), so tier 4: one vessel at a time.
#
# A trip runs from the first ping at sea to the first ping back in harbour. Trips of min_dur hours or less are
# dropped; with split, a trip over max_dur hours is cut at its longest gap between pings, again and again.
# The original cut one ping before the longest gap (which.max over INTV[2:.N] indexes from the first row) and
# read the gap before that ping in its 3-minute test; the recode kept both defects so the port could be
# verified against the old function, and the fix of 2026-10-12 (plan 013's logic table) removed them, so the
# output no longer matches rb_trip_jepol() where a voyage is split. Where the original stopped with an error
# (a long trip with fewer than two pings inside it), the trip is left whole, as the original does for a gap
# under 3 minutes.

.rb_jepol_one <- function(time, inh, vessel, min_dur, max_dur, split) {
  o <- order(time)
  time <- time[o]
  inh <- inh[o]
  n <- length(time)
  none <- data.frame(
    vessel = vessel[0],
    trip = character(0),
    depart = time[0],
    arrival = time[0]
  )
  intv <- c(NA, diff(as.numeric(time)) / 60)
  ev <- c(0, -diff(inh))
  ev[ev == -1] <- 2
  if (all(ev == 0)) {
    return(none)
  }
  if (inh[1] == 0) {
    ev[1] <- 1
  }
  # At sea at the end: the last ping closes the open voyage. When the last ping is
  # itself a departure it opens and closes a voyage of one ping, which has no
  # duration and is dropped below. define_trips() overwrote that departure with an
  # arrival, one departure short: the departures were then recycled into a voyage
  # spanning the port call, or the vessel stopped the run ("do not pair") - 791 of
  # 34,286 Icelandic vessel-years with port pings end this way.
  last_dep <- inh[n] == 0 && ev[n] == 1
  if (inh[n] == 0 && !last_dep) {
    ev[n] <- 2
  }
  dep <- time[ev == 1]
  arr <- time[ev == 2]
  if (last_dep) {
    arr <- c(arr, time[n])
  }
  if (length(dep) != length(arr) && length(dep) != 1 && length(arr) != 1) {
    stop(
      "Vessel ",
      vessel,
      ": departures and arrivals do not pair.",
      call. = FALSE
    )
  }
  k <- max(length(dep), length(arr))
  tr <- data.frame(
    trip = paste0(vessel, "_", seq_len(k)),
    depart = rep(dep, length.out = k),
    arrival = rep(arr, length.out = k)
  )
  hrs <- function(d) as.numeric(difftime(d$arrival, d$depart, units = "hours"))
  tr$dur <- hrs(tr)
  tr <- tr[tr$dur > min_dur, ]
  if (split) {
    while (any(tr$dur > max_dur)) {
      i <- which(tr$dur > max_dur)[1]
      inside <- which(time > tr$depart[i] & time < tr$arrival[i])
      j <- if (length(inside) >= 2) which.max(intv[inside]) else integer(0)
      g <- inside[j]
      if (!length(g) || intv[g] < 3) {
        tr$dur[i] <- max_dur
        next
      }
      nt <- data.frame(
        trip = paste(tr$trip[i], 1:2, sep = "_"),
        depart = c(tr$depart[i], time[g]),
        arrival = c(time[g - 1], tr$arrival[i])
      )
      nt$dur <- hrs(nt)
      tr <- rbind(tr[-i, ], nt)
      tr <- tr[order(tr$depart), ]
    }
  }
  tr <- tr[tr$dur != 0, ]
  if (any(tr$dur < min_dur)) {
    warning(
      "At least one trip for ",
      vessel,
      " is shorter than min_dur after splitting at max_dur.",
      call. = FALSE
    )
    tr <- tr[tr$dur > min_dur, ]
  }
  if (!nrow(tr)) {
    return(none)
  }
  data.frame(
    vessel = vessel,
    trip = tr$trip,
    depart = tr$depart,
    arrival = tr$arrival
  )
}

# All vessels: the trip table, and each ping's trip (NA outside every trip).
.rb_jepol <- function(
  vessel,
  time,
  inh,
  min_dur = 0.5,
  max_dur = 72,
  split = TRUE
) {
  if (anyNA(inh)) {
    stop("The in-harbour indicator has missing values.", call. = FALSE)
  }
  idx <- split(seq_along(vessel), vessel)
  trips <- do.call(
    rbind,
    lapply(names(idx), function(v) {
      ii <- idx[[v]]
      .rb_jepol_one(time[ii], inh[ii], vessel[ii][1], min_dur, max_dur, split)
    })
  )
  trip <- rep(NA_character_, length(vessel))
  if (!is.null(trips) && nrow(trips)) {
    for (v in unique(trips$vessel)) {
      ii <- idx[[as.character(v)]]
      t <- trips[trips$vessel == v, ]
      k <- findInterval(as.numeric(time[ii]), as.numeric(t$depart))
      hit <- k > 0 & as.numeric(time[ii]) <= as.numeric(t$arrival[pmax(k, 1)])
      trip[ii[hit]] <- t$trip[k[hit]]
    }
  }
  list(trips = trips, trip = trip)
}
# The "cnr" method: CNR-IRBIM's trip finder, create_fishing_trip() of the WKSSFGEO dev_branch
# (R-dev/galdelli_pulcinella_tassetti/R/global_functions.R, GNU GPL 3), recoded. The state machine and the
# joining of voyages split by a silence run in C++ (src/trip_cnr.cpp, which says what is kept and what is not);
# the ports of the voyage ends are found here, for the few pings that need one, with sf. Whole vessels are
# collected in batches, as for "jepol", so a lazy table (parquet) never comes into memory whole.
.rb_cnr_par <- function(cnr) {
  par <- list(
    n_in = 4L,
    speed_kn = 6,
    gap_h = 4,
    shift_h = 1,
    join_h = 24,
    min_n = 10L,
    buffer_deg = 0.001,
    reach_m = 50000,
    n_closest = 5L,
    distance = "geodesic"
  )
  bad <- setdiff(names(cnr), names(par))
  if (length(bad)) {
    stop(
      "Unknown `cnr` setting(s): ",
      paste(bad, collapse = ", "),
      ". Known: ",
      paste(names(par), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  par <- utils::modifyList(par, cnr)
  if (!par$distance %in% c("geodesic", "sphere")) {
    stop("`cnr$distance` must be \"geodesic\" or \"sphere\".", call. = FALSE)
  }
  par
}

# The ports, once per call: the polygons on WGS84 for distances, and grown by buffer_deg on the plane (the R
# code's st_buffer(ports, 0.001) with s2 off) for the port of an end ping.
.rb_cnr_ports <- function(ports, buffer_deg) {
  if (is.null(ports)) {
    return(NULL)
  }
  if (
    !"port_id" %in% names(ports) ||
      anyDuplicated(ports$port_id) ||
      anyNA(ports$port_id)
  ) {
    stop(
      "`ports` must have a column `port_id`, unique and never NA.",
      call. = FALSE
    )
  }
  hb <- sf::st_transform(ports, 4326)
  buf <- sf::st_buffer(sf::st_set_crs(sf::st_geometry(hb), NA), buffer_deg)
  list(
    id = hb$port_id,
    geom = sf::st_geometry(hb),
    buf = buf,
    area = as.numeric(sf::st_area(buf))
  )
}

.rb_cnr_points <- function(lon, lat, crs = NA) {
  sf::st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs = crs
  )
}

# The port (index into P) whose grown polygon holds each point; of two, the smaller, as rb_flag_ping_port().
.rb_cnr_in_port <- function(lon, lat, P) {
  out <- rep(NA_integer_, length(lon))
  if (!length(lon)) {
    return(out)
  }
  hit <- sf::st_intersects(.rb_cnr_points(lon, lat), P$buf)
  n <- lengths(hit)
  one <- n == 1
  out[one] <- unlist(hit[one])
  for (i in which(n > 1)) {
    h <- hit[[i]]
    out[i] <- h[order(P$area[h], h)][1]
  }
  out
}

# 1 where the point lies in the coastal zone (any of its polygons), else 0; planar, as the R code.
.rb_cnr_coast <- function(lon, lat, coast) {
  if (is.null(coast) || !length(lon)) {
    return(rep(0L, length(lon)))
  }
  pmin(lengths(sf::st_intersects(.rb_cnr_points(lon, lat), coast)), 1L)
}

# Distances in metres from the points to every port, a matrix per call: "geodesic" on the WGS84 ellipsoid
# (lwgeom, which sf::st_distance() calls with s2 off, as the R code ran), "sphere" on ramb's sphere (s2,
# R = 6,371,000 m). Neither reads sf_use_s2(), so the result never depends on that global option, and neither
# is planar: st_nearest_feature() is, with s2 off.
.rb_cnr_dist_fun <- function(P, distance) {
  if (distance == "geodesic") {
    .rb_need("lwgeom")
    return(function(pts) unclass(lwgeom::st_geod_distance(pts, P$geom)))
  }
  .rb_need("s2")
  g <- s2::as_s2_geography(P$geom)
  function(pts) {
    s2::s2_distance_matrix(
      s2::as_s2_geography(sf::st_geometry(pts)),
      g,
      radius = 6371000
    )
  }
}

# The port (index into P) for open voyage ends. recover = FALSE: closest_port(), the closest at any distance.
# recover = TRUE: closest_port_recovery(), of the n_closest ports within reach_m the reference port (the voyage's
# other end) if it is among them, else the closest; NA with none in reach. Ties go to the earlier port.
.rb_cnr_closest <- function(lon, lat, ref, P, par, recover) {
  out <- rep(NA_integer_, length(lon))
  if (!length(lon)) {
    return(out)
  }
  dist <- .rb_cnr_dist_fun(P, par$distance)
  pts <- .rb_cnr_points(lon, lat, 4326)
  for (i in split(seq_along(lon), (seq_along(lon) - 1) %/% 500)) {
    m <- dist(pts[i, ])
    out[i] <- vapply(
      seq_along(i),
      function(r) {
        d <- m[r, ]
        if (!recover) {
          return(which.min(d))
        }
        cand <- which(d <= par$reach_m)
        if (!length(cand)) {
          return(NA_integer_)
        }
        cand <- cand[order(d[cand], cand)][seq_len(min(
          par$n_closest,
          length(cand)
        ))]
        rf <- ref[i[r]]
        if (!is.na(rf) && rf %in% cand) rf else cand[1]
      },
      integer(1)
    )
  }
  out
}

# One batch of whole vessels, sorted by vid, time, lon, lat; k the vessel (or vessel-year) of each ping.
.rb_cnr_batch <- function(d, k, P, coast, par) {
  grp <- match(k, unique(k))
  tm <- as.numeric(d$time)
  cut <- cnr_cut_cpp(
    grp,
    tm,
    as.integer(!is.na(d$port_id)),
    as.numeric(d$speed),
    as.integer(par$n_in),
    par$speed_kn,
    par$gap_h,
    par$shift_h,
    as.integer(par$min_n)
  )
  if (!nrow(cut)) {
    return(NULL)
  }
  # ports are coded as indices into lv, the ports' ids (or, without ports, the pings' own)
  lv <- if (is.null(P)) unique(d$port_id[!is.na(d$port_id)]) else P$id
  end_port <- function(i) {
    out <- rep(NA_integer_, length(i))
    ok <- !is.na(i)
    out[ok] <- if (is.null(P)) {
      match(d$port_id[i[ok]], lv)
    } else {
      .rb_cnr_in_port(d$lon[i[ok]], d$lat[i[ok]], P)
    }
    out
  }
  j <- cnr_join_cpp(
    cut$grp,
    cut$s,
    cut$e,
    end_port(cut$dep),
    end_port(cut$e),
    .rb_cnr_coast(d$lon[cut$s], d$lat[cut$s], coast),
    .rb_cnr_coast(d$lon[cut$e], d$lat[cut$e], coast),
    tm,
    par$join_h
  )
  if (!is.null(P)) {
    # after a silence that was not joined, each side the closest port
    f <- which(j$from_closest)
    t <- which(j$to_closest)
    j$port_from[f] <- .rb_cnr_closest(
      d$lon[j$s[f]],
      d$lat[j$s[f]],
      NULL,
      P,
      par,
      FALSE
    )
    j$port_to[t] <- .rb_cnr_closest(
      d$lon[j$e[t]],
      d$lat[j$e[t]],
      NULL,
      P,
      par,
      FALSE
    )
    # then any end still open; each end's reference is the other end's port before this step
    f0 <- j$port_from
    t0 <- j$port_to
    f <- which(is.na(f0))
    t <- which(is.na(t0))
    j$port_from[f] <- .rb_cnr_closest(
      d$lon[j$s[f]],
      d$lat[j$s[f]],
      t0[f],
      P,
      par,
      TRUE
    )
    j$port_to[t] <- .rb_cnr_closest(
      d$lon[j$e[t]],
      d$lat[j$e[t]],
      f0[t],
      P,
      par,
      TRUE
    )
  }
  data.frame(
    vid = d$vid[j$s],
    year = as.numeric(format(d$time[j$s], "%Y", tz = "UTC")),
    k = k[j$s],
    T1 = d$time[j$s],
    T2 = d$time[j$e],
    port_from = lv[j$port_from],
    port_to = lv[j$port_to],
    n_pings = as.numeric(j$e - j$s + 1)
  )
}

.rb_cut_cnr <- function(
  p,
  con,
  lazy_in,
  map,
  min_pings,
  by_year,
  ports,
  coast,
  cnr,
  batch_pings
) {
  par <- .rb_cnr_par(cnr)
  P <- .rb_cnr_ports(ports, par$buffer_deg)
  if (!is.null(coast)) {
    coast <- sf::st_set_crs(sf::st_geometry(sf::st_transform(coast, 4326)), NA)
  }
  vn <- p |> dplyr::count(vid) |> dplyr::collect()
  vn <- vn[order(-vn$n, vn$vid), ]
  batches <- if (nrow(vn)) {
    split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  } else {
    list()
  }
  out <- lapply(batches, function(g) {
    d <- p |>
      dplyr::filter(vid %in% g) |>
      dplyr::select(vid, time, lon, lat, speed, port_id) |>
      dplyr::collect()
    d <- d[order(d$vid, d$time, d$lon, d$lat), ]
    k <- if (by_year) {
      paste(d$vid, format(d$time, "%Y", tz = "UTC"))
    } else {
      as.character(d$vid)
    }
    v <- .rb_cnr_batch(d, k, P, coast, par)
    if (is.null(v)) {
      return(NULL)
    }
    v <- v[order(v$k, v$T1), ]
    v$voyage_id <- as.numeric(stats::ave(
      seq_len(nrow(v)),
      v$k,
      FUN = seq_along
    ))
    v
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    pid <- dplyr::collect(utils::head(dplyr::select(p, port_id), 0))$port_id
    out <- data.frame(
      vid = vector(class(vn$vid)[1], 0),
      year = numeric(0),
      T1 = as.POSIXct(character(0), tz = "UTC"),
      T2 = as.POSIXct(character(0), tz = "UTC"),
      port_from = if (is.null(P)) pid else P$id[0],
      port_to = if (is.null(P)) pid else P$id[0],
      n_pings = numeric(0),
      voyage_id = numeric(0)
    )
  }
  out <- out[
    out$n_pings >= min_pings,
    c(
      "vid",
      if (by_year) "year",
      "voyage_id",
      "T1",
      "T2",
      "port_from",
      "port_to",
      "n_pings"
    )
  ]
  out <- out[order(out$vid, out$T1), ]
  rownames(out) <- NULL
  names(out)[1] <- map[["vid"]]
  if (!lazy_in) {
    .rb_unregister(con)
    return(out)
  }
  dplyr::tbl(con, .rb_register(con, out, "rb_voy_"))
}

# The "iapesca" method: Ifremer's Detect_FishingTrips() with harbours (iapesca 0.1, J. Rodriguez, GPL), recoded as
# one DuckDB query; checked against the R function, ping for ping, in fishycode plan 031's bench. Planar in a metric
# CRS, as the R function is with projected input. Kept from the R function so the two agree:
# - a ping is dropped as a duplicate when its time and its position each occurred on an earlier ping, matched
#   separately (CalcSpeed's duplicated(time) & duplicated(geometry));
# - min_stop_s is the time since the previous voyage end, not the length of the stop;
# - the buffer is 1 % of the diagonal only between buf_start_m and buf_max_m, and buf_start_m above the cap.
# Not kept: the R function's NA rows when a duplicate and a spurious speed meet, and its midnight parsing.
# "In port" is the exact distance to the nearest harbour, and only pings within 2 m inside a buffer's edge are
# tested against GEOS's 30-segment buffer polygon, which lies inside the exact offset, at most 1.3 m short at 3.7 km.
.rb_iapesca_par <- function(iapesca) {
  par <- list(
    max_speed_kn = 25,
    max_pass = 5L,
    buf_start_m = 2000,
    buf_max_m = 2 / 0.539957 * 1000,
    min_stop_s = 3600,
    crs_m = NULL
  )
  bad <- setdiff(names(iapesca), names(par))
  if (length(bad)) {
    stop(
      "Unknown `iapesca` setting(s): ",
      paste(bad, collapse = ", "),
      ". Known: ",
      paste(names(par), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  utils::modifyList(par, iapesca)
}

.rb_cut_iapesca <- function(
  p,
  con,
  lazy_in,
  map,
  min_pings,
  by_year,
  ports,
  iapesca
) {
  par <- .rb_iapesca_par(iapesca)
  tryCatch(DBI::dbExecute(con, "LOAD spatial"), error = function(e) {
    DBI::dbExecute(con, "INSTALL spatial; LOAD spatial")
  })
  if (is.null(par$crs_m)) {
    m <- dplyr::collect(dplyr::summarise(
      p,
      lon = mean(lon, na.rm = TRUE),
      lat = mean(lat, na.rm = TRUE)
    ))
    par$crs_m <- (if (isTRUE(m$lat >= 0)) 32600 else 32700) +
      floor((m$lon + 180) / 6) +
      1
  }
  crs <- sf::st_crs(par$crs_m)
  if (
    is.na(crs) ||
      isTRUE(crs$IsGeographic) ||
      !identical(crs$units_gdal, "metre")
  ) {
    stop("`iapesca$crs_m` must be a projected CRS in metres.", call. = FALSE)
  }
  if (!"port_id" %in% names(ports) || anyNA(ports$port_id)) {
    stop("`ports` must have a column `port_id`, never NA.", call. = FALSE)
  }
  hb <- sf::st_transform(ports, crs)
  hw <- data.frame(port_id = hb$port_id)
  hw$rb_wkb <- lapply(sf::st_as_binary(sf::st_geometry(hb)), as.raw)
  hn <- .rb_register(con, hw, "rb_harb_")
  reach <- par$buf_max_m + 2
  pass <- function(k) {
    sprintf(
      "
f%1$d AS (SELECT *, CASE WHEN pt IS NULL THEN FALSE WHEN t = pt THEN nm > 0 ELSE nm / ((t - pt) / 3600) >= %3$.17g END AS fl
          FROM (SELECT *, lag(t) OVER w AS pt, sqrt((x - lag(x) OVER w)^2 + (y - lag(y) OVER w)^2) * 0.539957 / 1000 AS nm
                FROM c%2$d WINDOW w AS (PARTITION BY vid, rb_year ORDER BY rid))),
c%1$d AS (SELECT vid, rb_year, time, rid, t, x, y FROM (
          SELECT *, fl AND NOT coalesce(lag(fl) OVER (PARTITION BY vid, rb_year ORDER BY rid), FALSE) AS dr FROM f%1$d)
          WHERE NOT dr)",
      k,
      k - 1,
      par$max_speed_kn
    )
  }
  np <- as.integer(par$max_pass)
  bs <- sprintf("%.17g", par$buf_start_m)
  bm <- sprintf("%.17g", par$buf_max_m)
  ms <- sprintf("%.17g", par$min_stop_s)
  sql <- paste0(
    "
WITH
s0 AS MATERIALIZED (
  SELECT vid, rb_year, time, epoch(time) AS t, ST_X(g) AS x, ST_Y(g) AS y,
         row_number() OVER (PARTITION BY vid, rb_year ORDER BY time, lon, lat) AS rid
  FROM (SELECT vid, time, lon, lat, ",
    if (by_year) "year(time)" else "0",
    " AS rb_year,
               ST_Transform(ST_Point(lon, lat), 'EPSG:4326', '",
    crs$input,
    "', always_xy := true) AS g
        FROM (",
    .rb_sql(dplyr::select(p, vid, time, lon, lat)),
    ")
        WHERE time IS NOT NULL AND lon IS NOT NULL AND lat IS NOT NULL)),
c0 AS (SELECT vid, rb_year, time, rid, t, x, y FROM (
       SELECT *, row_number() OVER (PARTITION BY vid, rb_year, t ORDER BY rid) > 1
                 AND row_number() OVER (PARTITION BY vid, rb_year, x, y ORDER BY rid) > 1 AS dup FROM s0)
       WHERE NOT dup),",
    if (np > 0) {
      paste0(paste(sapply(seq_len(np), pass), collapse = ","), ",")
    } else {
      ""
    },
    "
cl AS MATERIALIZED (SELECT *, row_number() OVER (PARTITION BY vid, rb_year ORDER BY rid) AS r FROM c",
    np,
    "),
v AS MATERIALIZED (SELECT vid, rb_year, count(*) AS n, floor(((max(t) - min(t)) / count(*)) / ",
    ms,
    ") + 1 AS mtl,
                          sqrt((max(x) - min(x))^2 + (max(y) - min(y))^2) / 100 AS inc FROM cl GROUP BY ALL),
h AS MATERIALIZED (SELECT port_id, g AS geom, ST_XMin(g) - ",
    reach,
    " AS x0, ST_XMax(g) + ",
    reach,
    " AS x1,
                          ST_YMin(g) - ",
    reach,
    " AS y0, ST_YMax(g) + ",
    reach,
    " AS y1
                   FROM (SELECT port_id, ST_GeomFromWKB(rb_wkb) AS g FROM ",
    hn,
    ")),
d AS MATERIALIZED (SELECT c.vid, c.rb_year, c.r, c.x, c.y, min(ST_Distance(h.geom, ST_Point(c.x, c.y))) AS d,
                          arg_min(h.port_id, ST_Distance(h.geom, ST_Point(c.x, c.y))) AS pid
                   FROM cl c JOIN h ON c.x BETWEEN h.x0 AND h.x1 AND c.y BETWEEN h.y0 AND h.y1 GROUP BY ALL),
b AS MATERIALIZED (SELECT vid, rb_year, j,
                          least(CASE WHEN inc > ",
    bs,
    " AND inc < ",
    bm,
    " THEN inc ELSE ",
    bs,
    " END + j * inc, ",
    bm,
    ") AS bj
                   FROM v, range(0, 6) r(j)),
x AS MATERIALIZED (SELECT d.vid, d.rb_year, d.r, b.j, bool_or(ST_Intersects(ST_Buffer(h.geom, b.bj, 30), ST_Point(d.x, d.y))) AS inp
                   FROM d JOIN b ON b.vid = d.vid AND b.rb_year = d.rb_year AND d.d > b.bj - 2 AND d.d <= b.bj
                   JOIN h ON d.x BETWEEN h.x0 AND h.x1 AND d.y BETWEEN h.y0 AND h.y1 GROUP BY ALL),
jj AS (SELECT b.vid, b.rb_year, b.j, b.bj, coalesce(bool_or(d.d <= b.bj - 2 OR coalesce(x.inp, FALSE)), FALSE) AS any_in
       FROM b LEFT JOIN d ON d.vid = b.vid AND d.rb_year = b.rb_year
       LEFT JOIN x ON x.vid = d.vid AND x.rb_year = d.rb_year AND x.r = d.r AND x.j = b.j GROUP BY ALL),
js AS (SELECT vid, rb_year, min(j) AS js FROM jj WHERE any_in OR j = 5 OR bj >= ",
    bm,
    " GROUP BY ALL),
fb AS (SELECT jj.vid, jj.rb_year, jj.j, jj.bj AS buf FROM jj JOIN js ON js.vid = jj.vid AND js.rb_year = jj.rb_year AND js.js = jj.j),
ip AS MATERIALIZED (SELECT c.vid, c.rb_year, c.r, c.t, c.time, d.pid,
                           coalesce(d.d <= fb.buf - 2, FALSE) OR coalesce(x.inp, FALSE) AS inp
                    FROM cl c JOIN fb ON fb.vid = c.vid AND fb.rb_year = c.rb_year
                    LEFT JOIN d ON d.vid = c.vid AND d.rb_year = c.rb_year AND d.r = c.r
                    LEFT JOIN x ON x.vid = c.vid AND x.rb_year = c.rb_year AND x.r = c.r AND x.j = fb.j),
sr AS (SELECT vid, rb_year, r, lead(r) OVER (PARTITION BY vid, rb_year ORDER BY r) AS nr FROM ip WHERE NOT inp),
cand AS (SELECT DISTINCT q.vid, q.rb_year, q.c
         FROM (SELECT vid, rb_year, 1 AS c FROM v
               UNION ALL SELECT s.vid, s.rb_year, s.r FROM sr s JOIN v ON v.vid = s.vid AND v.rb_year = s.rb_year
               WHERE s.nr - s.r > v.mtl) q
         JOIN v ON v.vid = q.vid AND v.rb_year = q.rb_year WHERE q.c < v.n - v.mtl),
ct AS (SELECT k.vid, k.rb_year, k.c, ip.t, lag(ip.t) OVER (PARTITION BY k.vid, k.rb_year ORDER BY k.c) AS pt
       FROM cand k JOIN ip ON ip.vid = k.vid AND ip.rb_year = k.rb_year AND ip.r = k.c),
kept AS (SELECT vid, rb_year, c, row_number() OVER (PARTITION BY vid, rb_year ORDER BY c) AS k
         FROM ct WHERE pt IS NULL OR t - pt > ",
    ms,
    "),
lab AS (SELECT s.vid, s.rb_year, s.r, s.time, coalesce(k.k, 1) AS lab FROM (SELECT * FROM ip WHERE NOT inp) s
        ASOF LEFT JOIN kept k ON s.vid = k.vid AND s.rb_year = k.rb_year AND s.r > k.c),
tr AS (SELECT vid, rb_year, lab, min(time) AS T1, max(time) AS T2, min(r) AS r1, max(r) AS r2, count(*) AS n_pings
       FROM lab GROUP BY ALL)
SELECT tr.vid, ",
    if (by_year) "tr.rb_year AS year, " else "",
    "CAST(tr.lab AS DOUBLE) AS voyage_id, tr.T1, tr.T2,
       CASE WHEN a.inp THEN a.pid END AS port_from, CASE WHEN z.inp THEN z.pid END AS port_to,
       CAST(tr.n_pings AS DOUBLE) AS n_pings
FROM tr LEFT JOIN ip a ON a.vid = tr.vid AND a.rb_year = tr.rb_year AND a.r = tr.r1 - 1
LEFT JOIN ip z ON z.vid = tr.vid AND z.rb_year = tr.rb_year AND z.r = tr.r2 + 1
WHERE tr.n_pings >= ",
    as.integer(min_pings)
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(
    out,
    !!!stats::setNames(rlang::syms("vid"), map[["vid"]])
  )
  if (lazy_in) {
    return(out)
  }
  out <- as.data.frame(dplyr::collect(out))
  .rb_unregister(con)
  out <- out[order(out[[map[["vid"]]]], out$T1), ]
  rownames(out) <- NULL
  out
}

#' Add the logbook trip to each VMS record
#'
#' Adds the logbook trip id to each VMS record of the same vessel whose
#' timestamp falls between the trip's departure and arrival; unmatched
#' records get `NA`. The match of `vmstools::mergeEflalo2Tacsat()`, but with
#' the column names passed as arguments rather than fixed.
#'
#' @param ve A data frame of VMS records, with the `vessel_id` and `time`
#'   columns.
#' @param le A data frame of logbook trips, with the `vessel_id`, `trip_id`,
#'   `departure` and `arrival` columns.
#' @param vessel_id The vessel id column (in both data frames), default `vid`.
#' @param trip_id The trip id column in `le`, default `tid`.
#' @param time The timestamp column in `ve`, default `time`.
#' @param departure,arrival The trip start and end columns in `le`, defaults
#'   `T1` and `T2`.
#'
#' @return `ve` with the trip id column added (the departure and arrival
#'   columns are not carried over).
#'
#' @references Hintzen, N.T., Bastardie, F., Beare, D., et al. (2012).
#'   "VMStools: Open-source software for the processing, analysis and
#'   visualisation of fisheries logbook and VMS data". Fisheries Research,
#'   115–116, 31–43. \doi{10.1016/j.fishres.2011.11.007}.
#' @export
rb_add_logbook_trip <- function(
  ve,
  le,
  vessel_id = vid,
  trip_id = tid,
  time = time,
  departure = T1,
  arrival = T2
) {
  # Turn column names into strings for checking
  vessel_id_chr <- as.character(substitute(vessel_id))
  trip_id_chr <- as.character(substitute(trip_id))
  time_chr <- as.character(substitute(time))
  departure_chr <- as.character(substitute(departure))
  arrival_chr <- as.character(substitute(arrival))

  # Check for required columns in VMS data
  vms_missing <- setdiff(c(vessel_id_chr, time_chr), names(ve))
  if (length(vms_missing) > 0) {
    warning(
      "Missing the following columns in VMS data: ",
      paste(vms_missing, collapse = ", ")
    )
  }

  # Check for required columns in logbook data
  logbook_missing <- setdiff(
    c(vessel_id_chr, trip_id_chr, departure_chr, arrival_chr),
    names(le)
  )
  if (length(logbook_missing) > 0) {
    warning(
      "Missing the following columns in logbook data: ",
      paste(logbook_missing, collapse = ", ")
    )
  }

  # A missing join/id column means the join below cannot run at all - return
  # the VMS data unmodified rather than letting dplyr::left_join error out
  # after the warning above has already told the caller why.
  if (length(vms_missing) > 0 || length(logbook_missing) > 0) {
    return(ve)
  }

  out <- ve |>
    dplyr::left_join(
      le |> dplyr::distinct({{ vessel_id }}, {{ trip_id }}, .keep_all = TRUE),
      by = dplyr::join_by(
        {{ vessel_id }},
        dplyr::between({{ time }}, {{ departure }}, {{ arrival }})
      )
    ) |>
    dplyr::select(-dplyr::any_of(c(departure_chr, arrival_chr)))

  # Warn if all trip_ids are NA after join
  if (all(is.na(out[[trip_id_chr]]))) {
    warning(
      "No trip IDs were matched to the VMS data. Check if timestamps and vessel IDs overlap between datasets."
    )
  }

  # Warn only on a name collision: dplyr then suffixes (.x/.y) and the
  # logbook's departure/arrival columns survive the drop in the join above
  clash <- intersect(c(departure_chr, arrival_chr), names(ve))
  if (length(clash) > 0) {
    warning(
      "Column(s) ",
      paste(clash, collapse = ", "),
      " occur in both 've' and the logbook's departure/arrival columns; ",
      "the join suffixes them (.x/.y) instead of dropping the logbook side."
    )
  }

  return(out)
}


#' Pad the port id onto the ends of a voyage
#'
#' The first ping of a voyage gets the previous ping's port id, and the last
#' ping gets the next one's, so a voyage's end pings carry the ports it sailed
#' between. A ping in port has no voyage: `NA`, as [rb_add_voyages()] leaves
#' it, or an id of 0 or less. Where the neighbour has no port id, the ping keeps
#' its own. The pings are in time order, one vessel at a time.
#'
#' @param voyage_id A voyage id, e.g. the `voyage_id` column from
#'   [rb_add_voyages()].
#' @param port_id A port id, e.g. the `port_id` column from
#'   [rb_flag_ping_port()]: the ports' own key, not a source's port code.
#'
#' @return The `port_id` vector, padded.
#' @export
#' @examples
#' rb_pad_harbour(voyage_id = c(NA, 1, 1, 1, NA), port_id = c(7, NA, NA, NA, 9))
rb_pad_harbour <- function(voyage_id, port_id) {
  sea <- !is.na(voyage_id) & voyage_id > 0
  differs <- function(x) is.na(x) | x != voyage_id
  dplyr::case_when(
    sea & differs(dplyr::lag(voyage_id)) ~ dplyr::coalesce(
      dplyr::lag(port_id),
      port_id
    ),
    sea & differs(dplyr::lead(voyage_id)) ~ dplyr::coalesce(
      dplyr::lead(port_id),
      port_id
    ),
    TRUE ~ port_id
  )
}

#' Link trips to the landings that ended them
#'
#' A step-1 builder: which landing (a sales note, a landing declaration) belongs to which trip. The trips can be
#' declared trips (a logbook's) or voyages cut from the track ([rb_create_voyages()]); the landings come from
#' a buyer or an authority, dated, sometimes with a port. Matching is a cascade of steps from the strictest
#' (same day, same port) to the loosest (two days apart, any port). A step works only on the trips and landings no
#' earlier step took, and accepts a pair only when it is the sole candidate both ways, so an ambiguous match is
#' left for a looser step or left unmatched, never guessed. `match_type` names the step.
#'
#' With `since_previous_days`, a last step links each trip still unmatched to the vessel's next landing within that
#' many days of the trip's end, matched or not: one landing then covers several trips (a weekly landing, or trips
#' landed together). `n_trips` counts the trips on each landing, so a consumer can tell one-to-one from
#' many-to-one (fishycode plan 013, item 5).
#'
#' @param trips A data frame or lazy DuckDB table: the `trip_key` columns, `vid`, `T2` (when the trip ended) and
#'   optionally the arrival port.
#' @param landings A data frame or lazy table: the `landing_key` columns, `vid`, `date` and optionally the port.
#' @param trip_key,landing_key The columns that identify a trip and a landing; their names must differ.
#' @param port_trip,port_landing The port columns, or `NULL`. Steps with `port = TRUE` need both.
#' @param steps The cascade: a data frame with `name`, `dt_days` (how far apart the trip's end date and the
#'   landing date may be) and `port` (whether the ports must agree), in the order tried.
#' @param since_previous_days `NULL`, or the last step's reach in days (e.g. 14).
#' @param vid,T2,date The columns.
#' @return One row per link: the trip key, the landing key, `vid`, `match_type`, `dt_days` (the landing date minus
#'   the trip's end date), `n_trips` (trips on the landing) and `n_landings` (landings on the trip). A data frame.
#' @export
#' @examples
#' trips <- tibble::tibble(
#'   tid = 1:4, vid = 1L, port = c("A", "A", "B", "B"),
#'   T2 = as.POSIXct(c("2024-03-01 18:00", "2024-03-03 20:00", "2024-03-05 23:30", "2024-03-06 17:00"), tz = "UTC")
#' )
#' landings <- tibble::tibble(
#'   lid = c(10, 11, 12), vid = 1L, port = c("A", "A", "B"),
#'   date = as.Date(c("2024-03-01", "2024-03-04", "2024-03-07"))
#' )
#' rb_link_trip_landings(trips, landings, "tid", "lid", port_trip = "port", port_landing = "port",
#'   since_previous_days = 3)
rb_link_trip_landings <- function(
  trips,
  landings,
  trip_key,
  landing_key,
  port_trip = NULL,
  port_landing = NULL,
  steps = data.frame(
    name = c("date_port", "date1_port", "date", "date1", "date2"),
    dt_days = c(0, 1, 0, 1, 2),
    port = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  ),
  since_previous_days = NULL,
  vid = "vid",
  T2 = "T2",
  date = "date"
) {
  if (length(intersect(trip_key, landing_key)) > 0) {
    rlang::abort(c(
      "`trip_key` and `landing_key` must have different names.",
      x = sprintf(
        "Both have %s.",
        paste0("`", intersect(trip_key, landing_key), "`", collapse = ", ")
      ),
      i = "Rename one of them, e.g. `schema` to `schema_land`."
    ))
  }
  .rb_check_input(
    trips,
    "rb_link_trip_landings",
    cols = c(vid = vid, T2 = T2),
    also = c(
      stats::setNames(trip_key, trip_key),
      if (!is.null(port_trip)) c(port_trip = port_trip)
    )
  )
  .rb_need_cols(landings, "landings", c(landing_key, vid, date, port_landing))
  if (any(steps$port) && (is.null(port_trip) || is.null(port_landing))) {
    steps <- steps[!steps$port, ]
  }
  con <- .rb_con_for(trips)
  q <- function(x) if (length(x) == 0) character(0) else .rb_q(con, x)
  t_sql <- .rb_in_sql(trips, con)
  l_sql <- .rb_in_sql(landings, con)
  tk <- paste(sprintf("t.%s", q(trip_key)), collapse = ", ")
  lk <- paste(sprintf("l.%s", q(landing_key)), collapse = ", ")
  tp <- if (is.null(port_trip)) {
    "CAST(NULL AS VARCHAR)"
  } else {
    sprintf("CAST(t.%s AS VARCHAR)", q(port_trip))
  }
  lp <- if (is.null(port_landing)) {
    "CAST(NULL AS VARCHAR)"
  } else {
    sprintf("CAST(l.%s AS VARCHAR)", q(port_landing))
  }
  # one row per trip and per landing, with a single text key each
  tt <- .rb_fill(
    "SELECT {tk}, t.{q(vid)} AS rb_vid, CAST(t.{q(T2)} AS DATE) AS rb_d, {tp} AS rb_p,
            concat_ws('|', {paste(sprintf('CAST(t.%s AS VARCHAR)', q(trip_key)), collapse = ', ')}) AS rb_tk
     FROM ({t_sql}) t"
  )
  ll <- .rb_fill(
    "SELECT {lk}, l.{q(vid)} AS rb_vid, CAST(l.{q(date)} AS DATE) AS rb_d, {lp} AS rb_p,
            concat_ws('|', {paste(sprintf('CAST(l.%s AS VARCHAR)', q(landing_key)), collapse = ', ')}) AS rb_lk
     FROM ({l_sql}) l"
  )
  taken_t <- character(0)
  taken_l <- character(0)
  links <- list()
  for (i in seq_len(nrow(steps))) {
    tu <- .rb_sql(dplyr::tbl(
      con,
      .rb_register(con, data.frame(rb_tk = taken_t))
    ))
    lu <- .rb_sql(dplyr::tbl(
      con,
      .rb_register(con, data.frame(rb_lk = taken_l))
    ))
    pc <- if (steps$port[i]) {
      "AND t.rb_p IS NOT NULL AND t.rb_p = l.rb_p"
    } else {
      ""
    }
    d <- DBI::dbGetQuery(
      con,
      .rb_fill(
        "WITH t AS (SELECT * FROM ({tt}) WHERE rb_tk NOT IN (SELECT rb_tk FROM ({tu}))),
              l AS (SELECT * FROM ({ll}) WHERE rb_lk NOT IN (SELECT rb_lk FROM ({lu}))),
              c AS (SELECT t.rb_tk, l.rb_lk, l.rb_d - t.rb_d AS dt_days
                    FROM t JOIN l ON l.rb_vid = t.rb_vid AND abs(l.rb_d - t.rb_d) <= {steps$dt_days[i]} {pc}),
              n AS (SELECT *, count(*) OVER (PARTITION BY rb_tk) AS nt, count(*) OVER (PARTITION BY rb_lk) AS nl FROM c)
         SELECT rb_tk, rb_lk, dt_days FROM n WHERE nt = 1 AND nl = 1"
      )
    )
    if (nrow(d) > 0) {
      d$match_type <- steps$name[i]
      links[[length(links) + 1]] <- d
      taken_t <- c(taken_t, d$rb_tk)
      taken_l <- c(taken_l, d$rb_lk)
    }
  }
  if (!is.null(since_previous_days)) {
    tu <- .rb_sql(dplyr::tbl(
      con,
      .rb_register(con, data.frame(rb_tk = taken_t))
    ))
    d <- DBI::dbGetQuery(
      con,
      .rb_fill(
        "WITH t AS (SELECT * FROM ({tt}) WHERE rb_tk NOT IN (SELECT rb_tk FROM ({tu})))
         SELECT t.rb_tk, l.rb_lk, l.rb_d - t.rb_d AS dt_days
         FROM t JOIN ({ll}) l ON l.rb_vid = t.rb_vid AND l.rb_d >= t.rb_d AND l.rb_d - t.rb_d <= {since_previous_days}
         QUALIFY row_number() OVER (PARTITION BY t.rb_tk ORDER BY l.rb_d, l.rb_lk) = 1"
      )
    )
    if (nrow(d) > 0) {
      d$match_type <- "since_previous"
      links[[length(links) + 1]] <- d
    }
  }
  lk_df <- if (length(links)) {
    do.call(rbind, links)
  } else {
    data.frame(
      rb_tk = character(0),
      rb_lk = character(0),
      dt_days = numeric(0),
      match_type = character(0)
    )
  }
  m_sql <- .rb_sql(dplyr::tbl(con, .rb_register(con, lk_df)))
  out <- DBI::dbGetQuery(
    con,
    .rb_fill(
      "WITH m AS ({m_sql}),
            t AS ({tt}), l AS ({ll})
       SELECT {paste(sprintf('t.%s', q(trip_key)), collapse = ', ')}, {paste(sprintf('l.%s', q(landing_key)), collapse = ', ')},
              t.rb_vid AS {q(vid)}, m.match_type, CAST(m.dt_days AS INTEGER) AS dt_days,
              count(*) OVER (PARTITION BY m.rb_lk) AS n_trips, count(*) OVER (PARTITION BY m.rb_tk) AS n_landings
       FROM m JOIN t ON t.rb_tk = m.rb_tk JOIN l ON l.rb_lk = m.rb_lk"
    )
  )
  .rb_unregister(con)
  tibble::as_tibble(out[
    do.call(order, unname(as.list(out[c(vid, trip_key)]))),
  ])
}
