# find_trip_stays.R — a step-1 builder: port stays, measured in elapsed time.
#
# Ported from fishycode's curate/port_stay.R (plan 011, decisions 060-064; ramb plan 013 phase 2). A stay
# is time spent at a port, not a ping inside its polygon: one port ping followed by silence is not a
# stay. Runs in DuckDB (tier 2): a data frame is registered and the result collected; a lazy table stays lazy.

.rb_con_for <- function(x) {
  if (inherits(x, "tbl_lazy")) {
    return(dbplyr::remote_con(x))
  }
  duckdbfs::cached_connection()
}
# Data frames registered for one call. A call on data frames releases them once it has collected its
# result: registered frames left behind in a loop accumulate (fishycode R/write_parquet.R).
.rb_reg <- new.env(parent = emptyenv())
.rb_register <- function(con, x, prefix = "rb_in_") {
  nm <- paste0(prefix, paste(sample(c(letters, 0:9), 10, TRUE), collapse = ""))
  duckdb::duckdb_register(con, nm, as.data.frame(x))
  assign(nm, TRUE, envir = .rb_reg)
  nm
}
.rb_unregister <- function(con) {
  for (nm in ls(.rb_reg)) {
    try(duckdb::duckdb_unregister(con, nm), silent = TRUE)
    rm(list = nm, envir = .rb_reg)
  }
}
.rb_as_lazy <- function(x, con) {
  if (inherits(x, "tbl_lazy")) {
    return(x)
  }
  dplyr::tbl(con, .rb_register(con, x))
}
.rb_sql <- function(x) as.character(dbplyr::sql_render(dplyr::ungroup(x)))

#' Find port stays in vessel tracks
#'
#' A step-1 builder of the fishing-activity flow: the stays that cut a track into voyages
#' ([rb_cut_trip_voyages()]). A stay is elapsed time at a port, from any of three rules on the pings
#' and, optionally, from declared port events:
#'
#' * `tag_then_gap`: two consecutive pings at the same port, the first within `radius_in` of it, at least
#'   the minimum stay apart, one of them inside the polygon;
#' * `gap_near_port`: the same, neither inside the polygon;
#' * `tag_span`: a run of pings inside the polygon spanning at least the minimum stay;
#' * `event_pair` (with `events`): an arrival then a departure event at the same port, adjacent in the
#'   vessel's events, both judged good, at least the minimum stay apart.
#'
#' A ping takes the port among those whose `radius_out` buffer holds it, preferring one whose
#' `radius_in` buffer also holds it, then the nearest. Seeds are extended over the contiguous inside pings
#' at either end and merged; ping stays and event stays are merged in time. `rule` names the strongest
#' evidence: event_pair > tag_then_gap > tag_span > gap_near_port.
#'
#' @param pings A data frame or lazy DuckDB table of cleaned pings: `vid`, `time`, `lon`, `lat`.
#' @param ports An sf object of port polygons with `port_id`, and optionally `radius_out` (metres;
#'   the departure radius, at least `radius_in`).
#' @param min_stay_h Minimum stay, hours, where `min_stay` has no row for the port.
#' @param min_stay Optional data frame `port_id`, `min_stay_h`: a minimum per port.
#' @param radius_in Arrival radius, metres.
#' @param events Optional data frame or lazy table of port events: `vid`, `time`, `io` (`"I"` arrival,
#'   `"U"` departure), `port_id`, `good` (logical).
#' @param t_in_window Optional `c(from, to)` (POSIXct): keep only stays that begin in `[from, to)`, applied
#'   before ping and event stays are merged. For processing a long series in chunks.
#' @param crs_m A metric CRS for the buffers (EPSG code). The default, 3575, suits the North Atlantic.
#' @param vid,time,lon,lat The ping columns, if named otherwise.
#'
#' @return One row per stay: `vid`, `port_id`, `T_in`, `T_out`, `dur_h`, `rule`, `evidence`,
#'   `T_in_ping`, `T_out_ping`, `T_in_ev`, `T_out_ev`, `basis` (`"reconstructed"`). A data frame for data
#'   frame input, a lazy table for lazy input.
#' @family trip
#' @export
rb_find_trip_stays <- function(
  pings,
  ports,
  min_stay_h = 4,
  min_stay = NULL,
  radius_in = 2000,
  events = NULL,
  t_in_window = NULL,
  crs_m = 3575,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat
) {
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
  hg <- if (is.null(min_stay)) {
    data.frame(port_id = character(0), g_h = numeric(0))
  } else {
    data.frame(port_id = min_stay$port_id, g_h = min_stay$min_stay_h)
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
  G <- min_stay_h
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
pstay AS (SELECT vid, h AS pid, min(t_s) AS t_in, max(t_e) AS t_out, list_distinct(list(rule)) AS rules FROM g GROUP BY vid, h, grp),
ev AS (SELECT vid, time, io, port_id AS hid_pid, good,
              lead(io) OVER w AS nio, lead(time) OVER w AS nt, lead(port_id) OVER w AS nh, lead(good) OVER w AS ngood
       FROM (%s) WINDOW w AS (PARTITION BY vid ORDER BY time, io)),
estay AS (SELECT vid, hid_pid AS pid, time AS t_in, nt AS t_out, ['event_pair'] AS rules
          FROM ev LEFT JOIN hg ON hg.pid = ev.hid_pid
          WHERE io = 'I' AND nio = 'U' AND good AND ngood AND hid_pid IS NOT NULL AND nh = hid_pid
            AND epoch(nt - time) >= coalesce(hg.g_h, %f) * 3600 AND %s),
u AS (SELECT vid, pid, t_in, t_out, rules, 'ping' AS src FROM pstay WHERE %s
      UNION ALL SELECT vid, pid, t_in, t_out, rules, 'ev' FROM estay),
m2 AS (SELECT *, max(t_out) OVER (PARTITION BY vid, pid ORDER BY t_in, t_out ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS pe FROM u),
g2 AS (SELECT *, sum(CASE WHEN pe IS NULL OR t_in > pe THEN 1 ELSE 0 END) OVER (PARTITION BY vid, pid ORDER BY t_in, t_out) AS grp FROM m2),
st AS (SELECT vid, pid, min(t_in) AS T_in, max(t_out) AS T_out,
              min(t_in) FILTER (src = 'ping') AS T_in_ping, max(t_out) FILTER (src = 'ping') AS T_out_ping,
              min(t_in) FILTER (src = 'ev') AS T_in_ev, max(t_out) FILTER (src = 'ev') AS T_out_ev,
              array_to_string(list_sort(list_distinct(flatten(list(rules)))), '+') AS evidence
       FROM g2 GROUP BY vid, pid, grp)
SELECT vid, pid AS port_id, T_in, T_out, epoch(T_out - T_in) / 3600 AS dur_h,
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
# cut_trip_voyages.R — step 1: voyages from port stays, and the trip each ping belongs to.
#
# Ported from fishycode's curate/ais_trip.R (decision 061; ramb plan 013 phase 2). Tier 2: one DuckDB query
# each; a data frame is registered and the result collected, a lazy table stays lazy.

# A ping takes the stay it falls in; inside two overlapping stays, the earlier one. Rows are numbered within the
# vessel (rb_rid), not over the whole table: an unpartitioned row_number() runs on one core, which made the
# step 5.5 times slower than the code it replaced (fishycode TODO 48).
# With `stay_id` in the stays, the ping also takes the stay's key, so a ping can carry the key and not the
# stay's values (fishycode plan 014).
.rb_ping_stay_sql <- function(p_sql, s_sql, by_year, stay_id = FALSE) {
  sprintf(
    "
WITH p AS (SELECT *, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid FROM (%s)),
s AS (SELECT vid, port_id, T_in, T_out, %s AS stay_id FROM (%s)),
ps AS (SELECT p.*, s.port_id AS stay_port_id, s.stay_id FROM p LEFT JOIN s ON s.vid = p.vid AND p.time BETWEEN s.T_in AND s.T_out
       QUALIFY row_number() OVER (PARTITION BY p.vid, p.rb_rid ORDER BY s.T_in NULLS LAST, s.port_id) = 1)
SELECT *, %s AS rb_year FROM ps",
    p_sql,
    if (stay_id) "stay_id" else "NULL::BIGINT",
    s_sql,
    if (by_year) "year(time)" else "0"
  )
}

#' Cut vessel tracks into voyages between port stays
#'
#' A step-1 builder of the fishing-activity flow. Three methods, one result: a table of voyages.
#'
#' * `"stays"`: a voyage is the sea between two stays (see [rb_find_trip_stays()]). A ping takes the stay it
#'   falls in (the earlier of two that overlap), the track is cut into runs of stay and sea, and each sea run
#'   becomes a voyage, from the port of the stay before it to the port of the stay after it.
#' * `"runs"`: as `"stays"`, but every run of pings tagged in a port ([rb_flag_ping_port()]) is a stay,
#'   however short. This is what `rb_trip()` numbered.
#' * `"jepol"`: the ICES VMS datacall rule (`define_trips_pol`, was `rb_trip_jepol()`): a voyage runs from the
#'   first ping at sea to the first ping back in port; voyages of `min_dur_h` hours or less are dropped, and
#'   with `split` one over `max_dur_h` hours is cut at a long gap between its pings. It reproduces the old
#'   function exactly, including where it cuts (one ping before the longest gap); a long voyage with fewer than
#'   two pings inside it, where the old function stopped, is left whole.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`, and `port_id` for `"runs"` and
#'   `"jepol"` (NA at sea).
#' @param stays For `"stays"`: stays as returned by [rb_find_trip_stays()]: `vid`, `port_id`, `T_in`, `T_out`.
#' @param method `"stays"`, `"runs"` or `"jepol"`.
#' @param min_pings Voyages with fewer pings are dropped. They are numbered before they are dropped, so the
#'   numbers have gaps where a short voyage was. Default 10 for `"stays"`, 1 (none dropped) otherwise.
#' @param by_year Cut voyages at New Year and number them per vessel and year.
#' @param min_dur_h,max_dur_h,split For `"jepol"`: the minimum and maximum voyage length in hours, and whether
#'   longer voyages are split.
#' @param batch_pings For `"jepol"` on a lazy table: about this many pings are collected at a time, whole vessels.
#' @param vid,time,port_id The ping columns, if named otherwise.
#'
#' @return One row per voyage: `vid`, `year` (with `by_year`), `voyage_id`, `T1`, `T2`, `port_from`,
#'   `port_to`, `n_pings`. A data frame for data frame input, a lazy table for lazy input.
#' @family trip
#' @export
rb_cut_trip_voyages <- function(
  pings,
  stays = NULL,
  method = c("stays", "runs", "jepol"),
  min_pings = NULL,
  by_year = TRUE,
  min_dur_h = 0.5,
  max_dur_h = 72,
  split = TRUE,
  batch_pings = 5e7,
  vid = vid,
  time = time,
  port_id = port_id
) {
  method <- match.arg(method)
  if (is.null(min_pings)) {
    min_pings <- if (method == "stays") 10 else 1
  }
  if (method == "stays" && is.null(stays)) {
    stop("Method \"stays\" needs `stays`.", call. = FALSE)
  }
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }}, port_id = {{ port_id }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p <- .rb_as_lazy(
    .rb_std_in(pings, if (method == "stays") map[c("vid", "time")] else map),
    con
  )
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
  ps <- if (method == "stays") {
    .rb_ping_stay_sql(
      .rb_sql(dplyr::select(p, vid, time)),
      .rb_sql(.rb_as_lazy(stays, con)),
      by_year
    )
  } else {
    sprintf(
      "SELECT *, port_id AS stay_port_id, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid, %s AS rb_year FROM (%s)",
      if (by_year) "year(time)" else "0",
      .rb_sql(dplyr::select(p, vid, time, port_id))
    )
  }
  sql <- sprintf(
    "
WITH ps AS (%s),
f AS (SELECT *, stay_port_id IS NOT NULL AS in_h,
             lag(stay_port_id IS NOT NULL) OVER w AS p_in, lag(stay_port_id) OVER w AS p_h
      FROM ps WINDOW w AS (PARTITION BY vid, rb_year ORDER BY time, rb_rid)),
r AS (SELECT *, sum(CASE WHEN p_in IS NULL OR in_h <> p_in OR (in_h AND stay_port_id <> p_h) THEN 1 ELSE 0 END)
                  OVER (PARTITION BY vid, rb_year ORDER BY time, rb_rid) AS run_id FROM f),
runs AS (SELECT vid, rb_year, run_id, bool_or(in_h) AS in_h, min(stay_port_id) AS h,
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

#' Assign each ping its trip
#'
#' Step 1 of the fishing-activity flow. Each ping gets the voyage it lies in (`voyage_id`, from
#' [rb_cut_trip_voyages()]) and, with `stays`, the port stay it lies in (`stay_port_id`). `trip_id` is
#' the voyage and `trip_basis` says where it came from: `"reconstructed"` (the track) or `"none"`. Declared
#' trips (logbook) and landings as sources, and their reconciliation with the voyages, are to come (fishycode
#' plan 013, phase 4). No ping is dropped.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`.
#' @param voyages Voyages from [rb_cut_trip_voyages()].
#' @param stays Optional stays from [rb_find_trip_stays()].
#' @param by_year As in [rb_cut_trip_voyages()]: voyages are numbered per vessel and year.
#' @param vid,time The ping columns, if named otherwise.
#'
#' @return `pings` with `stay_port_id` and, where the stays carry a `stay_id`, the stay's key `stay_id` (with
#'   `stays`), `voyage_id`, `trip_id`, `trip_basis`: the same
#'   type, the same rows.
#' @family trip
#' @export
rb_assign_trip <- function(
  pings,
  voyages,
  stays = NULL,
  by_year = TRUE,
  vid = vid,
  time = time
) {
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p <- .rb_as_lazy(.rb_std_in(pings, map), con)
  v <- .rb_as_lazy(voyages, con)
  base <- if (is.null(stays)) {
    sprintf(
      "SELECT *, NULL::VARCHAR AS stay_port_id, NULL::BIGINT AS stay_id, row_number() OVER (PARTITION BY vid ORDER BY time) AS rb_rid FROM (%s)",
      .rb_sql(p)
    )
  } else {
    sprintf(
      "SELECT * EXCLUDE (rb_year) FROM (%s)",
      .rb_ping_stay_sql(
        .rb_sql(p),
        .rb_sql(.rb_as_lazy(stays, con)),
        by_year,
        stay_id = "stay_id" %in% colnames(stays)
      )
    )
  }
  yr <- if (by_year) "AND v.year = year(b.time)" else ""
  drop <- if (is.null(stays)) {
    "rb_rid, stay_port_id, stay_id"
  } else if ("stay_id" %in% colnames(stays)) {
    "rb_rid"
  } else {
    "rb_rid, stay_id"
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
#' ([rb_cut_trip_voyages()]) of the same vessel that overlaps it by the most seconds (ties: more pings).
#' `n_candidates` counts the voyages that overlap it at all; a link is a link, not a merge: one voyage may be
#' linked to several trips.
#'
#' @param trips A data frame or lazy DuckDB table of declared trips: `vid`, `T1`, `T2` and the trip's key
#'   columns (`keys`).
#' @param voyages Voyages from [rb_cut_trip_voyages()].
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
# dropped; with split, a trip over max_dur hours is cut at a long gap between its pings, again and again. Two
# defects of the original are kept so that it is reproduced exactly; they are logic questions in fishycode's
# plan 013: the cut falls one ping before the longest gap, and the "gap under 3 minutes" test reads the gap
# before that ping. Where the original stopped with an error (a long trip with fewer than two pings inside it),
# the trip is left whole, as the original does for a gap under 3 minutes.

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
  if (inh[n] == 0) {
    ev[n] <- 2
  }
  dep <- time[ev == 1]
  arr <- time[ev == 2]
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
      j <- if (length(inside) >= 2) which.max(intv[inside][-1]) else integer(0)
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
#' Link VMS records to logbook trips
#'
#' Adds a trip id column to a VMS dataset by matching each VMS record to logbook entries where:
#'   - vessel IDs match, and
#'   - the VMS timestamp falls between logbook departure and arrival times.
#' Similar to \code{vmstools::mergeEflalo2Tacsat}, but here not dependent on specific variable names in the data frames.
#'
#' @param ve A data frame or tibble containing VMS data. Must have columns specified by \code{vessel_id} and \code{time}.
#' @param le A data frame or tibble containing logbook data. Must have columns specified by \code{vessel_id}, \code{trip_id}, \code{departure}, and \code{arrival}.
#' @param vessel_id Unquoted column name for vessel ID (must exist in both datasets). Default: \code{vid}.
#' @param trip_id Unquoted column name for trip ID in the logbook dataset. Default: \code{tid}.
#' @param time Unquoted column name for timestamp in the VMS dataset. Default: \code{time}.
#' @param departure Unquoted column name for trip departure time in the logbook dataset. Default: \code{T1}.
#' @param arrival Unquoted column name for trip arrival time in the logbook dataset. Default: \code{T2}.
#'
#' @return A tibble with all columns from the VMS data and the trip id column appended (if matched). The departure and arrival columns from the logbook are removed.
#' @export
#'
#' @examples
#' # rb_link_trip_logbook(ve, le)  # defaults: vid, tid, time, T1, T2
rb_link_trip_logbook <- function(
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

  # Warn about dropped columns
  warning(
    "Departure and arrival columns (",
    departure_chr,
    ", ",
    arrival_chr,
    ") will be removed from the output."
  )

  return(out)
}


#' Pad harbour id
#'
#' Add the harbour id to the first and the last point of an actual trip
#'
#' @param tid trip id, e.g. from `rb_assign_trip()` or `rb_cut_trip_voyages()`
#' @param hid harbour id
#'
#' @return The hid vector with added harbour id to the first and the last point
#' of a trip.
#' @export
#'
rb_pad_harbour <- function(tid, hid) {
  tibble::tibble(.tid = tid, .hid = hid) |>
    dplyr::mutate(
      .hid = dplyr::case_when(
        .tid != dplyr::lag(.tid) & .tid > 0 ~ dplyr::lag(.hid),
        .tid != dplyr::lead(.tid) & .tid > 0 ~ dplyr::lead(.hid),
        TRUE ~ .hid
      )
    ) |>
    dplyr::pull(.hid)
}
