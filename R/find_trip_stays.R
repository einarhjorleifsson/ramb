# find_trip_stays.R — a step-1 builder: port stays, measured in elapsed time.
#
# Ported from fishycode's curate/port_stay.R (plan 011, decisions 060-064; ramb plan 013 phase 2). A stay
# is time spent at a port, not a ping inside its polygon: one port ping followed by silence is not a
# stay. Runs in DuckDB (tier 2): a data frame is registered and the result collected; a lazy table stays lazy.

.rb_con_for <- function(x) {
  if (inherits(x, "tbl_lazy")) return(dbplyr::remote_con(x))
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
  if (inherits(x, "tbl_lazy")) return(x)
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
rb_find_trip_stays <- function(pings, ports, min_stay_h = 4, min_stay = NULL, radius_in = 2000,
                               events = NULL, t_in_window = NULL, crs_m = 3575,
                               vid = vid, time = time, lon = lon, lat = lat) {
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }}, lon = {{ lon }}, lat = {{ lat }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  tryCatch(DBI::dbExecute(con, "LOAD spatial"), error = function(e) DBI::dbExecute(con, "INSTALL spatial; LOAD spatial"))
  p <- .rb_as_lazy(.rb_std_in(pings, map), con) |> dplyr::select(vid, time, lon, lat)

  # ports: polygon and the two buffers, as WKT, registered on the connection
  hb <- sf::st_make_valid(ports)
  r_out <- if ("radius_out" %in% names(hb)) hb$radius_out else rep(radius_in, nrow(hb))
  r_out <- pmax(dplyr::coalesce(r_out, radius_in), radius_in)
  buf <- function(d) sf::st_transform(sf::st_buffer(sf::st_transform(hb, crs_m), d), 4326)
  hw <- data.frame(port_id = hb$port_id,
                   poly = sf::st_as_text(sf::st_geometry(hb), digits = 15),
                   near_in = sf::st_as_text(sf::st_geometry(buf(radius_in)), digits = 15),
                   near_out = sf::st_as_text(sf::st_geometry(buf(r_out)), digits = 15))
  hw_nm <- .rb_register(con, hw, "rb_hw_")
  hg <- if (is.null(min_stay)) data.frame(port_id = character(0), g_h = numeric(0)) else
    data.frame(port_id = min_stay$port_id, g_h = min_stay$min_stay_h)
  hg_nm <- .rb_register(con, hg, "rb_hg_")

  win_p <- win_e <- "TRUE"
  if (!is.null(t_in_window)) {
    f <- format(t_in_window[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"); t <- format(t_in_window[2], "%Y-%m-%d %H:%M:%S", tz = "UTC")
    win_p <- sprintf("t_in >= TIMESTAMP '%s' AND t_in < TIMESTAMP '%s'", f, t)
    win_e <- sprintf("time >= TIMESTAMP '%s' AND time < TIMESTAMP '%s'", f, t)
  }
  ev_sql <- if (is.null(events)) {
    "SELECT NULL::INTEGER AS vid, NULL::TIMESTAMP AS time, NULL::VARCHAR AS io, NULL::VARCHAR AS port_id, NULL::BOOLEAN AS good WHERE false"
  } else {
    .rb_sql(.rb_as_lazy(events, con) |> dplyr::select(vid, time, io, port_id, good))
  }
  G <- min_stay_h
  sql <- sprintf("
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
    .rb_sql(p), hw_nm, hg_nm, G, G, ev_sql, G, win_e, win_p)
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(out, !!!stats::setNames(rlang::syms("vid"), map[["vid"]]))
  if (lazy_in) return(out)
  out <- dplyr::collect(out)
  .rb_unregister(con)
  out[order(out[[map[["vid"]]]], out$T_in), ]
}

# A Suggests package, checked where a function needs it.
.rb_need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("This function needs the package '%s'. Install it with install.packages(\"%s\").", pkg, pkg), call. = FALSE)
}

# glue::glue() for SQL templates, in base R: each {expr} is evaluated in the caller's frame.
.rb_fill <- function(template, env = parent.frame()) {
  m <- gregexpr("\\{[^{}]+\\}", template)
  regmatches(template, m) <- list(vapply(regmatches(template, m)[[1]], function(e)
    paste(as.character(eval(parse(text = substr(e, 2, nchar(e) - 1)), env)), collapse = ""), character(1)))
  template
}
