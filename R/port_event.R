# port_event.R — step 1: recorded port events judged as events, before rb_create_port_calls() takes them.
#
# fishycode plan 048 §2.3. A port event is a report of an arrival or a departure, whatever system sent it: STK's I/U
# rows, a VMS's entry and exit reports, a tracker's geofence events, a logbook's declared departure. Taken into the
# track as pings, an event was judged by the speed filter as a position, and a port call trusted it only if the filter
# kept it (fishycode until plan 048). Here it is judged as what it is: a claim that the vessel was at a port at a time,
# tested against the port, against the vessel's other events, against its track, and against its sending unit. Runs
# in DuckDB (spatial extension for the port tests): a data frame is registered and collected, a lazy table stays lazy.

#' Judge recorded port events: position, sequence, the track and the sending unit
#'
#' A step-1 tool of the fishing-activity flow, between the cleaned track and [rb_create_port_calls()]: it adds to a
#' table of port events, recorded arrivals and departures, the first test each fails (`event_flag`) and whether it
#' may be trusted (`good`), the column [rb_create_port_calls()] reads. Every event stays, in the order it came; each
#' test runs only where its input is given, and an event a test cannot judge is not failed by it.
#'
#' The tests, in the order a flag is given:
#'
#' * `"other_port"` (with `ports` and a position): the event's position lies in another port's polygon, not within
#'   `radius_m` of its own port. Arrivals and departures alike.
#' * `"outside_port"` (with `ports` and a position): an arrival whose position is neither within `radius_m` of its own
#'   port's polygon nor in another port. A departure is not judged by it: it is often sent after the vessel has left.
#'   A position that is not the vessel's (a port's marker, a shore station's) is given as `NA` and is not judged.
#' * `"flicker"`: an arrival and a departure at one port less than `short_s` seconds apart, both events.
#' * `"repeat"`: an event of the same kind as the vessel's event before it (an arrival after an arrival, a departure
#'   after a departure), the later one.
#' * `"track"` (with `pings`): the vessel's pings either side of the event lie farther from its port (or, with no port,
#'   from its own position) than it could sail in the time between: the distance less `radius_m`, over the time (at
#'   least `min_dt_s`), above `kn_max` knots, on the ping before or the ping after. The implied speed of the worse
#'   side is `event_track_kn`. An event with no ping of its vessel on either side is not judged, and is not good:
#'   no track is no witness (`good` is `NA`).
#' * `"unit"` (with `unit`): where a sending unit's events in a year fail the tests above in a share of at least
#'   `unit_share`, and it has at least `unit_min` events that year, its other events too, unless the track has
#'   judged them: a unit that sends bad events in bursts is not trusted between them where nothing else vouches for
#'   an event. On STK's events of 2008 (fishycode plan 048 step 2), the events such units send that pass the tests are
#'   contradicted by the track 6.9 % of the time against at most 2.1 % for other units, so the screen helps where
#'   there is no track; where there is, it would have set aside 13,506 events of 2008, 2016 and 2024 that the track
#'   confirms.
#'
#' @param events A data frame or lazy DuckDB table of port events: `vid`, `time`, `io` (`"I"` arrival, `"U"`
#'   departure), and optionally `port_id` (the port the event names) and `lon`, `lat` (its position; `NA` where it is
#'   not the vessel's).
#' @param ports An sf object of port polygons with `port_id`, for the position tests and the track test's distance to
#'   the port. Optional.
#' @param pings A data frame or lazy DuckDB table of the cleaned track: `vid`, `time`, `lon`, `lat`, the event rows
#'   not among them. Optional; on the connection of a lazy `events`.
#' @param unit Optional: the column of `events` naming the unit that sent each event, for the unit test.
#' @param radius_m Metres: an event this close to its port's polygon is at the port, and the track test allows this
#'   much before it counts distance.
#' @param short_s Seconds: an arrival and a departure at one port closer than this are a flicker.
#' @param kn_max Knots: the highest speed the track test allows.
#' @param min_dt_s Seconds: the shortest time the track test divides by.
#' @param unit_share,unit_min The unit test: the share of a unit-year's events that fail, and its fewest events. The
#'   defaults are where, on STK's events of 2008, the track begins to contradict the unit's passing events often
#'   (fishycode plan 048 step 2).
#' @param crs_m A metric CRS (EPSG code) for the distances to the ports. The default, 3575, suits the North Atlantic.
#' @param vid,time,io,port_id,lon,lat The columns of `events` (and `vid`, `time`, `lon`, `lat` of `pings`), if named
#'   otherwise.
#'
#' @return `events` with `event_flag` (`NA` where no test fails), `event_track_kn` (`NA` where the track test did not
#'   run or could not judge) and `good`: `FALSE` for a flagged event, `NA` for one the track test could not judge, else
#'   `TRUE`. The same construct, the same rows in the same order.
#' @family trip
#' @export
rb_flag_port_event <- function(
  events,
  ports = NULL,
  pings = NULL,
  unit = NULL,
  radius_m = 2000,
  short_s = 60,
  kn_max = 25,
  min_dt_s = 10,
  unit_share = 0.7,
  unit_min = 20,
  crs_m = 3575,
  vid = vid,
  time = time,
  io = io,
  port_id = port_id,
  lon = lon,
  lat = lat
) {
  nm <- c(
    vid = rlang::as_name(rlang::enquo(vid)),
    time = rlang::as_name(rlang::enquo(time)),
    io = rlang::as_name(rlang::enquo(io)),
    port_id = rlang::as_name(rlang::enquo(port_id)),
    lon = rlang::as_name(rlang::enquo(lon)),
    lat = rlang::as_name(rlang::enquo(lat))
  )
  unit_q <- rlang::enquo(unit)
  unit_nm <- if (rlang::quo_is_null(unit_q)) NULL else rlang::as_name(unit_q)
  for (a in c("radius_m", "short_s", "kn_max", "min_dt_s", "unit_share", "unit_min")) {
    v <- get(a)
    if (!(is.numeric(v) && length(v) == 1 && !is.na(v) && v >= 0)) {
      stop(sprintf("`%s` must be one number, 0 or more.", a), call. = FALSE)
    }
  }
  if (!is.null(ports) && !inherits(ports, "sf")) {
    stop("`ports` must be an sf object of port polygons with `port_id`.", call. = FALSE)
  }
  if (!is.null(ports) && !"port_id" %in% names(ports)) {
    stop("`ports` must have a column `port_id`.", call. = FALSE)
  }
  proto <- .rb_check_input(
    events,
    "rb_flag_port_event",
    cols = nm[c("vid", "time", "io")],
    also = if (is.null(unit_nm)) character(0) else c(unit = unit_nm),
    types = c(time = "time"),
    fresh = TRUE
  )
  has <- names(proto)
  has_port <- nm[["port_id"]] %in% has
  has_pos <- all(nm[c("lon", "lat")] %in% has)
  if (!is.null(ports) && !has_port) {
    stop(
      sprintf("With `ports`, `events` must have a column `%s`: the port each event names.", nm[["port_id"]]),
      call. = FALSE
    )
  }
  if (!is.null(pings)) {
    if (!(is.data.frame(pings) || inherits(pings, "tbl_lazy"))) {
      stop("`pings` must be a data frame or a lazy DuckDB table.", call. = FALSE)
    }
    miss <- setdiff(nm[c("vid", "time", "lon", "lat")], colnames(pings))
    if (length(miss)) {
      stop(
        sprintf("`pings` must have %s.", paste0("`", miss, "`", collapse = ", ")),
        call. = FALSE
      )
    }
  }

  lazy_in <- inherits(events, "tbl_lazy")
  con <- .rb_con_for(events, pings)
  .rb_scope()
  if (!is.null(ports) || !is.null(pings)) {
    .rb_extension(con, "spatial")
  }
  given <- events
  if (!lazy_in) {
    events <- .rb_with_rid(events)
  }
  map <- nm[c("vid", "time", "io", if (has_port) "port_id", if (has_pos) c("lon", "lat"))]
  e_in <- .rb_as_lazy(.rb_std_in(events, map), con)
  # the io codes: arrivals and departures only
  bad_io <- DBI::dbGetQuery(
    con,
    sprintf("SELECT DISTINCT io FROM (%s) WHERE io IS NOT NULL AND io NOT IN ('I', 'U') LIMIT 3", .rb_sql(e_in))
  )$io
  if (length(bad_io)) {
    stop(
      sprintf(
        "`%s` must hold \"I\" (arrival) or \"U\" (departure); it holds %s.",
        nm[["io"]],
        paste0("\"", bad_io, "\"", collapse = ", ")
      ),
      call. = FALSE
    )
  }
  # the events numbered once, so every test joins back to its row
  ev <- .rb_temp_table(
    con,
    sprintf(
      "SELECT *, row_number() OVER (ORDER BY vid, time, io, rb_port, rb_lon, rb_lat, rb_unit) AS rb_n FROM (
         SELECT *, %s AS rb_port, %s AS rb_lon, %s AS rb_lat, %s AS rb_unit FROM (%s))",
      if (has_port) "CAST(port_id AS VARCHAR)" else "CAST(NULL AS VARCHAR)",
      if (has_pos) "CAST(lon AS DOUBLE)" else "CAST(NULL AS DOUBLE)",
      if (has_pos) "CAST(lat AS DOUBLE)" else "CAST(NULL AS DOUBLE)",
      if (is.null(unit_nm)) "CAST(NULL AS VARCHAR)" else sprintf("CAST(\"%s\" AS VARCHAR)", unit_nm),
      .rb_sql(e_in)
    ),
    keep = lazy_in
  )

  # ports: the polygon (degrees) and its metric twin
  harb <- "(SELECT NULL::VARCHAR AS port_id, NULL::GEOMETRY AS g, NULL::GEOMETRY AS gm WHERE false)"
  if (!is.null(ports)) {
    hb <- sf::st_make_valid(sf::st_transform(ports, 4326))
    hw <- data.frame(
      port_id = as.character(hb$port_id),
      poly = sf::st_as_text(sf::st_geometry(hb), digits = 15),
      poly_m = sf::st_as_text(sf::st_geometry(sf::st_transform(hb, crs_m)), digits = 15)
    )
    harb <- sprintf(
      "(SELECT port_id, ST_GeomFromText(poly) AS g, ST_GeomFromText(poly_m) AS gm FROM %s)",
      .rb_register(con, hw, "rb_harb_")
    )
  }
  to_m <- function(lon, lat) {
    sprintf(
      "ST_Transform(ST_Point(%s, %s), 'EPSG:4326', 'EPSG:%d', always_xy := true)",
      lon,
      lat,
      as.integer(crs_m)
    )
  }
  hav <- function(lon1, lat1, lon2, lat2) {
    sprintf(
      "2 * 6371008.8 * asin(sqrt(pow(sin(radians(%4$s - %2$s) / 2), 2) + cos(radians(%2$s)) * cos(radians(%4$s)) *
       pow(sin(radians(%3$s - %1$s) / 2), 2)))",
      lon1,
      lat1,
      lon2,
      lat2
    )
  }

  # position: each distinct position against the polygons, then the event's own port
  pos_sql <- if (is.null(ports) || !has_pos) {
    sprintf("SELECT rb_n, CAST(NULL AS VARCHAR) AS rb_pos FROM %s", ev)
  } else {
    sprintf(
      "
WITH h AS %1$s,
q AS (SELECT DISTINCT rb_lon, rb_lat FROM %2$s WHERE rb_lon IS NOT NULL AND rb_lat IS NOT NULL),
qin AS (SELECT q.rb_lon, q.rb_lat, h.port_id FROM q JOIN h ON ST_Intersects(h.g, ST_Point(q.rb_lon, q.rb_lat))),
own AS (SELECT e.rb_n, ST_Distance(%3$s, h.gm) AS d_own FROM %2$s e JOIN h ON h.port_id = e.rb_port
        WHERE e.rb_lon IS NOT NULL AND e.rb_lat IS NOT NULL),
oth AS (SELECT DISTINCT e.rb_n FROM %2$s e JOIN qin ON qin.rb_lon = e.rb_lon AND qin.rb_lat = e.rb_lat
        WHERE qin.port_id IS DISTINCT FROM e.rb_port)
SELECT e.rb_n,
       CASE WHEN own.rb_n IS NULL THEN NULL
            WHEN own.d_own <= %4$f THEN NULL
            WHEN oth.rb_n IS NOT NULL THEN 'other_port'
            WHEN e.io = 'I' THEN 'outside_port' END AS rb_pos
FROM %2$s e LEFT JOIN own USING (rb_n) LEFT JOIN oth USING (rb_n)",
      harb,
      ev,
      to_m("e.rb_lon", "e.rb_lat"),
      radius_m
    )
  }

  # sequence: each vessel's events in time, an arrival before a departure at one instant
  seq_sql <- sprintf(
    "
WITH s AS (SELECT rb_n, io, time, rb_port,
                  lag(io) OVER w AS pio, lag(time) OVER w AS pt, lag(rb_port) OVER w AS pport,
                  lead(io) OVER w AS nio, lead(time) OVER w AS nt, lead(rb_port) OVER w AS nport
           FROM %1$s WINDOW w AS (PARTITION BY vid ORDER BY time, io, rb_n))
SELECT rb_n,
       CASE WHEN (io = 'I' AND nio = 'U' AND rb_port IS NOT NULL AND nport = rb_port AND epoch(nt - time) < %2$f)
              OR (io = 'U' AND pio = 'I' AND rb_port IS NOT NULL AND pport = rb_port AND epoch(time - pt) < %2$f)
              THEN 'flicker'
            WHEN pio IS NOT NULL AND pio = io THEN 'repeat' END AS rb_seq
FROM s",
    ev,
    short_s
  )

  # track: the vessel's ping before and after, their distance to the port (or the event's position) less radius_m
  trk_sql <- if (is.null(pings)) {
    sprintf("SELECT rb_n, CAST(NULL AS DOUBLE) AS rb_kn FROM %s", ev)
  } else {
    pmap <- nm[c("vid", "time", "lon", "lat")]
    pp <- .rb_as_lazy(.rb_std_in(pings, pmap), con) |>
      dplyr::select(vid, time, lon, lat) |>
      dplyr::filter(!is.na(lon), !is.na(lat), !is.na(time))
    dist <- function(side) {
      sprintf(
        "CASE WHEN %1$s_t IS NULL THEN NULL WHEN gm IS NOT NULL THEN ST_Distance(%2$s, gm)
              WHEN rb_lon IS NOT NULL AND rb_lat IS NOT NULL THEN %3$s END",
        side,
        to_m(paste0(side, "_lon"), paste0(side, "_lat")),
        hav(paste0(side, "_lon"), paste0(side, "_lat"), "rb_lon", "rb_lat")
      )
    }
    leg <- function(side) {
      sprintf(
        "CASE WHEN d%1$s IS NULL THEN NULL
              ELSE greatest(d%1$s - %2$f, 0) / greatest(abs(epoch(time - %1$s_t)), %3$f) * 3600 / 1852 END",
        side,
        radius_m,
        min_dt_s
      )
    }
    sprintf(
      "
WITH p AS (%1$s),
e AS (SELECT rb_n, vid, time, rb_port, rb_lon, rb_lat FROM %2$s),
b AS (SELECT e.*, p.time AS b_t, p.lon AS b_lon, p.lat AS b_lat FROM e ASOF LEFT JOIN p ON e.vid = p.vid AND e.time >= p.time),
a AS (SELECT b.*, p.time AS a_t, p.lon AS a_lon, p.lat AS a_lat FROM b ASOF LEFT JOIN p ON b.vid = p.vid AND b.time <= p.time),
g AS (SELECT a.*, h.gm FROM a LEFT JOIN %3$s h ON h.port_id = a.rb_port),
gd AS (SELECT *, %4$s AS db, %5$s AS da FROM g),
k AS (SELECT rb_n, %6$s AS kb, %7$s AS ka FROM gd)
SELECT rb_n, CASE WHEN kb IS NULL THEN ka WHEN ka IS NULL THEN kb ELSE greatest(kb, ka) END AS rb_kn FROM k",
      .rb_sql(pp),
      ev,
      harb,
      dist("b"),
      dist("a"),
      leg("b"),
      leg("a")
    )
  }

  unit_sql <- if (is.null(unit_nm)) {
    "CAST(NULL AS BOOLEAN)"
  } else {
    sprintf(
      "count(*) OVER u >= %f AND avg(CASE WHEN rb_f0 IS NOT NULL THEN 1.0 ELSE 0.0 END) OVER u >= %f",
      unit_min,
      unit_share
    )
  }
  judged <- if (is.null(pings)) "TRUE" else "rb_kn IS NOT NULL"
  sql <- sprintf(
    "
WITH f AS (SELECT e.*, pos.rb_pos, sq.rb_seq, tk.rb_kn,
                  coalesce(pos.rb_pos, sq.rb_seq, CASE WHEN tk.rb_kn > %1$f THEN 'track' END) AS rb_f0
           FROM %2$s e LEFT JOIN (%3$s) pos USING (rb_n) LEFT JOIN (%4$s) sq USING (rb_n) LEFT JOIN (%5$s) tk USING (rb_n)),
u AS (SELECT *, %6$s AS rb_unit_bad FROM f WINDOW u AS (PARTITION BY rb_unit, year(time)))
SELECT * EXCLUDE (rb_n, rb_port, rb_lon, rb_lat, rb_unit, rb_pos, rb_seq, rb_kn, rb_f0, rb_unit_bad),
       coalesce(rb_f0, CASE WHEN rb_unit_bad AND rb_unit IS NOT NULL AND rb_kn IS NULL THEN 'unit' END) AS event_flag,
       rb_kn AS event_track_kn,
       CASE WHEN coalesce(rb_f0, CASE WHEN rb_unit_bad AND rb_unit IS NOT NULL AND rb_kn IS NULL THEN 'unit' END) IS NOT NULL
            THEN false
            WHEN NOT (%7$s) THEN NULL ELSE true END AS good
FROM u",
    kn_max,
    ev,
    pos_sql,
    seq_sql,
    trk_sql,
    unit_sql,
    judged
  )
  out <- .rb_std_out(dplyr::tbl(con, dplyr::sql(sql)), map)
  if (lazy_in) {
    return(.rb_hold(out))
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  .rb_as_given(out, given, ".rb_rid")
}
