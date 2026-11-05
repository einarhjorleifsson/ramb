#' Each vessel's own limits: its top speed, how far its times may be off, and the noise of its positions
#'
#' Step 0 of the fishing-activity flow, before the pings are flagged: what a speed rule needs to judge a vessel by its
#' own data rather than by a guessed ceiling (fishycode plan 065; its study, `$FISHYDATA/archive/2026-11-05-speed-ceiling/`,
#' found that every rule in use runs at a fixed 25 kn, for which no record gives a source). One row per vessel, or per
#' vessel and stream where a stream is given:
#'
#' * **Its ceiling,** the speed it is seen to reach, the vessel's on each of its streams' rows. Where it records its
#'   speed, the highest it records twice in a row (the maximum of a running median of 3 recorded speeds, within a stream,
#'   where the 3 are there; at least `min_recorded` reports, `min_moving` of them over 1 kn): `ceiling_basis`
#'   `"recorded"`. The recorded speed is a witness apart from the positions and the clock, and a median of 3 lets no
#'   single wrong report set it, a stream's first or last included. Else the speed its track sustains: the 99.9th
#'   percentile of a running median of 5 step speeds, within a stream where the 5 are there, over steps long enough that
#'   the noise moves a speed by under a knot (2 x `noise_m` / 1 kn, at least 10 s), at least 100 of them moving:
#'   `"track"`. A step's speed is the lower of the speeds from one ping back and from two: a copy of the
#'   track interleaved in its stream (two transmitters on one id) jumps every step but not every second one, and set a
#'   ceiling of hundreds of knots before (`dansk_truth`'s 1 min tracks: 61-226 times the true ceiling, now 0.90-0.95).
#'   Else `default_kn`: `"default"`. A sparse track misses a short burst at top speed, a ping or three: at 6 min,
#'   `dansk_truth`'s ceilings read 0.86-0.97 of the true one from the recorded speed.
#' * **How far its times may be off,** by stream: `grain_s`, the clock's grain (the coarsest of 1 h, 1 min, 1 s, 0.1 s
#'   and 1 ms that 99 % of its times are whole multiples of), and `time_error_s`, measured against the recorded speed:
#'   over steady moving steps (recorded speed over 2 kn at both ends, the two within 10 %), the 99th percentile of
#'   |written step - distance / recorded speed|, `time_error_basis` `"recorded"`. Where fewer than 30 such steps, or
#'   where it is over two median steps (then it measures the vessel stopping and turning between reports, not its
#'   clock), the grain stands for it: `"grain"`; `"none"` where the times have no grain either. A clock written to the
#'   minute doubles one step and halves the next, and a rule blames the next position (fishycode TODO 190). On a stream
#'   of hourly reports the measure still reads the vessel's changes of speed: `dansk_truth`'s hourly stream, on time to
#'   the second, gets 1,253 s.
#' * **The noise,** the dataset's: the median over the vessels of the 95th percentile of their steps at rest (recorded
#'   speed under 0.3 kn at both ends, 1 to 300 s apart, at least 30): `noise_basis` `"rest"`; with no recorded speed, of
#'   the middle ping's distance from the line between its neighbours, over steps of 1 to 120 s (at least 100):
#'   `"geometry"`; else `"none"`. The noise is the receivers', not a vessel's: some vessels' recorded speeds read 0 on
#'   pings kilometres apart.
#'
#' Speeds and steps are taken within a stream, never across clocks that may differ. The function labels nothing and
#' drops nothing; its table is for [rb_flag_ping_impossible()] and for the user, who can read where a vessel's two
#' witnesses disagree (`ceilings_agree`), a finding about its data in itself. Each value comes with the count it rests
#' on. Iceland's AIS of 2024, 1,850 vessels in six streams, takes about 30 s lazily.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon` and `lat`. Only these columns and `stream` and
#'   `speed` are read.
#' @param stream The stream column ([rb_add_ping_stream()]): steps are taken within each, and the clock is measured by
#'   each. Default `NULL`: one stream, a row per vessel.
#' @param speed The recorded speed over ground, knots (`NA` where none; below 0 or 102.3 or more counts as none, AIS's
#'   "not available"). Default `NULL`: none, and the ceiling is the track's. A speed derived from the positions is no
#'   witness: leave it out.
#' @param default_kn The ceiling of a vessel too little seen to know its own, knots.
#' @param min_recorded,min_moving The recorded speeds a recorded ceiling needs: in all, and over 1 kn.
#' @param vid,time,lon,lat The columns, if named otherwise.
#'
#' @return One row per vessel, or per vessel and stream, in the class of `x` (a lazy table for lazy input), sorted by
#'   them: `vid`, the stream under its own name, `pings`; the vessel's `ceiling_kn` and `ceiling_basis` (`"recorded"`,
#'   `"track"` or `"default"`), `recorded_kn` with `recorded_n` and `recorded_moving_n` (its reports, and those over 1
#'   kn), `track_kn` with `track_n` (its moving steps), each `NA` where it has too little, and `ceilings_agree` (the
#'   track's ceiling within 0.8-1.25 of the recorded one, `NA` without both); the stream's `grain_s`,
#'   `dt_median_s`, `time_error_s` with `time_error_basis` and `time_error_n` (its steady steps); the dataset's `noise_m`
#'   and `noise_basis`.
#' @family flag pings
#' @export
#' @examples
#' \donttest{
#' d <- rb_read_example("dansk_truth") |> dplyr::filter(clock %in% c("fix", "minute_round"))
#' lim <- rb_create_ping_limits(dplyr::collect(d), stream = stream, speed = speed)
#' lim[, c("vid", "stream", "ceiling_kn", "ceiling_basis", "grain_s", "time_error_s", "time_error_basis")]
#' }
rb_create_ping_limits <- function(
  x,
  stream = NULL,
  speed = NULL,
  default_kn = 25,
  min_recorded = 100,
  min_moving = 30,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat
) {
  stream_quo <- rlang::enquo(stream)
  has_stream <- !rlang::quo_is_null(stream_quo)
  speed_quo <- rlang::enquo(speed)
  has_speed <- !rlang::quo_is_null(speed_quo)
  .rb_check_input(
    x,
    "rb_create_ping_limits",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = c(
      if (has_stream) c(stream = rlang::as_name(stream_quo)),
      if (has_speed) c(speed = rlang::as_name(speed_quo))
    ),
    types = c(time = "time", lon = "numeric", lat = "numeric", if (has_speed) c(speed = "numeric"))
  )
  for (a in list(list("default_kn", default_kn), list("min_recorded", min_recorded), list("min_moving", min_moving))) {
    if (!is.numeric(a[[2]]) || length(a[[2]]) != 1 || is.na(a[[2]]) || a[[2]] <= 0) {
      stop(sprintf("`%s` must be one number over 0.", a[[1]]), call. = FALSE)
    }
  }
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }}, lon = {{ lon }}, lat = {{ lat }})
  if (has_stream) {
    map <- c(map, stream = rlang::as_name(stream_quo))
  }
  if (has_speed) {
    map <- c(map, speed = rlang::as_name(speed_quo))
  }
  con <- .rb_con_for(x)
  .rb_scope()
  # only the named columns are read, so another column of a grammar name (a `speed` beside the one passed) cannot clash
  p <- .rb_clock_in(dplyr::select(x, dplyr::all_of(unname(map))), map, con)
  src <- .rb_sql(p)
  KN <- sprintf("%.17e", 1852 / 3600) # m/s per knot
  stream_sql <- if (has_stream) "stream" else "'all'"
  speed_sql <- if (has_speed) "CASE WHEN speed >= 0 AND speed < 102.3 THEN CAST(speed AS DOUBLE) END" else "CAST(NULL AS DOUBLE)"
  # a thinned step's speed is the lower of the speeds from one and from two pings back: a copy of the track
  # interleaved in one stream (two transmitters on one id) jumps every step, but not every second one. The study took
  # one ping back; on Iceland's 2024 two cut the vessels whose track reads over 1.5 times their recorded ceiling from 35
  # to 9, and lower the median ratio from 0.936 to 0.919 (fishycode plan 065 step 3)
  lags <- 2L
  # ramb's sphere (R = 6,371,000 m, as rb_calc_distance()) between the points (lon_a, lat_a) and (lon_b, lat_b)
  hav <- function(lon_a, lat_a, lon_b, lat_b) {
    .rb_fill(
      "2 * 6371000 * asin(least(1, sqrt(pow(sin(radians({lat_b} - {lat_a}) / 2), 2) + cos(radians({lat_a})) * cos(radians({lat_b})) * pow(sin(radians({lon_b} - {lon_a}) / 2), 2))))"
    )
  }
  lag_cols <- paste(
    sprintf("lag(lon, %1$d) OVER w AS Xlon%1$d, lag(lat, %1$d) OVER w AS Xlat%1$d, lag(us, %1$d) OVER w AS Xus%1$d", seq_len(lags)),
    collapse = ", "
  )
  lag_v <- vapply(
    seq_len(lags),
    function(k) {
      sprintf(
        "%s / ((us - Xus%d) / 1e6) / %s",
        hav(sprintf("Xlon%d", k), sprintf("Xlat%d", k), "lon", "lat"),
        k,
        KN
      )
    },
    character(1)
  )
  lag_v <- if (lags == 1L) lag_v else sprintf("least(%s)", paste(lag_v, collapse = ", "))
  # the pings, valid, each with its stream, its recorded speed and its steps from the one before and to the one after
  base <- .rb_fill(
    "
p0 AS (SELECT vid, {stream_sql} AS rb_stream, time, CAST(lon AS DOUBLE) AS lon, CAST(lat AS DOUBLE) AS lat,
              {speed_sql} AS sp, epoch_us(time) AS us
       FROM ({src}) WHERE vid IS NOT NULL AND time IS NOT NULL AND lon IS NOT NULL AND lat IS NOT NULL),
st AS (SELECT *, lag(us) OVER w AS us0, lag(lon) OVER w AS lon0, lag(lat) OVER w AS lat0, lag(sp) OVER w AS sp0,
              lead(us) OVER w AS us2, lead(lon) OVER w AS lon2, lead(lat) OVER w AS lat2
       FROM p0 WINDOW w AS (PARTITION BY vid, rb_stream ORDER BY time, lon, lat)),
sd AS (SELECT *, (us - us0) / 1e6 AS dt, {hav('lon0', 'lat0', 'lon', 'lat')} AS d,
              (us2 - us) / 1e6 AS dt2, {hav('lon', 'lat', 'lon2', 'lat2')} AS d2,
              {hav('lon0', 'lat0', 'lon2', 'lat2')} AS dc
       FROM st)"
  )
  # the noise: the vessels' steps at rest, else the geometry of short steps; the dataset's median of the vessels'
  noise_q <- .rb_fill(
    "
WITH {base},
rest AS (SELECT vid, count(*) AS n, quantile_cont(d, 0.95) AS q FROM sd
         WHERE dt BETWEEN 1 AND 300 AND sp < 0.3 AND sp0 < 0.3 GROUP BY vid),
geo AS (SELECT vid, count(*) AS n,
               quantile_cont(CASE WHEN dc > 0 THEN 2 * sqrt(greatest(0, s * (s - d) * (s - d2) * (s - dc))) / dc
                                  ELSE least(d, d2) END, 0.95) AS q
        FROM (SELECT *, (d + d2 + dc) / 2 AS s FROM sd
              WHERE dt BETWEEN 1 AND 120 AND dt2 BETWEEN 1 AND 120 AND d IS NOT NULL AND d2 IS NOT NULL)
        GROUP BY vid)
SELECT (SELECT median(q) FROM rest WHERE n >= 30) AS rest_m, (SELECT median(q) FROM geo WHERE n >= 100) AS geo_m"
  )
  nz <- DBI::dbGetQuery(con, noise_q)
  noise <- if (!is.na(nz$rest_m)) nz$rest_m else nz$geo_m
  noise_basis <- if (!is.na(nz$rest_m)) "rest" else if (!is.na(nz$geo_m)) "geometry" else "none"
  span <- if (is.na(noise)) 30 else max(10, 2 * noise / (1852 / 3600))
  noise_sql <- if (is.na(noise)) "CAST(NULL AS DOUBLE)" else sprintf("%.17e", noise)
  # a time error measured, else the grain stands for it
  te_ok <- "te.n_steady >= 30 AND te.te99 <= 2 * dtm.dt_median_s"
  same_stream <- function(a) sprintf("gr.rb_stream IS NOT DISTINCT FROM %s.rb_stream", a)
  sql <- .rb_fill(
    "
WITH {base},
-- by stream, its clock: the grain (the coarsest unit 99 % of its times are whole multiples of) and the median step
gr AS (SELECT vid, rb_stream, count(*) AS pings,
              CASE WHEN avg(CASE WHEN us % 3600000000 = 0 THEN 1 ELSE 0 END) >= 0.99 THEN 3600
                   WHEN avg(CASE WHEN us % 60000000 = 0 THEN 1 ELSE 0 END) >= 0.99 THEN 60
                   WHEN avg(CASE WHEN us % 1000000 = 0 THEN 1 ELSE 0 END) >= 0.99 THEN 1
                   WHEN avg(CASE WHEN us % 100000 = 0 THEN 1 ELSE 0 END) >= 0.99 THEN 0.1
                   WHEN avg(CASE WHEN us % 1000 = 0 THEN 1 ELSE 0 END) >= 0.99 THEN 0.001 ELSE 0 END AS grain_s
       FROM p0 GROUP BY vid, rb_stream),
dtm AS (SELECT vid, rb_stream, median(dt) AS dt_median_s FROM sd WHERE dt > 0 GROUP BY vid, rb_stream),
-- by stream, how far the written times are off, against the recorded speed on steady moving steps
te AS (SELECT vid, rb_stream, count(*) AS n_steady,
              quantile_cont(abs(dt - d / (((sp + sp0) / 2) * {KN})), 0.99) AS te99
       FROM sd WHERE dt > 0 AND sp > 2 AND sp0 > 2 AND abs(sp - sp0) <= 0.1 * (sp + sp0) / 2 GROUP BY vid, rb_stream),
-- by vessel, the recorded witness: the highest speed recorded twice in a row (a running median of 3 within a stream,
-- where the 3 are there: a wrong first or last report cannot set it)
rs AS (SELECT vid, sp, row_number() OVER w AS rn, count(*) OVER (PARTITION BY vid, rb_stream) AS nn,
              median(sp) OVER (PARTITION BY vid, rb_stream ORDER BY time, lon, lat ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS m3
       FROM p0 WHERE sp IS NOT NULL WINDOW w AS (PARTITION BY vid, rb_stream ORDER BY time, lon, lat)),
rec AS (SELECT vid, count(*) AS n_rec, count(*) FILTER (WHERE sp > 1) AS n_moving,
               max(CASE WHEN rn > 1 AND rn < nn THEN m3 END) AS rec_sus
        FROM rs GROUP BY vid),
-- by vessel, the track's witness: within each stream, thinned to steps of at least {span} s, the running median of 5
-- of their speeds where the 5 are there (a stream's first step has one ping back, not two)
th AS (SELECT * FROM (SELECT *, min(us) OVER (PARTITION BY vid, rb_stream) AS us1 FROM p0)
       QUALIFY row_number() OVER (PARTITION BY vid, rb_stream, floor((us - us1) / ({span} * 1e6)) ORDER BY time, lon, lat) = 1),
tk AS (SELECT vid, rb_stream, time, lon, lat, {lag_v} AS v
       FROM (SELECT *, {lag_cols}, count(*) OVER (PARTITION BY vid, rb_stream) AS nk
             FROM th WINDOW w AS (PARTITION BY vid, rb_stream ORDER BY time, lon, lat))
       WHERE Xus1 IS NOT NULL AND us > Xus1 AND nk >= 6),
t5 AS (SELECT vid, v, row_number() OVER w AS rn, count(*) OVER (PARTITION BY vid, rb_stream) AS nn,
              median(v) OVER (PARTITION BY vid, rb_stream ORDER BY time, lon, lat ROWS BETWEEN 2 PRECEDING AND 2 FOLLOWING) AS m5
       FROM tk WINDOW w AS (PARTITION BY vid, rb_stream ORDER BY time, lon, lat)),
trk AS (SELECT vid, quantile_cont(m, 0.999) AS track_p999, count(*) FILTER (WHERE m > 1) AS n_moving_steps
        FROM (SELECT vid, m5 AS m FROM t5 WHERE rn > 2 AND rn <= nn - 2)
        GROUP BY vid)
SELECT gr.vid, gr.rb_stream, gr.pings,
       CASE WHEN rec.n_rec >= {min_recorded} AND rec.n_moving >= {min_moving} THEN rec.rec_sus END AS recorded_kn,
       coalesce(rec.n_rec, 0) AS recorded_n, coalesce(rec.n_moving, 0) AS recorded_moving_n,
       CASE WHEN trk.n_moving_steps >= 100 THEN trk.track_p999 END AS track_kn,
       coalesce(trk.n_moving_steps, 0) AS track_n,
       CASE WHEN rec.n_rec >= {min_recorded} AND rec.n_moving >= {min_moving} AND trk.n_moving_steps >= 100
                 AND rec.rec_sus > 0 THEN trk.track_p999 / rec.rec_sus BETWEEN 0.8 AND 1.25 END AS ceilings_agree,
       gr.grain_s, dtm.dt_median_s,
       CASE WHEN {te_ok} THEN te.te99 WHEN gr.grain_s > 0 THEN gr.grain_s END AS time_error_s,
       CASE WHEN {te_ok} THEN 'recorded' WHEN gr.grain_s > 0 THEN 'grain' ELSE 'none' END AS time_error_basis,
       coalesce(te.n_steady, 0) AS time_error_n,
       {noise_sql} AS noise_m, '{noise_basis}' AS noise_basis
FROM gr LEFT JOIN dtm ON gr.vid = dtm.vid AND {same_stream('dtm')}
        LEFT JOIN te ON gr.vid = te.vid AND {same_stream('te')}
        LEFT JOIN rec ON gr.vid = rec.vid LEFT JOIN trk ON gr.vid = trk.vid"
  )
  out <- dplyr::tbl(con, dplyr::sql(sql)) |>
    dplyr::mutate(
      ceiling_kn = dplyr::coalesce(.data$recorded_kn, .data$track_kn, !!default_kn),
      ceiling_basis = dplyr::case_when(
        !is.na(.data$recorded_kn) ~ "recorded",
        !is.na(.data$track_kn) ~ "track",
        TRUE ~ "default"
      ),
      .after = "pings"
    ) |>
    dplyr::arrange(.data$vid, .data$rb_stream)
  # one row per vessel, or per vessel and stream where a stream is given, under the user's names
  out <- if (has_stream) {
    dplyr::rename(out, !!map[["stream"]] := "rb_stream")
  } else {
    dplyr::select(out, -"rb_stream")
  }
  # a table of its own: the vessel column under the user's name, in the class of x
  out <- .rb_std_out(out, map["vid"])
  if (inherits(x, "tbl_lazy")) {
    return(.rb_hold(out))
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  .rb_as_given(out, x)
}
