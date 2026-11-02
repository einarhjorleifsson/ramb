# clock.R — step 0 of the fishing-activity flow: the clocks of the streams a track is merged from, before any speed,
# turn or window is derived across them.
#
# fishycode plan 059 (2026-11-01). Time is an observation with an error of its own: feeds merged into one track may
# keep different clocks (the Icelandic astd ran 10-86 s behind STK's AIS stream in some months), and a speed or turn
# derived across two of them breaks. The first test on a track is how many streams it holds and how their clocks
# differ; the task is to harmonize them against a reference stream, a convention, not to find true UTC.
#
#   pings |> rb_add_ping_stream()                     the dt pattern: cadence, phase, pings off the grid
#   rb_create_ping_lags(pings, reference = "a")       each stream's lag against the reference, per vessel and period
#   pings |> rb_add_ping_own_lag(reference = "a")     a ping's own lag: a copy of a reference report, or a displaced ping
#   pings |> rb_add_ping_time(lags)                   time harmonized, the delivered time beside, the lag and its basis
#
# Recoded from the study's SQL ($FISHYDATA/archive/2026-11-01-feed-clocks/scripts/02_feed_clock_check.R, parts 1, 3
# and 4) and the residual pair of the phase-model study (10_dansk_clocks.R). Every value a ping gets depends only on its
# vessel, stream, time and position (and recorded speed and course), so it is computed on their distinct values and
# joined back: a window over every row and column of a year is what ran out of memory (fishycode decision 239).
# Positions are projected locally on ramb's one sphere (R = 6,371 km), metres per degree `.rb_clock_m`.

.rb_clock_m <- 6371000 * pi / 180
.rb_clock_ms <- 1852 / 3600

# A period of `time` as a DATE (NULL: one period, a NULL date).
.rb_clock_period_sql <- function(period, col = "time") {
  if (is.null(period)) {
    return("CAST(NULL AS DATE)")
  }
  sprintf("CAST(date_trunc('%s', %s) AS DATE)", period, col)
}
.rb_clock_check_period <- function(period, call = rlang::caller_env()) {
  periods <- c("day", "week", "month", "quarter", "year")
  if (
    !is.null(period) &&
      !(is.character(period) && length(period) == 1 && period %in% periods)
  ) {
    rlang::abort(
      c(
        "`period` must be one of \"day\", \"week\", \"month\", \"quarter\", \"year\", or NULL.",
        i = "It sets the unit a lag is measured over: a vessel's stream in one period."
      ),
      call = call
    )
  }
}
.rb_clock_check_kn <- function(kn, call = rlang::caller_env()) {
  if (!(is.numeric(kn) && length(kn) == 2 && !anyNA(kn) && kn[1] < kn[2])) {
    rlang::abort(
      "`kn` must be two speeds in knots, low and high: the moving vessel's range.",
      call = call
    )
  }
}
# A data frame is numbered for .rb_as_given(); either is put on `con` and its columns renamed to the grammar's.
.rb_clock_in <- function(x, map, con, call = rlang::caller_env()) {
  twice <- map[duplicated(map) | duplicated(map, fromLast = TRUE)]
  if (length(twice)) {
    rlang::abort(
      c(
        sprintf(
          "%s all name the column `%s`.",
          paste0("`", names(twice), "`", collapse = " and "),
          twice[1]
        ),
        i = "Give each its own column (a copy, if one column must serve both)."
      ),
      call = call
    )
  }
  if (!inherits(x, "tbl_lazy")) {
    x <- .rb_with_rid(x)
  }
  .rb_as_lazy(.rb_std_in(x, map), con)
}
.rb_clock_out <- function(out, given, map, con) {
  out <- .rb_std_out(out, map)
  if (inherits(given, "tbl_lazy")) {
    return(.rb_hold(out))
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  .rb_as_given(out, given, ".rb_rid")
}
.rb_clock_strings <- function(con, x) {
  paste(as.character(DBI::dbQuoteString(con, x)), collapse = ", ")
}
# The order of step 0, as guards (fishycode plan 060 §8 item 3). A clock is measured before the pings are flagged: the
# impossible-position filter removes the pings of a clock that is off, and the few it keeps look on time (fishycode
# decision 271's first look at one VMS unit). And pings of two streams are compared only once their clocks are
# harmonized (lag_basis): a lag between them reads as distance. Each warns, with its class, and goes on.
.rb_warn_after_clean <- function(x, fn) {
  if ("ping_flag" %in% colnames(x)) {
    rlang::warn(
      c(
        sprintf(
          "`x` carries `ping_flag`: %s() measures the clocks of pings already flagged.",
          fn
        ),
        i = "Measure the clocks before the flags (step 0): a clock that is off has its pings removed as impossible, and the few kept look on time."
      ),
      class = "ramb_after_clean"
    )
  }
  invisible(x)
}
.rb_warn_unharmonised <- function(x, fn) {
  cn <- colnames(x)
  if (!"stream" %in% cn || "lag_basis" %in% cn) {
    return(invisible(x))
  }
  n <- if (inherits(x, "tbl_lazy")) {
    dplyr::pull(dplyr::summarise(x, n = dplyr::n_distinct(.data$stream)), "n")
  } else {
    length(unique(stats::na.omit(x$stream)))
  }
  if (n >= 2) {
    rlang::warn(
      c(
        sprintf(
          "`x` holds %d streams whose clocks are not harmonized, and %s() compares pings across them.",
          n,
          fn
        ),
        i = "Harmonize them first (step 0): rb_create_ping_lags(), rb_add_ping_own_lag(), rb_add_ping_time(); a lag between two clocks reads as distance."
      ),
      class = "ramb_unharmonised"
    )
  }
  invisible(x)
}

# The join back from a table of distinct keys to every ping, NULL-safe.
.rb_clock_on <- function(a, b, cols) {
  paste(
    sprintf("%1$s.%3$s IS NOT DISTINCT FROM %2$s.%3$s", a, b, cols),
    collapse = " AND "
  )
}

#' Tell the streams of a track and read their dt pattern
#'
#' Step 0 of the fishing-activity flow, the first test on a track: how many streams it is merged from, and how
#' regularly each sends. A stream is a sequence of pings from one clock: a feed, or a part of one told by a rule of
#' the source (fishycode decision 233 tells STK's three by the grid of the positions). Each ping gets its stream and
#' its place in the stream's pattern. Read the times before any speed is derived (fishycode plan 059): a feed sends
#' at its own pace and phase, and pings off its grid are candidates of another source.
#'
#' Without a stream column every ping is taken as one stream, `"1"`. The pattern then shows a merged track (pings
#' off the grid, a cadence that is not the feed's) but does not split it: interleaved regular streams with no column
#' to tell them are not separated here.
#'
#' @param x A data frame or a lazy DuckDB table with `vid` and `time`.
#' @param stream The column that holds the stream (e.g. the provider). If `x` has no such column, every ping is one
#'   stream, `"1"`, in a column of that name.
#' @param period The unit the cadence is taken over, per vessel and stream: `"day"`, `"week"`, `"month"`,
#'   `"quarter"`, `"year"`, or `NULL` for one.
#' @param tol A ping is off its stream's grid when an interval to its neighbour in the stream is more than `tol`
#'   cadences off a whole number of cadences (at least one): with a 10-s cadence and `tol = 0.25`, 8-12 s and 18-22 s
#'   are on the grid, 1-7 s and 13-17 s off it.
#' @param vid,time,lon,lat The columns, if named otherwise; `lon` and `lat` break ties in time.
#'
#' @return `x` with the stream column (if it was not there) and
#'   * `stream_dt_s`: seconds since the stream's previous ping of the vessel (`NA` for its first);
#'   * `stream_cadence_s`: the median of `stream_dt_s` over the vessel's stream in the period;
#'   * `stream_phase_s`: the time (seconds since 1970) modulo the cadence, rounded to whole seconds; steady along a
#'     regular stream, drifting where its interval is not a whole number of seconds;
#'   * `stream_off_grid`: `TRUE` where an interval before or after the ping, within its stream, is off the grid;
#'   * `stream_crossed`: `TRUE` where the vessel's previous ping in the merged track is from another stream.
#'
#'   The same type, the same rows. Ties in time are broken by `lon`, `lat`, as the ping flags do. A ping with no
#'   stream or no time is in no stream's pattern: `NA` and `FALSE`.
#' @family ping clocks
#' @export
#'
#' @examples
#' d <- data.frame(
#'   vid = 1L,
#'   time = as.POSIXct("2024-01-01", tz = "UTC") + c(0, 10, 20, 29, 30, 40),
#'   lon = -20 + 0:5 * 1e-3, lat = 64
#' )
#' rb_add_ping_stream(d)
rb_add_ping_stream <- function(
  x,
  stream = stream,
  period = "month",
  tol = 0.25,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat
) {
  .rb_check_input(
    x,
    "rb_add_ping_stream",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time))
    ),
    also = c(
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    types = c(time = "time")
  )
  .rb_clock_check_period(period)
  if (!(is.numeric(tol) && length(tol) == 1 && tol > 0 && tol < 0.5)) {
    stop(
      "`tol` must be a number between 0 and 0.5 (a share of the cadence).",
      call. = FALSE
    )
  }
  st <- rlang::as_name(rlang::enquo(stream))
  again <- intersect(
    c(
      "stream_dt_s",
      "stream_cadence_s",
      "stream_phase_s",
      "stream_off_grid",
      "stream_crossed"
    ),
    colnames(x)
  )
  if (length(again)) {
    rlang::abort(c(
      sprintf("`x` already has %s.", paste0("`", again, "`", collapse = ", ")),
      i = "rb_add_ping_stream() has probably been run on it already; drop those columns to run it again."
    ))
  }
  given <- x
  one <- !st %in% colnames(x)
  if (one && !missing(stream)) {
    rlang::abort(c(
      sprintf("`x` has no column `%s`.", st),
      i = "Name the column that holds the stream, or leave `stream` out to take every ping as one stream."
    ))
  }
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  if (!one) {
    map <- c(map, stream = st)
  }
  con <- .rb_con_for(x)
  .rb_scope()
  p <- .rb_clock_in(x, map, con)
  src <- .rb_sql(p)
  if (one) {
    src <- sprintf("SELECT *, '1' AS stream FROM (%s)", src)
  }
  per <- .rb_clock_period_sql(period)
  # An interval is on the grid within tol of a whole number of cadences, at least one.
  off <- function(dt) {
    sprintf(
      "(c.rb_cad > 0 AND abs(%1$s / c.rb_cad - CASE WHEN round(%1$s / c.rb_cad) < 1 THEN 1 ELSE round(%1$s / c.rb_cad) END) > %2$.17e)",
      dt,
      tol
    )
  }
  keys <- c("vid", "stream", "time", "lon", "lat")
  sql <- .rb_fill(
    "
WITH x AS ({src}),
k AS (SELECT DISTINCT vid, stream, time, lon, lat FROM x WHERE time IS NOT NULL AND stream IS NOT NULL),
w AS (
  SELECT *, {per} AS rb_per,
         epoch(time) - epoch(lag(time) OVER s) AS stream_dt_s,
         epoch(lead(time) OVER s) - epoch(time) AS rb_dtn,
         lag(stream) OVER m AS rb_ps
  FROM k
  WINDOW s AS (PARTITION BY vid, stream ORDER BY time, lon, lat),
         m AS (PARTITION BY vid ORDER BY time, lon, lat, stream)),
c AS (SELECT vid, stream, rb_per, median(stream_dt_s) AS rb_cad FROM w GROUP BY ALL),
r AS (
  SELECT w.vid, w.stream, w.time, w.lon, w.lat, w.stream_dt_s, c.rb_cad AS stream_cadence_s,
         CASE WHEN round(c.rb_cad) >= 1 THEN fmod(epoch(w.time), round(c.rb_cad)) END AS stream_phase_s,
         coalesce({off('w.stream_dt_s')} OR {off('w.rb_dtn')}, false) AS stream_off_grid,
         coalesce(w.rb_ps <> w.stream, false) AS stream_crossed
  FROM w LEFT JOIN c ON {.rb_clock_on('c', 'w', c('vid', 'stream', 'rb_per'))})
SELECT x.*, r.stream_dt_s, r.stream_cadence_s, round(r.stream_phase_s) AS stream_phase_s,
       coalesce(r.stream_off_grid, false) AS stream_off_grid, coalesce(r.stream_crossed, false) AS stream_crossed
FROM x LEFT JOIN r ON {.rb_clock_on('x', 'r', keys)}"
  )
  out <- dplyr::tbl(con, dplyr::sql(sql))
  if (one) {
    out <- dplyr::rename(out, !!st := "stream")
  }
  .rb_clock_out(out, given, map, con)
}

#' Measure each stream's lag against a reference stream
#'
#' Step 0 of the fishing-activity flow: how far each stream's clock is from the reference's, per vessel and period.
#' A clock that runs `L` seconds late puts a moving vessel's positions `L * v` behind along its track and nothing
#' across it, so the lag is read from the positions (fishycode plan 059, step 1).
#'
#' * `"interpolate"` (dense streams): for each reference ping of a moving vessel (its recorded speed within `kn`), the
#'   other stream's position at the same reported time, interpolated between its two pings around it (at most
#'   `max_gap_s` apart, moving within `kn` and within `max_dkn` of the reference's speed). Each pair gives
#'   `-along / v`: the offset along the stream's segment over its speed.
#' * `"residual"` (a sparse stream): each of the stream's pings against the reference pings before and after it (at
#'   most `max_gap_s` away), by the recorded speed and course of the two: `e = (along - v dt) / v`, how far ahead (+)
#'   or behind (-) of its time the ping lies from each side. A ping `L` s late leaves the pair (`-L`, `+L`); each pair
#'   gives `(e_next - e_prev) / 2`.
#' * `"track"` (a sparse stream with no reliable speed, fishycode plan 060): each of the stream's pings placed on the
#'   vessel's moving reference track by the shift that brings it nearest: projected onto every reference segment (two
#'   consecutive reference pings at most `max_gap_s` apart, moving within `kn` by their positions) that starts within
#'   `search_s` of it. A ping counts where the reference moves at its own stamp and the best shift puts it within
#'   `max_dist_m` of the track; its lag is minus the shift. Needs no recorded speed or course, and finds lags up to
#'   `search_s`: on Iceland's VMS, clocks an hour off that the other methods could not measure.
#'
#' A unit's lag is the median over its pairs, and every unit (vessel, stream, period) lands in one outcome:
#' `"reference"`; `"one stream"` (nothing to compare: the vessel has one stream in the period); `"no reference"`;
#' `"not measurable"` (fewer than `min_pairs` pairs); `"aligned"` (the lag within `lag_min_s` of 0); `"offset"` (the
#' distance at the lag at most `better` times the distance at none); else `"unclear"`.
#'
#' The reference is a convention (the densest regular stream, or the one stamped at receipt), recorded on each row;
#' the lags are relative to it, not to true UTC. Where a stream holds two pings of a vessel at one time, the first by
#' `lon`, `lat` is taken as its position then, as the ping flags break ties.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon`, `lat`, the stream column and, but for
#'   `"track"`, the recorded `speed` (knots); `"residual"` also needs the recorded `course` (degrees).
#' @param reference The reference stream, a value of the stream column.
#' @param method `"interpolate"`, `"residual"` or `"track"`.
#' @param stream The stream column ([rb_add_ping_stream()]).
#' @param period The unit of a lag: `"day"`, `"week"`, `"month"`, `"quarter"`, `"year"`, or `NULL` for one.
#' @param kn The moving vessel's speed range, knots.
#' @param max_dkn For `"interpolate"`: the stream's segment speed within this of the reference's, knots.
#' @param max_gap_s For `"interpolate"`, the longest segment of the stream; for `"residual"`, the longest interval to
#'   a reference ping; for `"track"`, the longest reference segment (default 1800), seconds.
#' @param min_pairs,lag_min_s,better The outcome's rule (above).
#' @param vid,time,lon,lat,speed,course The columns, if named otherwise.
#' @param search_s,max_dist_m For `"track"`: how far in time a reference segment may start from the ping, seconds;
#'   how close the best shift must bring it, metres.
#'
#' @return One row per vessel, stream and period, in the class of `x`: `reference`, `vid`, the stream column,
#'   `period` (the period's first day), `pings`, `pairs`, `lag_s` (the median; positive: the stream's time is later
#'   than the reference's for the same position), `lag_q10_s`, `lag_q90_s`, `along_m` (the median offset along the
#'   track at no lag), `across_m` (the median absolute offset across it; `NA` for `"residual"`; for `"track"`, the
#'   distance from the track at the ping's best shift), `dist_zero_m` and
#'   `dist_lag_m` (the median distance at no lag and at the lag), `outcome`. A lazy table for lazy input.
#' @family ping clocks
#' @export
rb_create_ping_lags <- function(
  x,
  reference,
  method = c("interpolate", "residual", "track"),
  stream = stream,
  period = "month",
  kn = c(2, 25),
  max_dkn = 2,
  max_gap_s = NULL,
  min_pairs = 30,
  lag_min_s = 10,
  better = 0.8,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  speed = speed,
  course = course,
  search_s = 7200,
  max_dist_m = 300
) {
  method <- match.arg(method)
  .rb_check_input(
    x,
    "rb_create_ping_lags",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat)),
      stream = rlang::as_name(rlang::enquo(stream))
    ),
    also = c(
      if (method != "track") c(speed = rlang::as_name(rlang::enquo(speed))),
      if (method == "residual") c(course = rlang::as_name(rlang::enquo(course)))
    ),
    types = c(
      time = "time",
      lon = "numeric",
      lat = "numeric",
      speed = "numeric"
    )
  )
  .rb_warn_after_clean(x, "rb_create_ping_lags")
  stopifnot(
    is.numeric(search_s),
    length(search_s) == 1,
    search_s > 0,
    is.numeric(max_dist_m),
    length(max_dist_m) == 1,
    max_dist_m > 0
  )
  if (
    missing(reference) ||
      !(is.character(reference) && length(reference) == 1 && !is.na(reference))
  ) {
    rlang::abort(c(
      "`reference` must be one stream, a value of the stream column.",
      i = "The reference is a convention: the densest regular stream, or the one stamped at receipt."
    ))
  }
  .rb_clock_check_period(period)
  .rb_clock_check_kn(kn)
  if (is.null(max_gap_s)) {
    max_gap_s <- switch(method, interpolate = 900, residual = 60, track = 1800)
  }
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }},
    stream = {{ stream }}
  )
  if (method != "track") {
    map <- c(map, .rb_cols(speed = {{ speed }}))
  }
  if (method == "residual") {
    map <- c(map, .rb_cols(course = {{ course }}))
  }
  lazy_in <- inherits(x, "tbl_lazy")
  con <- .rb_con_for(x)
  .rb_scope()
  p <- .rb_clock_in(x, map, con)
  ref <- .rb_clock_strings(con, reference)
  per <- .rb_clock_period_sql(period)
  M <- sprintf("%.17e", .rb_clock_m)
  MS <- sprintf("%.17e", .rb_clock_ms)
  lo <- kn[1]
  hi <- kn[2]
  crs <- switch(method, residual = ", speed, course", track = "", ", speed")
  pairs <- if (method == "track") {
    # the study's scan ($FISHYDATA/archive/2026-11-01-vms-clock-scan/scripts/01_scan.R), on ramb's sphere: the
    # reference cut into moving segments; each ping projected onto every segment of its vessel that starts within
    # search_s (buckets of search_s, so the join is on vessel and bucket), the nearest kept; the segment covering the
    # ping's own stamp gives the distance at no shift and the speed there
    .rb_fill(
      "
rk AS (SELECT vid, time, lon, lat FROM p WHERE stream = {ref}
       QUALIFY row_number() OVER (PARTITION BY vid, time ORDER BY lon, lat) = 1),
sg AS (SELECT vid, time AS t0, lead(time) OVER o AS t1, lon AS lon0, lat AS lat0, lead(lon) OVER o AS lon1,
              lead(lat) OVER o AS lat1
       FROM rk WINDOW o AS (PARTITION BY vid ORDER BY time)),
seg AS (SELECT vid, t0, t1, lon0, lat0, lon1, lat1, epoch(t1) - epoch(t0) AS dt,
               sqrt(pow((lon1 - lon0) * cos(radians(lat0)) * {M}, 2) + pow((lat1 - lat0) * {M}, 2)) / (epoch(t1) - epoch(t0)) AS v_ms,
               CAST(floor(epoch(t0) / {search_s}) AS BIGINT) AS b
        FROM sg WHERE t1 IS NOT NULL AND epoch(t1) - epoch(t0) > 0 AND epoch(t1) - epoch(t0) <= {max_gap_s}),
sv AS (SELECT row_number() OVER () AS rb_i, * FROM (
         SELECT DISTINCT vid, stream, period, time, lon, lat FROM p WHERE stream <> {ref}
         QUALIFY row_number() OVER (PARTITION BY vid, stream, time ORDER BY lon, lat) = 1)),
cand AS (
  SELECT v.rb_i, epoch(s.t0) - epoch(v.time) AS s0, s.dt,
         (s.lon0 - v.lon) * cos(radians(v.lat)) * {M} AS ax, (s.lat0 - v.lat) * {M} AS ay,
         (s.lon1 - s.lon0) * cos(radians(v.lat)) * {M} AS ex, (s.lat1 - s.lat0) * {M} AS ey
  FROM (SELECT sv.*, CAST(floor(epoch(time) / {search_s}) AS BIGINT) + k AS b FROM sv, range(-1, 2) r(k)) v
  JOIN seg s ON s.vid = v.vid AND s.b = v.b
  WHERE s.v_ms BETWEEN {lo} * {MS} AND {hi} * {MS} AND abs(epoch(s.t0) - epoch(v.time)) <= {search_s}),
cu AS (SELECT rb_i, s0, dt, ax, ay, ex, ey,
              CASE WHEN ex * ex + ey * ey > 0 THEN least(1, greatest(0, -(ax * ex + ay * ey) / (ex * ex + ey * ey))) END AS u
       FROM cand),
best AS (SELECT rb_i, arg_min(s0 + u * dt, sqrt(pow(ax + u * ex, 2) + pow(ay + u * ey, 2))) AS shift,
                min(sqrt(pow(ax + u * ex, 2) + pow(ay + u * ey, 2))) AS d_best
         FROM cu WHERE u IS NOT NULL GROUP BY rb_i),
own AS (SELECT sv.rb_i, s.v_ms AS v0,
               sqrt(pow((s.lon0 + (s.lon1 - s.lon0) * (epoch(sv.time) - epoch(s.t0)) / s.dt - sv.lon) * cos(radians(sv.lat)) * {M}, 2) +
                    pow((s.lat0 + (s.lat1 - s.lat0) * (epoch(sv.time) - epoch(s.t0)) / s.dt - sv.lat) * {M}, 2)) AS d0
        FROM sv ASOF JOIN seg s ON sv.vid = s.vid AND sv.time >= s.t0 WHERE sv.time <= s.t1),
pr AS (SELECT sv.vid, sv.stream, sv.period, o.v0 AS v_ms, b.shift * o.v0 AS along, b.d_best AS across,
              -b.shift AS lag_i, o.d0
       FROM sv JOIN best b USING (rb_i) JOIN own o USING (rb_i)
       WHERE o.v0 BETWEEN {lo} * {MS} AND {hi} * {MS} AND b.d_best <= {max_dist_m}),"
    )
  } else if (method == "interpolate") {
    .rb_fill(
      "
r AS (SELECT vid, time, lon, lat, speed, period FROM p WHERE stream = {ref} AND speed BETWEEN {lo} AND {hi}),
sx AS (SELECT vid, stream, time, lon, lat FROM p WHERE stream <> {ref}
       QUALIFY row_number() OVER (PARTITION BY vid, stream, time ORDER BY lon, lat) = 1),
rs AS (SELECT r.*, v.stream FROM r JOIN (SELECT DISTINCT vid, stream FROM sx) v ON r.vid = v.vid),
a AS (SELECT rs.*, s0.time AS t0, s0.lon AS lon0, s0.lat AS lat0
      FROM rs ASOF JOIN sx s0 ON rs.vid = s0.vid AND rs.stream = s0.stream AND rs.time >= s0.time),
b AS (SELECT a.*, s1.time AS t1, s1.lon AS lon1, s1.lat AS lat1
      FROM a ASOF JOIN sx s1 ON a.vid = s1.vid AND a.stream = s1.stream AND a.time < s1.time),
g AS (SELECT *, epoch(t1) - epoch(t0) AS dt_seg,
             (lon1 - lon0) * cos(radians(lat0)) * {M} AS sx_m, (lat1 - lat0) * {M} AS sy_m
      FROM b WHERE epoch(t1) - epoch(t0) BETWEEN 1 AND {max_gap_s}),
h AS (SELECT *, sqrt(sx_m * sx_m + sy_m * sy_m) AS seg_m, (epoch(time) - epoch(t0)) / dt_seg AS f FROM g),
k AS (SELECT vid, stream, period, seg_m / dt_seg AS v_ms,
             (lon0 + f * (lon1 - lon0) - lon) * cos(radians(lat)) * {M} AS dx,
             (lat0 + f * (lat1 - lat0) - lat) * {M} AS dy, sx_m / seg_m AS ux, sy_m / seg_m AS uy
      FROM h WHERE seg_m > 0 AND seg_m / dt_seg BETWEEN {lo} * {MS} AND {hi} * {MS}
        AND abs(seg_m / dt_seg / {MS} - speed) <= {max_dkn}),
pr AS (SELECT vid, stream, period, v_ms, dx * ux + dy * uy AS along, dx * uy - dy * ux AS across,
              -(dx * ux + dy * uy) / v_ms AS lag_i, sqrt(dx * dx + dy * dy) AS d0
       FROM k),"
    )
  } else {
    .rb_fill(
      "
q AS (SELECT DISTINCT vid, stream, period, time, lon, lat, speed, course FROM p
      WHERE stream <> {ref} AND speed IS NOT NULL AND course BETWEEN 0 AND 360),
rf AS (SELECT vid, time, lon, lat, speed, course FROM p
       WHERE stream = {ref} AND speed IS NOT NULL AND course BETWEEN 0 AND 360
       QUALIFY row_number() OVER (PARTITION BY vid, time ORDER BY lon, lat, speed, course) = 1),
{.rb_clock_residual_sql('q', 'rf', max_gap_s, lo, hi)},
pr AS (SELECT vid, stream, period, rb_v AS v_ms, -(e_next - e_prev) / 2 * rb_v AS along,
              CAST(NULL AS DOUBLE) AS across, (e_next - e_prev) / 2 AS lag_i,
              abs(e_next - e_prev) / 2 * rb_v AS d0
       FROM res WHERE e_prev IS NOT NULL AND e_next IS NOT NULL),"
    )
  }
  sql <- .rb_fill(
    "
WITH p AS (
  SELECT vid, time, lon, lat{crs}, stream, {per} AS period FROM ({.rb_sql(p)})
  WHERE vid IS NOT NULL AND time IS NOT NULL AND lon IS NOT NULL AND lat IS NOT NULL AND stream IS NOT NULL),
{pairs}
u AS (SELECT vid, stream, period, count(*) AS pairs, median(lag_i) AS lag_s,
             quantile_cont(lag_i, 0.1) AS lag_q10_s, quantile_cont(lag_i, 0.9) AS lag_q90_s,
             median(along) AS along_m, median(abs(across)) AS across_m, median(d0) AS dist_zero_m
      FROM pr GROUP BY ALL),
dl AS (SELECT pr.vid, pr.stream, pr.period,
              median(sqrt(power(pr.along + u.lag_s * pr.v_ms, 2) + coalesce(power(pr.across, 2), 0))) AS dist_lag_m
       FROM pr JOIN u ON {.rb_clock_on('pr', 'u', c('vid', 'stream', 'period'))} GROUP BY ALL),
n AS (SELECT vid, stream, period, count(*) AS pings FROM p GROUP BY ALL),
vm AS (SELECT vid, period, count(DISTINCT stream) AS streams, bool_or(stream = {ref}) AS has_ref FROM p GROUP BY ALL)
SELECT {ref} AS reference, n.vid, n.stream, n.period, n.pings, coalesce(u.pairs, 0) AS pairs, u.lag_s,
       u.lag_q10_s, u.lag_q90_s, u.along_m, u.across_m, u.dist_zero_m, dl.dist_lag_m,
       CASE WHEN n.stream = {ref} THEN 'reference'
            WHEN vm.streams = 1 THEN 'one stream'
            WHEN NOT vm.has_ref THEN 'no reference'
            WHEN coalesce(u.pairs, 0) < {min_pairs} THEN 'not measurable'
            WHEN abs(u.lag_s) < {lag_min_s} THEN 'aligned'
            WHEN dl.dist_lag_m <= {better} * u.dist_zero_m THEN 'offset'
            ELSE 'unclear' END AS outcome
FROM n JOIN vm ON {.rb_clock_on('n', 'vm', c('vid', 'period'))}
LEFT JOIN u ON {.rb_clock_on('n', 'u', c('vid', 'stream', 'period'))}
LEFT JOIN dl ON {.rb_clock_on('n', 'dl', c('vid', 'stream', 'period'))}"
  )
  out <- dplyr::tbl(
    con,
    dplyr::sql(sprintf("SELECT * FROM (%s) AS rb_out", sql))
  )
  out <- .rb_std_out(out, map[c("vid", "stream")])
  if (lazy_in) {
    return(.rb_hold(out))
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  out <- out[order(out[[map[["vid"]]]], out[[map[["stream"]]]], out$period), ]
  .rb_as_given(out, x)
}

# The along-track residual pair of each ping of `q` against the pings of `rf` before and after it (SQL relations with
# vid, time, lon, lat, speed in knots, course in degrees): e = (along - v dt) / v seconds, along the mean course of the
# two pings at their mean speed, for intervals of at most max_gap_s and a mean speed within lo-hi knots. Defines the
# CTEs rb_a, rb_b and `res` (q's columns, e_prev, e_next, rb_v: the mean of the two means, m/s). With rf NULL, the
# neighbours are q's own previous and next ping of the vessel (a window, ties by lon, lat).
.rb_clock_residual_sql <- function(q, rf, max_gap_s, lo, hi) {
  M <- sprintf("%.17e", .rb_clock_m)
  MS <- sprintf("%.17e", .rb_clock_ms)
  side <- function(s, d) {
    # d: +1 the neighbour is before (e of the ping against it), -1 after (e of the neighbour against the ping)
    lon_a <- if (d > 0) paste0("lon", s) else "lon"
    lat_a <- if (d > 0) paste0("lat", s) else "lat"
    lon_b <- if (d > 0) "lon" else paste0("lon", s)
    lat_b <- if (d > 0) "lat" else paste0("lat", s)
    t_a <- if (d > 0) paste0("t", s) else "time"
    t_b <- if (d > 0) "time" else paste0("t", s)
    sprintf(
      "CASE WHEN (speed + speed%1$s) / 2 BETWEEN %2$s AND %3$s AND epoch(%4$s) - epoch(%5$s) > 0
              AND epoch(%4$s) - epoch(%5$s) <= %6$s
       THEN (((%7$s - %8$s) * cos(radians(%9$s)) * %10$s * sin(atan2(sin(radians(course)) + sin(radians(course%1$s)), cos(radians(course)) + cos(radians(course%1$s))))
              + (%9$s - %11$s) * %10$s * cos(atan2(sin(radians(course)) + sin(radians(course%1$s)), cos(radians(course)) + cos(radians(course%1$s)))))
             - (speed + speed%1$s) / 2 * %12$s * (epoch(%4$s) - epoch(%5$s))) / ((speed + speed%1$s) / 2 * %12$s) END",
      s,
      lo,
      hi,
      t_b,
      t_a,
      max_gap_s,
      lon_b,
      lon_a,
      lat_b,
      M,
      lat_a,
      MS
    )
  }
  nb <- if (is.null(rf)) {
    sprintf(
      "rb_b AS (
  SELECT *, lag(time) OVER o AS tp, lag(lon) OVER o AS lonp, lag(lat) OVER o AS latp, lag(speed) OVER o AS speedp,
         lag(course) OVER o AS coursep, lead(time) OVER o AS tn, lead(lon) OVER o AS lonn, lead(lat) OVER o AS latn,
         lead(speed) OVER o AS speedn, lead(course) OVER o AS coursen
  FROM %s WINDOW o AS (PARTITION BY vid ORDER BY time, lon, lat))",
      q
    )
  } else {
    sprintf(
      "rb_a AS (
  SELECT %1$s.*, p.time AS tp, p.lon AS lonp, p.lat AS latp, p.speed AS speedp, p.course AS coursep
  FROM %1$s ASOF LEFT JOIN %2$s p ON %1$s.vid = p.vid AND %1$s.time > p.time),
rb_b AS (
  SELECT rb_a.*, n.time AS tn, n.lon AS lonn, n.lat AS latn, n.speed AS speedn, n.course AS coursen
  FROM rb_a ASOF LEFT JOIN %2$s n ON rb_a.vid = n.vid AND rb_a.time < n.time)",
      q,
      rf
    )
  }
  sprintf(
    "%s,
res AS (
  SELECT * EXCLUDE (tp, lonp, latp, speedp, coursep, tn, lonn, latn, speedn, coursen),
         %s AS e_prev, %s AS e_next, ((speed + speedp) / 2 + (speed + speedn) / 2) / 2 * %s AS rb_v
  FROM rb_b)",
    nb,
    side("p", 1),
    side("n", -1),
    MS
  )
}

#' Give a ping its own lag where the ping itself shows it
#'
#' Step 0 of the fishing-activity flow. A stream's lag per vessel and period ([rb_create_ping_lags()]) is the bulk;
#' some pings carry evidence of their own, and it comes first (fishycode plan 059, ask b):
#'
#' * `"copy"`: the ping is a copy of a report the reference also holds: a reference ping of the vessel within `dist_m`
#'   and at most `max_dt_s` from it, the vessel moving (the reference's recorded speed within `kn`), so two reports
#'   of a moving vessel cannot sit that close. Its own lag is the time between the two; the nearest in time wins.
#'   In Iceland, astd's copies of STK's reports in a late month that were on time, and STK's own late deliveries.
#' * `"residual"`: the ping is displaced against its neighbours, the along-track residual pair (`-d`, `+d`) by the
#'   recorded speed and course (see [rb_create_ping_lags()]): behind its time from the ping before and ahead of it
#'   to the ping after, by at least `min_s` each (or the reverse, early). Its own lag is `(e_next - e_prev) / 2`.
#'   The neighbours are the reference's pings, or with no `reference` the vessel's previous and next ping among
#'   those judged (the merged track of `streams`): a single ping dropped late into a regular stream (ramb's `dansk`:
#'   288 pings some 39 s late, one 39 s early).
#' * `"receipt"`: the ping carries the time it was received (`receipt`), as a monitoring centre keeps it, and its
#'   unit's clock is read from the gap `receipt - time` (fishycode plan 060). A ping stamped at its receipt (gap 0) has
#'   the receipt's clock: `lag_at_receipt`. For the others, the unit's *level* is the `level_prob` quantile of its
#'   own-stamped gaps within `level_window_s[1]` either side, gaps over `backlog_s` (a backlog delivered late) left out:
#'   a late delivery raises the median, less the floor. The stream's *usual level* is the median level of its pings
#'   in the `period`, since most clocks keep time. Where both the level and the level over `level_window_s[2]` depart
#'   from the usual by more than `depart_s`, the unit's clock is off, and the lag is `lag_usual` less the departure;
#'   elsewhere it is `lag_usual`, the stream's usual lag to the reference. A clock drifts for weeks; a slow day of
#'   delivery does not. A ping whose unit has no level gets none. Near a clock that is set right, the week around the
#'   ping reads it as on time, so the last days of a drift before the reset may be left unmoved: the price of telling
#'   a drift from a slow week of delivery (on Iceland's VMS a week before or after, either one, put fewer pings
#'   within 3 min of the track in all). The method judges every ping with a receipt,
#'   whatever its stream: give the receipt only to the pings it should judge. On Iceland's VMS (2007-2026, against the
#'   vessels' AIS tracks) it puts 93 % of the pings of units an hour off within 3 min of the track, against 1 % left
#'   alone, and 93 % of those on time (92 % left alone).
#'
#' With more than one, a copy comes first, then the receipt. A ping of a reference stream gets no copy or residual
#' lag. With `"copy"` and `"receipt"`, a copy of a reference ping the receipt moves takes that lag too, so it lands
#' on its original's harmonized time and stays a duplicate of it. A copy of a ping moved later, by its stream's lag in
#' [rb_add_ping_time()], keeps the original's delivered time.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon`, `lat` and the stream column; `"copy"`
#'   needs the recorded `speed` (knots), `"residual"` the recorded `speed` and `course` (degrees), `"receipt"` the
#'   receipt time.
#' @param reference The reference stream or streams. Needed for `"copy"`; for `"residual"`, `NULL` takes the
#'   neighbours in the merged track.
#' @param method `"copy"`, `"residual"`, `"receipt"`, or more than one.
#' @param streams The streams whose pings are judged by `"copy"` and `"residual"`; `NULL`, every stream but the
#'   reference.
#' @param stream The stream column ([rb_add_ping_stream()]).
#' @param dist_m,max_dt_s For `"copy"`: metres and seconds apart.
#' @param kn The moving vessel's speed range, knots.
#' @param min_s,max_gap_s For `"residual"`: the least residual on each side, seconds; the longest interval to a
#'   neighbour, seconds.
#' @param vid,time,lon,lat,speed,course The columns, if named otherwise.
#' @param receipt For `"receipt"`: the column of the time each ping was received (date-time).
#' @param unit For `"receipt"`: the column naming the unit whose clock stamps the ping (a transponder, a mobile id);
#'   `NULL`, each vessel's stream.
#' @param level_window_s For `"receipt"`: the level's window either side, and the longer one that confirms it, seconds.
#' @param level_prob,backlog_s For `"receipt"`: the level's quantile; the longest gap that counts in it, seconds.
#' @param depart_s For `"receipt"`: the departure from the usual level that marks a clock off, seconds.
#' @param lag_usual For `"receipt"`: the stream's usual lag to the reference, seconds (positive: later), one number,
#'   or a table with the stream column, `period` and `lag_s` (a stream or period it lacks gets no receipt lag).
#' @param lag_at_receipt For `"receipt"`: the lag of a ping stamped at its receipt: 0 where the reference is stamped
#'   at its receipt too.
#' @param period For `"receipt"`: the period of the usual level and `lag_usual` (as in [rb_create_ping_lags()]).
#'
#' @return `x` with `lag_own_s` (seconds; positive: the ping's time is later than its position's) and
#'   `lag_own_basis` (`"copy"`, `"receipt"`, `"residual"`, or `NA`). The same type, the same rows.
#' @family ping clocks
#' @export
rb_add_ping_own_lag <- function(
  x,
  reference = NULL,
  method = c("copy", "residual"),
  streams = NULL,
  stream = stream,
  dist_m = 2,
  max_dt_s = 180,
  kn = c(2, 25),
  min_s = 25,
  max_gap_s = 60,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  speed = speed,
  course = course,
  receipt = receipt,
  unit = NULL,
  level_window_s = c(86400, 604800),
  level_prob = 0.25,
  backlog_s = 10800,
  depart_s = 120,
  lag_usual = 0,
  lag_at_receipt = 0,
  period = "year"
) {
  method <- if (missing(method)) {
    "copy"
  } else {
    match.arg(method, c("copy", "residual", "receipt"), several.ok = TRUE)
  }
  rcp <- "receipt" %in% method
  unit_q <- rlang::enquo(unit)
  unit_col <- if (rlang::quo_is_null(unit_q)) NULL else rlang::as_name(unit_q)
  .rb_check_input(
    x,
    "rb_add_ping_own_lag",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat)),
      stream = rlang::as_name(rlang::enquo(stream))
    ),
    also = c(
      if (any(c("copy", "residual") %in% method)) {
        c(speed = rlang::as_name(rlang::enquo(speed)))
      },
      if ("residual" %in% method) {
        c(course = rlang::as_name(rlang::enquo(course)))
      },
      if (rcp) c(receipt = rlang::as_name(rlang::enquo(receipt))),
      if (rcp && !is.null(unit_col)) c(unit = unit_col)
    ),
    types = c(
      time = "time",
      lon = "numeric",
      lat = "numeric",
      speed = "numeric",
      receipt = "time"
    ),
    fresh = TRUE
  )
  .rb_warn_after_clean(x, "rb_add_ping_own_lag")
  if (rcp) {
    .rb_clock_check_period(period)
    stopifnot(
      is.numeric(level_window_s),
      length(level_window_s) == 2,
      all(level_window_s > 0),
      level_window_s[2] >= level_window_s[1],
      is.numeric(level_prob),
      length(level_prob) == 1,
      level_prob > 0,
      level_prob < 1,
      is.numeric(backlog_s),
      length(backlog_s) == 1,
      backlog_s > 0,
      is.numeric(depart_s),
      length(depart_s) == 1,
      depart_s >= 0,
      is.numeric(lag_at_receipt) ||
        (length(lag_at_receipt) == 1 && is.na(lag_at_receipt)),
      length(lag_at_receipt) == 1
    )
    if (
      !(is.data.frame(lag_usual) || inherits(lag_usual, "tbl_lazy")) &&
        !(is.numeric(lag_usual) && length(lag_usual) == 1 && !is.na(lag_usual))
    ) {
      rlang::abort(c(
        "`lag_usual` must be one number of seconds, or a table of the stream column, `period` and `lag_s`.",
        i = "It is the stream's usual lag to the reference: measured on the reference's track, or 0."
      ))
    }
  }
  if (
    "copy" %in%
      method &&
      (is.null(reference) || !is.character(reference) || anyNA(reference))
  ) {
    rlang::abort(c(
      "`method = \"copy\"` needs `reference`: the stream or streams whose reports the others may copy.",
      i = "With `method = \"residual\"` alone, `reference = NULL` judges each ping against its neighbours in the track."
    ))
  }
  if (!is.null(reference) && !(is.character(reference) && !anyNA(reference))) {
    stop(
      "`reference` must be stream values (character), or NULL.",
      call. = FALSE
    )
  }
  if (
    !is.null(streams) &&
      !(is.character(streams) && length(streams) > 0 && !anyNA(streams))
  ) {
    stop("`streams` must be stream values (character), or NULL.", call. = FALSE)
  }
  .rb_clock_check_kn(kn)
  stopifnot(
    is.numeric(dist_m),
    length(dist_m) == 1,
    dist_m > 0,
    is.numeric(max_dt_s),
    length(max_dt_s) == 1,
    max_dt_s >= 0,
    is.numeric(min_s),
    length(min_s) == 1,
    min_s > 0
  )
  given <- x
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }},
    stream = {{ stream }}
  )
  if (any(c("copy", "residual") %in% method)) {
    map <- c(map, .rb_cols(speed = {{ speed }}))
  }
  if ("residual" %in% method) {
    map <- c(map, .rb_cols(course = {{ course }}))
  }
  if (rcp) {
    map <- c(map, .rb_cols(receipt = {{ receipt }}))
    if (!is.null(unit_col)) map <- c(map, c(unit = unit_col))
  }
  con <- .rb_con_for(x, if (rcp && !is.numeric(lag_usual)) lag_usual)
  .rb_scope()
  p <- .rb_clock_in(x, map, con)
  refs <- if (is.null(reference)) NULL else .rb_clock_strings(con, reference)
  not_ref <- if (is.null(refs)) "true" else sprintf("stream NOT IN (%s)", refs)
  if (!is.null(streams)) {
    not_ref <- sprintf(
      "%s AND stream IN (%s)",
      not_ref,
      .rb_clock_strings(con, streams)
    )
  }
  lo <- kn[1]
  hi <- kn[2]
  keys <- c("vid", "stream", "time", "lon", "lat")
  ctes <- character(0)
  parts <- character(0)
  if ("copy" %in% method) {
    near <- .rb_near_pairs("rb_cb", "rb_ca", "vid", dist_m, max_dt_s, con)
    ctes <- c(
      ctes,
      .rb_fill(
        "
rb_ck AS MATERIALIZED (SELECT row_number() OVER () AS rb_i, * FROM (
  SELECT DISTINCT vid, stream, time, lon, lat FROM x
  WHERE {not_ref} AND vid IS NOT NULL AND time IS NOT NULL AND lon IS NOT NULL AND lat IS NOT NULL)),
rb_ca AS (SELECT rb_i, vid, time, lon, lat FROM rb_ck),
rb_cb AS MATERIALIZED (SELECT row_number() OVER () AS rb_i, * FROM (
  SELECT DISTINCT vid, time, lon, lat FROM x
  WHERE stream IN ({refs}) AND speed BETWEEN {lo} AND {hi} AND vid IS NOT NULL AND time IS NOT NULL)),
rb_np AS ({near}),
rb_cp AS (
  SELECT rb_ib AS rb_i, rb_ia, dt_s AS lag_copy FROM rb_np
  QUALIFY row_number() OVER (PARTITION BY rb_ib ORDER BY abs(dt_s), dt_s, d_m) = 1),
rb_copy AS (SELECT rb_ck.vid, rb_ck.stream, rb_ck.time, rb_ck.lon, rb_ck.lat, rb_cp.lag_copy,
                   rb_cb.time AS rb_otime, rb_cb.lon AS rb_olon, rb_cb.lat AS rb_olat
            FROM rb_cp JOIN rb_ck USING (rb_i) JOIN rb_cb ON rb_cb.rb_i = rb_cp.rb_ia)"
      )
    )
  }
  if ("residual" %in% method) {
    rf <- if (is.null(refs)) {
      NULL
    } else {
      "rb_rf"
    }
    q <- .rb_fill(
      "rb_q AS (SELECT DISTINCT vid, stream, time, lon, lat, speed, course FROM x
       WHERE {not_ref} AND vid IS NOT NULL AND time IS NOT NULL AND lon IS NOT NULL AND lat IS NOT NULL
         AND speed IS NOT NULL AND course BETWEEN 0 AND 360)"
    )
    if (!is.null(rf)) {
      q <- paste0(
        q,
        .rb_fill(
          ",
rb_rf AS (SELECT vid, time, lon, lat, speed, course FROM x
          WHERE stream IN ({refs}) AND vid IS NOT NULL AND time IS NOT NULL AND lon IS NOT NULL
            AND speed IS NOT NULL AND course BETWEEN 0 AND 360
          QUALIFY row_number() OVER (PARTITION BY vid, time ORDER BY lon, lat, speed, course) = 1)"
        )
      )
    }
    ctes <- c(
      ctes,
      q,
      .rb_clock_residual_sql("rb_q", rf, max_gap_s, lo, hi),
      .rb_fill(
        "rb_res AS (
  SELECT vid, stream, time, lon, lat, max((e_next - e_prev) / 2) AS lag_res FROM res
  WHERE (e_prev <= -{min_s} AND e_next >= {min_s}) OR (e_prev >= {min_s} AND e_next <= -{min_s})
  GROUP BY ALL)"
      )
    )
    parts <- c(
      parts,
      sprintf("LEFT JOIN rb_res ON %s", .rb_clock_on("x", "rb_res", keys))
    )
  }
  if (rcp) {
    # each ping once, with its earliest receipt (a report received twice is one ping); its unit's levels over its own-
    # stamped gaps within the backlog bound, by a window per unit; the stream's usual level per period, the median
    num <- function(v) {
      if (is.na(v)) "CAST(NULL AS DOUBLE)" else sprintf("%.17e", v)
    }
    part <- if (is.null(unit_col)) "vid, stream" else "rb_unit"
    usel <- if (is.null(unit_col)) {
      "CAST(NULL AS VARCHAR)"
    } else {
      "CAST(unit AS VARCHAR)"
    }
    per <- .rb_clock_period_sql(period, "time")
    lvl <- sprintf(
      "quantile_cont(rb_gap, %s) FILTER (WHERE rb_gap <> 0 AND abs(rb_gap) <= %s)",
      num(level_prob),
      num(backlog_s)
    )
    if (is.numeric(lag_usual)) {
      usual <- num(lag_usual)
      ujoin <- ""
    } else {
      lu <- .rb_as_lazy(.rb_std_in(lag_usual, map["stream"]), con)
      miss <- setdiff(c("stream", "period", "lag_s"), colnames(lu))
      if (length(miss)) {
        rlang::abort(c(
          sprintf(
            "`lag_usual` must have the stream column `%s`, `period` and `lag_s`.",
            map[["stream"]]
          ),
          x = sprintf("%s missing.", paste0("`", miss, "`", collapse = ", "))
        ))
      }
      usual <- "rb_u.lag_s"
      ujoin <- sprintf(
        "LEFT JOIN (SELECT stream, CAST(period AS DATE) AS period, lag_s FROM (%s)) rb_u
           ON rb_u.stream = r.stream AND rb_u.period IS NOT DISTINCT FROM r.rb_per",
        .rb_sql(lu)
      )
    }
    ctes <- c(
      ctes,
      .rb_fill(
        "
rb_rk AS MATERIALIZED (
  SELECT {usel} AS rb_unit, vid, stream, time, lon, lat, {per} AS rb_per, min(epoch(receipt - time)) AS rb_gap
  FROM x WHERE receipt IS NOT NULL AND vid IS NOT NULL AND time IS NOT NULL GROUP BY ALL),
rb_rl AS (
  SELECT *, {lvl} OVER rb_d AS rb_lvl, {lvl} OVER rb_w AS rb_lvlw FROM rb_rk
  WINDOW rb_d AS (PARTITION BY {part} ORDER BY time
                  RANGE BETWEEN INTERVAL '{level_window_s[1]} seconds' PRECEDING AND INTERVAL '{level_window_s[1]} seconds' FOLLOWING),
         rb_w AS (PARTITION BY {part} ORDER BY time
                  RANGE BETWEEN INTERVAL '{level_window_s[2]} seconds' PRECEDING AND INTERVAL '{level_window_s[2]} seconds' FOLLOWING)),
rb_l0 AS (SELECT stream, rb_per, median(rb_lvl) AS rb_l0 FROM rb_rl WHERE rb_gap <> 0 GROUP BY ALL),
rb_rcp AS (
  SELECT r.vid, r.stream, r.time, r.lon, r.lat,
         CASE WHEN r.rb_gap = 0 THEN {num(lag_at_receipt)}
              WHEN r.rb_lvl IS NULL THEN NULL
              WHEN abs(r.rb_lvl - l.rb_l0) > {num(depart_s)} AND abs(r.rb_lvlw - l.rb_l0) > {num(depart_s)}
                THEN {usual} - (r.rb_lvl - l.rb_l0)
              ELSE {usual} END AS lag_rcp
  FROM rb_rl r
  LEFT JOIN rb_l0 l ON l.stream IS NOT DISTINCT FROM r.stream AND l.rb_per IS NOT DISTINCT FROM r.rb_per
  {ujoin}
  QUALIFY row_number() OVER (PARTITION BY r.vid, r.stream, r.time, r.lon, r.lat ORDER BY r.rb_unit) = 1)"
      )
    )
    parts <- c(
      parts,
      sprintf("LEFT JOIN rb_rcp ON %s", .rb_clock_on("x", "rb_rcp", keys))
    )
  }
  if ("copy" %in% method) {
    # a copy follows its original's receipt lag (the reference row has no stream: where two streams hold the same
    # report, the least lag)
    copy_tab <- "rb_copy"
    if (rcp) {
      ctes <- c(
        ctes,
        "rb_cpr AS (
  SELECT c.vid, c.stream, c.time, c.lon, c.lat, c.lag_copy + coalesce(o.lag_rcp, 0) AS lag_copy
  FROM rb_copy c
  LEFT JOIN (SELECT vid, time, lon, lat, min(lag_rcp) AS lag_rcp FROM rb_rcp GROUP BY ALL) o
    ON o.vid = c.vid AND o.time = c.rb_otime AND o.lon = c.rb_olon AND o.lat = c.rb_olat)"
      )
      copy_tab <- "rb_cpr"
    }
    parts <- c(
      sprintf(
        "LEFT JOIN %s ON %s",
        copy_tab,
        .rb_clock_on("x", copy_tab, keys)
      ),
      parts
    )
  }
  nul <- "CAST(NULL AS DOUBLE)"
  copy_col <- if ("copy" %in% method) paste0(copy_tab, ".lag_copy") else nul
  rcp_col <- if (rcp) "rb_rcp.lag_rcp" else nul
  res_col <- if ("residual" %in% method) "rb_res.lag_res" else nul
  sql <- sprintf(
    "
WITH x AS (%s),
%s
SELECT x.*, CAST(coalesce(%s, %s, %s) AS DOUBLE) AS lag_own_s,
       CASE WHEN %s IS NOT NULL THEN 'copy' WHEN %s IS NOT NULL THEN 'receipt' WHEN %s IS NOT NULL THEN 'residual'
            END AS lag_own_basis
FROM x %s",
    .rb_sql(p),
    paste(ctes, collapse = ",\n"),
    copy_col,
    rcp_col,
    res_col,
    copy_col,
    rcp_col,
    res_col,
    paste(parts, collapse = "\n")
  )
  .rb_clock_out(dplyr::tbl(con, dplyr::sql(sql)), given, map, con)
}

#' Harmonize the streams' times
#'
#' Step 0 of the fishing-activity flow, after the lags are measured: each ping's time is moved by its lag, so that
#' every stream keeps the reference's clock, and the delivered time is kept beside it (fishycode plan 059 §3). The
#' lag, first that applies:
#'
#' 1. the ping's own lag ([rb_add_ping_own_lag()]): basis `"copy"`, `"receipt"` or `"residual"`;
#' 2. its stream's lag where the unit's outcome is `"offset"` ([rb_create_ping_lags()]): `"measured"`;
#' 3. none where the unit is the reference, `"aligned"` or `"one stream"`: `"none"`, lag 0;
#' 4. else (no stream, or a unit `"not measurable"`, `"unclear"` or with `"no reference"`): `"unmeasured"`, the lag
#'    `NA`, and the ping is not moved. A ping whose stream or lag cannot be told is flagged by its basis, never moved.
#'
#' Without `lags`, a ping with no own lag is `"none"` where its vessel has one stream in the period, else
#' `"unmeasured"`.
#'
#' Derive speeds, turns and windows after this step, and within one stream (fishycode, AGENTS.md).
#'
#' **A ping can move into another period.** Its lag can carry it across midnight, a month or New Year (on Iceland's
#' track, 53 pings moved back over New Year by 11-63 s). A table partitioned by the delivered time then holds pings of
#' another year: partition the output, and join anything by period, on the harmonized `time`. The lags themselves are
#' looked up by the delivered time's period, where they were measured.
#'
#' **The order of the pings** (the step 0 contract): before this step a track is sorted by its delivered time, which
#' is wrong where a clock drifts and a receipt time would be wrong after a backlog is flushed; from this step on, by
#' the harmonized `time`, with `time_delivered` beside it.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time` and the stream column, and `lag_own_s` and
#'   `lag_own_basis` if [rb_add_ping_own_lag()] has been run.
#' @param lags The table of [rb_create_ping_lags()], or `NULL`.
#' @param stream The stream column.
#' @param period The period `lags` was measured over (as in [rb_create_ping_lags()]); a ping takes the unit of its
#'   delivered time.
#' @param digits The lag is rounded to this many decimals of a second before it is applied (`NULL`: not rounded).
#' @param vid,time The columns, if named otherwise.
#'
#' @return `x` with the time column harmonized (the delivered time minus the lag), `time_delivered`, `lag_s` (the lag
#'   applied; `NA` where unmeasured) and `lag_basis`. The same type, the same rows.
#' @family ping clocks
#' @export
rb_add_ping_time <- function(
  x,
  lags = NULL,
  stream = stream,
  period = "month",
  digits = 0,
  vid = vid,
  time = time
) {
  .rb_check_input(
    x,
    "rb_add_ping_time",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      stream = rlang::as_name(rlang::enquo(stream))
    ),
    types = c(time = "time"),
    fresh = TRUE
  )
  .rb_clock_check_period(period)
  stopifnot(is.null(digits) || (is.numeric(digits) && length(digits) == 1))
  own <- all(c("lag_own_s", "lag_own_basis") %in% colnames(x))
  given <- x
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }}, stream = {{ stream }})
  con <- .rb_con_for(x, lags)
  .rb_scope()
  p <- .rb_clock_in(x, map, con)
  per <- .rb_clock_period_sql(period, "x.time")
  rnd <- function(v) {
    if (is.null(digits)) v else sprintf("round(%s, %d)", v, as.integer(digits))
  }
  if (!is.null(lags)) {
    need <- c(map[["vid"]], map[["stream"]], "period", "lag_s", "outcome")
    miss <- setdiff(need, colnames(lags))
    if (length(miss)) {
      rlang::abort(c(
        sprintf(
          "`lags` must have %s.",
          paste0("`", need, "`", collapse = ", ")
        ),
        x = sprintf("%s missing.", paste0("`", miss, "`", collapse = ", ")),
        i = "It is the table of `rb_create_ping_lags()`, with the same `vid` and `stream` names as `x`."
      ))
    }
    if (inherits(lags, "sf")) {
      lags <- sf::st_drop_geometry(lags)
    }
    lz <- .rb_as_lazy(.rb_std_in(lags, map[c("vid", "stream")]), con)
    # the lags table is small: a unit given twice would double its pings, a period of another unit match none
    chk <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT count(*) AS n, count(DISTINCT (vid, stream, period)) AS units,
                count(*) FILTER (WHERE period IS NOT NULL AND %s) AS other_period FROM (%s)",
        if (is.null(period)) {
          "true"
        } else {
          sprintf("CAST(date_trunc('%s', period) AS DATE) <> period", period)
        },
        .rb_sql(lz)
      )
    )
    if (chk$units < chk$n) {
      rlang::abort(c(
        "`lags` must hold one row per vessel, stream and period.",
        x = sprintf("%d rows, %d units.", chk$n, chk$units)
      ))
    }
    if (chk$other_period > 0) {
      rlang::abort(c(
        sprintf(
          "`lags` was not measured by %s.",
          if (is.null(period)) "one period" else period
        ),
        i = "Give `period` as in the call to `rb_create_ping_lags()`."
      ))
    }
    unit <- sprintf(
      "LEFT JOIN (SELECT vid, stream, period, lag_s AS rb_lag, outcome AS rb_outcome FROM (%s)) l
         ON l.vid = x.vid AND l.stream = x.stream AND l.period IS NOT DISTINCT FROM %s",
      .rb_sql(lz),
      per
    )
    unit_case <- sprintf(
      "WHEN l.rb_outcome = 'offset' THEN %s
       WHEN l.rb_outcome IN ('reference', 'aligned', 'one stream') THEN 0",
      rnd("l.rb_lag")
    )
    unit_basis <- "WHEN l.rb_outcome = 'offset' THEN 'measured'
       WHEN l.rb_outcome IN ('reference', 'aligned', 'one stream') THEN 'none'"
  } else {
    unit <- sprintf(
      "LEFT JOIN (SELECT vid, rb_per, count(DISTINCT stream) AS rb_streams FROM (SELECT vid, stream, %s AS rb_per
         FROM x WHERE stream IS NOT NULL) x GROUP BY ALL) l ON l.vid = x.vid AND l.rb_per IS NOT DISTINCT FROM %s",
      per,
      per
    )
    unit_case <- "WHEN l.rb_streams = 1 THEN 0"
    unit_basis <- "WHEN l.rb_streams = 1 THEN 'none'"
  }
  own_case <- if (own) {
    sprintf(
      "WHEN x.lag_own_basis IN ('copy', 'receipt', 'residual') AND x.lag_own_s IS NOT NULL THEN %s",
      rnd("x.lag_own_s")
    )
  } else {
    ""
  }
  own_basis <- if (own) {
    "WHEN x.lag_own_basis IN ('copy', 'receipt', 'residual') AND x.lag_own_s IS NOT NULL THEN x.lag_own_basis"
  } else {
    ""
  }
  sql <- sprintf(
    "
WITH x AS (%s),
j AS (
  SELECT x.*,
         CASE WHEN x.time IS NULL THEN NULL %s WHEN x.stream IS NULL THEN NULL %s END AS rb_lag_s,
         CASE %s WHEN x.stream IS NULL OR x.time IS NULL THEN 'unmeasured' %s ELSE 'unmeasured' END AS rb_basis
  FROM x %s)
SELECT * EXCLUDE (rb_lag_s, rb_basis)
       REPLACE (CASE WHEN rb_lag_s IS NULL THEN time ELSE time - to_microseconds(CAST(round(rb_lag_s * 1000000) AS BIGINT)) END AS time),
       time AS time_delivered, CAST(rb_lag_s AS DOUBLE) AS lag_s, rb_basis AS lag_basis
FROM j",
    .rb_sql(p),
    own_case,
    unit_case,
    own_basis,
    unit_basis,
    unit
  )
  .rb_clock_out(dplyr::tbl(con, dplyr::sql(sql)), given, map, con)
}
