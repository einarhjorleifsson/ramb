# record_windows.R — step 2 of the fishing-activity flow (logbook record) and the "window" method of step 4.
#
# Ported from fishycode's R/duration_limit.R, curate/station_window.R and R/trail_sql.R (plan 010, plan 011
# phase 5; ramb plan 013 phase 3, fishycode plan 014 phase 2), as exact ports. A record (a logbook station) is
# not one interval: a towed gear has a tow, a static gear a set and a haul with the soak between them not a
# window, a jigger one long jig, and a record with no usable time the whole day. Runs in DuckDB (tier 2): a
# data frame is registered and the result collected; a lazy table stays lazy.

.rb_q <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))
.rb_in_sql <- function(x, con) .rb_sql(.rb_as_lazy(x, con))
.rb_out <- function(con, sql, lazy_in, by = NULL) {
  # wrapped, so the lazy result composes: dbplyr puts a query as it is after UNION ALL, where a bare WITH fails
  out <- dplyr::tbl(
    con,
    dplyr::sql(sprintf("SELECT * FROM (%s) AS rb_out", sql))
  )
  if (lazy_in) {
    return(out)
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  if (!is.null(by)) {
    out <- out[do.call(base::order, unname(as.list(out[by]))), ]
  }
  tibble::as_tibble(out)
}

#' Judge logbook durations against a limit per gear
#'
#' A duration above the gear's limit is implausible: it is set to `NA`, never capped at the limit, since a cap
#' turns an error into a precise-looking value. The raw value is always kept.
#'
#' @param records A data frame or lazy DuckDB table with a gear key and a duration.
#' @param limits A data frame with the gear key and `dur_max_min`, the limit in the duration's unit. A gear
#'   with no limit is never implausible.
#' @param gear,duration The gear key and duration columns, if named otherwise (default `gid`, `duration_m`).
#' @return `records` with `<duration>_raw` (the value as given), `duration_source` (`data`, `missing` or
#'   `implausible`) and `<duration>` set to `NA` where implausible.
#' @export
rb_limit_record_duration <- function(
  records,
  limits,
  gear = gid,
  duration = duration_m
) {
  g <- rlang::as_name(rlang::enquo(gear))
  d <- rlang::as_name(rlang::enquo(duration))
  stopifnot(
    all(c(g, d) %in% colnames(records)),
    all(c(g, "dur_max_min") %in% names(limits))
  )
  lim <- dplyr::distinct(
    dplyr::select(limits, dplyr::all_of(c(g, "dur_max_min"))),
    .data[[g]],
    .keep_all = TRUE
  )
  if (inherits(records, "tbl_lazy")) {
    lim <- dplyr::copy_to(
      dbplyr::remote_con(records),
      lim,
      name = paste0("rb_lim_", sample(1e9, 1)),
      overwrite = TRUE
    )
  }
  raw <- paste0(d, "_raw")
  records |>
    dplyr::left_join(lim, by = g) |>
    dplyr::mutate(
      !!raw := .data[[d]],
      duration_source = dplyr::case_when(
        is.na(.data[[d]]) ~ "missing",
        !is.na(dur_max_min) & .data[[d]] > dur_max_min ~ "implausible",
        .default = "data"
      ),
      !!d := dplyr::if_else(
        duration_source == "implausible",
        NA_real_,
        .data[[d]]
      )
    ) |>
    dplyr::select(-dur_max_min)
}

#' Build the time windows in which a vessel is busy with each logbook record
#'
#' One row per (record, phase). Towed gear: one `tow` window (`t2`-`t3`). Static gear: a `set` window from `t1`
#' of the gear's set length, and a `haul` window (`t3`-`t4`, or the gear's typical haul ending at `t4`). Jigged
#' gear: one `jig` window (`t1`-`t4`). A record that gets none of these gets its whole `date` (`day`).
#'
#' `win_basis` says where each window came from: `recorded` (both ends are logbook times), `derived` (one end
#' from `duration_m`), `synthesised` (an assumed length, or the whole day) or `capped` (a tow, haul or jig whose
#' end passed the vessel's next window and was cut back to it).
#'
#' A ping belongs to at most one record, so the windows of a vessel other than `day` never overlap. Windows are
#' half-open, `[w_start, w_end)`. They are put in order by start, then end, then record; on a full tie the
#' record with the lower key comes last. A window ends where a later start that may cut it begins, if that is
#' sooner (`win_closed_by = "next_event"`; a set that runs its full assumed length has `"max"`):
#' - the recorded start of a tow, haul or jig cuts any window;
#' - a set's start (`t1`) cuts only an earlier set. A set never cuts a tow, haul or jig: a gillnetter may set one
#'   net and then go on hauling another;
#' - a start that is not a logbook time (a typical haul ending at `t4`, a jig `duration_m` before `t4`) cuts
#'   nothing.
#'
#' A window that starts inside one it may not cut starts at that window's end instead (`win_opened_by =
#' "previous_event"`). Of two windows with the same start, the one that ends first is cut to nothing when the
#' other may cut it (`w_end = w_start`; the row is kept); of two identical windows, the record with the higher
#' key. The typical haul
#' is the median recorded haul of the gear, used only where at least `min_hauls` are recorded.
#'
#' @param records A data frame or lazy DuckDB table, one row per record: the `key` columns, `vid`, `gear`,
#'   `gear_class` (`towed`, `static` or `jigged`), `set_max_min` (the set length, minutes), `t1`-`t4`,
#'   `duration_m` and `date`. `gear_class` and `set_max_min` come from the gear rules. Other columns are kept.
#' @param key The columns that identify a record.
#' @param min_hauls Recorded hauls a gear needs before its median haul is used.
#' @return The windows: the record's columns (without `date`), `phase`, `w_start`, `w_end`, `win_basis`,
#'   `win_closed_by`, `win_opened_by`, `n_events` and `events` (which of `t1`-`t4` the record has).
#' @export
rb_build_record_windows <- function(
  records,
  key = c(".sid", "schema"),
  min_hauls = 50
) {
  need <- c(
    key,
    "vid",
    "gear",
    "gear_class",
    "set_max_min",
    "t1",
    "t2",
    "t3",
    "t4",
    "duration_m",
    "date"
  )
  miss <- setdiff(need, colnames(records))
  if (length(miss)) {
    stop("records lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  lazy_in <- inherits(records, "tbl_lazy")
  con <- .rb_con_for(records)
  in_sql <- .rb_in_sql(records, con)
  k <- .rb_q(con, key)
  k_desc <- paste(k, "DESC", collapse = ", ")
  k_eq <- function(a, b) {
    paste(sprintf("%s.%s = %s.%s", a, k, b, k), collapse = " AND ")
  }
  mins <- function(x) sprintf("to_minutes(CAST(round(%s) AS BIGINT))", x)
  sql <- .rb_fill(
    "
  WITH b0 AS ({in_sql}),
  th AS (
    SELECT gear, median(epoch(t4 - t3) / 60.0) AS haul_min FROM b0
    WHERE gear_class = 'static' AND t3 IS NOT NULL AND t4 > t3 GROUP BY gear HAVING count(*) >= {min_hauls}),
  b AS (SELECT b0.* EXCLUDE (date), date_trunc('day', b0.date) AS d, th.haul_min FROM b0 LEFT JOIN th USING (gear)),
  tow AS (
    SELECT *, 'tow' AS phase,
      CASE WHEN t2 IS NOT NULL AND t3 > t2 THEN t2
           WHEN t2 IS NOT NULL AND duration_m > 0 THEN t2 END AS w_start,
      CASE WHEN t2 IS NOT NULL AND t3 > t2 THEN t3
           WHEN t2 IS NOT NULL AND duration_m > 0 THEN t2 + {mins('duration_m')} END AS w_end,
      CASE WHEN t2 IS NOT NULL AND t3 > t2 THEN 'recorded'
           WHEN t2 IS NOT NULL AND duration_m > 0 THEN 'derived' END AS win_basis,
      TRUE AS rb_start_rec
    FROM b WHERE gear_class = 'towed'),
  haul AS (
    SELECT *, 'haul' AS phase,
      CASE WHEN t3 IS NOT NULL AND t4 > t3 THEN t3
           WHEN t4 IS NOT NULL AND t1 < t4 THEN greatest(t4 - {mins('haul_min')}, t1)
           WHEN t4 IS NOT NULL THEN t4 - {mins('haul_min')}
           ELSE t3 END AS w_start,
      CASE WHEN t3 IS NOT NULL AND t4 > t3 THEN t4
           WHEN t4 IS NOT NULL THEN t4
           ELSE t3 + {mins('haul_min')} END AS w_end,
      CASE WHEN t3 IS NOT NULL AND t4 > t3 THEN 'recorded' ELSE 'synthesised' END AS win_basis,
      coalesce(t4 IS NULL OR (t3 IS NOT NULL AND t4 > t3), FALSE) AS rb_start_rec
    FROM b WHERE gear_class = 'static' AND haul_min IS NOT NULL AND (t4 IS NOT NULL OR t3 IS NOT NULL)),
  st_set AS (
    SELECT *, 'set' AS phase, t1 AS w_start, t1 + {mins('set_max_min')} AS w_end, 'synthesised' AS win_basis,
      TRUE AS rb_start_rec
    FROM b WHERE gear_class = 'static' AND t1 IS NOT NULL AND set_max_min IS NOT NULL),
  jig AS (
    SELECT *, 'jig' AS phase,
      CASE WHEN t1 IS NOT NULL AND t4 > t1 THEN t1
           WHEN t4 IS NOT NULL AND duration_m > 0 THEN t4 - {mins('duration_m')}
           WHEN t1 IS NOT NULL AND duration_m > 0 THEN t1 END AS w_start,
      CASE WHEN t1 IS NOT NULL AND t4 > t1 THEN t4
           WHEN t4 IS NOT NULL AND duration_m > 0 THEN t4
           WHEN t1 IS NOT NULL AND duration_m > 0 THEN t1 + {mins('duration_m')} END AS w_end,
      CASE WHEN t1 IS NOT NULL AND t4 > t1 THEN 'recorded'
           WHEN duration_m > 0 AND (t1 IS NOT NULL OR t4 IS NOT NULL) THEN 'derived' END AS win_basis,
      coalesce((t1 IS NOT NULL AND t4 > t1) OR (t1 IS NOT NULL AND t4 IS NULL), FALSE) AS rb_start_rec
    FROM b WHERE gear_class = 'jigged'),
  named AS (
    SELECT * FROM tow  WHERE w_start IS NOT NULL UNION ALL BY NAME
    SELECT * FROM haul WHERE w_start IS NOT NULL UNION ALL BY NAME
    SELECT * FROM st_set                          UNION ALL BY NAME
    SELECT * FROM jig  WHERE w_start IS NOT NULL),
  dayw AS (
    SELECT b.*, 'day' AS phase, b.d AS w_start, b.d + INTERVAL 1 DAY AS w_end, 'synthesised' AS win_basis
    FROM b WHERE NOT EXISTS (SELECT 1 FROM named n WHERE {k_eq('n', 'b')})),
  -- one order per vessel with no ties; the record with the lower key comes last, and so keeps a shared window
  o AS (SELECT *, row_number() OVER (PARTITION BY vid ORDER BY w_start, w_end, {k_desc}, phase) AS rb_ord FROM named),
  -- the starts that cut: a tow, haul or jig with a recorded start cuts any window; a set's start cuts only sets
  rs_w AS (SELECT vid, rb_ord, w_start AS rb_next FROM o WHERE rb_start_rec AND phase <> 'set'),
  rs_a AS (SELECT vid, rb_ord, w_start AS rb_next FROM o WHERE rb_start_rec),
  c1 AS (SELECT o.*, w.rb_next AS rb_next_w, a.rb_next AS rb_next_a FROM o
         ASOF LEFT JOIN rs_w w ON o.vid = w.vid AND o.rb_ord < w.rb_ord
         ASOF LEFT JOIN rs_a a ON o.vid = a.vid AND o.rb_ord < a.rb_ord),
  c2 AS (SELECT *, CASE WHEN phase = 'set' THEN rb_next_a ELSE rb_next_w END AS rb_next FROM c1),
  c3 AS (SELECT *, CASE WHEN rb_next < w_end THEN rb_next ELSE w_end END AS rb_end FROM c2),
  -- a window that starts inside one it could not cut starts at its end
  c4 AS (SELECT *, max(rb_end) OVER (PARTITION BY vid ORDER BY rb_ord ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
                   AS rb_prev FROM c3),
  win AS (
    SELECT * EXCLUDE (w_start, w_end, win_basis),
      CASE WHEN rb_prev > w_start THEN least(rb_prev, rb_end) ELSE w_start END AS w_start,
      rb_end AS w_end,
      CASE WHEN phase IN ('tow', 'haul', 'jig') AND rb_end < w_end THEN 'capped' ELSE win_basis END AS win_basis,
      CASE WHEN rb_end < w_end THEN 'next_event' WHEN phase = 'set' THEN 'max' END AS win_closed_by,
      CASE WHEN rb_prev > w_start THEN 'previous_event' END AS win_opened_by
    FROM c4
    UNION ALL BY NAME
    SELECT * FROM dayw)
  SELECT r.* EXCLUDE (d, haul_min, rb_start_rec, rb_ord, rb_next_w, rb_next_a, rb_next, rb_end, rb_prev, w_end,
                      win_basis, win_closed_by, win_opened_by),
    r.w_end, r.win_basis, r.win_closed_by, r.win_opened_by,
    (CASE WHEN r.t1 IS NOT NULL THEN 1 ELSE 0 END + CASE WHEN r.t2 IS NOT NULL THEN 1 ELSE 0 END
     + CASE WHEN r.t3 IS NOT NULL THEN 1 ELSE 0 END + CASE WHEN r.t4 IS NOT NULL THEN 1 ELSE 0 END)::TINYINT AS n_events,
    concat_ws('+', CASE WHEN r.t1 IS NOT NULL THEN 't1' END, CASE WHEN r.t2 IS NOT NULL THEN 't2' END,
              CASE WHEN r.t3 IS NOT NULL THEN 't3' END, CASE WHEN r.t4 IS NOT NULL THEN 't4' END) AS events
  FROM win r"
  )
  .rb_out(con, sql, lazy_in, by = c("vid", "w_start"))
}

#' Link pings to every logbook record window that holds them
#'
#' A ping matches every window of its vessel that contains its time; a window is half-open, so a ping at
#' `w_end` is not in it. Windows of a `fallback` phase (the whole-day windows) claim only pings that no other
#' window claims. A record matched in two of its own phases counts once, the phase earliest in `phase_rank`
#' winning. The result is a table of candidates: windows that overlap (two records on one whole day, or windows
#' built by other rules than [rb_build_record_windows()]) give one row per (ping, record).
#'
#' @param pings A data frame or lazy DuckDB table with `vid`, `time` and the speed column.
#' @param windows Windows from [rb_build_record_windows()]: `vid`, `w_start`, `w_end`, `phase`, the `key`
#'   columns and the `carry` columns.
#' @param key The columns that identify a record.
#' @param carry Window columns to carry onto each match.
#' @param fallback Phases that claim a ping only when no other window does.
#' @param phase_rank Phases in order of preference when one record matches a ping in several of them.
#' @param not_fishing Phases that are never fishing.
#' @param speed,speed_min,speed_max The ping speed and the window's speed range columns.
#' @param window_s Half the speed window for `fishing`, in seconds, as in [rb_assign_fishing()]: the median
#'   speed over the pings given within `window_s` either side ([rb_calc_speed_smooth()]); 0 (the default) tests
#'   each ping's own speed.
#' @return One row per (ping, record): `vid`, `time`, the key, the carried columns, `phase`, `phase_frac`
#'   (how far into the window, NA for a fallback window), `w_start`, `w_end` and `fishing` (the ping's speed
#'   is in the range of a phase that can be fishing; NA when the speed or range is missing).
#' @export
rb_link_record_pings <- function(
  pings,
  windows,
  key = c(".sid", "schema"),
  carry = character(0),
  fallback = "day",
  phase_rank = c("haul", "tow", "jig", "set", "day"),
  not_fishing = "set",
  speed = "speed",
  speed_min = "speed_min",
  speed_max = "speed_max",
  window_s = 0
) {
  .rb_check_input(
    pings,
    "rb_link_record_pings",
    also = c(speed = speed),
    types = c(time = "time")
  )
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p_sql <- .rb_in_sql(
    dplyr::select(pings, dplyr::all_of(c("vid", "time", speed))),
    con
  )
  w_sql <- .rb_in_sql(windows, con)
  q <- function(x) .rb_q(con, x)
  # the speed tested: the ping's own, or its windowed median over all the pings given (before the windows)
  if (window_s > 0) {
    p_sql <- sprintf(
      "SELECT vid, time, %s AS %s FROM (%s)",
      .rb_speed_smooth_sql(q(speed), window_s),
      q(speed),
      p_sql
    )
  }
  lst <- function(x) paste0("'", x, "'", collapse = ", ")
  wcols <- unique(c(key, carry, speed_min, speed_max))
  wsel <- paste0("w.", q(wcols), collapse = ", ")
  rank <- paste(
    sprintf("WHEN '%s' THEN %d", phase_rank, seq_along(phase_rank)),
    collapse = " "
  )
  part <- paste(q(c("vid", "time", key)), collapse = ", ")
  out_cols <- paste(q(unique(c(key, carry))), collapse = ", ")
  sql <- .rb_fill(
    "
  WITH w AS (SELECT *, CASE phase {rank} ELSE {length(phase_rank) + 1} END AS prank FROM ({w_sql})),
  m0 AS (
    SELECT p.vid, p.time, p.{q(speed)} AS rb_speed, {wsel}, w.phase, w.w_start, w.w_end, w.prank,
           (w.phase NOT IN ({lst(fallback)})) AS tier1
    FROM ({p_sql}) p JOIN w ON p.vid = w.vid AND p.time >= w.w_start AND p.time < w.w_end),
  m1 AS (SELECT * FROM m0 WHERE tier1
         UNION ALL
         SELECT * FROM m0 WHERE NOT tier1
           AND NOT EXISTS (SELECT 1 FROM m0 t WHERE t.tier1 AND t.vid = m0.vid AND t.time = m0.time)),
  m AS (SELECT * EXCLUDE (rk) FROM (
          SELECT *, row_number() OVER (PARTITION BY {part} ORDER BY prank) AS rk FROM m1) WHERE rk = 1)
  SELECT vid, time, {out_cols}, phase,
         CASE WHEN phase NOT IN ({lst(fallback)}) THEN epoch(time - w_start) / nullif(epoch(w_end - w_start), 0) END AS phase_frac,
         {paste(q(setdiff(c(speed_min, speed_max), c(key, carry))), collapse = ', ')}{if (length(setdiff(c(speed_min, speed_max), c(key, carry)))) ',' else ''}
         w_start, w_end,
         (phase NOT IN ({lst(not_fishing)}) AND rb_speed BETWEEN {q(speed_min)} AND {q(speed_max)}) AS fishing
  FROM m"
  )
  .rb_out(con, sql, lazy_in, by = c("vid", "time"))
}

#' Link each recorded event time to the ping that bounds its phase
#'
#' A logbook record's event times bound its phases: `t1` to `t2` is the setting, `t3` to `t4` the hauling, and for a
#' towed gear, which records only `t2` and `t3`, the tow lies between them. A start takes the vessel's next ping (the
#' first at or after it) and an end the previous one (the last at or before it), so the pings from a start's ping to
#' its end's ping are the phase; the pings strictly between a `t2` and a `t3` are a tow. When a phase holds no ping,
#' the start's ping comes after the end's. Each row also carries the gap between the pings on either side of the
#' event: how precisely the track can place it. A missing event time gives no row. The first building block of
#' reconciling declared times with the track.
#'
#' A ping bounds the events of at most one record, and of one phase of it (the `i`-th start and the `i`-th end
#' bound one phase, so the start and end of a phase holding one ping share it). When events of two records, or
#' of two phases of one, would share a ping, the one nearer to it in time keeps it; on a tie the earlier event
#' time, then the earlier phase, then the lower key. The others get no ping (`ping_lost`), and their `gap_s` shows that the track cannot
#' resolve them. Exclusivity holds within one call: split a large job by vessel, or keep the events near a split
#' in both parts and each event's row from one of them.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`.
#' @param records A data frame or lazy DuckDB table with the `key` columns, `vid` and the `starts` and `ends`
#'   columns.
#' @param key The columns that identify a record.
#' @param starts,ends The event-time columns that open a phase (they take the next ping) and close one (the
#'   previous ping); each a timestamp or `NA`. `starts[i]` and `ends[i]` bound one phase.
#' @param max_dt_s The farthest the ping may be from the event, in seconds; farther, and `time` and `dt_s` are `NA`.
#' @return One row per (record, event time present): the key, `vid`, `event` (the column name), `event_time`,
#'   `time` (the ping's time), `dt_s` (`time - event_time`, in seconds: at least 0 for a start, at most 0 for an
#'   end), `gap_s` (the time between the last ping at or before the event and the first at or after it; 0 when
#'   a ping falls on it, `NA` when the track does not reach both sides) and `ping_lost` (`TRUE` when the bounding
#'   ping, within `max_dt_s`, went to a nearer event of another record or phase; `time` and `dt_s` are then `NA`).
#' @export
rb_link_record_events <- function(
  pings,
  records,
  key = c(".sid", "schema"),
  starts = c("t1", "t3"),
  ends = c("t2", "t4"),
  max_dt_s = Inf
) {
  .rb_check_input(pings, "rb_link_record_events", types = c(time = "time"))
  if (length(intersect(starts, ends))) {
    stop(
      "an event is both a start and an end: ",
      paste(intersect(starts, ends), collapse = ", "),
      call. = FALSE
    )
  }
  events <- c(starts, ends)
  if (!length(events)) {
    stop("no event columns: give `starts`, `ends` or both", call. = FALSE)
  }
  miss <- setdiff(c(key, "vid", events), colnames(records))
  if (length(miss)) {
    stop("records lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  miss <- setdiff(c("vid", "time"), colnames(pings))
  if (length(miss)) {
    stop("pings lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  stopifnot(is.numeric(max_dt_s), length(max_dt_s) == 1, max_dt_s >= 0)
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p_sql <- .rb_in_sql(
    dplyr::select(pings, dplyr::all_of(c("vid", "time"))),
    con
  )
  r_sql <- .rb_in_sql(
    dplyr::select(records, dplyr::all_of(unique(c(key, "vid", events)))),
    con
  )
  q <- function(x) .rb_q(con, x)
  kcols <- paste(q(key), collapse = ", ")
  ke <- paste0("e.", q(key), collapse = ", ")
  k_eq <- paste(sprintf("k.%s = e.%s", q(key), q(key)), collapse = " AND ")
  ev <- paste(q(events), collapse = ", ")
  ph_case <- paste(
    vapply(
      seq_len(max(length(starts), length(ends))),
      function(i) {
        e_i <- c(starts[i], ends[i])
        e_i <- e_i[!is.na(e_i)]
        sprintf(
          "WHEN event IN (%s) THEN %d",
          paste0("'", e_i, "'", collapse = ", "),
          i
        )
      },
      character(1)
    ),
    collapse = " "
  )
  is_start <- if (length(starts)) {
    sprintf("event IN (%s)", paste0("'", starts, "'", collapse = ", "))
  } else {
    "FALSE"
  }
  # No cast: a cast between a zoned and a plain timestamp follows the session's time zone. The event columns
  # and `time` must share a type, and UNPIVOT says so if they do not.
  within <- if (is.finite(max_dt_s)) {
    sprintf(
      "abs(epoch(nt - event_time)) <= %s",
      format(max_dt_s, scientific = FALSE)
    )
  } else {
    "TRUE"
  }
  sql <- .rb_fill(
    "
  WITH r0 AS (SELECT {kcols}, vid, {ev} FROM ({r_sql})),
  r AS (SELECT * FROM (UNPIVOT r0 ON {ev} INTO NAME event VALUE event_time)),
  p AS (SELECT DISTINCT vid, time FROM ({p_sql}) WHERE time IS NOT NULL),
  b AS (SELECT r.*, p.time AS t_before FROM r ASOF LEFT JOIN p ON r.vid = p.vid AND r.event_time >= p.time),
  f AS (SELECT b.*, p.time AS t_after FROM b ASOF LEFT JOIN p ON b.vid = p.vid AND b.event_time <= p.time),
  n AS (SELECT *, CASE WHEN {is_start} THEN t_after ELSE t_before END AS nt FROM f),
  e AS (SELECT *, CASE WHEN {within} THEN nt END AS rb_fix, CASE {ph_case} END AS rb_phase FROM n),
  -- one claim per (record, phase) on a ping; the nearest claim keeps it
  c AS (SELECT {kcols}, vid, rb_phase, rb_fix, min(abs(epoch(rb_fix - event_time))) AS rb_adt, min(event_time) AS rb_et
        FROM e WHERE rb_fix IS NOT NULL GROUP BY ALL),
  k AS (SELECT * FROM c QUALIFY row_number() OVER (PARTITION BY vid, rb_fix ORDER BY rb_adt, rb_et, rb_phase, {kcols}) = 1)
  SELECT {ke}, e.vid, e.event, e.event_time,
         CASE WHEN k.vid IS NOT NULL THEN e.rb_fix END AS time,
         CASE WHEN k.vid IS NOT NULL THEN epoch(e.rb_fix - e.event_time) END AS dt_s,
         epoch(e.t_after - e.t_before) AS gap_s,
         (e.rb_fix IS NOT NULL AND k.vid IS NULL) AS ping_lost
  FROM e LEFT JOIN k ON {k_eq} AND k.vid = e.vid AND k.rb_phase = e.rb_phase AND k.rb_fix = e.rb_fix"
  )
  .rb_out(con, sql, lazy_in, by = c("vid", "event_time", "event"))
}

#' Assign each ping its logbook record
#'
#' Step 2 of the flow. Each ping gets `n_records`, the number of records whose windows hold it (0 for none),
#' and, where exactly one does, that record's key, the carried columns, its `phase` and `phase_frac`. Where
#' several do, those are NA: the ambiguity is not resolved here, and the matches keep all of them.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`; all its columns are kept.
#' @param matches Matches from [rb_link_record_pings()].
#' @param key,carry The record key and the columns to carry, as in the matches.
#' @return `pings` with `n_records`, the key, the carried columns, `phase` and `phase_frac`.
#' @export
rb_assign_record <- function(
  pings,
  matches,
  key = c(".sid", "schema"),
  carry = character(0)
) {
  .rb_check_input(pings, "rb_assign_record", types = c(time = "time"))
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p_sql <- .rb_in_sql(pings, con)
  m_sql <- .rb_in_sql(matches, con)
  q <- function(x) .rb_q(con, x)
  cols <- unique(c(key, carry, "phase", "phase_frac"))
  agg <- paste(
    sprintf("any_value(%s) AS %s", q(cols), q(cols)),
    collapse = ", "
  )
  one <- paste(
    sprintf("CASE WHEN a.n_records = 1 THEN a.%s END AS %s", q(cols), q(cols)),
    collapse = ",\n "
  )
  sql <- .rb_fill(
    "
  WITH a AS (SELECT vid, time, count(*) AS n_records, {agg} FROM ({m_sql}) GROUP BY vid, time)
  SELECT p.*, coalesce(a.n_records, 0) AS n_records,
         {one}
  FROM ({p_sql}) p LEFT JOIN a ON a.vid = p.vid AND a.time = p.time"
  )
  .rb_out(con, sql, lazy_in, by = c("vid", "time"))
}

#' Decide whether each ping is fishing
#'
#' Step 4 of the flow, at any data resolution. Two methods, used alone or as a cascade:
#'
#' * `"window"`: a ping is fishing when a record's window that holds it ([rb_link_record_pings()]) is of a
#'   phase that can be fishing and its speed is inside that window's speed range. It is NA when such a window
#'   holds it but the ping has no speed (unknown, not FALSE), and FALSE for a ping in no window, or only in
#'   windows that cannot be fishing (a set, or a gear with no speed range).
#' * `"speed"`: a ping is fishing when its speed is inside the speed range on the ping itself
#'   (`ping_speed_min`, `ping_speed_max`: the gear's range, e.g. from the voyage's logbook trip or its landing).
#'   No record is needed. NA where the ping has no range or no speed.
#'
#' Given both, `method` is a cascade, tried in order for each ping: `c("window", "speed")` takes the window
#' where a record's window holds the ping, and the speed range elsewhere, so the track decides where the record
#' is missing (fishycode plan 013, item 3). A ping that neither method can decide is NA. With `"speed"` in
#' `method`, `<name>_basis` says which method decided each ping: `"window"`, `"speed"` or `"none"`.
#'
#' Each approach can be kept as a column of its own (`name`), so the effort and catch of each can be summed
#' and compared ([rb_assign_effort()], [rb_share_catch()]).
#'
#' With `window_s`, the speed tested is the median over `window_s` seconds either side of the ping
#' ([rb_calc_speed_smooth()]), not its own. ±10 minutes (`window_s = 600`) called 92.7 % of the Icelandic
#' fixes in recorded tows fishing, against 89.2 % on their own speed (fishycode decision 090). The window sees
#' only the pings given: a table cut by month needs pings from either side of the month to be right at its
#' edges. A ping with no speed stays NA.
#'
#' @param pings A data frame or lazy DuckDB table with `vid`, `time` and the speed column; all its columns
#'   are kept.
#' @param matches Matches from [rb_link_record_pings()] (with the speed range columns); needed for `"window"`.
#' @param method `"window"` (the default), `"speed"`, or both in the order to try them.
#' @param not_fishing,speed,speed_min,speed_max As in [rb_link_record_pings()]: the phases that are never
#'   fishing, the pings' speed column, and the matches' speed range columns.
#' @param window_s Half the speed window, in seconds; 0 (the default) tests each ping's own speed.
#' @param ping_speed_min,ping_speed_max The speed range columns on the pings, for `"speed"`.
#' @param name The name of the column added.
#' @return `pings` with `name` (and `<name>_basis` when `"speed"` is used).
#' @export
rb_assign_fishing <- function(
  pings,
  matches = NULL,
  method = "window",
  not_fishing = "set",
  speed = "speed",
  speed_min = "speed_min",
  speed_max = "speed_max",
  window_s = 0,
  ping_speed_min = speed_min,
  ping_speed_max = speed_max,
  name = "fishing"
) {
  method <- unique(match.arg(method, c("window", "speed"), several.ok = TRUE))
  by_speed <- "speed" %in% method
  proto <- .rb_check_input(
    pings,
    "rb_assign_fishing",
    cols = c(speed = speed),
    also = if (by_speed) {
      c(ping_speed_min = ping_speed_min, ping_speed_max = ping_speed_max)
    },
    types = c(time = "time", speed = "numeric")
  )
  if (name %in% names(proto)) {
    rlang::abort(c(
      sprintf("`pings` already has a column `%s`.", name),
      i = "Give this approach another `name`, or drop the column."
    ))
  }
  if ("window" %in% method && is.null(matches)) {
    rlang::abort(c(
      "`matches` must be given for method \"window\".",
      i = "Run first: matches <- rb_link_record_pings(pings, windows)"
    ))
  }
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p_sql <- .rb_in_sql(pings, con)
  q <- function(x) .rb_q(con, x)
  sp <- q(speed)
  # the speed tested: the ping's own, or its windowed median over the pings given
  ps_sql <- sprintf(
    "SELECT *, %s AS rb_sp FROM (%s)",
    .rb_speed_smooth_sql(sp, window_s),
    p_sql
  )
  win <- "window" %in% method
  a_sql <- if (win) {
    m_sql <- .rb_in_sql(matches, con)
    lst <- paste0("'", not_fishing, "'", collapse = ", ")
    lo <- q(speed_min)
    hi <- q(speed_max)
    .rb_fill(
      "
    WITH m AS (SELECT m.vid, m.time, m.phase, m.{lo} AS lo, m.{hi} AS hi, p.rb_sp AS sp
               FROM ({m_sql}) m JOIN ({ps_sql}) p ON p.vid = m.vid AND p.time = m.time)
    SELECT vid, time,
      CASE WHEN bool_or(CASE WHEN phase IN ({lst}) OR lo IS NULL THEN FALSE WHEN sp IS NULL THEN NULL
                             ELSE sp BETWEEN lo AND hi END) THEN TRUE
           WHEN count(*) FILTER (WHERE NOT (phase IN ({lst}) OR lo IS NULL) AND sp IS NULL) > 0 THEN NULL
           ELSE FALSE END AS fishing
    FROM m GROUP BY vid, time"
    )
  } else {
    "SELECT CAST(NULL AS INTEGER) AS vid, CAST(NULL AS TIMESTAMP) AS time, CAST(NULL AS BOOLEAN) AS fishing WHERE FALSE"
  }
  in_win <- "a.vid IS NOT NULL"
  has_rng <- if (by_speed) {
    sprintf(
      "(p.%s IS NOT NULL AND p.%s IS NOT NULL)",
      q(ping_speed_min),
      q(ping_speed_max)
    )
  } else {
    "FALSE"
  }
  by_spd <- if (by_speed) {
    sprintf(
      "CASE WHEN p.rb_sp IS NULL THEN NULL ELSE p.rb_sp BETWEEN p.%s AND p.%s END",
      q(ping_speed_min),
      q(ping_speed_max)
    )
  } else {
    "NULL"
  }
  if (identical(method, "window")) {
    val <- sprintf("CASE WHEN %s THEN a.fishing ELSE FALSE END", in_win)
    basis <- NULL
  } else if (identical(method, "speed")) {
    val <- sprintf("CASE WHEN %s THEN %s END", has_rng, by_spd)
    basis <- sprintf("CASE WHEN %s THEN 'speed' ELSE 'none' END", has_rng)
  } else if (identical(method, c("window", "speed"))) {
    val <- sprintf(
      "CASE WHEN %s THEN a.fishing WHEN %s THEN %s END",
      in_win,
      has_rng,
      by_spd
    )
    basis <- sprintf(
      "CASE WHEN %s THEN 'window' WHEN %s THEN 'speed' ELSE 'none' END",
      in_win,
      has_rng
    )
  } else {
    val <- sprintf(
      "CASE WHEN %s THEN %s WHEN %s THEN a.fishing END",
      has_rng,
      by_spd,
      in_win
    )
    basis <- sprintf(
      "CASE WHEN %s THEN 'speed' WHEN %s THEN 'window' ELSE 'none' END",
      has_rng,
      in_win
    )
  }
  sql <- sprintf(
    "SELECT p.* EXCLUDE (rb_sp), %s AS %s%s
     FROM (%s) p LEFT JOIN (%s) a ON a.vid = p.vid AND a.time = p.time",
    val,
    q(name),
    if (is.null(basis)) {
      ""
    } else {
      sprintf(", %s AS %s", basis, q(paste0(name, "_basis")))
    },
    ps_sql,
    a_sql
  )
  .rb_out(con, sql, lazy_in, by = c("vid", "time"))
}

#' Build positions for records that have no pings
#'
#' A step-2 builder for the records no track covers: the years before vessel tracking, or a vessel the
#' tracking does not reach. Each record gets positions every `interval_s` seconds over its window
#' `[start, end)`, moved in a straight line from its start position (`lon1`, `lat1`) to its end position
#' (`lon2`, `lat2`), or kept at the start where there is no end position. A record whose window has no length
#' gets one position, at its start. `pos_basis` is `"synthesised"`, and `speed` (knots) is the straight-line
#' distance over the window's length, where there is an end position.
#'
#' The positions are pings like any other: [rb_assign_effort()] gives them their hours (`by` the key), and
#' [rb_share_catch()] the record's catch. They stand for the record's own window, so whether they are fishing is
#' the record's to say (e.g. `fishing = TRUE` for a haul's window), not the track's.
#'
#' @param records A data frame or lazy DuckDB table, one row per record (or per window): the `key` columns,
#'   `vid`, `start` and `end` (date-times) and the positions. Records with no `start` are left out.
#' @param key The columns that identify a record; they are kept on its positions.
#' @param interval_s Seconds between positions.
#' @param vid,start,end,lon1,lat1,lon2,lat2 The columns.
#' @return One row per position: the key, `vid`, `time`, `lon`, `lat`, `speed` and `pos_basis`. A data frame
#'   for data frame input, a lazy table for lazy input.
#' @export
#' @examples
#' r <- tibble::tibble(
#'   id = 1, vid = 7L,
#'   start = as.POSIXct("1998-05-02 06:00", tz = "UTC"), end = as.POSIXct("1998-05-02 07:00", tz = "UTC"),
#'   lon1 = -22, lat1 = 64, lon2 = -22.2, lat2 = 64.05
#' )
#' rb_build_record_pings(r, key = "id", interval_s = 900)
rb_build_record_pings <- function(
  records,
  key,
  interval_s = 600,
  vid = "vid",
  start = "start",
  end = "end",
  lon1 = "lon1",
  lat1 = "lat1",
  lon2 = "lon2",
  lat2 = "lat2"
) {
  stopifnot(is.numeric(interval_s), length(interval_s) == 1, interval_s > 0)
  .rb_check_input(
    records,
    "rb_build_record_pings",
    cols = c(vid = vid, start = start, end = end, lon1 = lon1, lat1 = lat1),
    also = c(stats::setNames(key, key)),
    types = c(start = "time", end = "time", lon1 = "numeric", lat1 = "numeric")
  )
  have <- if (inherits(records, "tbl_lazy")) {
    colnames(records)
  } else {
    names(records)
  }
  lazy_in <- inherits(records, "tbl_lazy")
  con <- .rb_con_for(records)
  q <- function(x) .rb_q(con, x)
  r_sql <- .rb_in_sql(records, con)
  has_end <- all(c(lon2, lat2) %in% have)
  l2 <- if (has_end) sprintf("r.%s", q(lon2)) else "NULL"
  a2 <- if (has_end) sprintf("r.%s", q(lat2)) else "NULL"
  kcols <- paste(sprintf("r.%s", q(key)), collapse = ", ")
  step <- sprintf(
    "to_microseconds(%s)",
    format(round(interval_s * 1e6), scientific = FALSE)
  )
  sql <- .rb_fill(
    "
  WITH r AS (
    SELECT {kcols}, r.{q(vid)} AS rb_vid, r.{q(start)} AS rb_s, r.{q(end)} AS rb_e,
           r.{q(lon1)} AS rb_x1, r.{q(lat1)} AS rb_y1, {l2} AS rb_x2, {a2} AS rb_y2,
           CASE WHEN r.{q(end)} > r.{q(start)} THEN epoch(r.{q(end)} - r.{q(start)}) END AS rb_len
    FROM ({r_sql}) r WHERE r.{q(start)} IS NOT NULL),
  t AS (
    SELECT r.*, unnest(CASE WHEN rb_len IS NULL THEN [rb_s] ELSE range(rb_s, rb_e, {step}) END) AS rb_t FROM r),
  f AS (SELECT t.*, CASE WHEN rb_len IS NULL OR rb_x2 IS NULL THEN 0 ELSE epoch(rb_t - rb_s) / rb_len END AS rb_f FROM t)
  SELECT {paste(q(key), collapse = ', ')}, rb_vid AS {q(vid)}, rb_t AS time,
         CASE WHEN rb_x2 IS NULL THEN rb_x1 ELSE rb_x1 + rb_f * (rb_x2 - rb_x1) END AS lon,
         CASE WHEN rb_y2 IS NULL THEN rb_y1 ELSE rb_y1 + rb_f * (rb_y2 - rb_y1) END AS lat,
         CASE WHEN rb_len IS NULL OR rb_x2 IS NULL OR rb_y2 IS NULL THEN NULL
              ELSE {.rb_haversine_sql('rb_x1', 'rb_y1', 'rb_x2', 'rb_y2')} / rb_len * 3600 / 1852 END AS speed,
         'synthesised' AS pos_basis
  FROM f"
  )
  .rb_out(con, sql, lazy_in, by = c(vid, "time"))
}

# The haversine distance (metres) as DuckDB SQL, on the sphere of rb_calc_distance(method = "haversine"): one more
# hand copy of that formula (see ramb's AGENTS.md, "One distance sphere"), pinned to it by a test. The argument of
# asin() is clamped with CASE, not LEAST(), which would turn a NULL into a distance.
.rb_haversine_sql <- function(lon1, lat1, lon2, lat2) {
  a <- sprintf(
    "(power(sin(radians(%s - %s) / 2), 2) + cos(radians(%s)) * cos(radians(%s)) * power(sin(radians(%s - %s) / 2), 2))",
    lat2,
    lat1,
    lat1,
    lat2,
    lon2,
    lon1
  )
  sprintf(
    "(2 * 6371000 * asin(sqrt(CASE WHEN %s > 1 THEN 1 ELSE %s END)))",
    a,
    a
  )
}

#' Find the set of each static-gear haul on the track
#'
#' A step-2 builder for static gear (nets, pots, longlines), where the logbook often records only the haul. The
#' haul's pings trace the gear's line; the set is the vessel's latest earlier pass along that line. Recoded from
#' the method of Ifremer's iapesca (`Retrieve_SettingOperations`; fishycode decision 078), not copied:
#'
#' * candidate pings are the vessel's pings within `max_back_h` hours before the haul starts, outside every haul
#'   window given, and near the haul's line: within a buffer of the nearest haul ping;
#' * the buffer is the smallest of `buffer_m[1]`, `buffer_m[1] + 10`, ... up to `buffer_m[2]` metres that finds
#'   any candidate (iapesca: 330 to 550 m);
#' * a pass is a run of candidates that follow each other in the vessel's track; each pass is scored by how late
#'   it is among the haul's passes (at least 0.25) times its number of pings (at most 4, scaled), and the best is
#'   the set, the later one on a tie.
#'
#' The distance is to the nearest haul ping, not to a line geometry, so dense pings give the closest result.
#'
#' @param hauls A data frame or lazy DuckDB table, one row per haul: the `key` columns, `vid`, and the haul's
#'   `start` and `end`.
#' @param pings A data frame or lazy table with `vid`, `time`, `lon`, `lat`.
#' @param key The columns that identify a haul.
#' @param max_back_h How long before the haul a set may be, hours (iapesca: 240).
#' @param buffer_m The smallest and largest buffer, metres.
#' @param start,end The haul's window columns.
#' @return One row per haul: the key, `vid`, `set_start` and `set_end` (the pass's first and last ping), `soak_h`
#'   (the haul's start minus `set_end`), `n_set_pings`, `buffer_m` and `set_basis` (`"reconstructed"`, or `"none"`
#'   where no pass was found). A data frame for data frame input, a lazy table for lazy input.
#' @export
rb_link_record_sets <- function(
  hauls,
  pings,
  key,
  max_back_h = 240,
  buffer_m = c(330, 550),
  start = "start",
  end = "end"
) {
  .rb_check_input(
    hauls,
    "rb_link_record_sets",
    cols = c(start = start, end = end),
    also = c(stats::setNames(key, key)),
    types = c(start = "time", end = "time")
  )
  .rb_need_cols(pings, "pings", c("vid", "time", "lon", "lat"))
  lazy_in <- inherits(hauls, "tbl_lazy")
  con <- .rb_con_for(hauls)
  q <- function(x) .rb_q(con, x)
  h_sql <- .rb_in_sql(hauls, con)
  p_sql <- .rb_in_sql(pings, con)
  ks <- paste(sprintf("h.%s", q(key)), collapse = ", ")
  kh <- paste(q(key), collapse = ", ")
  on_k <- function(a, b) {
    paste(sprintf("%s.%s = %s.%s", a, q(key), b, q(key)), collapse = " AND ")
  }
  dist <- .rb_haversine_sql("c.lon", "c.lat", "hp.lon", "hp.lat")
  b_lo <- buffer_m[1]
  b_hi <- buffer_m[2]
  sql <- .rb_fill(
    "
  WITH h AS (SELECT {ks}, h.vid, h.{q(start)} AS hs, h.{q(end)} AS he FROM ({h_sql}) h),
  p AS (SELECT vid, time, lon, lat, row_number() OVER (PARTITION BY vid ORDER BY time, lon, lat) AS rn FROM ({p_sql})),
  hp AS (SELECT {kh}, h.vid, p.lon, p.lat FROM h JOIN p ON p.vid = h.vid AND p.time >= h.hs AND p.time < h.he),
  inhaul AS (SELECT DISTINCT p.vid, p.rn FROM p JOIN h ON p.vid = h.vid AND p.time >= h.hs AND p.time < h.he),
  c AS (SELECT {kh}, h.hs, p.vid, p.time, p.lon, p.lat, p.rn
        FROM h JOIN p ON p.vid = h.vid AND p.time < h.hs AND p.time >= h.hs - INTERVAL '{max_back_h}' HOUR
        WHERE NOT EXISTS (SELECT 1 FROM inhaul i WHERE i.vid = p.vid AND i.rn = p.rn)),
  cd AS (SELECT c.*, min({dist}) AS d
         FROM c JOIN hp ON {on_k('hp', 'c')} AND hp.vid = c.vid
           AND abs(hp.lat - c.lat) <= {b_hi} / 111000.0
           AND abs(hp.lon - c.lon) <= {b_hi} / (111000.0 * cos(radians(c.lat)))
         GROUP BY ALL),
  b AS (SELECT {kh}, greatest({b_lo}, ceil(min(d) / 10) * 10) AS buf FROM cd GROUP BY ALL HAVING min(d) <= {b_hi}),
  k AS (SELECT cd.*, b.buf FROM cd JOIN b ON {on_k('b', 'cd')} WHERE cd.d <= b.buf),
  s AS (SELECT *, rn - row_number() OVER (PARTITION BY {kh} ORDER BY rn) AS seg FROM k),
  g AS (SELECT {kh}, seg, any_value(buf) AS buf, min(time) AS s0, max(time) AS s1, count(*) AS n FROM s GROUP BY ALL),
  sc AS (SELECT g.*,
           greatest(0.25, row_number() OVER (PARTITION BY {kh} ORDER BY s0) / count(*) OVER (PARTITION BY {kh})) *
           (least(n, 4) / max(least(n, 4)) OVER (PARTITION BY {kh})) AS score
         FROM g),
  best AS (SELECT * FROM sc QUALIFY row_number() OVER (PARTITION BY {kh} ORDER BY score DESC, s0 DESC) = 1)
  SELECT {ks}, h.vid, best.s0 AS set_start, best.s1 AS set_end,
         epoch(h.hs - best.s1) / 3600.0 AS soak_h, best.n AS n_set_pings, best.buf AS buffer_m,
         CASE WHEN best.s0 IS NULL THEN 'none' ELSE 'reconstructed' END AS set_basis
  FROM h LEFT JOIN best ON {on_k('best', 'h')}"
  )
  .rb_out(con, sql, lazy_in, by = c("vid", key))
}
