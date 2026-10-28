# record_windows.R — step 2 of the fishing-activity flow (logbook record) and the "window" method of step 4.
#
# Ported from fishycode's R/duration_limit.R, curate/station_window.R and R/trail_sql.R (plan 010, plan 011
# phase 5; ramb plan 013 phase 3, fishycode plan 014 phase 2), as exact ports. A record (a logbook station) is
# not one interval: a towed gear has a tow, a static gear a set and a haul with the soak between them not a
# window, a jigger one long jig, and a record with no usable time the whole day. Runs in DuckDB (tier 2): a
# data frame is registered and the result collected; a lazy table stays lazy.

.rb_q <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))
.rb_in_sql <- function(x, con) .rb_sql(.rb_as_lazy(x, con))
# The result of a call's query: lazy for lazy input, holding what the call registered; else collected, sorted by
# `by`, and given back as `given` came in (.rb_as_given(); in its row order where `rid` numbers its rows).
.rb_out <- function(con, sql, lazy_in, by = NULL, given = NULL, rid = NULL) {
  # wrapped, so the lazy result composes: dbplyr puts a query as it is after UNION ALL, where a bare WITH fails
  out <- dplyr::tbl(
    con,
    dplyr::sql(sprintf("SELECT * FROM (%s) AS rb_out", sql))
  )
  if (lazy_in) {
    return(.rb_hold(out))
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  # a row id puts the rows back in the input's order: the sort by `by` would be undone
  if (!is.null(by) && is.null(rid)) {
    out <- out[do.call(base::order, unname(as.list(out[by]))), ]
  }
  if (is.null(given)) {
    return(tibble::as_tibble(out))
  }
  .rb_as_given(out, given, rid)
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
    # registered, not copy_to(), which draws from R's random numbers; held for the lazy result
    .rb_scope()
    con <- dbplyr::remote_con(records)
    lim <- dplyr::tbl(con, .rb_register(con, lim, "rb_lim_"))
  }
  raw <- paste0(d, "_raw")
  out <- records |>
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
  if (inherits(records, "tbl_lazy")) .rb_hold(out) else out
}

#' Create the time windows in which a vessel is busy with each logbook record
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
rb_create_record_windows <- function(
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
  .rb_scope()
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
  .rb_out(con, sql, lazy_in, by = c("vid", "w_start"), given = records)
}

#' Link pings to every logbook record window that holds them
#'
#' A ping matches every window of its vessel that contains its time; a window is half-open, so a ping at
#' `w_end` is not in it. Windows of a `fallback` phase (the whole-day windows) claim only pings that no other
#' window claims. A record matched in two of its own phases counts once, the phase earliest in `phase_rank`
#' winning. The result is a table of candidates: windows that overlap (two records on one whole day, or windows
#' built by other rules than [rb_create_record_windows()]) give one row per (ping, record).
#'
#' @param pings A data frame or lazy DuckDB table with `vid`, `time` and the speed column.
#' @param windows Windows from [rb_create_record_windows()]: `vid`, `w_start`, `w_end`, `phase`, the `key`
#'   columns and the `carry` columns.
#' @param key The columns that identify a record.
#' @param carry Window columns to carry onto each match.
#' @param fallback Phases that claim a ping only when no other window does.
#' @param phase_rank Phases in order of preference when one record matches a ping in several of them.
#' @param not_fishing Phases that are never fishing.
#' @param speed,speed_min,speed_max The ping speed and the window's speed range columns.
#' @param window_s Half the speed window for `fishing`, in seconds, as in [rb_add_fishing()]: the median
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
  con <- .rb_con_for(pings, windows)
  .rb_scope()
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
  .rb_out(con, sql, lazy_in, by = c("vid", "time"), given = pings)
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
  con <- .rb_con_for(pings, records)
  .rb_scope()
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
  .rb_out(
    con,
    sql,
    lazy_in,
    by = c("vid", "event_time", "event"),
    given = pings
  )
}

#' Add the logbook record to each ping
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
rb_add_record <- function(
  pings,
  matches,
  key = c(".sid", "schema"),
  carry = character(0)
) {
  .rb_check_input(pings, "rb_add_record", types = c(time = "time"))
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings, matches)
  .rb_scope()
  given <- pings
  if (!lazy_in) {
    pings <- .rb_with_rid(pings)
  }
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
  .rb_out(
    con,
    sql,
    lazy_in,
    by = c("vid", "time"),
    given = given,
    rid = ".rb_rid"
  )
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
#' and compared ([rb_add_effort()], [rb_share_catch()]).
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
rb_add_fishing <- function(
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
    "rb_add_fishing",
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
  con <- .rb_con_for(pings, matches)
  .rb_scope()
  given <- pings
  if (!lazy_in) {
    pings <- .rb_with_rid(pings)
  }
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
  .rb_out(
    con,
    sql,
    lazy_in,
    by = c("vid", "time"),
    given = given,
    rid = ".rb_rid"
  )
}

#' Create positions for records that have no pings
#'
#' A step-2 builder for the records no track covers: the years before vessel tracking, or a vessel the
#' tracking does not reach. Each record gets positions every `interval_s` seconds over its window
#' `[start, end)`, moved in a straight line from its start position (`lon1`, `lat1`) to its end position
#' (`lon2`, `lat2`), or kept at the start where there is no end position. A record whose window has no length
#' gets one position, at its start. `pos_basis` is `"synthesised"`, and `speed` (knots) is the straight-line
#' distance over the window's length, where there is an end position.
#'
#' The positions are pings like any other: [rb_add_effort()] gives them their hours (`by` the key), and
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
#' rb_create_record_pings(r, key = "id", interval_s = 900)
rb_create_record_pings <- function(
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
    "rb_create_record_pings",
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
  .rb_scope()
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
  .rb_out(con, sql, lazy_in, by = c(vid, "time"), given = records)
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
#' haul traces the gear's line; the set is a pass of the vessel along that line in the hours before the haul. The
#' method of Ifremer's iapesca 1.3 (`Retrieve_SettingOperations()`, J. Rodriguez; see References), recoded in
#' DuckDB, vessel by vessel:
#'
#' * the haul's line comes from `method`: a line through the haul's pings (`"line"`, two pings or more), the haul's
#'   pings as points (`"pings"`), or the haul's declared start and end positions joined into a line (`"record"`;
#'   its start alone where it has no end), for a track too sparse to trace the haul. Several are tried in order for
#'   each haul, the first the haul has data for answering. iapesca builds its lines (nets) from labelled hauling
#'   pings with `Create_Nets()`, which ramb does not; its line's length caps a pass's length (below);
#' * a ping inside any haul's window (`start` to `end`) is a hauling ping, never part of a set; of pings at the
#'   same time in one trip only the first is kept (iapesca: the first in input order; here, the smallest `lon`,
#'   then `lat`); with `trip`, pings with no trip are left out, as iapesca's track holds trips only;
#' * one buffer serves all a vessel's hauls: `buffer_m[1]` metres, widened 10 m at a time until every haul's line
#'   has a ping within it that is not a hauling one (at any time), or `buffer_m[2]` is reached;
#' * a haul's candidates are the pings within the buffer of its line, more than `soak_h[1]` and less than
#'   `soak_h[2]` hours before the haul starts, in the haul's trip (that of its first ping, else of the vessel's
#'   last ping before it) or one of the `floor(soak_h[2] / 24) + 1` trips before it (at least 2) that started less
#'   than `soak_h[2]` hours before the haul's trip did;
#' * a pass is a run of candidates that follow each other in the vessel's track. It scores its rank in time among
#'   the haul's passes over their number (the latest scores 1), plus its length along the track (from its second
#'   ping on; a pass of one ping, the step to it), at most the line's length, over the longest pass's, plus, with
#'   `setting`, its pings labelled as setting over the most any pass has (1 for every pass where none or all of the
#'   candidates are labelled). The best is the set, the earliest on a tie;
#' * the set runs from the pass's first ping to its last, or to the end of its first trip where it spans two. Its
#'   speed is the mean of its pings' speeds in that trip, each from the previous ping of the same trip (0 for a
#'   trip's first);
#' * with `consolidate`, iapesca's second search: per vessel, the set speeds, and then the soaks of the hauls whose
#'   speed was kept, are trimmed as iapesca's `CleanSkewness()` does (from the skewed end, in steps of 1 % of
#'   the range, while the skewness exceeds 0.5 and at least 30 values and over 80 % of them remain). A haul whose
#'   soak lies outside the kept soaks' range, or whose speed was trimmed or is missing, is searched again with that
#'   range for `soak_h` (the number of trips back stays). Where the second search finds nothing, the haul keeps
#'   what the first found.
#'
#' Hauls are not paired one to one with sets: a set hauled in parts serves several hauls. Distances are on
#' [rb_calc_distance()]'s sphere, and a line is measured as points every `buffer_m[1] / 10` metres along it (under
#' 0.5 m from the distance to the line itself at 330 m); iapesca buffers the line on the plane of its input's CRS.
#'
#' @param hauls A data frame or lazy DuckDB table, one row per haul: the `key` columns, `vid`, and the haul's
#'   `start` and `end`; for `method = "record"` also its positions.
#' @param pings A data frame or lazy table with `vid`, `time`, `lon`, `lat`, and the `trip` and `setting` columns
#'   when they are named.
#' @param key The columns that identify a haul.
#' @param method Where the haul's line comes from: `"line"`, `"pings"` or `"record"`, or several, tried in order.
#' @param soak_h The hours before a haul's start within which its set lies, both ends excluded (iapesca's
#'   `tol.soaktime`, 1 to 240).
#' @param buffer_m The smallest and largest buffer, metres (iapesca's `buffer.min` and `buffer.max`, 330 and 550).
#' @param consolidate Whether hauls with an outlying soak or set speed are searched again (iapesca's
#'   `Consolidate`).
#' @param trip A ping column with the vessel's trip (a voyage of [rb_create_voyages()]), or `NULL`: a vessel's
#'   pings are then one trip.
#' @param setting A logical ping column, `TRUE` where the ping is known to be setting gear, or `NULL`.
#' @param start,end The haul's window columns.
#' @param lon1,lat1,lon2,lat2 The haul's declared start and end positions, for `method = "record"`.
#' @return One row per haul: the key, `vid`, `set_start` and `set_end`, `soak_h` (the haul's end minus
#'   `set_start`, as iapesca measures the soak), `set_speed` (knots), `n_set_pings`, `buffer_m` (the vessel's;
#'   `NA` where the haul has no line), `set_basis` (`"reconstructed"`, or `"none"` where no pass was found),
#'   `set_method` (the `method` that gave the haul's line; `NA` where none could) and `set_consolidated` (`TRUE`
#'   where the second search gave the set); with `trip` also `set_trip`, the trip the set is in. A data frame for
#'   data frame input, a lazy table for lazy input.
#' @references Rodriguez, J. iapesca: interpretation of fishing paths from vessels' geo-spatial data, R package
#'   (Ifremer), <https://gitlab.ifremer.fr/iapesca/r-packages_iapesca> (version 1.3, CC BY 4.0), whose sets the
#'   function gives for the nets it is handed (`data-raw/iapesca_sets.R`). Until 2026.10.28 ramb followed version
#'   0.1 (2022, GPL; <https://github.com/JulRodrigu/r-packages_iapesca>, since deprecated) with changes of its own:
#'   a window of 0 to 240 hours, a buffer per haul, a score of lateness (at least 0.25) times the pass's pings (at
#'   most 4), and the later pass on a tie (fishycode decision 232).
#' @export
rb_link_record_sets <- function(
  hauls,
  pings,
  key,
  method = "line",
  soak_h = c(1, 240),
  buffer_m = c(330, 550),
  consolidate = TRUE,
  trip = NULL,
  setting = NULL,
  start = "start",
  end = "end",
  lon1 = "lon1",
  lat1 = "lat1",
  lon2 = "lon2",
  lat2 = "lat2"
) {
  methods <- c("pings", "line", "record")
  if (
    !is.character(method) ||
      length(method) == 0 ||
      anyDuplicated(method) ||
      !all(method %in% methods)
  ) {
    stop(
      "`method` must be one or more of \"line\", \"pings\", \"record\", without repeats.",
      call. = FALSE
    )
  }
  stopifnot(
    is.numeric(soak_h),
    length(soak_h) == 2,
    !anyNA(soak_h),
    soak_h[1] >= 0,
    soak_h[2] > soak_h[1],
    is.numeric(buffer_m),
    length(buffer_m) == 2,
    !anyNA(buffer_m),
    buffer_m[1] > 0,
    buffer_m[2] >= buffer_m[1],
    is.logical(consolidate),
    length(consolidate) == 1,
    !is.na(consolidate),
    is.null(trip) || (is.character(trip) && length(trip) == 1),
    is.null(setting) || (is.character(setting) && length(setting) == 1)
  )
  rec <- "record" %in% method
  pos <- if (rec) {
    c(lon1 = lon1, lat1 = lat1, lon2 = lon2, lat2 = lat2)
  } else {
    character(0)
  }
  .rb_check_input(
    hauls,
    "rb_link_record_sets",
    cols = c(start = start, end = end),
    also = c(stats::setNames(key, key), pos),
    types = c(start = "time", end = "time")
  )
  .rb_need_cols(pings, "pings", c("vid", "time", "lon", "lat", trip, setting))
  lazy_in <- inherits(hauls, "tbl_lazy")
  con <- .rb_con_for(hauls, pings)
  .rb_scope()
  q <- function(x) .rb_q(con, x)
  h_sql <- .rb_in_sql(hauls, con)
  p_sql <- .rb_in_sql(pings, con)
  kh <- paste(q(key), collapse = ", ")
  kx <- function(a) paste(sprintf("%s.%s", a, q(key)), collapse = ", ")
  on_k <- function(a, b) {
    paste(sprintf("%s.%s = %s.%s", a, q(key), b, q(key)), collapse = " AND ")
  }
  hav <- .rb_haversine_sql
  b_lo <- buffer_m[1]
  k_max <- ceiling((buffer_m[2] - b_lo) / 10)
  cap <- b_lo + 10 * k_max
  step <- b_lo / 10
  n_back <- max(2, floor(soak_h[2] / 24) + 1)
  num <- function(x) sprintf("%.17g", x)
  trip_sql <- if (is.null(trip)) "0" else sprintf("p.%s", q(trip))
  set_sql <- if (is.null(setting)) {
    "FALSE"
  } else {
    sprintf("coalesce(CAST(p.%s AS BOOLEAN), FALSE)", q(setting))
  }
  ok <- "p.time IS NOT NULL AND p.lon IS NOT NULL AND p.lat IS NOT NULL"

  # the vessel's trips, numbered by their start (iapesca's Traj_Desc), and its pings as the set finder sees them:
  # one per time in a trip, numbered along the track, with the step from the previous ping of the trip
  tr <- .rb_temp_table(
    con,
    .rb_fill(
      "SELECT *, row_number() OVER (PARTITION BY vid ORDER BY ts, rb_trip) AS ti
       FROM (SELECT p.vid, {trip_sql} AS rb_trip, min(p.time) AS ts, max(p.time) AS te
             FROM ({p_sql}) p WHERE {ok} AND {trip_sql} IS NOT NULL GROUP BY ALL)"
    )
  )
  pp <- .rb_temp_table(
    con,
    .rb_fill(
      "SELECT vid, time, lon, lat, ti, rb_set, rn,
              CASE WHEN pt IS NULL THEN 0 ELSE {hav('plon', 'plat', 'lon', 'lat')} END AS dm,
              CASE WHEN pt IS NULL THEN 0
                   ELSE ({hav('plon', 'plat', 'lon', 'lat')} * 0.539957 / 1000) / ((epoch(time) - epoch(pt)) / 3600) END AS kn
       FROM (SELECT *, row_number() OVER (PARTITION BY vid ORDER BY ti, time, lon, lat) AS rn,
                    lag(time) OVER w AS pt, lag(lon) OVER w AS plon, lag(lat) OVER w AS plat
             FROM (SELECT p.vid, p.time, p.lon, p.lat, {set_sql} AS rb_set, t.ti,
                          row_number() OVER (PARTITION BY p.vid, t.ti, p.time ORDER BY p.lon, p.lat) AS rb_k
                   FROM ({p_sql}) p JOIN {tr} t ON t.vid = p.vid AND t.rb_trip = {trip_sql} WHERE {ok})
             WHERE rb_k = 1
             WINDOW w AS (PARTITION BY vid, ti ORDER BY time, lon, lat))"
    )
  )

  # each haul: its window, its trip, where its line comes from and the line's length
  has <- c(
    pings = "coalesce(nh.n, 0) >= 1",
    line = "coalesce(nh.n, 0) >= 2",
    record = "h.rb_x1 IS NOT NULL AND h.rb_y1 IS NOT NULL"
  )
  pick <- paste0(
    "CASE ",
    paste(sprintf("WHEN %s THEN '%s'", has[method], method), collapse = " "),
    " END"
  )
  h_pos <- if (rec) {
    sprintf(
      ", h.%s AS rb_x1, h.%s AS rb_y1, h.%s AS rb_x2, h.%s AS rb_y2",
      q(lon1),
      q(lat1),
      q(lon2),
      q(lat2)
    )
  } else {
    ""
  }
  rec_len <- if (rec) {
    sprintf(
      "WHEN 'record' THEN %s",
      hav(
        "h.rb_x1",
        "h.rb_y1",
        "CASE WHEN h.rb_x2 IS NULL OR h.rb_y2 IS NULL THEN h.rb_x1 ELSE h.rb_x2 END",
        "CASE WHEN h.rb_x2 IS NULL OR h.rb_y2 IS NULL THEN h.rb_y1 ELSE h.rb_y2 END"
      )
    )
  } else {
    ""
  }
  hm <- .rb_temp_table(
    con,
    .rb_fill(
      "WITH h AS (SELECT {kx('h')}, h.vid, h.{q(start)} AS hs, h.{q(end)} AS he{h_pos} FROM ({h_sql}) h),
       hp AS (SELECT {kx('h')}, h.vid, p.lon, p.lat, p.rn FROM h JOIN {pp} p ON p.vid = h.vid AND p.time >= h.hs AND p.time < h.he),
       nh AS (SELECT {kh}, count(*) AS n, min(rn) AS rn1,
                     sum(CASE WHEN plon IS NULL THEN 0 ELSE {hav('plon', 'plat', 'lon', 'lat')} END) AS len
              FROM (SELECT *, lag(lon) OVER w AS plon, lag(lat) OVER w AS plat FROM hp WINDOW w AS (PARTITION BY {kh} ORDER BY rn))
              GROUP BY ALL),
       hk AS (SELECT h.*, nh.n, nh.rn1, nh.len, {pick} AS set_method FROM h LEFT JOIN nh ON {on_k('nh', 'h')})
       SELECT h.* EXCLUDE (n, rn1, len), coalesce(p1.ti, pb.ti) AS hti,
              CASE h.set_method {rec_len} WHEN 'line' THEN h.len WHEN 'pings' THEN h.len END AS net_len
       FROM hk h LEFT JOIN {pp} p1 ON p1.vid = h.vid AND p1.rn = h.rn1
       ASOF LEFT JOIN {pp} pb ON pb.vid = h.vid AND h.hs > pb.time"
    )
  )

  # every ping other than a hauling one within reach of a haul's line, at any time, and its distance to it
  segs <- c(
    if ("line" %in% method) {
      .rb_fill(
        "SELECT {kh}, vid, x0, y0, x1, y1 FROM (
         SELECT {kx('hp')}, hp.vid, hp.lon AS x0, hp.lat AS y0,
                lead(hp.lon) OVER (PARTITION BY {kx('hp')} ORDER BY hp.rn) AS x1,
                lead(hp.lat) OVER (PARTITION BY {kx('hp')} ORDER BY hp.rn) AS y1
         FROM hp JOIN hm ON {on_k('hm', 'hp')} WHERE hm.set_method = 'line') WHERE x1 IS NOT NULL"
      )
    },
    if (rec) {
      .rb_fill(
        "SELECT {kh}, vid, rb_x1 AS x0, rb_y1 AS y0,
                CASE WHEN rb_x2 IS NULL OR rb_y2 IS NULL THEN rb_x1 ELSE rb_x2 END AS x1,
                CASE WHEN rb_x2 IS NULL OR rb_y2 IS NULL THEN rb_y1 ELSE rb_y2 END AS y1
         FROM hm WHERE set_method = 'record'"
      )
    }
  )
  ref <- c(
    if ("pings" %in% method) {
      .rb_fill(
        "SELECT {kx('hp')}, hp.vid, hp.lon, hp.lat
         FROM hp JOIN hm ON {on_k('hm', 'hp')} WHERE hm.set_method = 'pings'"
      )
    },
    if (length(segs)) {
      .rb_fill(
        "SELECT {kh}, vid, x0 + f * (x1 - x0) AS lon, y0 + f * (y1 - y0) AS lat FROM (
         SELECT sk.*, unnest(range(0, k + 1)) / k::DOUBLE AS f FROM (
           SELECT sg.*, greatest(1, ceil({hav('x0', 'y0', 'x1', 'y1')} / {step}))::BIGINT AS k
           FROM (SELECT * EXCLUDE (x1), x1 - 360 * round((x1 - x0) / 360) AS x1 FROM ({paste(segs, collapse = ' UNION ALL ')})) sg) sk)"
      )
    }
  )
  pairs <- .rb_near_pairs("ref", "c", "vid", cap, con = con)
  hd <- .rb_temp_table(
    con,
    .rb_fill(
      "WITH hm AS (SELECT * FROM {hm}),
       hp AS (SELECT {kx('h')}, h.vid, p.lon, p.lat, p.rn FROM hm h JOIN {pp} p ON p.vid = h.vid AND p.time >= h.hs AND p.time < h.he),
       ref AS MATERIALIZED (SELECT row_number() OVER () AS rb_i, * FROM ({paste(ref, collapse = ' UNION ALL ')})),
       hw AS (SELECT DISTINCT vid, rn FROM hp),
       c AS MATERIALIZED (SELECT p.rn AS rb_i, p.vid, p.lon, p.lat FROM {pp} p
                          WHERE NOT EXISTS (SELECT 1 FROM hw WHERE hw.vid = p.vid AND hw.rn = p.rn)),
       pr AS ({pairs})
       SELECT {kx('ref')}, ref.vid, pr.rb_ib AS rn, min(pr.d_m) AS d FROM pr JOIN ref ON ref.rb_i = pr.rb_ia GROUP BY ALL"
    )
  )
  # the vessel's buffer: widened until every line with a line has a ping within it, or at its cap
  bv <- .rb_temp_table(
    con,
    .rb_fill(
      "SELECT r.vid, CASE WHEN bool_or(d.dmin IS NULL) THEN {num(cap)}
                          ELSE {num(b_lo)} + 10 * least(greatest(ceil((max(d.dmin) - {num(b_lo)}) / 10), 0), {k_max}) END AS buf
       FROM (SELECT {kh}, vid FROM {hm} WHERE set_method IS NOT NULL) r
       LEFT JOIN (SELECT {kh}, min(d) AS dmin FROM {hd} GROUP BY ALL) d ON {on_k('d', 'r')}
       GROUP BY r.vid"
    )
  )

  # the search for each haul in `wn` (key, lo, hi: the window in hours before the haul)
  search <- function(wn) {
    .rb_fill(
      "WITH cand AS (
         SELECT {kx('hd')}, hd.vid, p.rn, p.time, p.ti, p.dm, p.kn, p.rb_set
         FROM {hd} hd JOIN {wn} w ON {on_k('w', 'hd')} JOIN {hm} h ON {on_k('h', 'hd')}
         JOIN {bv} b ON b.vid = hd.vid
         JOIN {pp} p ON p.vid = hd.vid AND p.rn = hd.rn
         JOIN {tr} tp ON tp.vid = hd.vid AND tp.ti = p.ti
         JOIN {tr} th ON th.vid = hd.vid AND th.ti = h.hti
         WHERE hd.d <= b.buf AND p.ti >= h.hti - {n_back} AND p.ti <= h.hti
           AND (epoch(th.ts) - epoch(tp.ts)) / 3600 < w.hi
           AND (epoch(h.hs) - epoch(p.time)) / 3600 > w.lo AND (epoch(h.hs) - epoch(p.time)) / 3600 < w.hi),
       sg0 AS (SELECT *, rn - row_number() OVER (PARTITION BY {kh} ORDER BY rn) AS g FROM cand),
       sg AS (SELECT *, rn = min(rn) OVER (PARTITION BY {kh}, g) AS is_first FROM sg0),
       ps AS (SELECT {kh}, vid, g, min(rn) AS r0, min(time) AS s0, max(time) AS s1, count(*) AS n,
                     CASE WHEN count(*) > 1 THEN sum(dm) FILTER (WHERE NOT is_first) ELSE sum(dm) END AS len,
                     count(*) FILTER (WHERE rb_set) AS ns, arg_min(ti, rn) AS ft, count(DISTINCT ti) AS ntr
              FROM sg GROUP BY ALL),
       sc AS (SELECT ps.*, CAST(row_number() OVER w AS DOUBLE) / count(*) OVER w AS prox, least(ps.len, h.net_len) AS lg,
                     sum(ps.ns) OVER w AS ns_all, sum(ps.n) OVER w AS n_all, max(ps.ns) OVER w AS ns_max
              FROM ps JOIN {hm} h ON {on_k('h', 'ps')}
              WINDOW w AS (PARTITION BY {kx('ps')} ORDER BY ps.r0 ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)),
       sc2 AS (SELECT *, prox + lg / CASE WHEN max(lg) OVER w > 0 THEN max(lg) OVER w ELSE 1 END
                         + CASE WHEN ns_all = 0 OR ns_all = n_all THEN 1 ELSE CAST(ns AS DOUBLE) / ns_max END AS total
               FROM sc WINDOW w AS (PARTITION BY {kh})),
       best AS (SELECT * FROM sc2 QUALIFY row_number() OVER (PARTITION BY {kh} ORDER BY total DESC, r0) = 1),
       sp AS (SELECT {kx('b')}, avg(sg.kn) AS set_speed FROM best b JOIN sg ON {on_k('sg', 'b')} AND sg.g = b.g AND sg.ti = b.ft
              GROUP BY ALL)
       SELECT {kx('b')}, b.s0 AS set_start, CASE WHEN b.ntr > 1 THEN t.te ELSE b.s1 END AS set_end, sp.set_speed,
              b.n AS n_set_pings, t.rb_trip AS set_trip
       FROM best b JOIN sp ON {on_k('sp', 'b')} JOIN {tr} t ON t.vid = b.vid AND t.ti = b.ft"
    )
  }
  first <- .rb_temp_table(
    con,
    search(.rb_fill(
      "(SELECT {kh}, {num(soak_h[1])} AS lo, {num(soak_h[2])} AS hi FROM {hm} WHERE set_method IS NOT NULL)"
    ))
  )
  second <- NULL
  if (consolidate) {
    got <- DBI::dbGetQuery(
      con,
      .rb_fill(
        "SELECT {kx('h')}, h.vid, s.set_speed, (epoch(h.he) - epoch(s.set_start)) / 3600 AS soak
         FROM {hm} h LEFT JOIN {first} s ON {on_k('s', 'h')} WHERE h.set_method IS NOT NULL"
      )
    )
    redo <- .rb_sets_to_redo(got, key)
    if (nrow(redo) > 0) {
      second <- .rb_temp_table(
        con,
        search(.rb_register(con, redo, "rb_redo_"))
      )
    }
  }
  found <- if (is.null(second)) {
    .rb_fill("SELECT *, FALSE AS rb_second FROM {first}")
  } else {
    .rb_fill(
      "SELECT *, TRUE AS rb_second FROM {second}
       UNION ALL BY NAME
       SELECT f.*, FALSE AS rb_second FROM {first} f WHERE NOT EXISTS (SELECT 1 FROM {second} s WHERE {on_k('s', 'f')})"
    )
  }
  trip_out <- if (is.null(trip)) "" else sprintf(", r.set_trip AS %s", q("set_trip"))
  sql <- .rb_fill(
    "SELECT {kx('h')}, h.vid, r.set_start, r.set_end, (epoch(h.he) - epoch(r.set_start)) / 3600.0 AS soak_h,
            r.set_speed, r.n_set_pings, CASE WHEN h.set_method IS NOT NULL THEN b.buf END AS buffer_m,
            CASE WHEN r.set_start IS NULL THEN 'none' ELSE 'reconstructed' END AS set_basis, h.set_method,
            coalesce(r.rb_second, FALSE) AS set_consolidated{trip_out}
     FROM {hm} h LEFT JOIN {bv} b ON b.vid = h.vid LEFT JOIN ({found}) r ON {on_k('r', 'h')}"
  )
  .rb_out(con, sql, lazy_in, by = c("vid", key), given = hauls)
}

# iapesca's consolidation (Retrieve_SettingOperations(Consolidate = TRUE)): per vessel, the hauls to search again
# and the window for it. `x` holds each haul with a line: the key, vid, set_speed and soak (NA where none was found).
.rb_sets_to_redo <- function(x, key) {
  out <- lapply(split(seq_len(nrow(x)), x$vid), function(i) {
    ssp <- .rb_clean_skewness(x$set_speed[i])
    skt <- .rb_clean_skewness(x$soak[i][!is.na(ssp)])
    tol <- suppressWarnings(range(skt, na.rm = TRUE))
    soak <- x$soak[i]
    i <- i[which(soak < tol[1] | soak > tol[2] | is.na(ssp))]
    if (length(i) == 0) {
      return(NULL)
    }
    data.frame(x[i, key, drop = FALSE], lo = tol[1], hi = tol[2])
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(x[0, key, drop = FALSE], lo = numeric(0), hi = numeric(0))
  }
  rownames(out) <- NULL
  out
}

# iapesca's CleanSkewness() (1.3), as published: values set to NA from the skewed end, in steps of 1 % of the
# range, while the skewness exceeds `lim` and at least 30 values (25 after the first step, counted before it) and
# over `keep` % of them remain. Its skewness is e1071's default (type 3). Where every value left is the same, the
# skewness is NaN and iapesca stops with an error; here the trimming stops.
.rb_clean_skewness <- function(vals, lim = 0.5, max_it = 30, keep = 80) {
  skew <- function(x) {
    n <- length(x)
    x <- x - mean(x)
    sqrt(n) * sum(x^3) / (sum(x^2)^(3 / 2)) * (1 - 1 / n)^(3 / 2)
  }
  s <- skew(vals[!is.na(vals)])
  if (is.na(s) || max_it <= 0) {
    return(vals)
  }
  n_data <- sum(!is.na(vals))
  lims <- range(vals[!is.na(vals)])
  if (!(abs(s) > lim && n_data >= 30)) {
    return(vals)
  }
  step <- diff(lims) / 100
  if (s > 0) lims[2] <- lims[2] - step
  if (s < 0) lims[1] <- lims[1] + step
  out <- vals < lims[1] | vals > lims[2]
  n_out <- sum(out, na.rm = TRUE)
  ok <- (n_data - n_out) / n_data * 100 > keep
  if (ok) {
    vals[out] <- NA
  }
  s <- skew(vals[!is.na(vals)])
  it <- 1
  n_new <- sum(!is.na(vals))
  while (isTRUE(abs(s) > lim & it < max_it & n_new >= 25 & ok)) {
    if (s > 0) lims[2] <- lims[2] - step
    if (s < 0) lims[1] <- lims[1] + step
    out <- vals < lims[1] | vals > lims[2]
    n_out <- n_out + sum(out, na.rm = TRUE)
    ok <- (n_data - n_out) / n_data * 100 > keep
    if (ok) {
      vals[out] <- NA
    }
    s <- skew(vals[!is.na(vals)])
    n_data <- sum(!is.na(vals))
    it <- it + 1
  }
  vals
}
