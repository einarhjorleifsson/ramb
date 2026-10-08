# cut_trip_voyages.R — step 1: voyages from harbour stays, and the trip each ping belongs to.
#
# Ported from fishycode's curate/ais_trip.R (decision 061; ramb plan 013 phase 2). Tier 2: one DuckDB query
# each; a data frame is registered and the result collected, a lazy table stays lazy.

# A ping takes the stay it falls in; inside two overlapping stays, the earlier one.
.rb_ping_stay_sql <- function(p_sql, s_sql, by_year) {
  sprintf("
WITH p AS (SELECT *, row_number() OVER () AS rb_rid FROM (%s)),
s AS (SELECT vid, harbour_id, T_in, T_out FROM (%s)),
ps AS (SELECT p.*, s.harbour_id AS stay_harbour_id FROM p LEFT JOIN s ON s.vid = p.vid AND p.time BETWEEN s.T_in AND s.T_out
       QUALIFY row_number() OVER (PARTITION BY p.rb_rid ORDER BY s.T_in NULLS LAST, s.harbour_id) = 1)
SELECT *, %s AS rb_year FROM ps", p_sql, s_sql, if (by_year) "year(time)" else "0")
}

#' Cut vessel tracks into voyages between harbour stays
#'
#' A step-1 builder of the fishing-activity flow. Three methods, one result: a table of voyages.
#'
#' * `"stays"`: a voyage is the sea between two stays (see [rb_find_trip_stays()]). A ping takes the stay it
#'   falls in (the earlier of two that overlap), the track is cut into runs of stay and sea, and each sea run
#'   becomes a voyage, from the harbour of the stay before it to the harbour of the stay after it.
#' * `"runs"`: as `"stays"`, but every run of pings tagged in a harbour ([rb_flag_ping_harbour()]) is a stay,
#'   however short. This is what `rb_trip()` numbered.
#' * `"jepol"`: the ICES VMS datacall rule (`define_trips_pol`, was `rb_trip_jepol()`): a voyage runs from the
#'   first ping at sea to the first ping back in harbour; voyages of `min_dur_h` hours or less are dropped, and
#'   with `split` one over `max_dur_h` hours is cut at a long gap between its pings. It reproduces the old
#'   function exactly, including where it cuts (one ping before the longest gap); a long voyage with fewer than
#'   two pings inside it, where the old function stopped, is left whole.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`, and `harbour_id` for `"runs"` and
#'   `"jepol"` (NA at sea).
#' @param stays For `"stays"`: stays as returned by [rb_find_trip_stays()]: `vid`, `harbour_id`, `T_in`, `T_out`.
#' @param method `"stays"`, `"runs"` or `"jepol"`.
#' @param min_pings Voyages with fewer pings are dropped. They are numbered before they are dropped, so the
#'   numbers have gaps where a short voyage was. Default 10 for `"stays"`, 1 (none dropped) otherwise.
#' @param by_year Cut voyages at New Year and number them per vessel and year.
#' @param min_dur_h,max_dur_h,split For `"jepol"`: the minimum and maximum voyage length in hours, and whether
#'   longer voyages are split.
#' @param batch_pings For `"jepol"` on a lazy table: about this many pings are collected at a time, whole vessels.
#' @param vid,time,harbour_id The ping columns, if named otherwise.
#'
#' @return One row per voyage: `vid`, `year` (with `by_year`), `voyage_id`, `T1`, `T2`, `harbour_from`,
#'   `harbour_to`, `n_pings`. A data frame for data frame input, a lazy table for lazy input.
#' @family trip
#' @export
rb_cut_trip_voyages <- function(pings, stays = NULL, method = c("stays", "runs", "jepol"), min_pings = NULL,
                                by_year = TRUE, min_dur_h = 0.5, max_dur_h = 72, split = TRUE, batch_pings = 5e7,
                                vid = vid, time = time, harbour_id = harbour_id) {
  method <- match.arg(method)
  if (is.null(min_pings)) min_pings <- if (method == "stays") 10 else 1
  if (method == "stays" && is.null(stays)) stop("Method \"stays\" needs `stays`.", call. = FALSE)
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }}, harbour_id = {{ harbour_id }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p <- .rb_as_lazy(.rb_std_in(pings, if (method == "stays") map[c("vid", "time")] else map), con)
  if (method == "jepol") {
    return(.rb_cut_jepol(p, con, lazy_in, map, min_pings, by_year, min_dur_h, max_dur_h, split, batch_pings))
  }
  ps <- if (method == "stays") {
    .rb_ping_stay_sql(.rb_sql(dplyr::select(p, vid, time)), .rb_sql(.rb_as_lazy(stays, con)), by_year)
  } else {
    sprintf("SELECT *, harbour_id AS stay_harbour_id, row_number() OVER () AS rb_rid, %s AS rb_year FROM (%s)",
            if (by_year) "year(time)" else "0", .rb_sql(dplyr::select(p, vid, time, harbour_id)))
  }
  sql <- sprintf("
WITH ps AS (%s),
f AS (SELECT *, stay_harbour_id IS NOT NULL AS in_h,
             lag(stay_harbour_id IS NOT NULL) OVER w AS p_in, lag(stay_harbour_id) OVER w AS p_h
      FROM ps WINDOW w AS (PARTITION BY vid, rb_year ORDER BY time, rb_rid)),
r AS (SELECT *, sum(CASE WHEN p_in IS NULL OR in_h <> p_in OR (in_h AND stay_harbour_id <> p_h) THEN 1 ELSE 0 END)
                  OVER (PARTITION BY vid, rb_year ORDER BY time, rb_rid) AS run_id FROM f),
runs AS (SELECT vid, rb_year, run_id, bool_or(in_h) AS in_h, min(stay_harbour_id) AS h,
                min(time) AS T1, max(time) AS T2, count(*) AS n_pings
         FROM r GROUP BY vid, rb_year, run_id),
t AS (SELECT *, CASE WHEN NOT in_h THEN lag(h) OVER w END AS harbour_from,
                CASE WHEN NOT in_h THEN lead(h) OVER w END AS harbour_to
      FROM runs WINDOW w AS (PARTITION BY vid, rb_year ORDER BY T1)),
v AS (SELECT *, row_number() OVER (PARTITION BY vid, rb_year ORDER BY T1) AS voyage_id FROM t WHERE NOT in_h)
SELECT vid, %s voyage_id, T1, T2, harbour_from, harbour_to, n_pings FROM v WHERE n_pings >= %d",
    ps, if (by_year) "rb_year AS year," else "", as.integer(min_pings))
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(out, !!!stats::setNames(rlang::syms("vid"), map[["vid"]]))
  if (lazy_in) return(out)
  out <- dplyr::collect(out)
  .rb_unregister(con)
  out[order(out[[map[["vid"]]]], out$T1), ]
}

#' Assign each ping its trip
#'
#' Step 1 of the fishing-activity flow. Each ping gets the voyage it lies in (`voyage_id`, from
#' [rb_cut_trip_voyages()]) and, with `stays`, the harbour stay it lies in (`stay_harbour_id`). `trip_id` is
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
#' @return `pings` with `stay_harbour_id` (with `stays`), `voyage_id`, `trip_id`, `trip_basis`: the same
#'   type, the same rows.
#' @family trip
#' @export
rb_assign_trip <- function(pings, voyages, stays = NULL, by_year = TRUE, vid = vid, time = time) {
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p <- .rb_as_lazy(.rb_std_in(pings, map), con)
  v <- .rb_as_lazy(voyages, con)
  base <- if (is.null(stays)) {
    sprintf("SELECT *, NULL::VARCHAR AS stay_harbour_id, row_number() OVER () AS rb_rid FROM (%s)", .rb_sql(p))
  } else {
    sprintf("SELECT * EXCLUDE (rb_year) FROM (%s)", .rb_ping_stay_sql(.rb_sql(p), .rb_sql(.rb_as_lazy(stays, con)), by_year))
  }
  yr <- if (by_year) "AND v.year = year(b.time)" else ""
  drop <- if (is.null(stays)) "rb_rid, stay_harbour_id" else "rb_rid"
  sql <- sprintf("
WITH b AS (%s), v AS (%s),
j AS (SELECT b.*, v.voyage_id FROM b LEFT JOIN v ON v.vid = b.vid %s AND b.time BETWEEN v.T1 AND v.T2
      QUALIFY row_number() OVER (PARTITION BY b.rb_rid ORDER BY v.T1) = 1)
SELECT * EXCLUDE (%s), voyage_id AS trip_id,
       CASE WHEN voyage_id IS NULL THEN 'none' ELSE 'reconstructed' END AS trip_basis
FROM j", base, .rb_sql(v), yr, drop)
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- .rb_std_out(out, map)
  if (lazy_in) return(out)
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
#'   `harbour_from`, `harbour_to`, `n_pings`, `overlap_s`, `trip_span_s`, `overlap_frac`, `n_candidates`, `basis`
#'   (`"ais_overlap"`). A data frame for data frame input, a lazy table for lazy input.
#' @family trip
#' @export
rb_link_trip_voyages <- function(trips, voyages, keys, t2_fix = c("end_of_t1_day", "none"), vid = vid) {
  t2_fix <- match.arg(t2_fix)
  map <- .rb_cols(vid = {{ vid }})
  lazy_in <- inherits(trips, "tbl_lazy")
  con <- .rb_con_for(trips)
  tr <- .rb_as_lazy(.rb_std_in(trips, map), con)
  v <- .rb_as_lazy(voyages, con)
  kq <- paste(sprintf('"%s"', keys), collapse = ", ")
  kl <- paste(sprintf('lb."%s"', keys), collapse = ", ")
  fix <- if (t2_fix == "end_of_t1_day") "greatest(T2, date_trunc('day', T1) + INTERVAL 1 DAY - INTERVAL 1 SECOND)" else "T2"
  sql <- sprintf("
WITH lb AS (SELECT %s, vid, T1, T2, %s AS T2_fix FROM (%s) WHERE vid IS NOT NULL AND T1 IS NOT NULL),
v AS (SELECT vid, year, voyage_id, T1, T2, harbour_from, harbour_to, n_pings FROM (%s)),
ov AS (SELECT %s, lb.vid, v.year, v.voyage_id, v.T1 AS T1_voyage, v.T2 AS T2_voyage, v.harbour_from, v.harbour_to, v.n_pings,
              date_diff('second', greatest(lb.T1, v.T1), least(lb.T2_fix, v.T2)) AS overlap_s,
              date_diff('second', lb.T1, lb.T2_fix) AS trip_span_s
       FROM lb JOIN v ON lb.vid = v.vid AND v.T1 <= lb.T2_fix AND v.T2 >= lb.T1),
ranked AS (SELECT *, row_number() OVER (PARTITION BY %s ORDER BY overlap_s DESC, n_pings DESC, T1_voyage, voyage_id) AS rn,
                  count(*) OVER (PARTITION BY %s) AS n_candidates
           FROM ov WHERE overlap_s > 0)
SELECT %s, vid, year, voyage_id, T1_voyage, T2_voyage, harbour_from, harbour_to, n_pings, overlap_s, trip_span_s,
       round(overlap_s::DOUBLE / nullif(trip_span_s, 0), 4) AS overlap_frac, n_candidates, 'ais_overlap' AS basis
FROM ranked WHERE rn = 1",
    kq, fix, .rb_sql(tr), .rb_sql(v), kl, kq, kq, kq)
  out <- dplyr::tbl(con, dplyr::sql(sql))
  out <- dplyr::rename(out, !!!stats::setNames(rlang::syms("vid"), map[["vid"]]))
  if (lazy_in) return(out)
  out <- as.data.frame(dplyr::collect(out))
  .rb_unregister(con)
  out
}

# The "jepol" method: whole vessels collected in batches, the rule applied in R (R/trip_jepol.R), one voyage table.
.rb_cut_jepol <- function(p, con, lazy_in, map, min_pings, by_year, min_dur_h, max_dur_h, split, batch_pings) {
  vn <- p |> dplyr::count(vid) |> dplyr::collect()
  vn <- vn[order(-vn$n, vn$vid), ]
  batches <- if (nrow(vn)) split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings) else list()
  out <- lapply(batches, function(g) {
    d <- p |> dplyr::filter(vid %in% g) |> dplyr::select(vid, time, harbour_id) |> dplyr::collect()
    d <- d[order(d$vid, d$time, d$harbour_id, na.last = TRUE), ]
    yr <- as.numeric(format(d$time, "%Y", tz = "UTC"))
    k <- if (by_year) paste(d$vid, yr) else as.character(d$vid)
    tr <- .rb_jepol(k, d$time, as.integer(!is.na(d$harbour_id)), min_dur_h, max_dur_h, split)$trip
    f <- which(!is.na(tr))
    if (!length(f)) return(NULL)
    id <- unique(tr[f])
    first <- f[match(id, tr[f])]
    last <- f[length(f) + 1 - match(id, rev(tr[f]))]
    prev <- pmax(first - 1, 1)
    v <- data.frame(vid = d$vid[first], year = yr[first], k = k[first], T1 = d$time[first], T2 = d$time[last],
                    harbour_from = ifelse(first > 1 & k[prev] == k[first], d$harbour_id[prev], NA),
                    harbour_to = d$harbour_id[last], n_pings = as.numeric(tabulate(match(tr[f], id), length(id))))
    v <- v[order(v$k, v$T1), ]
    v$voyage_id <- as.numeric(stats::ave(seq_len(nrow(v)), v$k, FUN = seq_along))
    v
  })
  out <- do.call(rbind, out)
  if (is.null(out)) out <- data.frame(vid = vector(class(vn$vid)[1], 0), year = numeric(0), k = character(0),
                                      T1 = as.POSIXct(character(0), tz = "UTC"), T2 = as.POSIXct(character(0), tz = "UTC"),
                                      harbour_from = character(0), harbour_to = character(0), n_pings = numeric(0),
                                      voyage_id = numeric(0))
  out <- out[out$n_pings >= min_pings, c("vid", if (by_year) "year", "voyage_id", "T1", "T2", "harbour_from", "harbour_to", "n_pings")]
  out <- out[order(out$vid, out$T1), ]
  rownames(out) <- NULL
  names(out)[1] <- map[["vid"]]
  if (!lazy_in) { .rb_unregister(con); return(out) }
  dplyr::tbl(con, .rb_register(con, out, "rb_voy_"))
}
