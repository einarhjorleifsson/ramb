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
#' A step-1 builder of the fishing-activity flow. A voyage is the sea between two stays (see
#' [rb_find_trip_stays()]): a ping takes the stay it falls in (the earlier of two that overlap), the track is
#' cut into runs of stay and sea, and each sea run becomes a voyage, from the harbour of the stay before it to
#' the harbour of the stay after it.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`.
#' @param stays Stays as returned by [rb_find_trip_stays()]: `vid`, `harbour_id`, `T_in`, `T_out`.
#' @param method Only `"stays"` so far. (`"runs"` and `"jepol"`, cutting at runs of in-harbour pings, follow.)
#' @param min_pings Voyages with fewer pings are dropped. They are numbered before they are dropped, so the
#'   numbers have gaps where a short voyage was.
#' @param by_year Cut voyages at New Year and number them per vessel and year.
#' @param vid,time The ping columns, if named otherwise.
#'
#' @return One row per voyage: `vid`, `year` (with `by_year`), `voyage_id`, `T1`, `T2`, `harbour_from`,
#'   `harbour_to`, `n_pings`. A data frame for data frame input, a lazy table for lazy input.
#' @family trip
#' @export
rb_cut_trip_voyages <- function(pings, stays, method = "stays", min_pings = 10, by_year = TRUE,
                                vid = vid, time = time) {
  method <- match.arg(method)
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p <- .rb_as_lazy(.rb_std_in(pings, map), con) |> dplyr::select(vid, time)
  s <- .rb_as_lazy(stays, con)
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
    .rb_ping_stay_sql(.rb_sql(p), .rb_sql(s), by_year), if (by_year) "rb_year AS year," else "", as.integer(min_pings))
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
