# vessel.R — the vessel stage: which ids of two position feeds are one vessel, by co-location.
#
# Generalised from fishycode's R/audit_witnesses.R (audit_w1_month(), the W1 witness of decision 047, ids by vid)
# and the plan 022 scratch run that labelled STK's units by MMSI; both ran the same blocked join by hand. Here it
# runs on the proximity engine (near.R), on ramb's sphere, with no lat/lon box and with block widths derived from the
# tolerance at each latitude instead of at 65 deg N.

#' Link the vessel ids of two tracks by co-location
#'
#' Two position feeds that name vessels differently (a tracking unit and an MMSI, an MMSI and a registry number)
#' are linked where their pings move together. For each id of `x`, each id of `y` and each `period`:
#'
#' * `n_x`: the id_x's moving pings in the period;
#' * `hits`: those with a moving ping of the id_y within `dt_s` seconds and `dist_m` metres;
#' * `opportunity`: those with any moving ping of the id_y within `dt_s`, wherever it is;
#' * `score = hits / opportunity`. Dividing by the opportunity, not by `n_x`, keeps a sparse feed from looking like
#'   a failed match (fishycode decision 047: 96-100 % against 3 %).
#'
#' A ping is moving at `speed >= min_speed` knots, in both feeds: vessels lying side by side in port are not
#' evidence. The period is that of the x ping. The ids of `y` are ranked for each id of `x` and period, by score and
#' then hits; the best other id and its score are kept on every row (the runner-up for the leader, the leader for
#' the rest), so a vessel that moves with another (pair trawlers, a tender, a net buoy beside its carrier) shows its
#' partner instead of hiding it.
#'
#' The defaults are Iceland's calibration (fishycode decision 047: 5 min, 2 km, 1 knot; a random other vessel
#' scores a hit 2 in 100,000 times), and `strong` is plan 022's rule for a month. Filter both feeds to the area of
#' interest first. Linking a feed to itself pairs every id with itself; drop `id_x == id_y` from the result if that
#' is not wanted.
#'
#' Distances are on [rb_calc_distance()]'s sphere (haversine), and the bounds are inclusive.
#'
#' @param x,y Data frames or lazy DuckDB tables with an id, `time`, `lon`, `lat` and, unless `min_speed` is `NULL`,
#'   `speed` (knots). Lazy tables must share a connection.
#' @param id_x,id_y The id columns.
#' @param dt_s,dist_m How close in time (seconds) and space (metres) a y ping must be for a hit.
#' @param min_speed The speed, knots, from which a ping counts as moving; `NULL` counts every ping.
#' @param period `"day"`, `"week"`, `"month"`, `"quarter"` or `"year"`, or `NULL` for the whole span.
#' @param min_hits The hits a pair needs to be scored.
#' @param pairs Optional pairs to score whatever their hits, e.g. an existing key to be checked: a data frame
#'   with the result's two id columns and, optionally, `period`; without it, a pair is scored in every period its
#'   id_x has pings.
#' @param min_opportunity,min_score When a pair is `strong`.
#' @return One row per (id_x, id_y, period) scored: the two ids (named as in `x` and `y`, with `_x` and `_y` added
#'   when the names are the same), `period` (the period's first day; `NA` for `period = NULL`), `n_x`, `hits`,
#'   `opportunity`, `score`, `median_m` (the median distance of the hits), `rank`, `<id_y>_other`, `score_other`
#'   and `strong`. A data frame for data frame `x`, a lazy table for lazy `x`.
#' @export
rb_link_vessel_tracks <- function(
  x,
  y,
  id_x = "vid",
  id_y = "vid",
  dt_s = 300,
  dist_m = 2000,
  min_speed = 1,
  period = "month",
  min_hits = 3,
  pairs = NULL,
  min_opportunity = 20,
  min_score = 0.8
) {
  stopifnot(
    is.character(id_x),
    length(id_x) == 1,
    is.character(id_y),
    length(id_y) == 1,
    is.numeric(dt_s),
    length(dt_s) == 1,
    dt_s >= 0,
    is.numeric(dist_m),
    length(dist_m) == 1,
    dist_m > 0,
    is.null(min_speed) || (is.numeric(min_speed) && length(min_speed) == 1),
    is.numeric(min_hits),
    length(min_hits) == 1
  )
  periods <- c("day", "week", "month", "quarter", "year")
  if (
    !is.null(period) &&
      !(is.character(period) && length(period) == 1 && period %in% periods)
  ) {
    stop(
      "`period` must be one of \"day\", \"week\", \"month\", \"quarter\", \"year\", or NULL.",
      call. = FALSE
    )
  }
  sp <- if (is.null(min_speed)) character(0) else c(speed = "speed")
  .rb_check_input(
    x,
    "rb_link_vessel_tracks",
    also = c(stats::setNames(id_x, id_x), sp),
    types = c(time = "time")
  )
  .rb_need_cols(y, "y", c(id_y, "time", "lon", "lat", sp))
  out_x <- if (id_x == id_y) paste0(id_x, "_x") else id_x
  out_y <- if (id_x == id_y) paste0(id_y, "_y") else id_y
  if (!is.null(pairs)) {
    .rb_need_cols(pairs, "pairs", c(out_x, out_y))
  }
  lazy_in <- inherits(x, "tbl_lazy")
  con <- .rb_con_for(x, y, pairs)
  q <- function(v) .rb_q(con, v)
  x_sql <- .rb_in_sql(x, con)
  y_sql <- .rb_in_sql(y, con)
  moving <- if (is.null(min_speed)) {
    ""
  } else {
    sprintf(" AND speed >= %s", min_speed)
  }
  per <- if (is.null(period)) {
    "CAST(NULL AS DATE)"
  } else {
    sprintf("CAST(date_trunc('%s', time) AS DATE)", period)
  }
  cand_pairs <- if (is.null(pairs)) {
    ""
  } else {
    pp <- .rb_in_sql(pairs, con)
    has_per <- "period" %in% colnames(pairs)
    if (has_per) {
      sprintf(
        "\n    UNION SELECT %s, %s, CAST(period AS DATE) FROM (%s)",
        q(out_x),
        q(out_y),
        pp
      )
    } else {
      sprintf(
        "\n    UNION SELECT pp.%s, pp.%s, xp.rb_per FROM (%s) pp JOIN (SELECT DISTINCT rb_idx, rb_per FROM xs) xp ON xp.rb_idx = pp.%s",
        q(out_x),
        q(out_y),
        pp,
        q(out_x)
      )
    }
  }
  near <- .rb_near_pairs("xs", "ys", character(0), dist_m, dt_s, con)
  sql <- .rb_fill(
    "
  WITH xs AS MATERIALIZED (
    SELECT row_number() OVER () AS rb_i, {q(id_x)} AS rb_idx, time, lon, lat, {per} AS rb_per
    FROM ({x_sql}) WHERE {q(id_x)} IS NOT NULL AND time IS NOT NULL{moving}),
  ys AS MATERIALIZED (
    SELECT row_number() OVER () AS rb_i, {q(id_y)} AS rb_idy, time, lon, lat
    FROM ({y_sql}) WHERE {q(id_y)} IS NOT NULL AND time IS NOT NULL{moving}),
  pr AS ({near}),
  hit AS (SELECT xs.rb_idx, ys.rb_idy, xs.rb_per, xs.rb_i, min(pr.d_m) AS d
          FROM pr JOIN xs ON xs.rb_i = pr.rb_ia JOIN ys ON ys.rb_i = pr.rb_ib GROUP BY ALL),
  hits AS (SELECT rb_idx, rb_idy, rb_per, count(*) AS hits, median(d) AS median_m FROM hit GROUP BY ALL),
  cand AS MATERIALIZED (
    SELECT rb_idx, rb_idy, rb_per FROM hits WHERE hits >= {min_hits}{cand_pairs}),
  xc AS (SELECT cand.rb_idy, xs.rb_idx, xs.rb_per, xs.time
         FROM cand JOIN xs ON xs.rb_idx = cand.rb_idx AND xs.rb_per IS NOT DISTINCT FROM cand.rb_per),
  prv AS (SELECT xc.*, ys.time AS rb_tp FROM xc ASOF LEFT JOIN ys ON xc.rb_idy = ys.rb_idy AND xc.time >= ys.time),
  nxt AS (SELECT prv.*, ys.time AS rb_tn FROM prv ASOF LEFT JOIN ys ON prv.rb_idy = ys.rb_idy AND prv.time <= ys.time),
  opp AS (SELECT rb_idx, rb_idy, rb_per,
                 count(*) FILTER (WHERE epoch(time) - epoch(rb_tp) <= {dt_s} OR epoch(rb_tn) - epoch(time) <= {dt_s})
                   AS opportunity
          FROM nxt GROUP BY ALL),
  nx AS (SELECT rb_idx, rb_per, count(*) AS n_x FROM xs GROUP BY ALL),
  sc AS (
    SELECT cand.*, coalesce(nx.n_x, 0) AS n_x, coalesce(hits.hits, 0) AS hits, coalesce(opp.opportunity, 0) AS opportunity,
           coalesce(hits.hits, 0)::DOUBLE / CASE WHEN opp.opportunity > 0 THEN opp.opportunity END AS score,
           hits.median_m
    FROM cand
    LEFT JOIN hits ON hits.rb_idx = cand.rb_idx AND hits.rb_idy = cand.rb_idy AND hits.rb_per IS NOT DISTINCT FROM cand.rb_per
    LEFT JOIN opp ON opp.rb_idx = cand.rb_idx AND opp.rb_idy = cand.rb_idy AND opp.rb_per IS NOT DISTINCT FROM cand.rb_per
    LEFT JOIN nx ON nx.rb_idx = cand.rb_idx AND nx.rb_per IS NOT DISTINCT FROM cand.rb_per),
  rk AS (SELECT sc.*, row_number() OVER (PARTITION BY rb_idx, rb_per ORDER BY score DESC NULLS LAST, hits DESC, rb_idy) AS rank
         FROM sc),
  ot AS (
    SELECT rk.*,
      CASE WHEN rank = 1 THEN nth_value(rb_idy, 2) OVER w ELSE first_value(rb_idy) OVER w END AS rb_other,
      CASE WHEN rank = 1 THEN nth_value(score, 2) OVER w ELSE first_value(score) OVER w END AS score_other
    FROM rk
    WINDOW w AS (PARTITION BY rb_idx, rb_per ORDER BY rank ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING))
  SELECT rb_idx AS {q(out_x)}, rb_idy AS {q(out_y)}, rb_per AS period, n_x, hits, opportunity, score, median_m, rank,
         rb_other AS {q(paste0(out_y, '_other'))}, score_other,
         coalesce(opportunity >= {min_opportunity} AND score >= {min_score}, false) AS strong
  FROM ot"
  )
  .rb_out(con, sql, lazy_in, by = c(out_x, "period", "rank"))
}
