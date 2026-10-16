# near.R — the proximity engine: every pair of points within a distance (and a time), as DuckDB SQL.
#
# Shared by rb_link_record_sets() (a haul's line against the vessel's earlier pings, space only) and
# rb_link_vessel_tracks() (two position feeds, time and space). Both were a blocked join written by hand: the
# co-location audit of fishycode (R/audit_witnesses.R, decision 047) on 10-minute x 0.1 deg x 0.25 deg blocks with
# the lon edge taken at 65 deg N, and the set finder on a lat/lon box around each haul ping.
#
# Blocking. Lat bands of height h = 4 x the distance in degrees on ramb's sphere (R = 6,371 km). Within a band the
# lon cells are at least 4 x the band's lon tolerance wide, the tolerance being exact: from the haversine,
# sin(d / 2R) >= cos(phi_max) sin(dlon / 2), so a pair within d differs in lon by at most 2 asin(d / (2R cos phi_e)),
# phi_e the band's poleward edge plus the lat tolerance. Where that has no solution (near a pole) the band is one
# cell. The widths divide 360, so cells wrap across +-180. Time buckets are 4 x dt.
#
# Only `a` is replicated, into the neighbour band, cell or bucket whose edge it lies within the tolerance of (the
# audit's rule, with the tolerance at the band's own latitude). Every tolerance is at most a quarter of its block,
# so a row is never replicated both ways in one dimension, and `b`, which sits in one block, meets each `a` row
# at most once: no DISTINCT is needed. The caller passes the smaller table as `a`.
#
# The pairs are then filtered exactly: the haversine of .rb_haversine_sql() (the one sphere) <= dist_m, and
# |dt| <= dt_s; both bounds inclusive. A row with a NULL coordinate or time meets nothing.

.rb_near_R <- 6371000
.rb_near_deg <- function(m) m / (.rb_near_R * pi / 180)

# The number of lon cells in lat band `cy` (SQL expression), for a band height h and lat tolerance tol (degrees).
.rb_near_ncell <- function(cy, h, tol, dist_m) {
  phi <- sprintf(
    "(CASE WHEN abs(%1$s * %2$.17g - 90) > abs((%1$s + 1) * %2$.17g - 90) THEN abs(%1$s * %2$.17g - 90) ELSE abs((%1$s + 1) * %2$.17g - 90) END + %3$.17g)",
    cy,
    h,
    tol
  )
  s <- sprintf(
    "(%.17g / (2 * %.17g * cos(radians(%s))))",
    dist_m,
    .rb_near_R,
    phi
  )
  sprintf(
    "(CASE WHEN %1$s >= 90 OR %2$s >= 1 THEN 1 ELSE greatest(1, floor(360 / (4 * degrees(2 * asin(CASE WHEN %2$s >= 1 THEN 1 ELSE %2$s END)))))::BIGINT END)",
    phi,
    s
  )
}

# The lon tolerance (degrees) in band `cy`; only read where the band has more than one cell.
.rb_near_tol_lon <- function(cy, h, tol, dist_m) {
  phi <- sprintf(
    "(CASE WHEN abs(%1$s * %2$.17g - 90) > abs((%1$s + 1) * %2$.17g - 90) THEN abs(%1$s * %2$.17g - 90) ELSE abs((%1$s + 1) * %2$.17g - 90) END + %3$.17g)",
    cy,
    h,
    tol
  )
  s <- sprintf(
    "(%.17g / (2 * %.17g * cos(radians(%s))))",
    dist_m,
    .rb_near_R,
    phi
  )
  sprintf("degrees(2 * asin(CASE WHEN %1$s >= 1 THEN 1 ELSE %1$s END))", s)
}

# a, b: names of relations (CTEs) with an integer row id `rb_i`, `lon`, `lat`, the `by` columns and, when dt_s is
# given, `time`. Returns a SELECT of (rb_ia, rb_ib, d_m, dt_s) for every pair within dist_m (and dt_s) and equal on
# `by`, to be placed as a CTE or subquery.
.rb_near_pairs <- function(a, b, by = character(0), dist_m, dt_s = NULL, con) {
  stopifnot(is.numeric(dist_m), length(dist_m) == 1, dist_m > 0)
  timed <- !is.null(dt_s)
  if (timed) {
    stopifnot(is.numeric(dt_s), length(dt_s) == 1, dt_s >= 0)
  }
  q <- function(x) .rb_q(con, x)
  tol <- .rb_near_deg(dist_m) * (1 + 1e-9)
  h <- 4 * .rb_near_deg(dist_m)
  bt <- 4 * max(dt_s, 1)
  lonn <- "(((lon + 180) % 360 + 360) % 360)"
  w <- function(cy) sprintf("(360.0 / %s)", .rb_near_ncell(cy, h, tol, dist_m))
  by_a <- if (length(by)) paste0(paste(q(by), collapse = ", "), ", ") else ""
  on_by <- if (length(by)) {
    paste0(
      paste(sprintf("ar.%1$s = br.%1$s", q(by)), collapse = " AND "),
      " AND "
    )
  } else {
    ""
  }
  dims <- "(VALUES (-1), (0), (1)) AS rb_n(n)"
  t_a <- if (timed) {
    sprintf(", time, floor(epoch(time) / %s)::BIGINT AS rb_tb", bt)
  } else {
    ""
  }
  dims.t <- "(VALUES (-1), (0), (1)) AS rb_t(n)"
  sql <- .rb_fill(
    paste0(
      "
  WITH a0 AS (
    SELECT rb_i, {by_a}lon, lat{t_a}, floor((lat + 90) / {h})::BIGINT AS rb_cy, {lonn} AS rb_x
    FROM {a} WHERE lon IS NOT NULL AND lat IS NOT NULL",
      if (timed) " AND time IS NOT NULL" else "",
      "),
  a1 AS (
    SELECT a0.*, rb_cy + rb_n.n AS rb_cy2 FROM a0, {dims}
    WHERE rb_n.n = 0
       OR (rb_n.n = -1 AND rb_cy > 0 AND (lat + 90) - rb_cy * {h} <= {tol})
       OR (rb_n.n = 1 AND (rb_cy + 1) * {h} < 180 AND (rb_cy + 1) * {h} - (lat + 90) <= {tol})),
  a2 AS (
    SELECT a1.*, {.rb_near_ncell('rb_cy2', h, tol, dist_m)} AS rb_nc, {.rb_near_tol_lon('rb_cy2', h, tol, dist_m)} AS rb_tl
    FROM a1),
  a3 AS (
    SELECT a2.*, floor(rb_x / (360.0 / rb_nc))::BIGINT AS rb_cx0 FROM a2),
  ar AS (
    SELECT rb_i, {by_a}lon, lat, rb_cy2 AS rb_cy, ((rb_cx0 + rb_n.n) % rb_nc + rb_nc) % rb_nc AS rb_cx",
      if (timed) ", time, rb_tb" else "",
      "
    FROM a3, {dims}
    WHERE rb_n.n = 0
       OR (rb_nc > 1 AND rb_n.n = -1 AND rb_x - rb_cx0 * (360.0 / rb_nc) <= rb_tl)
       OR (rb_nc > 1 AND rb_n.n = 1 AND (rb_cx0 + 1) * (360.0 / rb_nc) - rb_x <= rb_tl)),
  ",
      if (timed) {
        "art AS (SELECT ar.* EXCLUDE (rb_tb), rb_tb + rb_t.n AS rb_tb FROM ar, {dims.t}
    WHERE rb_t.n = 0 OR (rb_t.n = -1 AND epoch(time) - rb_tb * {bt} <= {dt_s})
                     OR (rb_t.n = 1 AND (rb_tb + 1) * {bt} - epoch(time) <= {dt_s})),
  "
      } else {
        ""
      },
      "b0 AS (
    SELECT rb_i, {by_a}lon, lat",
      if (timed) ", time, floor(epoch(time) / {bt})::BIGINT AS rb_tb" else "",
      ", floor((lat + 90) / {h})::BIGINT AS rb_cy
    FROM {b} WHERE lon IS NOT NULL AND lat IS NOT NULL",
      if (timed) " AND time IS NOT NULL" else "",
      "),
  br AS (
    SELECT b0.*, floor({lonn} / {w('rb_cy')})::BIGINT % {.rb_near_ncell('rb_cy', h, tol, dist_m)} AS rb_cx FROM b0)
  SELECT ar.rb_i AS rb_ia, br.rb_i AS rb_ib, {.rb_haversine_sql('ar.lon', 'ar.lat', 'br.lon', 'br.lat')} AS d_m",
      if (timed) ", epoch(br.time) - epoch(ar.time) AS dt_s" else "",
      "
  FROM ",
      if (timed) "art" else "ar",
      " AS ar JOIN br ON {on_by}ar.rb_cy = br.rb_cy AND ar.rb_cx = br.rb_cx",
      if (timed) " AND ar.rb_tb = br.rb_tb" else "",
      "
  WHERE {.rb_haversine_sql('ar.lon', 'ar.lat', 'br.lon', 'br.lat')} <= {dist_m}",
      if (timed) " AND abs(epoch(br.time) - epoch(ar.time)) <= {dt_s}" else ""
    )
  )
  sql
}
