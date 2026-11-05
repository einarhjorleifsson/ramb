# whackies.R — the filters behind rb_flag_ping_impossible().
#
# Internal engines, called by the step-0 flagger (flag_ping.R); not exported.
# Each judges implausible ("whacky") vessel positions by a different rule:
#
#   .rb_whack_sda             speed-distance-angle (argosfilter's sdafilter,
#                             re-implemented in C++): "vmask" and "spike" stages
#   .rb_whack_forward         forward scan against the last kept ping (C++)
#   .rb_whack_fwdbwd          too fast both from the previous and to the next
#   .rb_whack_sequential_fast cluster-aware sequential removal (base R)
#   .whack_clean_df           the recommended recipe: sda, then forward
#
# History: the rb_whacky_* originals (speed / distance / mendo / trip variants)
# and the exported rb_whack_* wrappers were removed 2026-10-11; the public API
# is rb_flag_ping_impossible(method = ...).

# .rb_whack_forward -----------------------------------------------------------
# Forward-only speed filter (in-memory, O(n)). Compares each ping against the
# last *retained* ping rather than the immediate predecessor, so it catches
# consecutive clusters that .rb_whack_fwdbwd misses. The reference resets
# unconditionally when the time gap exceeds max_gap_h. A time step shorter
# than min_dt_s counts as min_dt_s.
#
# One option, off by default (rb_flag_ping_impossible()): noise_m, with a `.rb_feed` column (an integer
# code per ping, NA unknown): a leg between pings of different feeds no longer than noise_m metres is
# never impossible.
#
# x: data frame with vid, lon, lat, time (POSIXct). Returns x sorted by
# (vid, time) with a logical column whack2 (TRUE = flagged). Grouping on the
# input is restored.
.rb_whack_forward <- function(
  x,
  kn_max = 25,
  max_gap_h = 4,
  min_dt_s = 10,
  noise_m = 0
) {
  ms_max <- kn_max * 0.514444
  max_gap_sec <- max_gap_h * 3600

  # Compiled core (src/whack_forward.cpp) — ~1000x the R for-loop this
  # replaced (2026-09-23), same algorithm, verified byte-identical against it
  # on whacks1 and stress cases (isolated spikes, runs, a >4h gap, an injected
  # NA). `time` is passed as seconds-since-epoch, which is POSIXct's own
  # underlying numeric representation, so no unit conversion is needed here.
  .fwd <- function(lon, lat, time, feed) {
    whack_forward_cpp(
      lon,
      lat,
      as.numeric(time),
      ms_max,
      max_gap_sec,
      min_dt_s,
      feed,
      noise_m
    )
  }

  grp_vars <- dplyr::group_vars(x)
  # lon and lat break ties in time, as in .rb_whack_sda(): pings of one second at two places
  # otherwise take the input's order, and the flags change with it
  out <- x |>
    dplyr::ungroup() |>
    dplyr::arrange(vid, time, lon, lat)
  out$.rb_fc <- .rb_feed_codes(out)
  out <- out |>
    dplyr::group_by(vid) |>
    dplyr::mutate(whack2 = .fwd(lon, lat, time, .rb_fc)) |>
    dplyr::ungroup()
  out$.rb_fc <- NULL
  if (length(grp_vars) > 0) {
    out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(grp_vars)))
  }
  out
}

# The integer feed code per row for the compiled filters: -1 where the feed is unknown (NA) or where the
# frame has no `.rb_feed` column (the noise floor is then off). Fixed per call by the flagger, which writes
# `.rb_feed`; a frame passed straight to an engine has none.
.rb_feed_codes <- function(x) {
  if (!".rb_feed" %in% names(x)) {
    return(rep(-1L, nrow(x)))
  }
  k <- as.integer(factor(as.character(x$.rb_feed)))
  k[is.na(k)] <- 0L
  k - 1L
}

# .rb_whack_fwdbwd ------------------------------------------------------------
# Forward-backward speed filter: flags a ping whose implied speed exceeds
# kn_max in BOTH the incoming and the outgoing direction — isolated spikes.
# Misses consecutive clusters (neither endpoint of a bad segment passes both
# checks); use .rb_whack_sequential_fast for those.
#
# Dispatches on class: data.frame (vectorised R, Haversine) or tbl_lazy
# (pure SQL with LAG/LEAD). A time step shorter than min_dt_s counts as
# min_dt_s. Returns x with a logical column whack; first and last ping of a
# vessel are always FALSE. Row order of the output is not the input's.
.rb_whack_fwdbwd <- function(x, kn_max = 25, min_dt_s = 10) {
  ms_max <- kn_max * 0.514444

  if (inherits(x, "data.frame")) {
    grp_vars <- dplyr::group_vars(x)

    .fwdbwd <- function(lon, lat, time) {
      n <- length(lon)
      if (n < 3L) {
        return(rep(FALSE, n))
      }
      dist <- rb_calc_distance(lon[-n], lat[-n], lon[-1], lat[-1])
      dt <- pmax(as.numeric(diff(time), units = "secs"), min_dt_s)
      spd <- dist / dt
      spd_in <- c(NA_real_, spd)
      spd_out <- c(spd, NA_real_)
      (!is.na(spd_in) & !is.na(spd_out)) &
        (spd_in > ms_max) &
        (spd_out > ms_max)
    }

    # lon and lat break ties in time, in both branches (see .rb_whack_forward())
    out <- x |>
      dplyr::ungroup() |>
      dplyr::arrange(vid, time, lon, lat) |>
      dplyr::group_by(vid) |>
      dplyr::mutate(whack = .fwdbwd(lon, lat, time)) |>
      dplyr::ungroup()

    # Restore original grouping if present
    if (length(grp_vars) > 0) {
      out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(grp_vars)))
    }
    out
  } else if (inherits(x, "tbl_lazy")) {
    orig_cols <- colnames(x)
    col_sel <- paste(sprintf('"%s"', orig_cols), collapse = ", ")
    con <- dbplyr::remote_con(x)
    inner_sql <- as.character(dbplyr::sql_render(dplyr::ungroup(x)))

    # Haversine, R = 6,371,000 m: the rb_calc_distance() formula (R/track.R) is the reference
    # of record; the same sphere lives in src/whack_forward.cpp and src/whack_sda.cpp.
    # Deliberately no LEAST(1, ...) clamp: DuckDB's LEAST ignores NULLs, which would turn
    # NA-coordinate pairs into the antipodal distance; unclamped, NULL propagates.
    sql <- .rb_fill(
      "
      WITH src AS ({inner_sql}),
      w AS (
        SELECT *,
          LAG(lon)  OVER vt AS lon0,
          LAG(lat)  OVER vt AS lat0,
          LAG(time) OVER vt AS t0,
          LEAD(lon)  OVER vt AS lon2,
          LEAD(lat)  OVER vt AS lat2,
          LEAD(time) OVER vt AS t2
        FROM src
        WINDOW vt AS (PARTITION BY vid ORDER BY time, lon, lat)
      ),
      spd AS (
        SELECT *,
          2 * 6371000 * ASIN(SQRT(
            POWER(SIN(RADIANS((lat  - lat0) / 2)), 2) +
            COS(RADIANS(lat0)) * COS(RADIANS(lat)) *
            POWER(SIN(RADIANS((lon  - lon0) / 2)), 2)
          )) / GREATEST(extract(epoch from (time::TIMESTAMP - t0::TIMESTAMP)), {min_dt_s}) AS spd_in,
          2 * 6371000 * ASIN(SQRT(
            POWER(SIN(RADIANS((lat2 - lat)  / 2)), 2) +
            COS(RADIANS(lat))  * COS(RADIANS(lat2)) *
            POWER(SIN(RADIANS((lon2 - lon)  / 2)), 2)
          )) / GREATEST(extract(epoch from (t2::TIMESTAMP - time::TIMESTAMP)), {min_dt_s}) AS spd_out
        FROM w
      )
      SELECT {col_sel},
        (spd_in > {ms_max} AND spd_out > {ms_max}) AS whack
      FROM spd
    "
    )

    dplyr::tbl(con, dplyr::sql(sql))
  } else {
    stop("`x` must be a data.frame or a lazy DuckDB tbl (tbl_lazy).")
  }
}

# .rb_whack_sequential_fast ---------------------------------------------------
# Cluster-aware sequential speed filter on plain vectors (in-memory, O(n × k)
# for k bad pings). Iteratively removes the first segment exceeding kn_max —
# its latter endpoint, or the first endpoint when the bad segment is the very
# first — recomputing speeds on the survivors until none exceeds. Apply per
# vessel: group_by(vid) |> mutate(whack = .rb_whack_sequential_fast(lon, lat, time)).
#
# Returns a logical vector, TRUE = flagged. Tracks with fewer than 2 points
# are all FALSE.
.rb_whack_sequential_fast <- function(
  lon,
  lat,
  time,
  kn_max = 25,
  min_dt_s = 10
) {
  ms_max <- kn_max * 0.514444
  n <- length(lon)
  if (n < 2L) {
    return(rep(FALSE, n))
  }
  keep <- rep(TRUE, n)

  repeat {
    idx <- which(keep)
    m <- length(idx)
    if (m < 2L) {
      break
    }
    dist <- rb_calc_distance(
      lon[idx[-m]],
      lat[idx[-m]],
      lon[idx[-1]],
      lat[idx[-1]]
    )
    dt <- pmax(as.numeric(diff(time[idx]), units = "secs"), min_dt_s)
    spd <- dist / dt
    bad <- which(spd > ms_max)
    if (length(bad) == 0L) {
      break
    }
    p <- bad[1L]
    keep[if (p == 1L) idx[1L] else idx[p + 1L]] <- FALSE
  }
  !keep
}

# .rb_whack_sda ---------------------------------------------------------------
# Speed-distance-angle filter (compiled): argosfilter::sdafilter (Freitas et
# al. 2008) re-implemented as one C++ pass over all vessels. Per vessel, on
# the track sorted by time:
#   1. vmask - rms speed from each ping to its two neighbours on either side;
#      local maxima above kn_max are flagged and thinned out, repeatedly.
#      A vmask flag stands only if the ping is more than vmask_min_dist
#      metres from its predecessor (default 0: every flag stands).
#   2. spike - a ping is flagged when the two legs at it enclose an angle of
#      at most ang[k] degrees and either both legs are longer than distlim[k]
#      metres or, where speedlim_kn[k] > 0, both legs are faster.
# Every speed uses a time step of at least min_dt_s; argosfilter adds 1 s to
# every step instead. The defaults are fishycode decision 067's, from its sda_tune study (archived in
# $FISHYDATA/archive/2026-10-whack-studies/ by its plan 062).
#
# noise_m (off at 0): with a `.rb_feed` column on x, a leg between pings of different feeds no
# longer than noise_m metres counts as no distance in both stages (rb_flag_ping_impossible()).
#
# x: data frame with vid, lon, lat, time. Returns x sorted by (vid, time),
# ties by (lon, lat), with whack_sda: NA, "vmask" or "spike".
.rb_whack_sda <- function(
  x,
  kn_max = 25,
  ang = 25,
  distlim = rep(0, length(ang)),
  speedlim_kn = rep(kn_max, length(ang)),
  vmask_min_dist = 0,
  min_dt_s = 10,
  noise_m = 0
) {
  grp_vars <- dplyr::group_vars(x)
  # lon and lat break ties in time, so the result does not depend on the row order of the input
  # (parquet reads come back in a different order from run to run)
  x <- dplyr::arrange(dplyr::ungroup(x), vid, time, lon, lat)
  code <- match(x$vid, unique(x$vid))
  r <- whack_sda_cpp(
    x$lat,
    x$lon,
    as.numeric(x$time),
    code,
    kn_max * 0.514444,
    as.numeric(ang),
    as.numeric(distlim),
    as.numeric(speedlim_kn) * 0.514444,
    vmask_min_dist,
    min_dt_s,
    .rb_feed_codes(x),
    noise_m
  )
  x$whack_sda <- c(NA_character_, "vmask", "spike")[r + 1L]
  if (length(grp_vars) > 0) {
    x <- dplyr::group_by(x, dplyr::across(dplyr::all_of(grp_vars)))
  }
  x
}

# .whack_clean_df -------------------------------------------------------------
# The recommended recipe, on a data frame: .rb_whack_sda (judges each ping
# from both sides, so a bad ping is not adopted as an anchor) followed by
# .rb_whack_forward on what stage 1 left (resolves runs and interleaved
# streams). Labels, never removes.
#
# noise_m is the stages' one option (see .rb_whack_sda() and .rb_whack_forward()).
#
# Returns x sorted by (vid, time) with whack (logical) and whack_stage
# (NA, "vmask", "spike" or "forward").
.whack_clean_df <- function(
  x,
  kn_max = 25,
  max_gap_h = 4,
  min_dt_s = 10,
  noise_m = 0,
  ...
) {
  grp_vars <- dplyr::group_vars(x)
  out <- .rb_whack_sda(
    x,
    kn_max = kn_max,
    min_dt_s = min_dt_s,
    noise_m = noise_m,
    ...
  )
  s1 <- !is.na(out$whack_sda)
  fwd <- .rb_whack_forward(
    out[!s1, ],
    kn_max = kn_max,
    max_gap_h = max_gap_h,
    min_dt_s = min_dt_s,
    noise_m = noise_m
  )
  # .rb_whack_forward() re-sorts by vid, time, lon, lat; out[!s1, ] already is, so the order is the same
  stage <- out$whack_sda
  stage[!s1] <- ifelse(fwd$whack2, "forward", NA_character_)
  out$whack_sda <- NULL
  out$whack_stage <- stage
  out$whack <- !is.na(stage)
  if (length(grp_vars) > 0) {
    out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(grp_vars)))
  }
  out
}
# whack_duplicates.R — near-duplicate pings across AIS feeds.
#
# Internal engine behind rb_flag_ping_duplicate() (flag_ping.R); not exported.
# The exported rb_whack_duplicates() wrapper was removed 2026-10-11.
#
# Flags the second copy of a position report that two feeds both received:
# same vessel, a few seconds apart, metres apart (e.g. a national receiver
# network and a commercial feed). The copies share neither timestamp nor
# position exactly, so they survive a match on (vid, time). In the Icelandic
# record 2013-2026 they were 1.2 % of all pings, with a median gap of 1-4 s
# and a median distance of 0.1-0.4 m by feed pair.
#
# Rule: a ping is a near-duplicate when a ping of the same vid from a feed
# ranked higher in `priority` lies within max_dt_s seconds and within the
# distance kn_max allows in max_dt_s (128.6 m at the defaults). Only the n
# nearest pings on each side, in time order, are compared. The top-ranked
# feed is never flagged. Feeds not named in priority rank last and equal, so
# two of them never flag each other; nor do two pings of the same feed.
#
# Returns a logical vector, TRUE for the copy to drop, in input order.
.rb_whack_duplicates <- function(
  vid,
  time,
  lon,
  lat,
  feed,
  priority,
  max_dt_s = 10,
  kn_max = 25,
  n = 3
) {
  max_m <- kn_max * 1852 / 3600 * max_dt_s
  pr <- match(feed, priority)
  pr[is.na(pr)] <- length(priority) + 1L
  t <- as.numeric(time)
  o <- order(vid, t, pr, lon, lat)
  v <- vid[o]
  t <- t[o]
  x <- lon[o]
  y <- lat[o]
  p <- pr[o]
  m <- length(o)
  dup <- logical(m)
  if (m < 2L) {
    return(dup)
  }
  for (k in seq_len(min(n, m - 1L))) {
    i <- seq_len(m - k)
    j <- i + k
    near <- v[i] == v[j] &
      abs(t[j] - t[i]) <= max_dt_s &
      rb_calc_distance(x[i], y[i], x[j], y[j]) <= max_m
    near[is.na(near)] <- FALSE
    dup[j[near & p[i] < p[j]]] <- TRUE
    dup[i[near & p[j] < p[i]]] <- TRUE
  }
  dup[order(o)]
}

# .rb_whack_local -------------------------------------------------------------
# Each leg judged against the vessel's own speed (compiled, src/whack_local.cpp): rb_flag_ping_impossible(method =
# "local"). The local speed of a ping is the median of the recorded speeds over ground of the `k` pings on each side;
# a leg is impossible when longer than margin_m + (factor x local speed + margin_kn) x its time step, and where no
# ping near has a recorded speed, longer than margin_m + kn_max x its time step. Stage 1, "local_spike": both legs
# impossible and the leg that skips the ping possible (the first and last ping of a track or stretch: their one leg),
# repeated on the pings left; stage 2, "local_forward": a forward scan from the last ping kept. Hand-set (fishycode plan
# 062, decided e), from the prototype of its step 3, which caught the most faults at every ping rate on ramb's planted
# faults and kept the truly fast vessels; tuning on half the vessels changed little (fishycode decision 276).
#
# A recorded speed below 0 or of 102.3 knots or more is none: AIS writes 102.3 for "not available".
# noise_m, with `.rb_feed`, is the other engines' noise floor between feeds.
#
# x: data frame with vid, time, lon, lat and `.rb_speed` (knots, NA where none). Returns x sorted by (vid, time, lon,
# lat) with whack_local (NA, "local_spike" or "local_forward"), whack_basis ("recorded" or "track") and whack_v (the
# local speed in knots a flag was judged by, NA under the fixed limit).
.rb_whack_local <- function(
  x,
  kn_max = 25,
  min_dt_s = 10,
  max_gap_h = 4,
  noise_m = 0,
  k = 5,
  factor = 1.5,
  margin_kn = 5,
  margin_m = 50
) {
  grp_vars <- dplyr::group_vars(x)
  x <- dplyr::arrange(dplyr::ungroup(x), vid, time, lon, lat)
  sog <- if (".rb_speed" %in% names(x)) {
    as.numeric(x$.rb_speed)
  } else {
    rep(NA_real_, nrow(x))
  }
  sog[!is.na(sog) & (sog < 0 | sog >= 102.3)] <- NA_real_
  # each ping's own limits (rb_flag_ping_impossible(limits = )), the fixed ones where it has none
  own_col <- function(nm) if (nm %in% names(x)) as.numeric(x[[nm]]) else rep(NA_real_, nrow(x))
  kn <- own_col(".rb_kn")
  own <- !is.na(kn)
  kn[!own] <- kn_max * 0.514444
  m0 <- own_col(".rb_m0")
  m0[is.na(m0)] <- margin_m
  te <- own_col(".rb_te")
  te[is.na(te)] <- 0
  r <- whack_local_v_cpp(
    as.numeric(x$lon),
    as.numeric(x$lat),
    as.numeric(x$time),
    match(x$vid, unique(x$vid)),
    sog * 0.514444,
    .rb_feed_codes(x),
    kn,
    m0,
    te,
    own,
    min_dt_s,
    max_gap_h * 3600,
    as.integer(k),
    factor,
    margin_kn * 0.514444,
    noise_m
  )
  x$whack_local <- c(NA_character_, "local_spike", "local_forward")[r$code + 1L]
  x$whack_basis <- c(NA_character_, "recorded", "track", "ceiling")[r$basis + 1L]
  x$whack_v <- r$v / 0.514444
  if (length(grp_vars) > 0) {
    x <- dplyr::group_by(x, dplyr::across(dplyr::all_of(grp_vars)))
  }
  x
}

# .rb_whack_copies ------------------------------------------------------------
# One report twice (fishycode plan 062 step 4a; the prototype of its step 3, scripts/copy_rule.R, with the misfit's
# vote at 0: on one feed only a second feed decides). Internal engine behind rb_flag_ping_duplicate(speed = ).
#
# A copy repeats a report's position (within copy_m metres), its recorded speed (exactly) and its course (within
# 0.5 degrees, where both have one); a vessel that passes the same place again does not repeat its speed and course to
# the decimal. Only pings moving 1 knot or more are compared: at rest every report repeats. Of a pair of one vessel:
#   * in one feed, near_s seconds apart or less: the later is a "duplicate" (basis "recorded"), the report sent twice;
#   * in two feeds ranked apart in `priority`: the lower-ranked feed's is flagged (basis "priority"), "duplicate" when
#     within max_dt_s of the other, "shifted" (a copy stamped at another time) beyond;
#   * in one feed (or two of equal rank) further apart: which one is the copy is decided by a second feed, a witness:
#     a ping of another feed of the vessel's within 120 s that places it within 300 m plus what its recorded speed
#     covers in the time between. The copy, stamped at a time the vessel was elsewhere, has none. The pairs of one run
#     (one vessel, one shift, to 10 s) are decided together, and only when every decisive pair agrees: the copy is
#     "shifted" (basis "witness"). Otherwise both pings stay, unflagged, labelled repeats (the maintainer, 2026-11-03:
#     on one feed with no time of receipt, which of two identical reports is the copy is not in the data).
# The track around is no witness: on fishycode's reload of 2026-10-12 to 10-14, the fit to the neighbours picked the
# live ping as the copy (decision 276).
#
# The pairs are found in DuckDB on a grid of 200 m cells in which each ping also looks into the cells within 25 m of
# it, so a batch of millions of pings never compares a vessel's pings at one speed all against all.
#
# Returns, in input order: flag (NA, "duplicate", "shifted"), basis (NA, "recorded", "priority", "witness") and
# repeat_dt_s (for an undecided pair's pings: the seconds to the other ping, NA otherwise). A ping already flagged by
# one pair keeps that flag.
.rb_whack_copies <- function(
  vid,
  time,
  lon,
  lat,
  feed,
  priority,
  speed,
  course = NULL,
  copy_m = 20,
  near_s = 2,
  max_dt_s = 10,
  min_kn = 1,
  course_tol = 0.5,
  witness_s = 120,
  witness_m = 300
) {
  n <- length(vid)
  flag <- basis <- rep(NA_character_, n)
  rdt <- rep(NA_real_, n)
  if (n < 2L) {
    return(list(flag = flag, basis = basis, repeat_dt_s = rdt))
  }
  t <- as.numeric(time)
  o <- order(vid, t, lon, lat)
  y <- data.frame(
    i = seq_len(n),
    vid = match(vid[o], unique(vid[o])),
    t = t[o],
    lon = as.numeric(lon[o]),
    lat = as.numeric(lat[o]),
    feed = as.character(feed[o]),
    rnk = match(as.character(feed[o]), priority),
    speed = as.numeric(speed[o]),
    course = if (is.null(course)) NA_real_ else as.numeric(course[o])
  )
  y$rnk[is.na(y$rnk)] <- length(priority) + 1L
  con <- .rb_connection()
  .rb_scope()
  nm <- .rb_register(con, y, "rb_copy_")
  hav <- function(a, b) {
    .rb_fill(
      "2 * 6371000 * asin(sqrt(pow(sin(radians({b}.lat - {a}.lat) / 2), 2) + cos(radians({a}.lat)) * cos(radians({b}.lat)) * pow(sin(radians({b}.lon - {a}.lon) / 2), 2)))"
    )
  }
  # each ping's 200 m cell: a band of latitude, and in it a cell of longitude as wide as 200 m at the band's middle
  lonm <- function(band) {
    .rb_fill(
      "lon * 111320.0 * cos(radians(least(89.9, greatest(-89.9, ({band} + 0.5) * 200 / 111320.0))))"
    )
  }
  pr <- DBI::dbGetQuery(
    con,
    .rb_fill(
      "
WITH y AS (SELECT *, lat * 111320.0 AS ym FROM {nm}
           WHERE speed >= {min_kn} AND speed < 102.3 AND lon IS NOT NULL AND lat IS NOT NULL AND t IS NOT NULL),
b AS (SELECT *, CAST(floor(ym / 200) AS BIGINT) AS cy FROM y),
tg AS (SELECT *, CAST(floor({lonm('cy')} / 200) AS BIGINT) AS cx FROM b),
pb AS (SELECT b.*, b.cy + d.dy AS py FROM b CROSS JOIN (VALUES (-1), (0), (1)) AS d(dy)
       WHERE d.dy = 0 OR (d.dy = -1 AND b.ym - b.cy * 200 < 25) OR (d.dy = 1 AND (b.cy + 1) * 200 - b.ym < 25)),
px AS (SELECT *, {lonm('py')} AS xm FROM pb),
pc AS (SELECT px.*, CAST(floor(xm / 200) AS BIGINT) + d.dx AS pxc FROM px CROSS JOIN (VALUES (-1), (0), (1)) AS d(dx)
       WHERE d.dx = 0 OR (d.dx = -1 AND xm - floor(xm / 200) * 200 < 25) OR (d.dx = 1 AND (floor(xm / 200) + 1) * 200 - xm < 25))
SELECT a.i, b.i AS j, b.t - a.t AS dt, a.feed AS fa, b.feed AS fb, a.rnk AS ra, b.rnk AS rb
FROM pc a JOIN tg b ON a.vid = b.vid AND a.speed = b.speed AND a.py = b.cy AND a.pxc = b.cx AND a.i < b.i
WHERE (a.course IS NULL OR b.course IS NULL OR
       least(abs(a.course - b.course), 360 - abs(a.course - b.course)) <= {course_tol})
  AND {hav('a', 'b')} <= {copy_m}"
    )
  )
  if (!nrow(pr)) {
    return(list(flag = flag, basis = basis, repeat_dt_s = rdt))
  }
  f <- rep(NA_character_, n)
  bs <- rep(NA_character_, n)
  # label the pings k not yet labelled, the first of a ping named twice
  put <- function(k, lab, why) {
    lab <- rep_len(lab, length(k))
    ok <- is.na(f[k]) & !duplicated(k)
    f[k[ok]] <<- lab[ok]
    bs[k[ok]] <<- why
  }
  near <- pr$fa == pr$fb & abs(pr$dt) <= near_s
  put(ifelse(pr$dt[near] >= 0, pr$j[near], pr$i[near]), "duplicate", "recorded")
  rp <- pr[!near, ]
  two <- rp$fa != rp$fb & rp$ra != rp$rb
  lo <- ifelse(rp$ra[two] > rp$rb[two], rp$i[two], rp$j[two])
  put(
    lo,
    ifelse(abs(rp$dt[two]) <= max_dt_s, "duplicate", "shifted"),
    "priority"
  )
  one <- rp[!two, ]
  rep_i <- integer(0)
  rep_dt <- numeric(0)
  if (nrow(one)) {
    m <- unique(c(one$i, one$j))
    nm_m <- .rb_register(con, data.frame(i = m), "rb_copym_")
    w <- DBI::dbGetQuery(
      con,
      .rb_fill(
        "
WITH a AS (SELECT y.*, CAST(floor(y.t / {witness_s}) AS BIGINT) + d.k AS tb
           FROM {nm} y JOIN {nm_m} USING (i) CROSS JOIN (VALUES (-1), (0), (1)) AS d(k)),
b AS (SELECT *, CAST(floor(t / {witness_s}) AS BIGINT) AS tb FROM {nm} WHERE lon IS NOT NULL AND lat IS NOT NULL)
SELECT DISTINCT a.i FROM a JOIN b ON a.vid = b.vid AND a.tb = b.tb AND b.feed <> a.feed
WHERE abs(b.t - a.t) <= {witness_s}
  AND {hav('a', 'b')} <= {witness_m} + coalesce(a.speed, 0) * 0.514444 * abs(b.t - a.t)"
      )
    )$i
    wi <- one$i %in% w
    wj <- one$j %in% w
    # each pair's vote, + where j is the copy; the pairs of one run share its shift
    vote <- 3 * (wi & !wj) - 3 * (wj & !wi)
    run <- paste(y$vid[one$i], round(one$dt / 10))
    tot <- stats::ave(vote, run, FUN = sum)
    split_run <- stats::ave(vote, run, FUN = function(v) {
      any(v > 0) & any(v < 0)
    })
    ok <- tot != 0 & !split_run
    put(ifelse(tot > 0, one$j, one$i)[ok], "shifted", "witness")
    rep_i <- c(one$i[!ok], one$j[!ok])
    rep_dt <- c(one$dt[!ok], -one$dt[!ok])
  }
  keep <- is.na(f[rep_i]) & !duplicated(rep_i)
  rdt[rep_i[keep]] <- rep_dt[keep]
  # back to input order
  list(flag = f[order(o)], basis = bs[order(o)], repeat_dt_s = rdt[order(o)])
}
