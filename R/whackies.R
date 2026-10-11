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
# x: data frame with vid, lon, lat, time (POSIXct). Returns x sorted by
# (vid, time) with a logical column whack2 (TRUE = flagged). Grouping on the
# input is restored.
.rb_whack_forward <- function(x, kn_max = 25, max_gap_h = 4, min_dt_s = 10) {
  ms_max <- kn_max * 0.514444
  max_gap_sec <- max_gap_h * 3600

  # Compiled core (src/whack_forward.cpp) — ~1000x the R for-loop this
  # replaced (2026-09-23), same algorithm, verified byte-identical against it
  # on whacks1 and stress cases (isolated spikes, runs, a >4h gap, an injected
  # NA). `time` is passed as seconds-since-epoch, which is POSIXct's own
  # underlying numeric representation, so no unit conversion is needed here.
  .fwd <- function(lon, lat, time) {
    whack_forward_cpp(lon, lat, as.numeric(time), ms_max, max_gap_sec, min_dt_s)
  }

  grp_vars <- dplyr::group_vars(x)
  out <- x |>
    dplyr::ungroup() |>
    dplyr::arrange(vid, time) |>
    dplyr::group_by(vid) |>
    dplyr::mutate(whack2 = .fwd(lon, lat, time)) |>
    dplyr::ungroup()
  if (length(grp_vars) > 0) {
    out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(grp_vars)))
  }
  out
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

    out <- x |>
      dplyr::ungroup() |>
      dplyr::arrange(vid, time) |>
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
        WINDOW vt AS (PARTITION BY vid ORDER BY time)
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
# every step instead. Defaults follow fishycode curate/checks/sda_tune.R.
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
  min_dt_s = 10
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
    min_dt_s
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
# Returns x sorted by (vid, time) with whack (logical) and whack_stage
# (NA, "vmask", "spike" or "forward").
.whack_clean_df <- function(x, kn_max = 25, max_gap_h = 4, min_dt_s = 10, ...) {
  grp_vars <- dplyr::group_vars(x)
  out <- .rb_whack_sda(x, kn_max = kn_max, min_dt_s = min_dt_s, ...)
  s1 <- !is.na(out$whack_sda)
  fwd <- .rb_whack_forward(
    out[!s1, ],
    kn_max = kn_max,
    max_gap_h = max_gap_h,
    min_dt_s = min_dt_s
  )
  # .rb_whack_forward() re-sorts by vid, time; out[!s1, ] already is, so the order is the same
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
