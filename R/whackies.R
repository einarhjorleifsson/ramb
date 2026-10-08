# whackies.R — filters for whacky (implausible) vessel positions.
#
# Two families:
#   rb_whacky_*  : the original speed / distance filters
#   whack_*      : forward-backward and forward-only filters added 2026-09,
#                  migrated from the logbooks pipeline where they had been
#                  developed as a fork of this file.

#' Forward-only speed filter for whacky AIS positions (in-memory, O(n))
#'
#' Second-pass filter designed to run **after** [rb_whack_fwdbwd()] on the already-
#' cleaned data frame. Where `rb_whack_fwdbwd` misses consecutive clusters (because
#' the bad points form an internally slow but geographically wrong track),
#' `rb_whack_forward` catches them by always comparing against the **last retained**
#' ping rather than the immediate predecessor.
#'
#' @section Algorithm:
#' Iterates forward through the track (sorted by `vid`, `time`). For each ping,
#' the implied speed from the last *retained* ping is computed. If that speed
#' exceeds `kn_max` **and** the time gap is below `max_gap_h`, the ping is
#' flagged and skipped — the reference ping does not advance. The reference
#' resets unconditionally when the time gap exceeds `max_gap_h` (legitimate
#' long gaps, e.g. vessel off-air overnight, should not trigger the filter).
#'
#' A time step shorter than `min_dt_s` counts as `min_dt_s`. Until 2026-10-08 the floor
#' was 1 microsecond, and a duplicate report 0.4 m away in the same second was flagged as
#' faster than `kn_max`.
#'
#' @section Comparison with other filters:
#' | | `rb_whack_fwdbwd` | `rb_whack_sequential_fast` | `rb_whack_forward` |
#' |---|---|---|---|
#' | Dispatch | data.frame + tbl_lazy | in-memory only | in-memory only |
#' | Clusters | misses consecutive runs | correct | correct |
#' | Speed | O(n) | O(n × k) | O(n) |
#' | Aggressive | no (both-direction check) | moderate | yes (forward-only) |
#'
#' @param x A data frame (or tibble). Must contain columns `vid`, `lon`, `lat`,
#'   `time` (`POSIXct`). Typically the result of `filter(!whack)` after
#'   `rb_whack_fwdbwd()`.
#' @param kn_max Speed threshold in knots (default: 25).
#' @param max_gap_h Time gaps larger than this (hours) reset the reference
#'   without flagging (default 4). Prevents legitimate port-to-port transits
#'   from being erroneously removed.
#' @param min_dt_s Shortest time step, in seconds, used for a speed (default 10). A shorter
#'   step counts as `min_dt_s`: over a few seconds the error in position and the whole-second
#'   timestamps dominate the speed. Within `min_dt_s` a ping is therefore flagged only if it
#'   is more than `kn_max * min_dt_s` away (about 130 m at the defaults).
#'
#' @return The input data frame with an added logical column `whack2`:
#'   `TRUE` = flagged by this second pass.
#'
#' @seealso [rb_whack_clean()], where it is the second stage; [rb_whack_fwdbwd()]
#'   for the former first-pass filter.
#'
#' @examples
#' \dontrun{
#'
#' clean <- df |>
#'   rb_whack_fwdbwd() |>
#'   filter(!whack) |>
#'   rb_whack_forward()
#' }
#'
#' @export
rb_whack_forward <- function(x, kn_max = 25, max_gap_h = 4, min_dt_s = 10) {
  .rb_superseded("rb_whack_forward", 'rb_flag_ping_impossible(method = "forward")')
  ms_max      <- kn_max * 0.514444
  max_gap_sec <- max_gap_h * 3600

  # Compiled core (src/whack_forward.cpp) — ~1000x the R for-loop this
  # replaced (2026-09-23), same algorithm, verified byte-identical against it
  # on whacks1 and stress cases (isolated spikes, runs, a >4h gap, an injected
  # NA). `time` is passed as seconds-since-epoch, which is POSIXct's own
  # underlying numeric representation, so no unit conversion is needed here.
  # Speeds use a time step of at least min_dt_s (2026-10-08; was 1e-6 s), in the
  # port and in the test's R reference alike.
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
  if (length(grp_vars) > 0)
    out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(grp_vars)))
  out
}


#' Forward-backward speed filter for whacky AIS positions (unified dispatch)
#'
#' Flags AIS positions whose implied speed is implausibly high in **both** the
#' incoming **and** outgoing direction — i.e. isolated spikes. The function
#' dispatches automatically on the class of `x`:
#'
#' * **`data.frame`** — vectorised R; Haversine distances computed per vessel
#'   group; O(n) single pass per vessel.
#' * **`tbl_lazy`** (lazy DuckDB / dbplyr table) — pure SQL using
#'   `LAG`/`LEAD` window functions; no data is pulled into R memory.
#'
#' @section Algorithm:
#' A point \eqn{i} is flagged `whack = TRUE` when
#' \deqn{\text{speed}_{i-1 \to i} > \text{kn\_max} \;\AND\; \text{speed}_{i \to i+1} > \text{kn\_max}}
#' Distances are computed via the Haversine formula; a time difference shorter
#' than `min_dt_s` counts as `min_dt_s`.
#'
#' @section Cluster limitation:
#' The forward-backward criterion detects **isolated** spikes (one bad point
#' surrounded by good ones). It **misses consecutive clusters** of two or more
#' adjacent bad points because neither endpoint of a bad segment passes both
#' checks. Use [rb_whack_sequential_fast()] for cluster-aware flagging (in-memory
#' only).
#'
#' @section Grouping:
#' * **`data.frame` path** — the table is ungrouped internally, sorted by
#'   `(vid, time)`, then re-grouped by `vid` for the calculation. Any grouping
#'   present on the input is restored on the output; **original row order is
#'   not restored**.
#' * **`tbl_lazy` path** — grouping is silently dropped (the SQL `WINDOW`
#'   clause always partitions by `vid`). The returned object is an ungrouped
#'   lazy table.
#'
#' @param x A `data.frame` (or tibble) **or** a `tbl_lazy` lazy DuckDB table.
#'   Must contain columns `vid` (vessel ID), `lon` (decimal degrees), `lat`
#'   (decimal degrees), and `time` (`POSIXct` for data frames; `TIMESTAMP` for
#'   DuckDB). May be pre-filtered or pre-grouped.
#' @param kn_max Speed threshold in knots (default: 25). Points are flagged only when
#'   both neighbouring implied speeds exceed this value.
#' @param min_dt_s Shortest time step, in seconds, used for a speed (default 10). A shorter
#'   step counts as `min_dt_s`: over a few seconds the error in position and the whole-second
#'   timestamps dominate the speed. Within `min_dt_s` a ping is therefore flagged only if it
#'   is more than `kn_max * min_dt_s` away (about 130 m at the defaults).
#'
#' @return The same type as `x` (data.frame or tbl_lazy) with one additional
#'   logical column `whack`: `TRUE` = position flagged as implausible,
#'   `FALSE` = position retained.  For `data.frame` input, first and last
#'   points within each vessel group are always `FALSE` (one-sided: no
#'   incoming/outgoing neighbour respectively).
#'
#' @seealso [rb_whack_sequential_fast()] for a cluster-aware in-memory variant.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(duckdbfs)
#'
#' # --- data.frame path ---
#'
#' # ungrouped — works the same as grouped; vid column drives partitioning
#' whacks1 |> rb_whack_fwdbwd()
#'
#' # pre-grouped input; grouping is restored on output
#' whacks1 |> group_by(vid) |> rb_whack_fwdbwd()
#'
#' # --- lazy DuckDB path ---
#' ais <- open_dataset("<ais trail dataset>")
#' ais |>
#'   filter(year == 2025, provider == "stk") |>
#'   rb_whack_fwdbwd() |>
#'   count(whack) |>
#'   collect()
#' }
#'
#' @export
rb_whack_fwdbwd <- function(x, kn_max = 25, min_dt_s = 10) {
  .rb_superseded("rb_whack_fwdbwd", 'rb_flag_ping_impossible(method = "fwdbwd")')
  ms_max <- kn_max * 0.514444

  if (inherits(x, "data.frame")) {

    grp_vars <- dplyr::group_vars(x)

    .fwdbwd <- function(lon, lat, time) {
      n <- length(lon)
      if (n < 3L) return(rep(FALSE, n))
      dist <- rb_calc_distance(lon[-n], lat[-n], lon[-1], lat[-1])
      dt  <- pmax(as.numeric(diff(time), units = "secs"), min_dt_s)
      spd <- dist / dt
      spd_in  <- c(NA_real_, spd)
      spd_out <- c(spd, NA_real_)
      (!is.na(spd_in) & !is.na(spd_out)) & (spd_in > ms_max) & (spd_out > ms_max)
    }

    out <- x |>
      dplyr::ungroup() |>
      dplyr::arrange(vid, time) |>
      dplyr::group_by(vid) |>
      dplyr::mutate(whack = .fwdbwd(lon, lat, time)) |>
      dplyr::ungroup()

    # Restore original grouping if present
    if (length(grp_vars) > 0)
      out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(grp_vars)))
    out

  } else if (inherits(x, "tbl_lazy")) {

    orig_cols <- colnames(x)
    col_sel   <- paste(sprintf('"%s"', orig_cols), collapse = ", ")
    con       <- dbplyr::remote_con(x)
    inner_sql <- as.character(dbplyr::sql_render(dplyr::ungroup(x)))

    sql <- .rb_fill("
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
    ")

    dplyr::tbl(con, dplyr::sql(sql))

  } else {
    stop("`x` must be a data.frame or a lazy DuckDB tbl (tbl_lazy).")
  }
}


#' Cluster-aware sequential speed filter (in-memory, vectorised)
#'
#' Iteratively removes AIS positions that imply implausible travel speeds,
#' handling **consecutive clusters** of bad points that [rb_whack_fwdbwd()] misses.
#' At each iteration the first segment exceeding `kn_max` is identified and its
#' **latter** endpoint removed (or the first endpoint when the bad segment is the
#' very first); the Haversine distances and speeds are then recomputed on the
#' surviving points. The loop continues until no segment exceeds the threshold.
#'
#' @section Comparison with `rb_whack_fwdbwd()`:
#' | | `rb_whack_fwdbwd` | `rb_whack_sequential_fast` |
#' |---|---|---|
#' | Dispatch | data.frame **and** tbl_lazy | in-memory only |
#' | Clusters | misses consecutive runs | correct |
#' | Speed | O(n) single pass | O(n × k) where k = bad points |
#' | Usage | `rb_whack_fwdbwd(df)` | `group_by(vid) |> mutate(whack = rb_whack_sequential_fast(lon, lat, time))` |
#'
#' This re-implements the algorithm of `ramb::rb_whacky_speed` in base R (no
#' dplyr/traipse inside the loop), which makes it roughly 7× faster on large
#' vessel tracks.
#'
#' @section In-memory only:
#' This function operates on plain R vectors. To apply it per vessel on a
#' grouped data frame use:
#' ```r
#' df |>
#'   group_by(vid) |>
#'   mutate(whack = rb_whack_sequential_fast(lon, lat, time)) |>
#'   ungroup()
#' ```
#' For a lazy DuckDB table use [rb_whack_fwdbwd()] instead (isolated spikes only)
#' or `collect()` first.
#'
#' @param lon Numeric vector of longitudes in decimal degrees.
#' @param lat Numeric vector of latitudes in decimal degrees.
#' @param time `POSIXct` vector of observation timestamps. Must be the same
#'   length as `lon` and `lat`.
#' @param kn_max Speed threshold in knots (default: 25). Any segment implying a speed
#'   greater than this value triggers point removal.
#' @param min_dt_s Shortest time step, in seconds, used for a speed (default 10). A shorter
#'   step counts as `min_dt_s`: over a few seconds the error in position and the whole-second
#'   timestamps dominate the speed. Within `min_dt_s` a ping is therefore flagged only if it
#'   is more than `kn_max * min_dt_s` away (about 130 m at the defaults).
#'
#' @return A logical vector of the same length as `lon`. `TRUE` = position
#'   flagged as implausible (would be removed); `FALSE` = position retained.
#'   Tracks with fewer than 2 points are returned as all-`FALSE`.
#'
#' @seealso [rb_whack_fwdbwd()] for an O(n) single-pass filter that also supports
#'   lazy DuckDB tables.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Apply per vessel on a collected data frame
#' ais_df |>
#'   group_by(vid) |>
#'   mutate(whack = rb_whack_sequential_fast(lon, lat, time)) |>
#'   ungroup() |>
#'   filter(!whack)
#' }
#'
#' @export
rb_whack_sequential_fast <- function(lon, lat, time, kn_max = 25, min_dt_s = 10) {
  .rb_superseded("rb_whack_sequential_fast", 'rb_flag_ping_impossible(method = "sequential")')
  ms_max <- kn_max * 0.514444
  n  <- length(lon)
  if (n < 2L) return(rep(FALSE, n))
  keep <- rep(TRUE, n)

  repeat {
    idx <- which(keep)
    m   <- length(idx)
    if (m < 2L) break
    dist <- rb_calc_distance(lon[idx[-m]], lat[idx[-m]], lon[idx[-1]], lat[idx[-1]])
    dt  <- pmax(as.numeric(diff(time[idx]), units = "secs"), min_dt_s)
    spd <- dist / dt
    bad <- which(spd > ms_max)
    if (length(bad) == 0L) break
    p <- bad[1L]
    keep[if (p == 1L) idx[1L] else idx[p + 1L]] <- FALSE
  }
  !keep
}




#' Sequential speed filter for whacky positions (original ramb implementation)
#'
#' Iteratively removes the first position that exceeds the speed threshold,
#' recomputes speeds, and repeats until all speeds are within tolerance.
#'
#' @section Algorithm:
#' Computes along-track speeds on the WGS84 ellipsoid (as `traipse::track_speed()`). The leading
#' `NA` speed (convention of `traipse`) is replaced by zero so the first point
#' is never flagged on speed alone. At each iteration the **first** position
#' whose incoming speed exceeds `ms_max` is removed; if that position is index
#' 2 (ambiguous between the pair), index 1 is removed instead.
#'
#' @section Known limitation:
#' **Does not correctly flag the first data point** if it is the source of the
#' error (the incoming speed for the first point is always set to 0). Prefer
#' [rb_whack_sequential_fast()] for new code — it is ~7× faster and does not have
#' this edge case.
#'
#' @param lon Numeric vector of longitudes in decimal degrees.
#' @param lat Numeric vector of latitudes in decimal degrees.
#' @param time `POSIXct` vector of observation timestamps.
#' @param kn_max Speed threshold in knots (default 25).
#'
#' @return A logical vector the same length as `lon`. `TRUE` = position
#'   classified as whacky; `FALSE` = retained.
#'
#' @seealso [rb_whack_sequential_fast()] for a faster drop-in replacement that
#'   handles the first-point edge case.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' data |>
#'   group_by(id) |>
#'   mutate(whack = rb_whacky_speed(lon, lat, time))
#' }
#'
#' @export
rb_whacky_speed <- function(lon, lat, time, kn_max = 25) {
  .rb_superseded("rb_whacky_speed", 'rb_flag_ping_impossible()')

  if(length(lon) != length(lat) | length(lon) != length(time)) {
    stop("Length of coordinates and time must be the same")
  }

  ms_max <- .rb_kn2ms(kn_max)
  .rid_original <- 1:length(lon)

  d <-
    tibble::tibble(time = time,
                   x = lon,
                   y = lat) |>
    dplyr::mutate(.rid = 1:dplyr::n(),
                  speed = .rb_track_speed(x, y, time),
                  speed = tidyr::replace_na(speed, 0))

  while(any(d$speed > ms_max, na.rm = TRUE)) {
    a_whack <-
      d |>
      dplyr::filter(speed > ms_max) |>
      dplyr::slice(1) |>
      dplyr::pull(.rid)
    if(a_whack == 2) a_whack <- 1
    d <-
      d |>
      dplyr::filter(.rid != a_whack) |>
      dplyr::mutate(speed = .rb_track_speed(x, y, time),
                    speed = tidyr::replace_na(speed, 0))
  }

  x <- ifelse(.rid_original %in% d$.rid, FALSE, TRUE)

  p <- round(sum(x) / length(x) * 100, digits = 1)
  #if(p > 5) {
  #  message(paste0(p, "% of the data are classified as whacky points"))
  #}
  return(x)

}



#' Sequential distance filter for whacky positions
#'
#' Iteratively removes positions whose step distance to the previous retained
#' point exceeds `miles_max` nautical miles, recomputing distances after each
#' removal. Useful as a complement to speed-based filters when timestamps are
#' unreliable.
#'
#' @section Algorithm:
#' Computes step distances on the WGS84 ellipsoid (as `traipse::track_distance()`). The leading `NA`
#' (first point has no predecessor) is replaced by zero. At each iteration the
#' first position exceeding `meters_max` is removed and distances are
#' recomputed. The loop exits when no step distance remains above the threshold.
#'
#' @section Limitation:
#' The filter is intentionally liberal: a genuinely long transit leg may be
#' removed if its step distance exceeds the threshold even though the implied
#' speed is reasonable. Use speed-based filters ([rb_whack_fwdbwd()],
#' [rb_whack_sequential_fast()]) when timestamps are available.
#'
#' @param lon Numeric vector of longitudes in decimal degrees.
#' @param lat Numeric vector of latitudes in decimal degrees.
#' @param miles_max Maximum allowed step distance in nautical miles (default 6).
#'   Internally converted to metres as `miles_max * 1852`.
#'
#' @return A logical vector the same length as `lon`. `TRUE` = position
#'   classified as whacky; `FALSE` = retained.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' data |>
#'   group_by(id) |>
#'   mutate(whack = rb_whacky_distance(lon, lat))
#' }
#'
rb_whacky_distance <- function(lon, lat, miles_max = 6) {

  if(length(lon) != length(lat)) {
    stop("Length of coordinates and time must be the same")
  }

  meters_max <- miles_max * 1852

  .rid_original <- 1:length(lon)

  d <-
    tibble::tibble(x = lon,
                   y = lat) |>
    dplyr::mutate(.rid = 1:dplyr::n(),
                  distance = .rb_track_distance(x, y),
                  distance = tidyr::replace_na(distance, 0))

  while(any(d$distance > meters_max, na.rm = TRUE)) {
    a_whack <-
      d |>
      dplyr::filter(distance > meters_max) |>
      dplyr::slice(1) |>
      dplyr::pull(.rid)
    d <-
      d |>
      dplyr::filter(.rid != a_whack) |>
      dplyr::mutate(distance = .rb_track_distance(x, y),
                    distance = tidyr::replace_na(distance, 0))
  }

  x <- ifelse(.rid_original %in% d$.rid, FALSE, TRUE)
  return(x)

}



# Adapted from Mendoza et al. (2024), ICES J. Mar. Sci. 81(2):390.
# https://academic.oup.com/icesjms/article/81/2/390/7516127

#' Sequential speed filter after Mendoza et al. (2024)
#'
#' A fast but aggressive speed filter adapted from Mendoza et al. (2024). At
#' each iteration the **entire pair** of positions producing an excessive speed
#' are dropped together (unlike [rb_whacky_speed()] which removes only the
#' latter endpoint). This makes it roughly twice as fast but removes an equal
#' number of valid and invalid points around each bad segment.
#'
#' @section Input requirements:
#' The data frame must contain columns:
#' * `x`, `y` - projected coordinates in **metres** (not decimal degrees).
#' * `time` - `POSIXct` timestamps.
#' * `device_id` - vessel or device identifier (used for grouping).
#' * `seq` - unique row sequence identifier (used for filtering).
#'
#' @section Limitation:
#' Because both endpoints of a bad segment are removed, the filter is less
#' conservative than [rb_whacky_speed()]: it discards as many valid points as
#' invalid ones. Prefer [rb_whack_sequential_fast()] for new work.
#'
#' @param df A data frame with columns `x`, `y`, `time`, `device_id`, `seq`
#'   (see *Input requirements*).
#' @param speed_filter Speed threshold in knots (default 25).
#'
#' @return The input data frame with all rows implying speeds above
#'   `speed_filter` removed, along with computed columns `dx`, `dy`, `dt`,
#'   `dd`, and `speed`.
#'
#' @references Mendoza et al. (2024). ICES J. Mar. Sci. 81(2):390.
#'
#' @export
#'
rb_whacky_speed_mendo <- function(df, speed_filter = 25) {
  .rb_superseded("rb_whacky_speed_mendo", 'rb_flag_ping_impossible()')

  ms2knots = 1.9438 #(m/s)/knots

  df <-
    df |>
    dplyr::group_by(device_id) |>
    dplyr::mutate(dx = c(0,abs(diff(x))),
                  dy = c(0,abs(diff(y)))) |>
    dplyr::mutate(dt = c(as.numeric(time - dplyr::lag(time),units="secs")),
                  dd = sqrt(dx^2 + dy^2),
                  speed = dd / dt * ms2knots) |>
    dplyr::ungroup()

  repeat {
    subset <-
      df |>
      dplyr::filter(speed > speed_filter)
    sel <- factor(subset$seq)
    nrows <- length(sel)
    if (nrows==0) {
      break
    }  else {
      df <- df[!df$seq %in% sel,]
    }
    df <-
      df |>
      dplyr::group_by(device_id) |>
      dplyr::mutate(dx = c(0, abs(diff(x))),
                    dy = c(0, abs(diff(y)))) |>
      dplyr::mutate(dt = c(as.numeric(time - dplyr::lag(time), units="secs"))) |>
      dplyr::mutate(dd = sqrt(dx^2 + dy^2)) |>
      dplyr::mutate(speed = dd / dt * ms2knots) |>
      # add so data returned to user is
      dplyr::ungroup()
  }
  return(df)
}

#' Trip-level speed filter using the `trip` package
#'
#' Wraps [trip::speedfilter()] to flag or remove whacky AIS positions within a
#' single trip. Optionally runs [rb_whacky_speed()] as a second pass to catch
#' any residual whacky points that the `trip` package missed.
#'
#' @section Input:
#' `d` must be an sf data frame (or a data frame coercible to `Spatial` via
#' [methods::as()]) with columns `lon`, `lat`, and `time`. Only a **single
#' trip** should be passed — grouping is handled externally.
#'
#' @section Two-pass design:
#' 1. [trip::speedfilter()] applied at `max_speed` knots.
#' 2. If `filter = TRUE`, [rb_whacky_speed()] is applied to the residual as a
#'    safety net.
#'
#' @param d A (sf) data frame for a single trip with columns `lon`, `lat`,
#'   `time`.
#' @param filter Logical. If `TRUE` (default), whacky points are removed and
#'   `.whacky` column is dropped from the return value. If `FALSE`, `.whacky`
#'   is retained as a logical column and no rows are dropped.
#' @param max_speed Speed threshold in knots (default 20).
#'
#' @return If `filter = TRUE`: the input data frame with whacky rows removed
#'   and no `.whacky` column. If `filter = FALSE`: the input data frame with an
#'   added logical column `.whacky` (`TRUE` = whacky).
#'
#' @seealso [rb_whack_fwdbwd()], [rb_whack_sequential_fast()] for alternatives that
#'   do not depend on the `trip` package.
#'
#' @export
#'
rb_whacky_speed_trip <- function(d, filter = TRUE, max_speed = 20) {
  .rb_superseded("rb_whacky_speed_trip", 'rb_flag_ping_impossible()')
  tr <- methods::as(d |> dplyr::mutate(.idtrip = 1), "Spatial")
  tr <- suppressWarnings( trip::trip(tr, c("time", ".idtrip")) )
  d$.whacky <- !trip::speedfilter(tr, max.speed = .rb_kn2ms(max_speed) / 1000 * 60 * 60)
  if(filter) {
    d |>
      dplyr::filter(!.whacky) |>
      dplyr::select(-.whacky)
  }
  # some extra precaution - would be of interest why not captured above
  if(filter) {
    d <-
      d |>
      dplyr::mutate(.whacky = ramb::rb_whacky_speed(lon, lat, time)) |>
      dplyr::filter(!.whacky) |>
      dplyr::select(-.whacky)
  }

  return(d)
}


#' Speed-distance-angle filter for whacky positions (compiled)
#'
#' Flags implausible positions with the speed-distance-angle algorithm of
#' `argosfilter::sdafilter()` (Freitas et al. 2008), re-implemented as one compiled pass
#' over all vessels. Nothing is removed: every row is returned, with a label.
#'
#' @section Algorithm:
#' Per vessel, on the track sorted by time:
#' 1. **vmask** - the root-mean-square speed from each ping to its two neighbours on either
#'    side. Local maxima of that speed above `kn_max` are flagged and taken out of the
#'    working track; this repeats until no remaining ping exceeds `kn_max`.
#' 2. A vmask flag stands only if the ping is more than `vmask_min_dist` metres from its
#'    predecessor (default 0: every flag stands; `argosfilter` uses 5000).
#' 3. **Spikes** - a ping is flagged when the two legs at it enclose an angle of at most
#'    `ang[k]` degrees and either both legs are longer than `distlim[k]` metres or, where
#'    `speedlim_kn[k] > 0`, both legs are faster than that. Repeats on the thinned track
#'    until none is left. The speed form is the time-aware one: the same leg is long or
#'    short depending on the time it took.
#'
#' Every speed uses a time step of at least `min_dt_s`; `argosfilter` adds 1 s to every
#' step instead.
#'
#' The package defaults tuned for Argos seal tracks (15/25 degrees, 2.5/5 km, 5 km) leave
#' tens of thousands of speed spikes in vessel AIS. The defaults here follow the tuning in
#' fishycode `curate/checks/sda_tune.R`: a speed test, `vmask_min_dist = 0`.
#'
#' @param x A data frame with `vid`, `lon`, `lat` and `time` (`POSIXct`).
#' @param kn_max Speed threshold in knots (default 25).
#' @param ang Angle limits in degrees (default 25).
#' @param distlim Distance limits in metres, one per `ang`; used where `speedlim_kn` is 0.
#' @param speedlim_kn Speed limits in knots, one per `ang` (default `kn_max`); 0 = use `distlim`.
#' @param vmask_min_dist See above (default 0).
#' @param min_dt_s Shortest time step, in seconds, used for a speed (default 10). A shorter
#'   step counts as `min_dt_s`: over a few seconds the error in position and the whole-second
#'   timestamps dominate the speed. Within `min_dt_s` a ping is therefore flagged only if it
#'   is more than `kn_max * min_dt_s` away (about 130 m at the defaults).
#'
#' @return `x`, sorted by `vid`, `time` (ties by `lon`, `lat`), with `whack_sda`: `NA` = not flagged,
#'   `"vmask"` or `"spike"` = the step that flagged it.
#'
#' @references Freitas, C., Lydersen, C., Fedak, M.A. and Kovacs, K.M. (2008). A simple
#'   new algorithm to filter marine mammal Argos locations. Marine Mammal Science 24:315-325.
#'
#' @seealso [rb_whack_clean()], the full recipe; [rb_whack_forward()], [rb_whack_fwdbwd()].
#' @export
rb_whack_sda <- function(x, kn_max = 25, ang = 25, distlim = rep(0, length(ang)),
                      speedlim_kn = rep(kn_max, length(ang)), vmask_min_dist = 0,
                      min_dt_s = 10) {
  .rb_superseded("rb_whack_sda", 'rb_flag_ping_impossible(method = "sda")')
  grp_vars <- dplyr::group_vars(x)
  # lon and lat break ties in time, so the result does not depend on the row order of the input
  # (parquet reads come back in a different order from run to run)
  x <- dplyr::arrange(dplyr::ungroup(x), vid, time, lon, lat)
  code <- match(x$vid, unique(x$vid))
  r <- whack_sda_cpp(x$lat, x$lon, as.numeric(x$time), code, kn_max * 0.514444,
                     as.numeric(ang), as.numeric(distlim), as.numeric(speedlim_kn) * 0.514444,
                     vmask_min_dist, min_dt_s)
  x$whack_sda <- c(NA_character_, "vmask", "spike")[r + 1L]
  if (length(grp_vars) > 0) x <- dplyr::group_by(x, dplyr::across(dplyr::all_of(grp_vars)))
  x
}


#' Label whacky positions: vmask, leg-speed spike, forward scan
#'
#' The recommended filter for vessel positions. It labels, it does not remove: every
#' ping is returned, so the track can be rebuilt with or without the flagged ones and the
#' reason for each flag is kept.
#'
#' Two stages, because neither is enough on its own:
#' 1. [rb_whack_sda()] - judges each ping from **both sides** (rms speed to four neighbours,
#'    then both legs at the ping), so it can flag a bad ping that the forward scan would
#'    adopt as its anchor, e.g. the first ping after a long gap.
#' 2. [rb_whack_forward()] on what stage 1 left - compares with the last **kept** ping, so it
#'    resolves runs and interleaved streams that stage 1 cannot judge from the neighbours.
#'
#' Measured on 543.6 M AIS pings (2007-2026): 1.45 M flagged (0.27 %), against 2.87 M with the
#' former 1-microsecond time step. The difference is almost all near-duplicate reports from two
#' merged feeds (96 % between feeds, 92 % within 50 m of the ping before); the kept track is
#' 0.003 % shorter and keeps 706 legs over 1 km at over 25 kn, against 716.
#'
#' The time-step floor `min_dt_s = 10` was chosen on June 2016 and June 2023 (7.2 M pings):
#' with a floor of up to 10 s no kept leg jumps more than 200 m at over 25 kn; at 20 s, 575 do,
#' and at 60 s, 6,881. Same-second reports come mostly from two feeds merged.
#'
#' @section Data frame and DuckDB:
#' The labelling always runs in compiled code, in R; DuckDB never runs the filter itself.
#' * **data.frame** - the whole frame is sorted by `vid`, `time` and labelled in one
#'   compiled call. A table too big for memory is split by the caller into batches of whole
#'   vessels, which is exact for the reason given below.
#' * **lazy DuckDB table** (`tbl_lazy`) - the algorithm is sequential per vessel (a ping's
#'   fate depends on the pings kept before it), which SQL window functions cannot express,
#'   so the table is processed in **batches of whole vessels** (a vessel's result depends on
#'   no other vessel, so batching is exact). Each batch is collected with only
#'   `vid`, `time`, `lon`, `lat`, labelled in compiled code, and only the **flagged** rows
#'   (well under 1 percent) are written back to a temporary table on the same connection. The
#'   result is a lazy table: `x` left-joined to those labels, so the next verbs, a filter
#'   or a `write_dataset()`, run in DuckDB and the full table is never held in R. Row order
#'   of the lazy result is not guaranteed.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `lon`, `lat` and `time`.
#' @param kn_max Speed threshold in knots (default 25).
#' @param max_gap_h Passed to [rb_whack_forward()] (default 4).
#' @param min_dt_s Shortest time step for a speed, in seconds, used by both stages
#'   (default 10); see [rb_whack_forward()].
#' @param batch_pings Lazy tables only: approximate pings per batch of whole vessels
#'   (default 5 million, which keeps R below a few GB).
#' @param ... Passed to [rb_whack_sda()].
#'
#' @return `x` with `whack` (logical) and `whack_stage` (`NA`, `"vmask"`, `"spike"` or
#'   `"forward"`). A data frame comes back sorted by `vid` and `time`; a lazy table comes back lazy.
#'
#' @examples
#' \dontrun{
#' # data frame
#' d <- pings |> rb_whack_clean()
#' track <- dplyr::filter(d, !whack)
#'
#' # DuckDB / parquet: the pings pass through R one batch of whole vessels at a time;
#' # the result is a lazy table
#' pings <- duckdbfs::open_dataset("ping_tagged")
#' pings |> rb_whack_clean() |> dplyr::filter(!whack) |> duckdbfs::write_dataset("ping_clean")
#' }
#' @export
rb_whack_clean <- function(x, kn_max = 25, max_gap_h = 4, min_dt_s = 10, batch_pings = 5e6, ...) {
  .rb_superseded("rb_whack_clean", 'rb_flag_ping_impossible(method = "clean")')
  if (inherits(x, "tbl_lazy")) return(.whack_clean_lazy(x, kn_max, max_gap_h, min_dt_s, batch_pings, ...))
  .whack_clean_df(x, kn_max, max_gap_h, min_dt_s, ...)
}

.whack_clean_df <- function(x, kn_max, max_gap_h, min_dt_s, ...) {
  grp_vars <- dplyr::group_vars(x)
  out <- rb_whack_sda(x, kn_max = kn_max, min_dt_s = min_dt_s, ...)
  s1 <- !is.na(out$whack_sda)
  fwd <- rb_whack_forward(out[!s1, ], kn_max = kn_max, max_gap_h = max_gap_h, min_dt_s = min_dt_s)
  # rb_whack_forward() re-sorts by vid, time; out[!s1, ] already is, so the order is the same
  stage <- out$whack_sda
  stage[!s1] <- ifelse(fwd$whack2, "forward", NA_character_)
  out$whack_sda <- NULL
  out$whack_stage <- stage
  out$whack <- !is.na(stage)
  if (length(grp_vars) > 0) out <- dplyr::group_by(out, dplyr::across(dplyr::all_of(grp_vars)))
  out
}

.whack_clean_lazy <- function(x, kn_max, max_gap_h, min_dt_s, batch_pings, ...) {
  con <- dbplyr::remote_con(x)
  # A deterministic row number per vessel, computed in DuckDB and identically in the batch
  # (the batch is sorted by time, lon, lat before it is labelled), is the join key back.
  keyed <- x |>
    dplyr::group_by(vid) |>
    dbplyr::window_order(time, lon, lat) |>
    dplyr::mutate(.whack_rn = dplyr::row_number()) |>
    dplyr::ungroup()
  vn <- x |> dplyr::count(vid) |> dplyr::collect() |> dplyr::arrange(dplyr::desc(n))
  batches <- split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  tmp <- paste0("whack_labels_", format(as.hexmode(sample.int(.Machine$integer.max, 1)), width = 8))
  first <- TRUE
  for (g in batches) {
    # filter to the batch BEFORE numbering, so DuckDB sorts only this batch, not the table
    d <- x |>
      dplyr::filter(vid %in% g) |>
      dplyr::select(vid, time, lon, lat) |>
      dplyr::group_by(vid) |>
      dbplyr::window_order(time, lon, lat) |>
      dplyr::mutate(.whack_rn = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      dplyr::arrange(vid, time, lon, lat)
    lab <- .whack_clean_df(d, kn_max, max_gap_h, min_dt_s, ...)
    lab <- lab[lab$whack, c("vid", ".whack_rn", "whack_stage")]
    lab$.whack_rn <- as.numeric(lab$.whack_rn)
    if (first) {
      DBI::dbWriteTable(con, tmp, lab, temporary = TRUE, overwrite = TRUE)
      first <- FALSE
    } else if (nrow(lab) > 0) {
      DBI::dbAppendTable(con, tmp, lab)
    }
  }
  if (first) stop("rb_whack_clean(): the table has no vessels.", call. = FALSE)
  # window_order() clears the numbering's order: left on the result it is added to a later
  # arrange(), which fails after a count() or summarise() ("time" must appear in GROUP BY)
  keyed |>
    dplyr::left_join(dplyr::tbl(con, tmp), by = c("vid", ".whack_rn")) |>
    dplyr::mutate(whack = !is.na(whack_stage)) |>
    dplyr::select(-.whack_rn) |>
    dbplyr::window_order()
}


#' Deprecated names of the whacky-position filters
#'
#' `whack_clean()`, `whack_sda()`, `whack_forward()`, `whack_fwdbwd()` and
#' `whack_sequential_fast()` were renamed [rb_whack_clean()], [rb_whack_sda()],
#' [rb_whack_forward()], [rb_whack_fwdbwd()] and [rb_whack_sequential_fast()] on 2026-10-08,
#' in line with the rest of the package. The old names still work, with a warning.
#'
#' @param ... Passed to the new function.
#' @name whack-deprecated
#' @keywords internal
NULL

#' @rdname whack-deprecated
#' @export
whack_clean <- function(...) {
  .Deprecated("rb_whack_clean", package = "ramb")
  rb_whack_clean(...)
}

#' @rdname whack-deprecated
#' @export
whack_sda <- function(...) {
  .Deprecated("rb_whack_sda", package = "ramb")
  rb_whack_sda(...)
}

#' @rdname whack-deprecated
#' @export
whack_forward <- function(...) {
  .Deprecated("rb_whack_forward", package = "ramb")
  rb_whack_forward(...)
}

#' @rdname whack-deprecated
#' @export
whack_fwdbwd <- function(...) {
  .Deprecated("rb_whack_fwdbwd", package = "ramb")
  rb_whack_fwdbwd(...)
}

#' @rdname whack-deprecated
#' @export
whack_sequential_fast <- function(...) {
  .Deprecated("rb_whack_sequential_fast", package = "ramb")
  rb_whack_sequential_fast(...)
}
