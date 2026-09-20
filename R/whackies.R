# whackies.R — filters for whacky (implausible) vessel positions.
#
# Two families:
#   rb_whacky_*  : the original speed / distance filters
#   whack_*      : forward-backward and forward-only filters added 2026-09,
#                  migrated from the logbooks pipeline where they had been
#                  developed as a fork of this file.

#' Forward-only speed filter for whacky AIS positions (in-memory, O(n))
#'
#' Second-pass filter designed to run **after** [whack_fwdbwd()] on the already-
#' cleaned data frame. Where `whack_fwdbwd` misses consecutive clusters (because
#' the bad points form an internally slow but geographically wrong track),
#' `whack_forward` catches them by always comparing against the **last retained**
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
#' @section Comparison with other filters:
#' | | `whack_fwdbwd` | `whack_sequential_fast` | `whack_forward` |
#' |---|---|---|---|
#' | Dispatch | data.frame + tbl_lazy | in-memory only | in-memory only |
#' | Clusters | misses consecutive runs | correct | correct |
#' | Speed | O(n) | O(n × k) | O(n) |
#' | Aggressive | no (both-direction check) | moderate | yes (forward-only) |
#'
#' @param x A data frame (or tibble). Must contain columns `vid`, `lon`, `lat`,
#'   `time` (`POSIXct`). Typically the result of `filter(!whack)` after
#'   `whack_fwdbwd()`.
#' @param kn_max Speed threshold in knots (default: 25).
#' @param max_gap_h Time gaps larger than this (hours) reset the reference
#'   without flagging (default 4). Prevents legitimate port-to-port transits
#'   from being erroneously removed.
#'
#' @return The input data frame with an added logical column `whack2`:
#'   `TRUE` = flagged by this second pass.
#'
#' @seealso [whack_fwdbwd()] for the first-pass filter.
#'
#' @examples
#' \dontrun{
#'
#' clean <- df |>
#'   whack_fwdbwd() |>
#'   filter(!whack) |>
#'   whack_forward()
#' }
#'
#' @export
whack_forward <- function(x, kn_max = 25, max_gap_h = 4) {
  ms_max      <- kn_max * 0.514444
  max_gap_sec <- max_gap_h * 3600
  r           <- 6371000

  .fwd <- function(lon, lat, time) {
    n    <- length(lon)
    flag <- rep(FALSE, n)
    if (n < 2L) return(flag)
    prev <- 1L
    for (i in 2:n) {
      # Skip pings with NA coordinates — treat as missing, advance reference
      if (is.na(lon[i]) || is.na(lat[i]) || is.na(time[i])) {
        prev <- i
        next
      }
      dt <- as.numeric(time[i] - time[prev], units = "secs")
      if (is.na(dt) || dt > max_gap_sec) {
        # Legitimate gap or NA time — reset reference, never flag
        prev <- i
        next
      }
      phi1 <- lat[prev] * pi / 180;  phi2 <- lat[i] * pi / 180
      dphi <- (lat[i] - lat[prev]) * pi / 180
      dlam <- (lon[i] - lon[prev]) * pi / 180
      d    <- 2 * r * asin(pmin(1, sqrt(
        sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlam/2)^2)))
      if (is.na(d) || d / max(dt, 1e-6) > ms_max) {
        flag[i] <- TRUE   # bad — do NOT advance prev
      } else {
        prev <- i         # good — advance reference
      }
    }
    flag
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
#' Distances are computed via the Haversine formula; time differences are
#' floored at 1 µs to avoid division by zero.
#'
#' @section Cluster limitation:
#' The forward-backward criterion detects **isolated** spikes (one bad point
#' surrounded by good ones). It **misses consecutive clusters** of two or more
#' adjacent bad points because neither endpoint of a bad segment passes both
#' checks. Use [whack_sequential_fast()] for cluster-aware flagging (in-memory
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
#'
#' @return The same type as `x` (data.frame or tbl_lazy) with one additional
#'   logical column `whack`: `TRUE` = position flagged as implausible,
#'   `FALSE` = position retained.  For `data.frame` input, first and last
#'   points within each vessel group are always `FALSE` (one-sided: no
#'   incoming/outgoing neighbour respectively).
#'
#' @seealso [whack_sequential_fast()] for a cluster-aware in-memory variant.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(duckdbfs)
#'
#' # --- data.frame path ---
#'
#' # ungrouped — works the same as grouped; vid column drives partitioning
#' whacks1 |> whack_fwdbwd()
#'
#' # pre-grouped input; grouping is restored on output
#' whacks1 |> group_by(vid) |> whack_fwdbwd()
#'
#' # --- lazy DuckDB path ---
#' ais <- open_dataset("<ais trail dataset>")
#' ais |>
#'   filter(year == 2025, provider == "stk") |>
#'   whack_fwdbwd() |>
#'   count(whack) |>
#'   collect()
#' }
#'
#' @export
whack_fwdbwd <- function(x, kn_max = 25) {
  ms_max <- kn_max * 0.514444
  r      <- 6371000

  if (inherits(x, "data.frame")) {

    grp_vars <- dplyr::group_vars(x)

    .fwdbwd <- function(lon, lat, time) {
      n <- length(lon)
      if (n < 3L) return(rep(FALSE, n))
      phi1 <- lat[-n] * pi / 180;  phi2 <- lat[-1] * pi / 180
      dphi <- (lat[-1] - lat[-n]) * pi / 180
      dlam <- (lon[-1] - lon[-n]) * pi / 180
      dist <- 2 * r * asin(pmin(1, sqrt(
        sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlam/2)^2)))
      dt  <- pmax(as.numeric(diff(time), units = "secs"), 1e-6)
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

    sql <- glue::glue("
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
          )) / GREATEST(extract(epoch from (time::TIMESTAMP - t0::TIMESTAMP)), 1e-6) AS spd_in,
          2 * 6371000 * ASIN(SQRT(
            POWER(SIN(RADIANS((lat2 - lat)  / 2)), 2) +
            COS(RADIANS(lat))  * COS(RADIANS(lat2)) *
            POWER(SIN(RADIANS((lon2 - lon)  / 2)), 2)
          )) / GREATEST(extract(epoch from (t2::TIMESTAMP - time::TIMESTAMP)), 1e-6) AS spd_out
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
#' handling **consecutive clusters** of bad points that [whack_fwdbwd()] misses.
#' At each iteration the first segment exceeding `kn_max` is identified and its
#' **latter** endpoint removed (or the first endpoint when the bad segment is the
#' very first); the Haversine distances and speeds are then recomputed on the
#' surviving points. The loop continues until no segment exceeds the threshold.
#'
#' @section Comparison with `whack_fwdbwd()`:
#' | | `whack_fwdbwd` | `whack_sequential_fast` |
#' |---|---|---|
#' | Dispatch | data.frame **and** tbl_lazy | in-memory only |
#' | Clusters | misses consecutive runs | correct |
#' | Speed | O(n) single pass | O(n × k) where k = bad points |
#' | Usage | `whack_fwdbwd(df)` | `group_by(vid) |> mutate(whack = whack_sequential_fast(lon, lat, time))` |
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
#'   mutate(whack = whack_sequential_fast(lon, lat, time)) |>
#'   ungroup()
#' ```
#' For a lazy DuckDB table use [whack_fwdbwd()] instead (isolated spikes only)
#' or `collect()` first.
#'
#' @param lon Numeric vector of longitudes in decimal degrees.
#' @param lat Numeric vector of latitudes in decimal degrees.
#' @param time `POSIXct` vector of observation timestamps. Must be the same
#'   length as `lon` and `lat`.
#' @param kn_max Speed threshold in knots (default: 25). Any segment implying a speed
#'   greater than this value triggers point removal.
#'
#' @return A logical vector of the same length as `lon`. `TRUE` = position
#'   flagged as implausible (would be removed); `FALSE` = position retained.
#'   Tracks with fewer than 2 points are returned as all-`FALSE`.
#'
#' @seealso [whack_fwdbwd()] for an O(n) single-pass filter that also supports
#'   lazy DuckDB tables.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Apply per vessel on a collected data frame
#' ais_df |>
#'   group_by(vid) |>
#'   mutate(whack = whack_sequential_fast(lon, lat, time)) |>
#'   ungroup() |>
#'   filter(!whack)
#' }
#'
#' @export
whack_sequential_fast <- function(lon, lat, time, kn_max = 25) {
  ms_max <- kn_max * 0.514444
  r  <- 6371000
  n  <- length(lon)
  if (n < 2L) return(rep(FALSE, n))
  keep <- rep(TRUE, n)

  repeat {
    idx <- which(keep)
    m   <- length(idx)
    if (m < 2L) break
    phi1 <- lat[idx[-m]] * pi / 180;  phi2 <- lat[idx[-1]] * pi / 180
    dphi <- (lat[idx[-1]] - lat[idx[-m]]) * pi / 180
    dlam <- (lon[idx[-1]] - lon[idx[-m]]) * pi / 180
    dist <- 2 * r * asin(pmin(1, sqrt(
      sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlam/2)^2)))
    dt  <- pmax(as.numeric(diff(time[idx]), units = "secs"), 1e-6)
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
#' Uses [traipse::track_speed()] to compute along-track speeds. The leading
#' `NA` speed (convention of `traipse`) is replaced by zero so the first point
#' is never flagged on speed alone. At each iteration the **first** position
#' whose incoming speed exceeds `ms_max` is removed; if that position is index
#' 2 (ambiguous between the pair), index 1 is removed instead.
#'
#' @section Known limitation:
#' **Does not correctly flag the first data point** if it is the source of the
#' error (the incoming speed for the first point is always set to 0). Prefer
#' [whack_sequential_fast()] for new code — it is ~7× faster and does not have
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
#' @seealso [whack_sequential_fast()] for a faster drop-in replacement that
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

  if(length(lon) != length(lat) | length(lon) != length(time)) {
    stop("Length of coordinates and time must be the same")
  }

  ms_max <- rb_kn2ms(kn_max)
  .rid_original <- 1:length(lon)

  d <-
    tibble::tibble(time = time,
                   x = lon,
                   y = lat) |>
    dplyr::mutate(.rid = 1:dplyr::n(),
                  speed = traipse::track_speed(x, y, time),
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
      dplyr::mutate(speed = traipse::track_speed(x, y, time),
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
#' Uses [traipse::track_distance()] to compute step distances. The leading `NA`
#' (first point has no predecessor) is replaced by zero. At each iteration the
#' first position exceeding `meters_max` is removed and distances are
#' recomputed. The loop exits when no step distance remains above the threshold.
#'
#' @section Limitation:
#' The filter is intentionally liberal: a genuinely long transit leg may be
#' removed if its step distance exceeds the threshold even though the implied
#' speed is reasonable. Use speed-based filters ([whack_fwdbwd()],
#' [whack_sequential_fast()]) when timestamps are available.
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
                  distance = traipse::track_distance(x, y),
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
      dplyr::mutate(distance = traipse::track_distance(x, y),
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
#' invalid ones. Prefer [whack_sequential_fast()] for new work.
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
#' @seealso [whack_fwdbwd()], [whack_sequential_fast()] for alternatives that
#'   do not depend on the `trip` package.
#'
#' @export
#'
rb_whacky_speed_trip <- function(d, filter = TRUE, max_speed = 20) {
  tr <- methods::as(d |> dplyr::mutate(.idtrip = 1), "Spatial")
  tr <- suppressWarnings( trip::trip(tr, c("time", ".idtrip")) )
  d$.whacky <- !trip::speedfilter(tr, max.speed = ramb::rb_kn2ms(max_speed) / 1000 * 60 * 60)
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
