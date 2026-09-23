#' Interpolate positions onto event timestamps from a reference track
#'
#' For each row of `x` (an "event" needing a position - e.g. a logger fix
#' whose own lon/lat is unreliable or absent), finds the two nearest points in
#' `track` bracketing its timestamp - within the same group (`by`) - and
#' linearly interpolates the `value_vars` columns onto it. No extrapolation:
#' an event outside its group's track time coverage gets `NA`.
#'
#' Extracted 2026-09-23 from `fishycode`'s `curate/logbook_sensor_gps.R`,
#' which had this as inline `group_modify()` logic that re-filtered the whole
#' track table once per `(vid, trip)` group - O(groups x track_rows), and the
#' actual bottleneck once the AIS chain itself sped up (whack_forward's fix,
#' same commit range). Both paths here are O(events + track), not
#' O(groups x track).
#'
#' @section Algorithm:
#' Dispatches on the class of `x`:
#' * **data.frame** - `track` is split by `by` once (not once per group),
#'   then [stats::approx()] (`rule = 1`, no extrapolation; `ties = mean` for
#'   duplicate track timestamps) is applied per group.
#' * **tbl_lazy** (DuckDB) - two `ASOF LEFT JOIN`s (one each direction) find
#'   the bracketing track point per event; the interpolation formula runs in
#'   SQL. Nothing is collected into R.
#'
#' Both paths compute `dt_sec`: the time gap (seconds) between the two track
#' fixes bracketing each event - `NA` when the event has no coverage, `0` when
#' the event's timestamp exactly matches a track fix.
#'
#' @param x Events needing a position: a data.frame/tibble or a `tbl_lazy`.
#'   Must contain the `by` columns and `time_var`.
#' @param track Reference positions, matching `x`'s class (data.frame with
#'   data.frame, tbl_lazy with tbl_lazy sharing the same DuckDB connection).
#'   Must contain the `by` columns, `time_var`, and `value_vars`.
#' @param by Character vector of grouping columns present in both `x` and
#'   `track` (default `c("vid", "trip")`) - interpolation never crosses a
#'   group boundary (e.g. a voyage).
#' @param time_var Column name (string) of the timestamp in both `x` and
#'   `track` (default `"time"`). `POSIXct` for data.frame input, a timestamp
#'   type for tbl_lazy input.
#' @param value_vars Character vector of columns in `track` to interpolate
#'   onto `x` (default `c("lon", "lat")`).
#'
#' @return `x` with `value_vars` added/overwritten (interpolated; `NA` where
#'   the event falls outside its group's track coverage) and a new `dt_sec`
#'   column.
#'
#' @examples
#' \dontrun{
#' # in-memory
#' fixed <- logger_fixes |> rb_interpolate_track(track, by = c("vid", "trip"))
#'
#' # lazy DuckDB - nothing collected until the caller does
#' fixed <- open_dataset("logger_fixes") |>
#'   rb_interpolate_track(open_dataset("track"), by = c("vid", "trip"))
#' }
#'
#' @export
rb_interpolate_track <- function(x, track, by = c("vid", "trip"),
                                  time_var = "time",
                                  value_vars = c("lon", "lat")) {
  if (inherits(x, "data.frame")) {
    .rb_interpolate_track_df(x, track, by, time_var, value_vars)
  } else if (inherits(x, "tbl_lazy")) {
    .rb_interpolate_track_sql(x, track, by, time_var, value_vars)
  } else {
    stop("`x` must be a data.frame or a lazy DuckDB tbl (tbl_lazy).")
  }
}

.rb_interpolate_track_df <- function(x, track, by, time_var, value_vars) {
  n <- nrow(x)
  for (v in value_vars) x[[v]] <- rep(NA_real_, n)
  x$dt_sec <- rep(NA_real_, n)
  if (n == 0L || nrow(track) == 0L) return(x)

  # Group ids from ONE shared level set, so a code means the same group in
  # both x and track (a group in x absent from track gets code NA -> the
  # compiled core's bounds check leaves it NA, same as "no coverage").
  x_key   <- do.call(paste, c(x[by], sep = "\r"))
  trk_key <- do.call(paste, c(track[by], sep = "\r"))
  levels  <- unique(trk_key)
  x_code   <- match(x_key, levels)
  trk_code <- match(trk_key, levels)

  keep <- !is.na(trk_code)
  trk_code <- trk_code[keep]
  trk_time <- as.numeric(track[[time_var]][keep])
  trk_vals <- as.matrix(track[keep, value_vars, drop = FALSE])
  storage.mode(trk_vals) <- "double"

  # Collapse duplicate (group, time) track rows by averaging - matches
  # approx(..., ties = mean). Fully vectorised (one aggregate), not a
  # per-group R loop, so this doesn't reintroduce what the C++ core exists to
  # avoid. The compiled core requires strictly increasing, duplicate-free
  # times within a group; this is what guarantees that precondition.
  ord <- order(trk_code, trk_time)
  trk_code <- trk_code[ord]; trk_time <- trk_time[ord]; trk_vals <- trk_vals[ord, , drop = FALSE]
  dup_key <- paste(trk_code, trk_time, sep = "\r")
  if (anyDuplicated(dup_key)) {
    g <- match(dup_key, unique(dup_key))
    trk_code <- trk_code[!duplicated(g)]
    trk_time <- trk_time[!duplicated(g)]
    trk_vals <- rowsum(trk_vals, g) / as.vector(table(g))
  }

  n_groups <- length(levels)
  # grp_start[g] = number of track rows with code <= g (trk_code already
  # sorted ascending), i.e. the half-open [start, end) slice for group g is
  # grp_start[g-1]..grp_start[g] (0-indexed). tabulate() + cumsum is the
  # vectorised equivalent of one pass building group boundaries.
  grp_start <- c(0L, cumsum(tabulate(trk_code, n_groups)))

  res <- interpolate_track_cpp(trk_code, trk_time, trk_vals, grp_start,
                                x_code, as.numeric(x[[time_var]]))
  for (j in seq_along(value_vars)) x[[value_vars[j]]] <- res$values[, j]
  x$dt_sec <- res$dt_sec
  x
}

.rb_interpolate_track_sql <- function(x, track, by, time_var, value_vars) {
  con <- dbplyr::remote_con(x)
  orig_cols <- colnames(x)

  x_sql   <- as.character(dbplyr::sql_render(dplyr::ungroup(x)))
  trk_sql <- as.character(dbplyr::sql_render(dplyr::ungroup(track)))

  q <- function(nm) sprintf('"%s"', nm)

  join_cond <- function(a, b) {
    paste(sprintf("%s.%s = %s.%s", a, q(by), b, q(by)), collapse = " AND ")
  }

  x_col_sel <- paste(sprintf("xr.%s", q(orig_cols)), collapse = ", ")
  val_sel_b <- paste(sprintf("t.%s AS %s", q(value_vars), q(paste0(value_vars, "__0"))), collapse = ", ")
  val_sel_f <- paste(sprintf("t.%s AS %s", q(value_vars), q(paste0(value_vars, "__2"))), collapse = ", ")

  val_out <- paste(sprintf(
    'CASE WHEN b.__t0 IS NULL OR f.__t2 IS NULL THEN NULL
          WHEN b.__t0 = f.__t2 THEN b.%1$s
          ELSE b.%1$s + (f.%2$s - b.%1$s) * epoch(xr.%4$s - b.__t0) / epoch(f.__t2 - b.__t0)
     END AS %3$s',
    q(paste0(value_vars, "__0")), q(paste0(value_vars, "__2")), q(value_vars), q(time_var)
  ), collapse = ",\n      ")

  sql <- glue::glue("
    WITH src AS ({x_sql}),
    xr AS (SELECT src.*, ROW_NUMBER() OVER () AS __rid FROM src),
    trk AS ({trk_sql}),
    b AS (
      SELECT xr.__rid, t.{q(time_var)} AS __t0, {val_sel_b}
      FROM xr ASOF LEFT JOIN trk t ON {join_cond('xr', 't')} AND xr.{q(time_var)} >= t.{q(time_var)}
    ),
    f AS (
      SELECT xr.__rid, t.{q(time_var)} AS __t2, {val_sel_f}
      FROM xr ASOF LEFT JOIN trk t ON {join_cond('xr', 't')} AND xr.{q(time_var)} <= t.{q(time_var)}
    )
    SELECT {x_col_sel},
      {val_out},
      CASE WHEN b.__t0 IS NULL OR f.__t2 IS NULL THEN NULL
           ELSE epoch(f.__t2 - b.__t0) END AS dt_sec
    FROM xr
    JOIN b ON xr.__rid = b.__rid
    JOIN f ON xr.__rid = f.__rid
  ")

  dplyr::tbl(con, dplyr::sql(sql))
}
