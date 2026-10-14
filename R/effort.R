# effort.R — step 5 of the fishing-activity flow: the effort each ping stands for.
#
# fishycode plan 013, item 1 (2026-10-14). Effort is computed for every ping, fishing or not: the approaches to
# "is it fishing?" stand side by side as columns (the window, the speed range, the per-unit choice), and a
# summary picks its approach by filtering on that column. Tier 2: one DuckDB query; a data frame is registered
# and the result collected, a lazy table stays lazy. The first function to check its input against the step
# registry (registry.R).

#' Give each ping the effort it stands for
#'
#' Step 5 of the flow. Every ping gets the time it stands for (`hours`), that time times the vessel's engine
#' power (`kw_hours`) and, where the gear's width is known, the area its gear sweeps in that time (`pressure`).
#' This is done for every ping, fishing or not. Each approach to whether a ping is fishing is a column of its
#' own, so the effort of one approach is the sum over the pings that column calls fishing:
#' `pings |> dplyr::filter(fishing) |> dplyr::summarise(hours = sum(hours, na.rm = TRUE))`.
#'
#' A ping stands for the interval to the vessel's next ping (`interval = "next"`) or since its previous one
#' (`"previous"`, the ICES VMS data call's choice), within the vessel and the `by` groups (e.g. the voyage, so
#' an interval never spans a stay in port). An interval longer than `max_interval_s` is capped at it or dropped
#' (`long`); one shorter than `min_interval_s` is dropped. The ICES data call drops intervals over 2 hours and
#' under 5 seconds: `long = "drop"`, `min_interval_s = 5`. The first or last ping of a group has no interval.
#'
#' `pressure` is the swept area of towed gear: hours × speed × width. It is what the gear sweeps if it is
#' fishing, so sum it over fishing pings only. Pressure for static gear (quantity × soak) is not built yet.
#'
#' Missing inputs are not errors (the flow gives the same columns at every data resolution): with no engine
#' power `kw_hours` is `NA`; with no `width` `pressure` is `NA`.
#'
#' @param pings A data frame or lazy DuckDB table with `vid` and `time`; all its columns are kept.
#' @param vessels Optional data frame or lazy table with the `vessel_by` columns and the engine power (`kw`,
#'   kilowatts). `NULL` takes `kw` from `pings` if it is there.
#' @param by Further columns an interval may not cross, e.g. `"voyage"`.
#' @param interval `"next"` (the default) or `"previous"`.
#' @param max_interval_s,long The longest interval a ping stands for, in seconds, and what happens to a
#'   longer one: `"cap"` (the default) or `"drop"`.
#' @param min_interval_s Shorter intervals are dropped; 0 (the default) keeps them all.
#' @param kw The engine power column, in `vessels` or in `pings`.
#' @param vessel_by The columns `vessels` is joined by.
#' @param width Optional column of `pings` with the towed gear's width, metres. `NULL`: no pressure.
#' @param speed The speed column, knots; used with `width`.
#' @param vid,time The vessel and time columns.
#' @return `pings` with `hours`, `kw_hours`, `pressure` (km²), `pressure_unit` (`"km2"` where there is a
#'   pressure), `interval_flag` (`NA`, or `"edge"`, `"capped"`, `"dropped_long"`, `"dropped_short"`) and
#'   `effort_basis` (`"derived"`, or `"none"` where there are no hours): the same type, the same rows.
#' @export
#' @examples
#' t0 <- as.POSIXct("2024-03-01 06:00:00", tz = "UTC")
#' pings <- tibble::tibble(
#'   vid = 1L,
#'   time = t0 + c(0, 600, 1200, 12000),
#'   speed = c(3.2, 3.0, 3.4, 9.8),
#'   fishing = c(TRUE, TRUE, FALSE, FALSE),
#'   width = 60
#' )
#' vessels <- tibble::tibble(vid = 1L, kw = 1000)
#' e <- rb_assign_effort(pings, vessels, width = "width")
#' e[, c("time", "hours", "kw_hours", "pressure", "interval_flag")]
#' # the effort of the pings called fishing
#' sum(e$hours[e$fishing])
rb_assign_effort <- function(
  pings,
  vessels = NULL,
  by = NULL,
  interval = c("next", "previous"),
  max_interval_s = 7200,
  long = c("cap", "drop"),
  min_interval_s = 0,
  kw = "kw",
  vessel_by = "vid",
  width = NULL,
  speed = "speed",
  vid = "vid",
  time = "time"
) {
  interval <- match.arg(interval)
  long <- match.arg(long)
  also <- c(
    stats::setNames(by, by),
    if (!is.null(width)) c(width = width, speed = speed)
  )
  proto <- .rb_check_input(
    pings,
    "rb_assign_effort",
    cols = c(vid = vid, time = time),
    also = also,
    types = c(
      time = "time",
      if (!is.null(width)) c(width = "numeric", speed = "numeric")
    ),
    fresh = TRUE
  )
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  q <- function(x) .rb_q(con, x)
  p_sql <- .rb_in_sql(pings, con)

  # engine power: from the vessels, from the pings, or none
  if (!is.null(vessels)) {
    if (kw %in% names(proto)) {
      rlang::abort(
        c(
          sprintf("`pings` and `vessels` both have a column `%s`.", kw),
          i = "Drop one of them: the engine power is taken from one table only."
        )
      )
    }
    v_proto <- if (inherits(vessels, "tbl_lazy")) {
      dplyr::collect(utils::head(vessels, 0))
    } else {
      vessels[0, ]
    }
    miss <- setdiff(c(vessel_by, kw), names(v_proto))
    if (length(miss) > 0) {
      rlang::abort(c(
        "`vessels` must have the join columns and the engine power.",
        x = sprintf("Missing: %s.", paste0("`", miss, "`", collapse = ", "))
      ))
    }
    miss <- setdiff(vessel_by, names(proto))
    if (length(miss) > 0) {
      rlang::abort(c(
        "`pings` must have the columns `vessels` is joined by.",
        x = sprintf("Missing: %s.", paste0("`", miss, "`", collapse = ", "))
      ))
    }
    v_sql <- .rb_in_sql(vessels, con)
    on <- paste(
      sprintf("v.%s = p.%s", q(vessel_by), q(vessel_by)),
      collapse = " AND "
    )
    kw_join <- sprintf(
      "LEFT JOIN (SELECT %s, %s AS rb_kw FROM (%s)) v ON %s",
      paste(q(vessel_by), collapse = ", "),
      q(kw),
      v_sql,
      on
    )
    kw_sel <- "v.rb_kw"
  } else {
    kw_join <- ""
    kw_sel <- if (kw %in% names(proto)) {
      sprintf("CAST(p.%s AS DOUBLE)", q(kw))
    } else {
      "CAST(NULL AS DOUBLE)"
    }
  }

  part <- paste0("p.", q(c(vid, by)), collapse = ", ")
  tm <- q(time)
  dt <- if (interval == "next") {
    sprintf(
      "epoch(lead(p.%s) OVER (PARTITION BY %s ORDER BY p.%s) - p.%s)",
      tm,
      part,
      tm,
      tm
    )
  } else {
    sprintf(
      "epoch(p.%s - lag(p.%s) OVER (PARTITION BY %s ORDER BY p.%s))",
      tm,
      tm,
      part,
      tm
    )
  }
  over <- if (long == "cap") sprintf("%s / 3600.0", max_interval_s) else "NULL"
  over_flag <- if (long == "cap") "'capped'" else "'dropped_long'"
  press <- if (is.null(width)) {
    "CAST(NULL AS DOUBLE)"
  } else {
    # knots × 1.852 = km/h; × hours = km; × width / 1000 = km²
    sprintf("d.hours * d.%s * 1.852 * d.%s / 1000.0", q(speed), q(width))
  }
  sql <- .rb_fill(
    "
  WITH d0 AS (SELECT p.*, {dt} AS rb_dt, {kw_sel} AS rb_kw FROM ({p_sql}) p {kw_join}),
  d AS (SELECT d0.*,
          CASE WHEN rb_dt IS NULL OR rb_dt < {min_interval_s} THEN NULL
               WHEN rb_dt > {max_interval_s} THEN {over}
               ELSE rb_dt / 3600.0 END AS hours,
          CASE WHEN rb_dt IS NULL THEN 'edge'
               WHEN rb_dt < {min_interval_s} THEN 'dropped_short'
               WHEN rb_dt > {max_interval_s} THEN {over_flag} END AS interval_flag
        FROM d0)
  SELECT d.* EXCLUDE (rb_dt, rb_kw, hours, interval_flag),
         d.hours,
         d.hours * d.rb_kw AS kw_hours,
         {press} AS pressure,
         CASE WHEN ({press}) IS NOT NULL THEN 'km2' END AS pressure_unit,
         d.interval_flag,
         CASE WHEN d.hours IS NULL THEN 'none' ELSE 'derived' END AS effort_basis
  FROM d"
  )
  .rb_out(con, sql, lazy_in, by = c(vid, time))
}
