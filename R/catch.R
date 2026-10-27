# catch.R — step 6 of the fishing-activity flow: catch shared over the fishing pings.
#
# fishycode plan 013, item 2 (2026-10-14). Catch is shared at the finest level the data can separate, not per
# record. A unit is a set of catch-bearing events (record windows, trips, landings) that cannot be told apart in
# time, and its catch goes to its fishing pings by effort. Catch with no fishing ping in its unit moves up a level,
# so kilograms in equal kilograms out. Tier 2: DuckDB queries; the island merge and the move-up chain are small
# tables resolved in R between queries.

#' Group the events that carry catch into catch units
#'
#' A step-6 builder. An event is anything that carries catch and covers a stretch of a vessel's time: a logbook
#' record's window (a tow, a haul, a day), a trip, a landing over its trip or trips, a week or a month of
#' landings. Events of one vessel and one level whose intervals overlap, or that share a key (the set and the
#' haul of one record), cannot be told apart on the track. They form one unit, and their catch is shared
#' together by [rb_share_catch()]. A timed record whose window is its own is a unit of one; the records of one
#' day without times are one unit.
#'
#' `level` orders the kinds of event from the finest (1), e.g. 1 for timed records, 2 for day records, 3 for
#' trips, 4 for landings. Units are formed within a level; [rb_share_catch()] moves catch up the levels.
#'
#' A landing with only a date is given its `end` (the start of the day after it) and no `start`: it then starts
#' where the vessel's previous event of the same level ends, or `max_days` before its own end. `start_filled`
#' marks those.
#'
#' @param events A data frame or lazy DuckDB table, one row per interval: the `key` columns, `vid`, `start` and
#'   `end` (date-times; the interval is half-open), `level` (integer, 1 the finest) and `resolution` (what the
#'   event is, e.g. `"haul"`, `"day"`, `"trip"`, `"week"`, `"month"`, `"landing"`). An event may have several
#'   rows (a record's set and haul windows). Other columns are kept.
#' @param key The columns that identify an event.
#' @param match_by Columns that must be equal for events to share a unit, and for a ping to belong to it in
#'   [rb_share_catch()] (e.g. an ICES rectangle); `NA` in a unit matches any ping.
#' @param max_days How far back an event with no `start` reaches when the vessel has no earlier event.
#' @param vid,start,end The vessel and interval columns.
#' @return `events` with `start` filled where it was missing, `start_filled`, `unit_id` (integer) and `n_events`
#'   (the events in the unit): the same type, the same rows.
#' @export
#' @examples
#' t0 <- as.POSIXct("2024-03-01", tz = "UTC")
#' h <- function(x) t0 + x * 3600
#' events <- tibble::tibble(
#'   id = c("a", "b", "c", "c", "L"),
#'   vid = 1L,
#'   start = c(h(6), h(0), h(0), h(30), NA),
#'   end = c(h(9), h(24), h(24), h(32), h(48)),
#'   level = c(1L, 2L, 2L, 2L, 4L),
#'   resolution = c("haul", "day", "day", "day", "landing")
#' )
#' # the two day records share a unit (they overlap); "c"'s second window joins it by key
#' rb_add_catch_units(events, key = "id")
rb_add_catch_units <- function(
  events,
  key,
  match_by = NULL,
  max_days = 31,
  vid = "vid",
  start = "start",
  end = "end"
) {
  .rb_check_input(
    events,
    "rb_add_catch_units",
    cols = c(vid = vid, start = start, end = end),
    also = c(stats::setNames(key, key), stats::setNames(match_by, match_by)),
    types = c(start = "time", end = "time")
  )
  lazy_in <- inherits(events, "tbl_lazy")
  con <- .rb_con_for(events)
  .rb_scope()
  given <- events
  if (!lazy_in) {
    events <- .rb_with_rid(events)
  }
  q <- function(x) if (length(x) == 0) character(0) else .rb_q(con, x)
  e_sql <- .rb_in_sql(events, con)
  part <- paste(c(q(vid), q("level"), q(match_by)), collapse = ", ")
  ord <- paste(c(q(start), q(end), q(key)), collapse = ", ")
  # the previous event is the latest that ends strictly earlier: two events of a vessel ending together (two
  # landings in a day, one per buyer) get the same start, so they share a unit, and no tie is left to chance
  # (fishycode decision 102)
  isl_sql <- .rb_fill(
    "
  WITH e0 AS (
    SELECT e.* REPLACE (
             CASE WHEN e.{q(start)} IS NULL
                  THEN coalesce(max(e.{q(end)}) OVER (PARTITION BY e.{q(vid)}, e.{q('level')} ORDER BY e.{q(end)}
                                    RANGE BETWEEN UNBOUNDED PRECEDING AND INTERVAL 1 MICROSECOND PRECEDING),
                                e.{q(end)} - INTERVAL '{max_days}' DAY)
                  ELSE e.{q(start)} END AS {q(start)}),
           e.{q(start)} IS NULL AS start_filled
    FROM ({e_sql}) e),
  e1 AS (SELECT *, max({q(end)}) OVER (PARTITION BY {part} ORDER BY {ord}
                                     ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS rb_prev_end
         FROM e0),
  e2 AS (SELECT *, sum(CASE WHEN rb_prev_end IS NULL OR {q(start)} >= rb_prev_end THEN 1 ELSE 0 END)
                     OVER (PARTITION BY {part} ORDER BY {ord} ROWS UNBOUNDED PRECEDING) AS rb_isl
         FROM e1)
  SELECT * EXCLUDE (rb_prev_end, rb_isl), dense_rank() OVER (ORDER BY {part}, rb_isl) AS rb_island FROM e2"
  )
  # the islands are computed once: the links below and the result must see the same island numbers
  isl_name <- .rb_temp_table(con, isl_sql, keep = lazy_in)
  isl_sql <- sprintf("SELECT * FROM %s", isl_name)
  # islands that share an event key are one unit: their links are few, and are resolved in R
  same <- paste(
    sprintf("a.%s = b.%s", q(c(key, vid, "level")), q(c(key, vid, "level"))),
    collapse = " AND "
  )
  edges <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT DISTINCT a.rb_island AS i, b.rb_island AS j FROM (%s) a JOIN (%s) b ON %s
       WHERE a.rb_island < b.rb_island",
      isl_sql,
      isl_sql,
      same
    )
  )
  map <- .rb_components(edges$i, edges$j)
  m_sql <- if (nrow(map) > 0) {
    .rb_sql(dplyr::tbl(con, .rb_register(con, map)))
  } else {
    "SELECT CAST(NULL AS BIGINT) AS rb_island, CAST(NULL AS BIGINT) AS rb_root WHERE FALSE"
  }
  kcols <- paste(q(key), collapse = ", ")
  sql <- .rb_fill(
    "
  WITH i AS ({isl_sql}),
  u AS (SELECT i.*, coalesce(m.rb_root, i.rb_island) AS rb_u FROM i LEFT JOIN ({m_sql}) m ON m.rb_island = i.rb_island),
  n AS (SELECT rb_u, count(*) AS n_events FROM (SELECT DISTINCT rb_u, {kcols} FROM u) GROUP BY rb_u)
  SELECT u.* EXCLUDE (rb_island, rb_u), dense_rank() OVER (ORDER BY u.rb_u) AS unit_id, n.n_events
  FROM u JOIN n ON n.rb_u = u.rb_u"
  )
  .rb_out(
    con,
    sql,
    lazy_in,
    by = c("unit_id", start),
    given = given,
    rid = ".rb_rid"
  )
}

# Connected components of an undirected edge list (i, j): each node gets the smallest node of its component.
.rb_components <- function(i, j) {
  nodes <- sort(unique(c(i, j)))
  if (length(nodes) == 0) {
    return(data.frame(rb_island = numeric(0), rb_root = numeric(0)))
  }
  ii <- match(i, nodes)
  jj <- match(j, nodes)
  lab <- seq_along(nodes)
  repeat {
    m <- pmin(lab[ii], lab[jj])
    low <- tapply(c(m, m), c(ii, jj), min)
    at <- as.integer(names(low))
    new <- lab
    new[at] <- pmin(new[at], as.integer(low))
    if (identical(new, lab)) {
      break
    }
    lab <- new
  }
  data.frame(rb_island = nodes, rb_root = nodes[lab])
}

#' Share catch over the fishing pings
#'
#' Step 6 of the flow. Each catch unit's catch ([rb_add_catch_units()]) is shared over the unit's fishing pings
#' by effort (their `hours`), species by species. The pings at a coarser level go to its pings not already
#' served at a finer level, and to all its pings if none is left. Catch whose unit holds no fishing ping moves up
#' to the unit of the next level that overlaps it in time (a haul's to its trip, a trip's to its landing), and
#' finally to the vessel's fishing pings in that month. What still finds no fishing ping is kept with no time and
#' basis `"none"`, so the kilograms out equal the kilograms in.
#'
#' Each approach to whether a ping is fishing is a column of its own (`fishing`), and the result says which was
#' used (`approach`), so the catch can be shared under each and compared.
#'
#' `method = "eflalo"` keeps the ICES VMS data call's sharing on EFLALO-shaped tables: `units` is then the
#' events table (`.eid`, `VE_COU`, `VE_REF`, `FT_REF`, `date`, `LE_RECT`, `LE_KG`, `LE_EURO`), `pings` have
#' `.pid`, `VE_COU`, `VE_REF`, `time`, `FT_REF`, `ir` and `state`, and the result is the pings with `LE_KG`,
#' `LE_EURO` and `.how`; see `vignette("catch")`.
#'
#' @param pings A data frame or lazy DuckDB table with `vid`, `time`, the fishing column and `hours`
#'   ([rb_add_effort()]).
#' @param units Units from [rb_add_catch_units()]: `unit_id`, `level`, `resolution`, `vid`, `start`, `end`,
#'   the `key` columns and any `match_by` columns.
#' @param catch A data frame or lazy table with the `key` columns, the species and the weight (and optionally
#'   the value), in one unit of weight.
#' @param method `"units"` (the default) or `"eflalo"`.
#' @param key The columns that tie the catch to the units' events; `"unit_id"` when the catch is already per unit.
#' @param species,kg,value The catch's species, weight and (optional) value columns.
#' @param fishing,hours The pings' fishing column (one approach) and effort column.
#' @param match_by As in [rb_add_catch_units()].
#' @param vid,time,start,end The vessel, time and interval columns.
#' @param ... For `method = "eflalo"`: `remove_diagnostic`, `raise_unallocated`, `raise_unmatched`.
#' @return One row per (fishing ping, unit, species): `vid`, `time`, `unit_id` and `resolution` (the unit the
#'   catch was recorded in; a unit of several kinds of event joins them with `+`, e.g. `"haul+set"`),
#'   `shared_unit_id` and `shared_resolution` (the unit it was shared in: the same, a
#'   coarser one, or `"vessel_month"`), the species, the weight (and value), `catch_basis` (`"derived"`, or
#'   `"none"` for catch with no fishing ping, which has no time) and `approach`. A data frame for data frame input,
#'   a lazy table for lazy input.
#' @export
#' @examples
#' t0 <- as.POSIXct("2024-03-01", tz = "UTC")
#' h <- function(x) t0 + x * 3600
#' pings <- tibble::tibble(vid = 1L, time = h(c(1, 2, 3, 30)), fishing = TRUE, hours = 1)
#' events <- tibble::tibble(
#'   id = c("haul", "trip"), vid = 1L, start = h(c(0, 0)), end = h(c(4, 40)),
#'   level = 1:2, resolution = c("haul", "trip")
#' )
#' units <- rb_add_catch_units(events, key = "id")
#' catch <- tibble::tibble(id = c("haul", "trip"), species = "cod", kg = c(300, 100))
#' rb_share_catch(pings, units, catch, key = "id")
rb_share_catch <- function(
  pings,
  units,
  catch = NULL,
  method = c("units", "eflalo"),
  key = "unit_id",
  species = "species",
  kg = "kg",
  value = NULL,
  fishing = "fishing",
  hours = "hours",
  match_by = NULL,
  vid = "vid",
  time = "time",
  start = "start",
  end = "end",
  ...
) {
  method <- match.arg(method)
  if (method == "eflalo") {
    out <- .rb_share_catch_eflalo(.rb_with_rid(pings), units, ...)
    return(.rb_as_given(out, pings, ".rb_rid"))
  }
  .rb_check_input(
    pings,
    "rb_share_catch",
    cols = c(vid = vid, time = time),
    also = c(
      fishing = fishing,
      hours = hours,
      stats::setNames(match_by, match_by)
    ),
    types = c(time = "time")
  )
  .rb_need_cols(
    units,
    "units",
    c(
      "unit_id",
      "level",
      "resolution",
      vid,
      start,
      end,
      if (!identical(key, "unit_id")) key,
      match_by
    ),
    hint = "Build the units with rb_add_catch_units()."
  )
  if (is.null(catch)) {
    rlang::abort("`catch` must be given for method \"units\".")
  }
  .rb_need_cols(catch, "catch", c(key, species, kg, value))
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  .rb_scope()
  q <- function(x) if (length(x) == 0) character(0) else .rb_q(con, x)
  p_sql <- .rb_in_sql(pings, con)
  u_sql <- .rb_in_sql(units, con)
  c_sql <- .rb_in_sql(catch, con)
  val <- !is.null(value)
  v_sum <- if (val) sprintf(", sum(c.%s) AS rb_val", q(value)) else ""
  # catch per unit, through the events' keys
  c1_sql <- if (identical(key, "unit_id")) {
    sprintf(
      "SELECT c.unit_id, c.%s AS rb_sp, sum(c.%s) AS rb_kg%s FROM (%s) c GROUP BY ALL",
      q(species),
      q(kg),
      v_sum,
      c_sql
    )
  } else {
    on <- paste(sprintf("k.%s = c.%s", q(key), q(key)), collapse = " AND ")
    sprintf(
      "SELECT k.unit_id, c.%s AS rb_sp, sum(c.%s) AS rb_kg%s
       FROM (%s) c JOIN (SELECT DISTINCT %s, unit_id FROM (%s)) k ON %s GROUP BY ALL",
      q(species),
      q(kg),
      v_sum,
      c_sql,
      paste(q(key), collapse = ", "),
      u_sql,
      on
    )
  }
  # catch tied to no unit: kept, with basis none
  c0_sql <- if (identical(key, "unit_id")) {
    sprintf(
      "SELECT c.%s AS rb_sp, c.%s AS rb_kg%s FROM (%s) c WHERE c.unit_id NOT IN (SELECT unit_id FROM (%s))",
      q(species),
      q(kg),
      if (val) sprintf(", c.%s AS rb_val", q(value)) else "",
      c_sql,
      u_sql
    )
  } else {
    on <- paste(sprintf("k.%s = c.%s", q(key), q(key)), collapse = " AND ")
    sprintf(
      "SELECT c.%s AS rb_sp, c.%s AS rb_kg%s FROM (%s) c WHERE NOT EXISTS (SELECT 1 FROM (%s) k WHERE %s)",
      q(species),
      q(kg),
      if (val) sprintf(", c.%s AS rb_val", q(value)) else "",
      c_sql,
      u_sql,
      on
    )
  }
  uu_sql <- .rb_fill(
    "SELECT unit_id, any_value(level) AS lvl, array_to_string(list_sort(list_distinct(list(resolution))), '+') AS res, any_value({q(vid)}) AS rb_vid,
            min({q(start)}) AS u_start, max({q(end)}) AS u_end
     FROM ({u_sql}) GROUP BY unit_id"
  )
  mb <- paste(
    sprintf(
      " AND (u.%s IS NULL OR u.%s = p.%s)",
      q(match_by),
      q(match_by),
      q(match_by)
    ),
    collapse = ""
  )
  m_sql <- .rb_fill(
    "SELECT DISTINCT u.unit_id AS tgt, p.{q(vid)} AS rb_vid, p.{q(time)} AS rb_time, coalesce(p.{q(hours)}, 0) AS w
     FROM ({p_sql}) p JOIN ({u_sql}) u
       ON p.{q(vid)} = u.{q(vid)} AND p.{q(time)} >= u.{q(start)} AND p.{q(time)} < u.{q(end)}{mb}
     WHERE p.{q(fishing)}"
  )

  # the move-up chain: units with catch and no fishing ping, resolved a level at a time
  pend <- DBI::dbGetQuery(
    con,
    .rb_fill(
      "SELECT uu.unit_id AS origin, uu.lvl, uu.rb_vid, uu.u_start, uu.u_end, uu.u_start AS o_start
       FROM ({uu_sql}) uu
       WHERE uu.unit_id IN (SELECT unit_id FROM ({c1_sql})) AND uu.unit_id NOT IN (SELECT tgt FROM ({m_sql}))"
    )
  )
  moves <- data.frame(origin = numeric(0), tgt = numeric(0), tlvl = numeric(0))
  fall <- pend[0, c("origin", "rb_vid", "o_start")]
  while (nrow(pend) > 0) {
    pt <- .rb_sql(dplyr::tbl(con, .rb_register(con, pend)))
    par <- DBI::dbGetQuery(
      con,
      .rb_fill(
        "WITH uu AS ({uu_sql}),
         cand AS (
           SELECT c.origin, uu.unit_id AS parent, uu.lvl AS plvl, uu.u_start AS p_start, uu.u_end AS p_end,
                  epoch(least(u.{q(end)}, c.u_end) - greatest(u.{q(start)}, c.u_start)) AS ov
           FROM ({pt}) c JOIN ({u_sql}) u
             ON u.{q(vid)} = c.rb_vid AND u.{q(start)} <= c.u_end AND u.{q(end)} >= c.u_start
           JOIN uu ON uu.unit_id = u.unit_id
           WHERE uu.lvl > c.lvl)
         SELECT origin, parent, plvl, p_start, p_end, parent IN (SELECT tgt FROM ({m_sql})) AS has_m
         FROM cand QUALIFY row_number() OVER (PARTITION BY origin ORDER BY plvl, ov DESC, parent) = 1"
      )
    )
    found <- par[par$has_m, ]
    moves <- rbind(
      moves,
      data.frame(origin = found$origin, tgt = found$parent, tlvl = found$plvl)
    )
    up <- par[!par$has_m, ]
    nopar <- pend[!pend$origin %in% par$origin, ]
    fall <- rbind(fall, nopar[, c("origin", "rb_vid", "o_start")])
    pend <- merge(
      pend[, c("origin", "rb_vid", "o_start")],
      data.frame(
        origin = up$origin,
        lvl = up$plvl,
        u_start = up$p_start,
        u_end = up$p_end
      )
    )
  }
  # the last resort: the vessel's fishing pings in the month the catch was recorded
  top <- DBI::dbGetQuery(
    con,
    sprintf("SELECT max(lvl) AS l FROM (%s)", uu_sql)
  )$l
  syn <- data.frame(
    tgt = numeric(0),
    rb_vid = fall$rb_vid[0],
    s = fall$o_start[0],
    e = fall$o_start[0]
  )
  if (nrow(fall) > 0) {
    fall$s <- lubridate::floor_date(fall$o_start, "month")
    keys <- unique(fall[, c("rb_vid", "s")])
    keys$tgt <- -seq_len(nrow(keys))
    keys$e <- lubridate::floor_date(keys$s + 32 * 86400, "month")
    fall <- merge(fall, keys, by = c("rb_vid", "s"))
    moves <- rbind(
      moves,
      data.frame(origin = fall$origin, tgt = fall$tgt, tlvl = top + 1)
    )
    syn <- keys[, c("tgt", "rb_vid", "s", "e")]
  }
  mv_sql <- .rb_sql(dplyr::tbl(con, .rb_register(con, moves)))
  sy_sql <- .rb_sql(dplyr::tbl(con, .rb_register(con, syn)))
  vsel <- function(x) if (val) x else ""
  sql <- .rb_fill(
    "
  WITH uu AS ({uu_sql}),
  c1 AS ({c1_sql}),
  m AS ({m_sql}),
  ms AS (SELECT DISTINCT sy.tgt, p.{q(vid)} AS rb_vid, p.{q(time)} AS rb_time, coalesce(p.{q(hours)}, 0) AS w
         FROM ({p_sql}) p JOIN ({sy_sql}) sy ON p.{q(vid)} = sy.rb_vid AND p.{q(time)} >= sy.s AND p.{q(time)} < sy.e
         WHERE p.{q(fishing)}),
  mt AS (SELECT * FROM m UNION ALL SELECT * FROM ms),
  tg AS (SELECT c.unit_id AS origin, coalesce(mv.tgt, c.unit_id) AS tgt, coalesce(mv.tlvl, uu.lvl) AS tlvl
         FROM (SELECT DISTINCT unit_id FROM c1) c JOIN uu ON uu.unit_id = c.unit_id
         LEFT JOIN ({mv_sql}) mv ON mv.origin = c.unit_id),
  tl AS (SELECT DISTINCT tgt, tlvl FROM tg),
  cl AS (SELECT mt.rb_vid, mt.rb_time, min(tl.tlvl) AS claim FROM mt JOIN tl ON tl.tgt = mt.tgt GROUP BY ALL),
  el AS (SELECT mt.*, cl.claim >= tl.tlvl AS free
         FROM mt JOIN tl ON tl.tgt = mt.tgt JOIN cl ON cl.rb_vid = mt.rb_vid AND cl.rb_time = mt.rb_time),
  el2 AS (SELECT tgt, rb_vid, rb_time, w FROM (SELECT *, bool_or(free) OVER (PARTITION BY tgt) AS any_free FROM el)
          WHERE free OR NOT any_free),
  tw AS (SELECT tgt, sum(w) AS ww, count(*) AS n FROM el2 GROUP BY tgt),
  sh AS (SELECT tg.origin, tg.tgt, e.rb_vid, e.rb_time, c1.rb_sp,
                c1.rb_kg * CASE WHEN tw.ww > 0 THEN e.w / tw.ww ELSE 1.0 / tw.n END AS rb_kg
                {vsel(', c1.rb_val * CASE WHEN tw.ww > 0 THEN e.w / tw.ww ELSE 1.0 / tw.n END AS rb_val')},
                'derived' AS catch_basis
         FROM tg JOIN c1 ON c1.unit_id = tg.origin JOIN el2 e ON e.tgt = tg.tgt JOIN tw ON tw.tgt = tg.tgt),
  un AS (SELECT tg.origin, tg.tgt, uu.rb_vid, CAST(NULL AS TIMESTAMP) AS rb_time, c1.rb_sp, c1.rb_kg
                {vsel(', c1.rb_val')}, 'none' AS catch_basis
         FROM tg JOIN c1 ON c1.unit_id = tg.origin JOIN uu ON uu.unit_id = tg.origin
         WHERE tg.tgt NOT IN (SELECT tgt FROM tw)),
  nu AS (SELECT CAST(NULL AS BIGINT) AS origin, CAST(NULL AS BIGINT) AS tgt, NULL AS rb_vid,
                CAST(NULL AS TIMESTAMP) AS rb_time, rb_sp, rb_kg {vsel(', rb_val')}, 'none' AS catch_basis
         FROM ({c0_sql})),
  al AS (SELECT * FROM sh UNION ALL BY NAME SELECT * FROM un UNION ALL BY NAME SELECT * FROM nu)
  SELECT al.rb_vid AS {q(vid)}, al.rb_time AS {q(time)}, al.origin AS unit_id, o.res AS resolution,
         CASE WHEN al.tgt < 0 THEN NULL ELSE al.tgt END AS shared_unit_id,
         CASE WHEN al.tgt < 0 THEN 'vessel_month' ELSE t.res END AS shared_resolution,
         al.rb_sp AS {q(species)}, al.rb_kg AS {q(kg)} {vsel(paste0(', al.rb_val AS ', q(value)))},
         al.catch_basis, '{fishing}' AS approach
  FROM al LEFT JOIN uu o ON o.unit_id = al.origin LEFT JOIN uu t ON t.unit_id = al.tgt"
  )
  .rb_out(
    con,
    sql,
    lazy_in,
    by = c(vid, time, "unit_id", species),
    given = pings
  )
}

# Stop with a clear message when a table other than a step's main input lacks columns.
.rb_need_cols <- function(
  x,
  arg,
  need,
  hint = NULL,
  call = rlang::caller_env()
) {
  have <- if (inherits(x, "tbl_lazy")) colnames(x) else names(x)
  miss <- setdiff(unique(need), have)
  if (length(miss) > 0) {
    rlang::abort(
      c(
        sprintf(
          "`%s` must have the columns %s.",
          arg,
          paste0("`", unique(need), "`", collapse = ", ")
        ),
        x = sprintf("Missing: %s.", paste0("`", miss, "`", collapse = ", ")),
        if (!is.null(hint)) c(i = hint)
      ),
      call = call
    )
  }
}

# The ICES VMS datacall's catch sharing, kept as rb_share_catch(method = "eflalo") (fishycode plan 013, item 2).
# EFLALO-shaped: pings with .pid, VE_COU, VE_REF, time, FT_REF, ir (ICES rectangle) and state (1 = fishing);
# events with .eid, VE_COU, VE_REF, FT_REF, date, LE_RECT, LE_KG and LE_EURO. Each event's weight and value is
# split evenly over the fishing pings of its trip at the finest level its information allows, and only to pings
# not already allocated at a finer level: trip + rectangle + day, then trip + rectangle, then trip. Returns the
# pings with LE_KG, LE_EURO and .how (the level that served them); remove_diagnostic = FALSE keeps more columns.
# raise_unallocated and raise_unmatched are reserved and not implemented.
.rb_share_catch_eflalo <- function(
  pings,
  events,
  remove_diagnostic = TRUE,
  raise_unallocated = FALSE,
  raise_unmatched = FALSE
) {
  # Input checks ----
  # Check for required columns and correct data types in 'pings' and 'events' data frames
  required_pings <- c(
    ".pid",
    "VE_COU",
    "VE_REF",
    "time",
    "FT_REF",
    "ir",
    "state"
  )
  required_events <- c(
    ".eid",
    "VE_COU",
    "VE_REF",
    "FT_REF",
    "date",
    "LE_RECT",
    "LE_KG",
    "LE_EURO"
  )

  # Check for columns
  missing_pings <- setdiff(required_pings, colnames(pings))
  if (length(missing_pings) > 0) {
    stop(
      "Missing required columns in pings: ",
      paste(missing_pings, collapse = ", ")
    )
  }
  missing_events <- setdiff(required_events, colnames(events))
  if (length(missing_events) > 0) {
    stop(
      "Missing required columns in events: ",
      paste(missing_events, collapse = ", ")
    )
  }

  # Check classes for pings
  if (!is.integer(pings$.pid)) {
    stop(".pid in pings must be integer")
  }
  if (!is.character(pings$VE_COU)) {
    stop("VE_COU in pings must be character")
  }
  if (!is.character(pings$VE_REF)) {
    stop("VE_REF in pings must be character")
  }
  if (!inherits(pings$time, "POSIXct")) {
    stop("time in pings must be POSIXct/datetime")
  }
  if (!is.character(pings$FT_REF)) {
    stop("FT_REF in pings must be character")
  }
  if (!is.character(pings$ir)) {
    stop("ir in pings must be character")
  }
  if (!is.integer(pings$state)) {
    stop("state in pings must be integer")
  }
  if (!all(pings$state %in% c(0, 1))) {
    stop("state in pings must be 0 or 1")
  }

  # Check classes for events
  if (!is.integer(events$.eid)) {
    stop(".eid in events must be integer")
  }
  if (!is.character(events$VE_COU)) {
    stop("VE_COU in events must be character")
  }
  if (!is.character(events$VE_REF)) {
    stop("VE_REF in events must be character")
  }
  if (!is.character(events$FT_REF)) {
    stop("FT_REF in events must be character")
  }
  if (!inherits(events$date, "Date")) {
    stop("date in events must be Date")
  }
  if (!is.character(events$LE_RECT)) {
    stop("LE_RECT in events must be character")
  }
  if (!is.numeric(events$LE_KG)) {
    stop("LE_KG in events must be numeric")
  }
  if (!is.numeric(events$LE_EURO)) {
    stop("LE_EURO in events must be numeric")
  }

  # Prepare extra columns ----
  pings <- pings |> dplyr::mutate(date = lubridate::as_date(time))
  events <-
    events |>
    dplyr::mutate(state = 1L)

  # Mapping step ----
  # Determine at which allocation step each event should be joined
  # Create step_plan to assign events to allocation steps based on available keys
  step_plan <-
    events |>
    dplyr::select(.eid, VE_COU, VE_REF, FT_REF, date, LE_RECT) |>
    dplyr::left_join(
      pings |>
        dplyr::filter(state == 1) |>
        dplyr::select(VE_COU, VE_REF, FT_REF, date, LE_RECT = ir) |>
        dplyr::distinct() |>
        dplyr::mutate(vms = "yes"),
      by = dplyr::join_by(VE_COU, VE_REF, FT_REF),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      .step = dplyr::case_when(
        date.x == date.y & LE_RECT.x == LE_RECT.y & vms == "yes" ~ 1L,
        is.na(date.x) & LE_RECT.x == LE_RECT.y & vms == "yes" ~ 2L,
        vms == "yes" ~ 3L,
        .default = 9999L
      )
    ) |>
    dplyr::arrange(.step) |>
    dplyr::select(.eid, .step) |>
    dplyr::distinct(.eid, .keep_all = TRUE)

  events <-
    events |>
    dplyr::left_join(step_plan, by = dplyr::join_by(.eid))

  # Keep track of .pid that are not mapped after each step
  PID_left <- pings$.pid

  # Step 1: Join by trip + rectangle + day -------------------------------------
  # Allocate events to pings at the most specific (date & rectangle) level
  events1 <-
    events |>
    dplyr::filter(.step == 1) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, date, LE_RECT, state) |>
    dplyr::summarise(
      LE_KG = sum(LE_KG, na.rm = TRUE),
      LE_EURO = sum(LE_EURO, na.rm = TRUE),
      .groups = "drop"
    )

  pings1 <- pings |>
    dplyr::inner_join(
      events1,
      by = c("VE_COU", "VE_REF", "FT_REF", "ir" = "LE_RECT", "date", "state")
    ) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, ir, date) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = "1 trip_rectangle_day"
    ) |>
    dplyr::ungroup()

  PID_left <- PID_left[!PID_left %in% pings1$.pid]

  # Step 2: Join by trip + rectangle -------------------------------------------
  # Allocate remaining events to pings at the rectangle level

  pings_left <-
    pings |>
    dplyr::filter(.pid %in% PID_left)

  events2 <-
    events |>
    dplyr::filter(.step == 2L) |>
    # avoid many-to-many among other things
    dplyr::group_by(VE_COU, VE_REF, FT_REF, LE_RECT, state) |>
    dplyr::summarise(
      LE_KG = sum(LE_KG, na.rm = TRUE),
      LE_EURO = sum(LE_EURO, na.rm = TRUE),
      .groups = "drop"
    )

  pings2 <-
    pings_left |>
    dplyr::select(-date) |>
    dplyr::inner_join(
      events2,
      by = c("VE_COU", "VE_REF", "FT_REF", "ir" = "LE_RECT", "state")
    ) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, ir, state) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = "2 trip_rectangle"
    ) |>
    dplyr::ungroup()

  PID_left <- PID_left[!PID_left %in% pings2$.pid]

  # Step 3: Join by trip -------------------------------------------------------
  # Allocate remaining events to pings at the trip level

  pings_left <-
    pings |>
    dplyr::filter(.pid %in% PID_left)
  events3 <-
    events |>
    dplyr::filter(.step == 3L) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, state) |>
    dplyr::summarise(
      LE_KG = sum(LE_KG, na.rm = TRUE),
      LE_EURO = sum(LE_EURO, na.rm = TRUE),
      .groups = "drop"
    )
  pings3 <-
    pings_left |>
    dplyr::left_join(events3, by = c("VE_COU", "VE_REF", "FT_REF", "state")) |>
    dplyr::group_by(VE_COU, VE_REF, FT_REF, state) |>
    dplyr::mutate(
      .wt = ifelse(state == 1, 1 / dplyr::n(), NA_real_),
      LE_KG = LE_KG * .wt,
      LE_EURO = LE_EURO * .wt,
      .how = ifelse(state == 1, "3 trip", NA)
    ) |>
    dplyr::ungroup()

  # Combine allocation results from all steps into a single output data frame ----
  out <-
    dplyr::bind_rows(
      pings1 |> dplyr::mutate(.how = as.character(.how)),
      pings2 |> dplyr::mutate(.how = as.character(.how)),
      pings3 |> dplyr::mutate(.how = as.character(.how))
    ) |>
    dplyr::arrange(.pid)

  # Reserved: raise trips whose events were only partly allocated (not implemented)
  if (raise_unallocated) {
    # Under consideration
    # Will likely be used to raise all catches within a trip with
    #  those that were not allocated
  }

  # Reserved: share events whose trip has no match in pings (not implemented)
  if (raise_unmatched) {
    # Under consideration
    # Will likely be used to raise any event trip catches that are not matched
    # with trips in the pings data - likely first by vessel then by month, ...
  }

  # Remove diagnostic columns if requested
  if (remove_diagnostic) {
    out <-
      out |>
      dplyr::select(-c(date, .wt))
  }

  # Output checks --------------------------------------------------------------
  # Validate that output matches input in row count and total LE_KG allocation
  if (nrow(pings) != nrow(out)) {
    stop("Number of rows in output does not match input pings - File an issue")
  }

  kg_out <- sum(out$LE_KG, na.rm = TRUE)
  kg_in <- sum(events$LE_KG, na.rm = TRUE)

  if (abs(kg_out - kg_in) > 1e-6) {
    warning(paste("kg in: ", round(kg_in), " kg out: ", round(kg_out)))
    warning("Sum of LE_KG in output does not match events - File an issue")
  }

  return(out)
}
#' Sum the columns whose names share a prefix
#'
#' Row-wise sums of the numeric columns whose names start with a prefix,
#' added as a column named by the prefix itself; the summed columns are
#' optionally removed. For eflalo-style catch tables, where the per-species
#' columns (`LE_KG_COD`, `LE_KG_HAD`, ...) sum to the total catch `LE_KG`.
#'
#' @param d A data frame (typically an eflalo data frame).
#' @param prefix A string (default "LE_KG"). Prefix of column names to sum across. All columns starting with this string will be included in the sum.
#' @param remove Logical (default TRUE). If TRUE, removes all original columns whose names start with \code{prefix} after summing.
#'
#' @return
#' The input data frame with a new column named \code{prefix} containing the row sums. If \code{remove = TRUE}, the original columns are removed.
#'
#' @details
#' The function will error if no columns match \code{prefix}, or if any matching columns are not numeric.
#'
#' @examples
#' # Example: Summing all columns starting with 'catch_'
#' df <- data.frame(
#'   id = 1:3,
#'   LE_KG_COD = c(1, 2, 3),
#'   LE_KG_HAD = c(0, 1, 0),
#'   LE_KG_HER = c(2, 1, 2)
#' )
#' rb_sum_across(df, prefix = "LE_KG")
#'
#' # Keep original columns
#' rb_sum_across(df, prefix = "LE_KG", remove = FALSE)
#'
#' @export
rb_sum_across <- function(d, prefix = "LE_KG", remove = TRUE) {
  # Input checks
  if (is.null(prefix) || !is.character(prefix) || length(prefix) != 1) {
    stop("prefix must be a single character string.")
  }
  cols_to_sum <- grep(paste0("^", prefix), names(d), value = TRUE)
  if (length(cols_to_sum) == 0) {
    stop("No columns found starting with '", prefix, "'.")
  }
  if (!all(sapply(d[cols_to_sum], is.numeric))) {
    stop("All columns to sum must be numeric.")
  }

  d <- dplyr::mutate(
    d,
    !!prefix := rowSums(dplyr::across(dplyr::starts_with(prefix)))
  )

  if (remove) {
    # Remove only the original columns, not the new sum column
    cols_to_remove <- setdiff(cols_to_sum, prefix)
    d <- dplyr::select(d, -dplyr::all_of(cols_to_remove))
  }
  return(d)
}
