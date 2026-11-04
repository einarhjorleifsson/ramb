# flag_ping.R — step 0 of the fishing-activity flow: flag pings that are not usable positions.
#
# The three flaggers share one label column, `ping_flag`: NA for a usable ping, otherwise the reason
# ("invalid", "duplicate", "shifted", or the impossible-position stage that caught it), and its evidence,
# `ping_flag_basis` (fishycode plan 062's cascade: each flag takes the best evidence the ping has and says which).
# Each tests only rows not yet flagged, so a chain applies them in order:
#
#   pings |> rb_flag_ping_invalid() |> rb_flag_ping_duplicate(priority = ...) |> rb_flag_ping_impossible()
#
# No row is dropped; the caller decides what to set aside (fishycode plan 013).

# Column arguments ---------------------------------------------------------------
# The functions work on the grammar's names (vid, time, lon, lat). A caller with other names passes them
# (`vid = vessel_id`); they are renamed in, and back out on the result.
.rb_cols <- function(...) {
  q <- rlang::enquos(...)
  vapply(q, rlang::as_name, character(1))
}
.rb_std_in <- function(x, map) {
  ren <- map[map != names(map)]
  if (!length(ren)) {
    return(x)
  }
  clash <- intersect(names(ren), colnames(x))
  if (length(clash)) {
    stop(
      "Column(s) ",
      paste(clash, collapse = ", "),
      " already exist; rename them first.",
      call. = FALSE
    )
  }
  dplyr::rename(x, !!!rlang::syms(ren))
}
.rb_std_out <- function(x, map) {
  ren <- map[map != names(map)]
  if (!length(ren)) {
    return(x)
  }
  dplyr::rename(x, !!!stats::setNames(rlang::syms(names(ren)), ren))
}
# A column of NAs of a type: in DuckDB a bare NULL (dbplyr's NA) is of type NULL, which the lazy route cannot give
# back as it was.
.rb_na_col <- function(x, nm, type = c("VARCHAR", "DOUBLE")) {
  type <- match.arg(type)
  v <- if (inherits(x, "tbl_lazy")) {
    dplyr::sql(sprintf("CAST(NULL AS %s)", type))
  } else if (type == "VARCHAR") {
    NA_character_
  } else {
    NA_real_
  }
  dplyr::mutate(x, !!nm := !!v)
}
.rb_flag_init <- function(x) {
  if (!"ping_flag" %in% colnames(x)) {
    x <- .rb_na_col(x, "ping_flag")
  }
  if (!"ping_flag_basis" %in% colnames(x)) {
    # a flag made before the basis existed (ramb before 2026.11.04) has none
    x <- .rb_na_col(x, "ping_flag_basis")
  }
  x
}

# The methods withdrawn by fishycode plan 062 (Decided k, 2026-11-04): each warns once per session, and goes in the
# next release. Their scores on ramb's planted faults are in fishycode's decision 276.
.rb_withdrawn_method <- function(method) {
  key <- paste0("rb_flag_ping_impossible:", method)
  if (!isTRUE(.rb_warned[[key]])) {
    assign(key, TRUE, envir = .rb_warned)
    rlang::warn(
      sprintf(
        "`rb_flag_ping_impossible(method = \"%s\")` will be removed in the next release; use \"clean\", \"forward\" or \"local\".",
        method
      ),
      class = "ramb_withdrawn"
    )
  }
  invisible(NULL)
}

# Lazy tables: label in batches of whole vessels, write them whole ---------------------------------
# For rules that are sequential along a track and so cannot run in DuckDB itself. Each batch of whole vessels is
# collected, sorted in a total order (vessel, time, lon, lat, then every other column), labelled in R by `fun` (a
# data-frame function that updates `ping_flag` and may add columns), and written whole to a temporary parquet file,
# each input column cast back to its type; the result reads those files, still lazy. The pings with no vessel are
# written unlabelled. The files live until the R session ends.
#
# Until 2026.11.04 only the labels were written, and joined back to the input by each ping's row number within its
# vessel: on a year of 72 M pings the join, a window per flagger, cost more than the flagging (fishycode plan 062,
# step 1 test (ii): 155 s against 123 s), and two copies alike in every sorted column could swap labels between the
# routes. A table with a column R cannot carry and give back as it was (a list, a struct, a blob, a time of day) still
# goes that way. `cols` and `order_by` serve that route only; `extra` names the columns `fun` adds there.
.rb_round_trip_types <- paste0(
  "^(BOOLEAN|TINYINT|SMALLINT|INTEGER|BIGINT|UTINYINT|USMALLINT|UINTEGER|UBIGINT|FLOAT|DOUBLE|DECIMAL\\(.*\\)|",
  "VARCHAR|DATE|TIMESTAMP|TIMESTAMP WITH TIME ZONE|TIMESTAMP_S|TIMESTAMP_MS|ENUM\\(.*\\))$"
)
.rb_label_lazy <- function(
  x,
  fun,
  cols,
  order_by,
  batch_pings = 5e6,
  what = "label",
  extra = character(0)
) {
  con <- dbplyr::remote_con(x)
  schema <- DBI::dbGetQuery(con, paste("DESCRIBE", .rb_sql(x)))
  if (!all(grepl(.rb_round_trip_types, schema$column_type))) {
    return(.rb_label_lazy_join(
      x,
      fun,
      cols,
      order_by,
      batch_pings,
      what,
      extra
    ))
  }
  vn <- x |>
    dplyr::filter(!is.na(vid)) |>
    dplyr::count(vid) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(n))
  if (!nrow(vn)) {
    stop(what, ": the table has no vessels.", call. = FALSE)
  }
  batches <- split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  dir <- tempfile("rb_labels_")
  dir.create(dir)
  q <- function(nm) DBI::dbQuoteIdentifier(con, nm)
  # each input column as it was: a time with a zone comes back from R as UTC
  cast <- function(nm, type) {
    if (type == "TIMESTAMP WITH TIME ZONE") {
      sprintf("timezone('UTC', CAST(%s AS TIMESTAMP)) AS %s", q(nm), q(nm))
    } else {
      sprintf("CAST(%s AS %s) AS %s", q(nm), type, q(nm))
    }
  }
  sel_in <- paste(
    mapply(cast, schema$column_name, schema$column_type),
    collapse = ", "
  )
  front <- c("vid", "time", "lon", "lat")
  added <- NULL
  for (i in seq_along(batches)) {
    d <- x |>
      dplyr::filter(vid %in% !!batches[[i]]) |>
      dplyr::collect()
    d <- as.data.frame(d)
    d <- d[
      do.call(
        order,
        c(
          unname(as.list(d[c(front, setdiff(names(d), front))])),
          method = "radix"
        )
      ),
      ,
      drop = FALSE
    ]
    out <- fun(d)
    if (is.null(added)) {
      added <- setdiff(names(out), schema$column_name)
    }
    out <- out[, c(schema$column_name, added), drop = FALSE]
    nm <- paste0("rb_lab_", i)
    # Register, write with COPY on the same connection, release: a frame left registered in a loop
    # accumulates (fishycode R/write_parquet.R: 48 GB spilled before that was found).
    duckdb::duckdb_register(con, nm, out)
    DBI::dbExecute(
      con,
      sprintf(
        "COPY (SELECT %s%s FROM %s) TO '%s' (FORMAT parquet)",
        sel_in,
        if (length(added)) {
          paste0(", ", paste(q(added), collapse = ", "))
        } else {
          ""
        },
        nm,
        file.path(dir, sprintf("b%05d.parquet", i))
      )
    )
    duckdb::duckdb_unregister(con, nm)
  }
  # the pings with no vessel, unlabelled, the added columns typed as the batches wrote them
  types <- DBI::dbGetQuery(
    con,
    sprintf(
      "DESCRIBE SELECT * FROM read_parquet('%s')",
      file.path(dir, "b00001.parquet")
    )
  )
  types <- stats::setNames(types$column_type, types$column_name)
  DBI::dbExecute(
    con,
    sprintf(
      "COPY (SELECT * %s FROM (%s) WHERE vid IS NULL) TO '%s' (FORMAT parquet)",
      paste0(
        vapply(
          added,
          function(a) sprintf(", CAST(NULL AS %s) AS %s", types[[a]], q(a)),
          ""
        ),
        collapse = ""
      ),
      .rb_sql(x),
      file.path(dir, "novid.parquet")
    )
  )
  dplyr::tbl(
    con,
    dplyr::sql(sprintf("SELECT * FROM read_parquet('%s/*.parquet')", dir))
  )
}

# The route of ramb before 2026.11.04, for a table with a column R cannot give back as it was: the labels written
# alone, and joined back by each ping's row number within its vessel, in `order_by` order, numbered the same way in
# DuckDB and in the batch.
.rb_label_lazy_join <- function(
  x,
  fun,
  cols,
  order_by,
  batch_pings = 5e6,
  what = "label",
  extra = character(0)
) {
  con <- dbplyr::remote_con(x)
  ob <- rlang::syms(order_by)
  keyed <- x |>
    dplyr::group_by(vid) |>
    dbplyr::window_order(!!!ob) |>
    dplyr::mutate(.rb_rn = dplyr::row_number()) |>
    dplyr::ungroup()
  vn <- x |>
    dplyr::count(vid) |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(n))
  if (!nrow(vn)) {
    stop(what, ": the table has no vessels.", call. = FALSE)
  }
  labs <- c("ping_flag", "ping_flag_basis", extra)
  batches <- split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  dir <- tempfile("rb_labels_")
  dir.create(dir)
  for (i in seq_along(batches)) {
    g <- batches[[i]]
    d <- x |>
      dplyr::filter(vid %in% g) |>
      dplyr::select(dplyr::all_of(unique(c(
        "vid",
        cols,
        intersect(labs, colnames(x))
      )))) |>
      dplyr::group_by(vid) |>
      dbplyr::window_order(!!!ob) |>
      dplyr::mutate(.rb_rn = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      dplyr::arrange(vid, !!!ob)
    lab <- fun(d)[, c("vid", ".rb_rn", labs)]
    lab$.rb_rn <- as.numeric(lab$.rb_rn)
    nm <- paste0("rb_lab_", i)
    duckdb::duckdb_register(con, nm, lab)
    DBI::dbExecute(
      con,
      sprintf(
        "COPY %s TO '%s' (FORMAT parquet)",
        nm,
        file.path(dir, sprintf("b%05d.parquet", i))
      )
    )
    duckdb::duckdb_unregister(con, nm)
  }
  labels <- dplyr::tbl(
    con,
    dplyr::sql(sprintf("SELECT * FROM read_parquet('%s/*.parquet')", dir))
  )
  keyed |>
    dplyr::select(-dplyr::any_of(labs)) |>
    dplyr::left_join(labels, by = c("vid", ".rb_rn")) |>
    dplyr::select(-".rb_rn") |>
    dbplyr::window_order()
}

#' Flag invalid ping positions
#'
#' Step 0 of the fishing-activity flow. A position that is a placeholder, not a fix, is flagged
#' `"invalid"` in `ping_flag`, unless the row already carries a flag. No row is dropped.
#'
#' @param x A data frame or a lazy DuckDB table with `lon` and `lat`.
#' @param rules Which tests to apply: `"lon_eq_lat"` (longitude equal to latitude, which also catches
#'   0, 0; a feed that wrote the latitude into the longitude), `"out_of_range"` (|lon| > 180 or
#'   |lat| > 90), and, not by default, `"after_receipt"`: a ping cannot be received before it is made, so one
#'   stamped more than `ahead_s` after its `receipt` comes from a clock that runs fast, flagged `"after receipt"`
#'   (fishycode plan 060 §8; Iceland's STK holds 266 thousand pings of 2009 stamped over a minute after their
#'   receipt). A clock a second or two fast is common and harmless.
#' @param lon,lat The position columns, if not named `lon` and `lat`.
#' @param time,receipt For `"after_receipt"`: the ping's time and the time it was received (date-times).
#' @param ahead_s For `"after_receipt"`: seconds after the receipt that are tolerated.
#'
#' @return `x` with `ping_flag` and `ping_flag_basis` added or updated (`"recorded"`: the ping's own values decide):
#'   the same type, the same rows.
#' @family flag pings
#' @export
#'
#' @examples
#' d <- data.frame(lon = c(-20, 64.1, 0, 200), lat = c(64, 64.1, 0, 64))
#' rb_flag_ping_invalid(d)
rb_flag_ping_invalid <- function(
  x,
  rules = c("lon_eq_lat", "out_of_range"),
  lon = lon,
  lat = lat,
  time = time,
  receipt = receipt,
  ahead_s = 60
) {
  rules <- match.arg(
    rules,
    c("lon_eq_lat", "out_of_range", "after_receipt"),
    several.ok = TRUE
  )
  aft <- "after_receipt" %in% rules
  .rb_check_input(
    x,
    "rb_flag_ping_invalid",
    cols = c(
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = if (aft) {
      c(
        time = rlang::as_name(rlang::enquo(time)),
        receipt = rlang::as_name(rlang::enquo(receipt))
      )
    } else {
      character(0)
    },
    types = c(
      lon = "numeric",
      lat = "numeric",
      if (aft) c(time = "time", receipt = "time")
    )
  )
  stopifnot(is.numeric(ahead_s), length(ahead_s) == 1, ahead_s >= 0)
  map <- .rb_cols(lon = {{ lon }}, lat = {{ lat }})
  if (aft) {
    map <- c(map, .rb_cols(time = {{ time }}, receipt = {{ receipt }}))
  }
  x <- .rb_flag_init(.rb_std_in(x, map))
  eq <- "lon_eq_lat" %in% rules
  oor <- "out_of_range" %in% rules
  # seconds after the receipt, in each engine's own arithmetic
  if (aft) {
    x <- if (inherits(x, "tbl_lazy")) {
      dplyr::mutate(x, .rb_after = dplyr::sql("epoch(time) - epoch(receipt)"))
    } else {
      dplyr::mutate(x, .rb_after = as.numeric(time) - as.numeric(receipt))
    }
  } else {
    x <- dplyr::mutate(x, .rb_after = NA_real_)
  }
  # the ping's own recorded values decide: basis "recorded"
  x <- dplyr::mutate(
    x,
    .rb_new = dplyr::case_when(
      !is.na(ping_flag) ~ NA_character_,
      !!eq & lon == lat ~ "invalid",
      !!oor & (abs(lon) > 180 | abs(lat) > 90) ~ "invalid",
      !!aft & !is.na(.rb_after) & .rb_after > !!ahead_s ~ "after receipt",
      TRUE ~ NA_character_
    ),
    ping_flag_basis = dplyr::if_else(
      is.na(.rb_new),
      ping_flag_basis,
      "recorded"
    ),
    ping_flag = dplyr::coalesce(ping_flag, .rb_new)
  )
  x <- dplyr::select(x, -".rb_after", -".rb_new")
  .rb_std_out(x, map)
}

#' Flag a copy of a report
#'
#' Step 0 of the fishing-activity flow. A report a vessel's track holds twice is flagged in `ping_flag`, with what
#' decided it in `ping_flag_basis`. Only rows not yet flagged are tested, and they are not used as anchors. No row is
#' dropped.
#'
#' Two rules, the second with a recorded speed (fishycode plan 062's cascade: each copy is decided by the best
#' evidence there is):
#'
#' * **Across feeds, at one time.** When several feeds (a national AIS network and a commercial one) report the
#'   same fix, the lower-priority feed's copy is `"duplicate"` (basis `"priority"`): a ping of the same vessel from a
#'   higher-priority feed lies within `max_dt_s` seconds and the distance `kn_max` covers in that time, among the
#'   `n_neighbours` neighbours on each side in time order.
#' * **One report twice, at any time** (with `speed`, and `course` where the feed has it). A copy repeats a report's
#'   position (within `copy_m` metres), its recorded speed (exactly) and its course (within 0.5 degrees); a vessel
#'   that passes the same place again does not repeat its speed and course to the decimal. Pings moving under 1 knot
#'   are not compared. Of such a pair, in one feed and `near_s` seconds apart or less, the later is `"duplicate"`
#'   (basis `"recorded"`): the report sent twice. In two feeds, the lower-priority feed's (basis `"priority"`):
#'   `"duplicate"` within `max_dt_s` of the other, else `"shifted"`, a copy stamped at another time. In one feed
#'   further apart, which one is the copy is decided by a second feed, a witness that places the vessel at one of
#'   them (a ping of another feed within 120 s and 300 m plus what its recorded speed covers in the time between): the
#'   other is `"shifted"` (basis `"witness"`). The pairs of one run, one vessel at one shift, are decided together,
#'   and only when every pair that a witness decides agrees. Otherwise both pings stay, unflagged, and
#'   `ping_repeat_dt_s` gives each the seconds to the other: a label, not a removal. On one feed with no time of
#'   receipt, which of two identical reports is the copy is not in the data; the track around is no witness, since
#'   where copies are many it fits the copy as well as the true ping (fishycode decision 276).
#'
#' Take the copies out before [rb_flag_ping_impossible()]: a true ping between two copies looks as wrong as a copy,
#' and on ramb's planted faults the copies were most of the good fishing pings its `"local"` method took.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon`, `lat` and the feed column.
#' @param priority The feeds, highest priority first. A feed not listed ranks last. No default: the
#'   order is a judgement about the feeds. It may be left out where the data hold one feed.
#' @param feed The column naming the feed.
#' @param max_dt_s,kn_max,n_neighbours Seconds apart, speed in knots that sets the distance, neighbours compared.
#' @param speed,course The columns of the recorded speed over ground (knots) and course over ground (degrees), for the
#'   second rule. Default `NULL`: the first rule only. A speed of 102.3 knots or more counts as none.
#' @param copy_m,near_s For the second rule: how far apart two reports may lie and still be one (metres), and how
#'   far apart in time a report sent twice in one feed may be (seconds).
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` and `ping_flag_basis` added or updated, and with `speed`, `ping_repeat_dt_s`: the same
#'   type, the same rows.
#' @family flag pings
#' @export
#'
#' @examples
#' t0 <- as.POSIXct("2024-01-01", tz = "UTC")
#' # one feed; a stretch of the track sent again a day later, stamped with the later time
#' d <- data.frame(
#'   vid = 1,
#'   time = t0 + c(60 * 0:4, 86400 + 60 * 1:2),
#'   lon = -20 + 0.004 * c(0:4, 1:2),
#'   lat = 64,
#'   speed = c(10.1, 10.2, 10.3, 10.2, 10.1, 10.2, 10.3),
#'   course = 90,
#'   provider = "a"
#' )
#' rb_flag_ping_duplicate(d, speed = speed, course = course)[, c("time", "ping_flag", "ping_repeat_dt_s")]
rb_flag_ping_duplicate <- function(
  x,
  priority,
  feed = provider,
  max_dt_s = 10,
  kn_max = 25,
  n_neighbours = 3,
  speed = NULL,
  course = NULL,
  copy_m = 20,
  near_s = 2,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  batch_pings = 5e6
) {
  speed_quo <- rlang::enquo(speed)
  course_quo <- rlang::enquo(course)
  has_speed <- !rlang::quo_is_null(speed_quo)
  has_course <- !rlang::quo_is_null(course_quo)
  if (has_course && !has_speed) {
    stop(
      "`course` is read with `speed`, by the rule for one report twice.",
      call. = FALSE
    )
  }
  .rb_check_input(
    x,
    "rb_flag_ping_duplicate",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = c(
      feed = rlang::as_name(rlang::enquo(feed)),
      if (has_speed) c(speed = rlang::as_name(speed_quo)),
      if (has_course) c(course = rlang::as_name(course_quo))
    ),
    types = c(
      time = "time",
      lon = "numeric",
      lat = "numeric",
      if (has_speed) c(speed = "numeric"),
      if (has_course) c(course = "numeric")
    )
  )
  .rb_warn_unharmonised(x, "rb_flag_ping_duplicate")
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }},
    feed = {{ feed }}
  )
  if (has_speed) {
    map <- c(map, speed = rlang::as_name(speed_quo))
  }
  if (has_course) {
    map <- c(map, course = rlang::as_name(course_quo))
  }
  if (missing(priority)) {
    fc <- map[["feed"]]
    feeds <- if (inherits(x, "tbl_lazy")) {
      dplyr::pull(dplyr::distinct(dplyr::select(x, dplyr::all_of(fc))), 1)
    } else {
      unique(x[[fc]])
    }
    feeds <- stats::na.omit(as.character(feeds))
    if (length(feeds) > 1) {
      stop(
        "`priority` is required: the feeds, highest priority first.",
        call. = FALSE
      )
    }
    priority <- feeds
  }
  x <- .rb_flag_init(.rb_std_in(x, map))
  if (has_speed && !"ping_repeat_dt_s" %in% colnames(x)) {
    x <- .rb_na_col(x, "ping_repeat_dt_s", "DOUBLE")
  }
  fun <- function(d) {
    i <- which(is.na(d$ping_flag))
    if (!length(i)) {
      return(d)
    }
    dup <- .rb_whack_duplicates(
      d$vid[i],
      d$time[i],
      d$lon[i],
      d$lat[i],
      d$feed[i],
      priority = priority,
      max_dt_s = max_dt_s,
      kn_max = kn_max,
      n = n_neighbours
    )
    d$ping_flag[i[dup]] <- "duplicate"
    d$ping_flag_basis[i[dup]] <- "priority"
    if (has_speed) {
      # every ping unflagged before this call, the cross-feed copies just flagged included: a copy at its own time
      # is a witness to where the vessel was
      cp <- .rb_whack_copies(
        d$vid[i],
        d$time[i],
        d$lon[i],
        d$lat[i],
        d$feed[i],
        priority = priority,
        speed = d$speed[i],
        course = if (has_course) d$course[i],
        copy_m = copy_m,
        near_s = near_s,
        max_dt_s = max_dt_s
      )
      new <- is.na(d$ping_flag[i]) & !is.na(cp$flag)
      d$ping_flag[i[new]] <- cp$flag[new]
      d$ping_flag_basis[i[new]] <- cp$basis[new]
      r <- is.na(d$ping_flag[i]) & !is.na(cp$repeat_dt_s)
      d$ping_repeat_dt_s[i[r]] <- cp$repeat_dt_s[r]
    }
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(
      x,
      fun,
      cols = c(
        "time",
        "lon",
        "lat",
        "feed",
        if (has_speed) "speed",
        if (has_course) "course"
      ),
      order_by = c("time", "lon", "lat", "feed"),
      batch_pings = batch_pings,
      what = "rb_flag_ping_duplicate()",
      extra = if (has_speed) "ping_repeat_dt_s" else character(0)
    )
  } else {
    fun(x)
  }
  .rb_std_out(x, map)
}

#' Flag positions no vessel could have reached
#'
#' Step 0 of the fishing-activity flow. A position or jump that implies an impossible speed is flagged
#' in `ping_flag` with the stage that caught it, and in `ping_flag_basis` with the evidence that judged it. Only
#' rows not yet flagged are tested, so invalid and duplicate pings never serve as anchors. No row is dropped.
#'
#' Which method: with a recorded speed over ground, `"local"`, given the column (`speed = `). On ramb's planted
#' faults (`rb_read_example("dansk_planted")`) it caught the most wrong positions at every ping rate from 10 s to
#' 1 h, out-and-backs under `kn_max` included, and kept the truly fast vessels, which every fixed limit flags. Its
#' cost in good fishing pings is the copies of reports interleaved with the track, so take them out first
#' ([rb_flag_ping_duplicate()] with `speed`). Without a recorded speed, `"local"` judges at the fixed limit and still
#' caught more than `"clean"` and `"forward"` at every rate; of those two, neither is best everywhere: `"clean"`
#' erodes good pings beside a fault it cannot resolve, `"forward"` loses a stretch when the first ping after a gap is
#' wrong (fishycode plan 062 step 3, decision 276).
#'
#' Two opt-in arguments serve a track merged from several feeds (a national AIS network and a commercial
#' one, terrestrial and satellite, AIS and VMS). Their defaults leave the rule as it was.
#'
#' * `feed` and `noise_nm`: two feeds that both report a vessel place it a few hundred metres apart, and a
#'   few seconds apart that reads as an impossible speed. With `feed`, a leg between pings of *different*
#'   feeds no longer than `noise_nm` is never impossible, at every stage; a leg inside one feed, or a
#'   cross-feed leg beyond `noise_nm`, is judged as before.
#' * `evidence = TRUE`: the columns that let a flag be audited. For each ping this call flagged, the legs
#'   to the nearest ping on each side that is *not* flagged (the track that is left, so a run of flagged
#'   pings is judged against the pings that stay):
#'   `ping_flag_prev_dt_s`, `ping_flag_prev_dist_m` and, with `feed`, `ping_flag_prev_feed`, and the same
#'   with `next`; and `ping_flag_side`, which of the two legs is impossible on the rule above (a leg
#'   within the noise floor, or longer than `max_gap_h`, is not): `"prev"`, `"next"`, `"both"`, or
#'   `"none"` when neither is (the flag came from the rms speed or the angle of the ping, or the ping has
#'   no neighbour on that side). For `"local"` a leg is impossible on its own limit, at the local speed the ping was
#'   judged by, which is `ping_flag_speed_kn` (`NA` under the fixed limit). The columns are `NA` for pings this call
#'   did not flag.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon` and `lat`.
#' @param method The rule:
#'   * `"clean"` (default): a two-stage filter at the fixed limit `kn_max`: speed-distance-angle ("vmask",
#'     "spike"), which judges each ping from both sides, the algorithm of `argosfilter::sdafilter()` (Freitas et al.
#'     2008) re-implemented in C++; then a forward scan against the last kept ping ("forward"), which resolves runs
#'     and interleaved streams.
#'   * `"forward"`: the forward scan alone ("forward"): each ping is judged against the last
#'     *retained* ping, so consecutive clusters are caught.
#'   * `"local"`: each leg judged against the vessel's own speed, from its recorded speed over ground (`speed`), with
#'     no heading. The local speed of a ping is the median recorded speed of the `k` pings on each side (5), its own
#'     left out; a leg is impossible when longer than `margin_m` (50 m) plus `factor` (1.5) times the local speed plus
#'     `margin_kn` (5 knots), times its time step. Where no ping near has a recorded speed, the limit is `margin_m`
#'     plus `kn_max` times the time step (basis `"track"`). Two stages: a ping whose two legs are both impossible
#'     while the leg that skips it is not ("local_spike"; the first and last ping of a track or of a stretch after a
#'     gap: their one leg), repeated on the pings left; then a forward scan with the same limit ("local_forward").
#'     The constants are set by hand, passed in `...` (`k`, `factor`, `margin_kn`, `margin_m`).
#'   * `"sda"`, `"fwdbwd"` and `"sequential"`: withdrawn; they warn, and go in the next release. `"sda"` is
#'     `"clean"`'s first stage alone; `"fwdbwd"` a ping too fast both from the previous and to the next; `"sequential"`
#'     equals `"forward"` at 30 times its time.
#' @param kn_max Speed threshold, knots: the fixed limit (for `"local"`, where no recorded speed is near).
#' @param min_dt_s A time step shorter than this counts as this many seconds.
#' @param max_gap_h For `"clean"` and `"forward"`: after a gap longer than this the scan restarts.
#' @param ... Passed to the method (`k`, `factor`, `margin_kn` and `margin_m` for `"local"`; `ang`, `distlim` for
#'   `"clean"`'s first stage).
#' @param feed The column naming the feed of each ping, for the noise floor `noise_nm`. Default `NULL`: no
#'   floor. A ping with no feed (`NA`) never takes part in it. Not for `"fwdbwd"` and `"sequential"`.
#' @param noise_nm The noise floor between feeds, nautical miles (1852 m). Only with `feed`.
#' @param speed For `"local"`: the column of the recorded speed over ground, knots (`NA` where none; below 0 or 102.3
#'   or more counts as none, AIS's "not available"). Default `NULL`: none, and every leg is judged at the fixed
#'   limit.
#' @param evidence `TRUE` adds the evidence columns, see Details. Default `FALSE`.
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` and `ping_flag_basis` added or updated: the same type, the same rows; with `evidence =
#'   TRUE`, the evidence columns too. The basis of a flag this call makes is `"recorded"` where the vessel's recorded
#'   speed set the limit (`"local"`), and `"track"` where the fixed limit did.
#'
#' @references For `"sda"`: Freitas, C., Lydersen, C., Fedak, M.A. and Kovacs,
#'   K.M. (2008). "A simple new algorithm to filter marine mammal Argos
#'   locations". Marine Mammal Science, 24(2), 315–325.
#'   \doi{10.1111/j.1748-7692.2007.00180.x}; the algorithm of
#'   [argosfilter::sdafilter()], re-implemented in C++.
#' @family flag pings
#' @export
#'
#' @examples
#' t0 <- as.POSIXct("2024-01-01", tz = "UTC")
#' # one vessel heard by two feeds: the second feed's ping is 219 m from the first's, 5 s later (85 knots,
#' # if read as one track)
#' d <- data.frame(
#'   vid = 1,
#'   time = t0 + c(0, 60, 120, 125, 180, 240, 300),
#'   lon = -20 + c(0, 0.001, 0.002, 0.0065, 0.003, 0.004, 0.005),
#'   lat = 64,
#'   feed = c("a", "a", "a", "b", "a", "a", "a")
#' )
#' rb_flag_ping_impossible(d)$ping_flag
#' # the cross-feed leg is within 0.3 nautical miles (556 m): not impossible
#' rb_flag_ping_impossible(d, feed = feed)$ping_flag
#' rb_flag_ping_impossible(d, evidence = TRUE)[4, ]
rb_flag_ping_impossible <- function(
  x,
  method = c("clean", "forward", "local", "sda", "fwdbwd", "sequential"),
  kn_max = 25,
  min_dt_s = 10,
  max_gap_h = 4,
  ...,
  feed = NULL,
  noise_nm = 0.3,
  speed = NULL,
  evidence = FALSE,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  batch_pings = 5e6
) {
  feed_quo <- rlang::enquo(feed)
  has_feed <- !rlang::quo_is_null(feed_quo)
  speed_quo <- rlang::enquo(speed)
  has_speed <- !rlang::quo_is_null(speed_quo)
  .rb_check_input(
    x,
    "rb_flag_ping_impossible",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = c(
      if (has_feed) c(feed = rlang::as_name(feed_quo)),
      if (has_speed) c(speed = rlang::as_name(speed_quo))
    ),
    types = c(
      time = "time",
      lon = "numeric",
      lat = "numeric",
      if (has_speed) c(speed = "numeric")
    )
  )
  .rb_warn_unharmonised(x, "rb_flag_ping_impossible")
  method <- match.arg(method)
  if (method %in% c("sda", "fwdbwd", "sequential")) {
    .rb_withdrawn_method(method)
  }
  if (has_feed && !method %in% c("clean", "sda", "forward", "local")) {
    stop(
      "`feed` (the noise floor between feeds) is used by the methods \"clean\", \"forward\" and \"local\", not \"",
      method,
      "\".",
      call. = FALSE
    )
  }
  if (has_speed && method != "local") {
    stop(
      "`speed` (the recorded speed over ground) is used by the method \"local\", not \"",
      method,
      "\".",
      call. = FALSE
    )
  }
  if (has_feed) {
    if (
      !is.numeric(noise_nm) ||
        length(noise_nm) != 1 ||
        is.na(noise_nm) ||
        noise_nm < 0
    ) {
      stop("`noise_nm` must be one number, 0 or more.", call. = FALSE)
    }
  }
  noise_m <- if (has_feed) noise_nm * 1852 else 0
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  if (has_feed) {
    map <- c(map, feed = rlang::as_name(feed_quo))
  }
  if (has_speed) {
    map <- c(map, speed = rlang::as_name(speed_quo))
  }
  x <- .rb_flag_init(.rb_std_in(x, map))
  dots <- list(...)
  local <- method == "local"
  ev_cols <- if (evidence) .rb_evidence_cols(has_feed, local) else character(0)
  fun <- function(d) {
    if (evidence) {
      d <- .rb_evidence_init(d, has_feed, local)
    }
    i <- which(is.na(d$ping_flag))
    if (!length(i)) {
      return(d)
    }
    s <- dplyr::ungroup(d[
      i,
      setdiff(names(d), c("ping_flag", "ping_flag_basis", ev_cols)),
      drop = FALSE
    ])
    s$.rb_i <- i
    if (has_feed) {
      s$.rb_feed <- s$feed
    }
    if (has_speed) {
      s$.rb_speed <- s$speed
    }
    basis <- NULL
    judged <- NULL
    stage <- switch(
      method,
      clean = {
        o <- do.call(
          .whack_clean_df,
          c(
            list(
              s,
              kn_max = kn_max,
              max_gap_h = max_gap_h,
              min_dt_s = min_dt_s,
              noise_m = noise_m
            ),
            dots
          )
        )
        stats::setNames(o$whack_stage, o$.rb_i)
      },
      sda = {
        o <- do.call(
          .rb_whack_sda,
          c(
            list(s, kn_max = kn_max, min_dt_s = min_dt_s, noise_m = noise_m),
            dots
          )
        )
        stats::setNames(o$whack_sda, o$.rb_i)
      },
      forward = {
        o <- .rb_whack_forward(
          s,
          kn_max = kn_max,
          max_gap_h = max_gap_h,
          min_dt_s = min_dt_s,
          noise_m = noise_m
        )
        stats::setNames(ifelse(o$whack2, "forward", NA_character_), o$.rb_i)
      },
      local = {
        o <- do.call(
          .rb_whack_local,
          c(
            list(
              s,
              kn_max = kn_max,
              min_dt_s = min_dt_s,
              max_gap_h = max_gap_h,
              noise_m = noise_m
            ),
            dots
          )
        )
        basis <- stats::setNames(o$whack_basis, o$.rb_i)
        judged <- stats::setNames(o$whack_v, o$.rb_i)
        stats::setNames(o$whack_local, o$.rb_i)
      },
      fwdbwd = {
        o <- .rb_whack_fwdbwd(s, kn_max = kn_max, min_dt_s = min_dt_s)
        stats::setNames(ifelse(o$whack, "fwdbwd", NA_character_), o$.rb_i)
      },
      sequential = {
        o <- s |>
          dplyr::arrange(vid, time, lon, lat) |>
          dplyr::group_by(vid) |>
          dplyr::mutate(
            .w = .rb_whack_sequential_fast(
              lon,
              lat,
              time,
              kn_max = kn_max,
              min_dt_s = min_dt_s
            )
          ) |>
          dplyr::ungroup()
        stats::setNames(ifelse(o$.w, "sequential", NA_character_), o$.rb_i)
      }
    )
    rows <- as.integer(names(stage))
    d$ping_flag[rows] <- unname(stage)
    # the evidence that judged each flag: the fixed limit over the track, or the vessel's recorded speed
    if (is.null(basis)) {
      basis <- ifelse(is.na(stage), NA_character_, "track")
    }
    d$ping_flag_basis[rows] <- ifelse(
      is.na(stage),
      d$ping_flag_basis[rows],
      unname(basis)
    )
    if (evidence) {
      new <- logical(nrow(d))
      new[rows[!is.na(stage)]] <- TRUE
      if (local) {
        d$ping_flag_speed_kn[rows] <- ifelse(
          is.na(stage),
          NA_real_,
          unname(judged)
        )
      }
      d <- .rb_flag_evidence(
        d,
        new,
        kn_max = kn_max,
        min_dt_s = min_dt_s,
        max_gap_h = max_gap_h,
        noise_m = noise_m,
        has_feed = has_feed,
        local = if (local) .rb_local_limits(dots)
      )
    }
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(
      x,
      fun,
      cols = c(
        "time",
        "lon",
        "lat",
        if (has_feed) "feed",
        if (has_speed) "speed"
      ),
      order_by = c("time", "lon", "lat", if (has_feed) "feed"),
      batch_pings = batch_pings,
      what = "rb_flag_ping_impossible()",
      extra = ev_cols
    )
  } else {
    fun(x)
  }
  .rb_std_out(x, map)
}

# The constants of the "local" rule, as .rb_whack_local() takes them, with those the caller passed in `...`.
.rb_local_limits <- function(dots) {
  f <- formals(.rb_whack_local)
  out <- list(factor = f$factor, margin_kn = f$margin_kn, margin_m = f$margin_m)
  out[intersect(names(out), names(dots))] <- dots[intersect(
    names(out),
    names(dots)
  )]
  out
}

# Evidence of a flag (rb_flag_ping_impossible(evidence = TRUE)) -------------------------
# For each ping flagged in this call, the legs to the nearest ping on each side that is not flagged (by this
# call or an earlier one: the track that is left), in the order the filters use, (vid, time, lon, lat).
# The legs are the audit trail of a flag, and are measured on the track that is left, so a run of flagged
# pings is judged against the pings that stay, not against each other. `ping_flag_side` names the side(s)
# whose leg is impossible on the rule of the call: faster than kn_max on a step of at least min_dt_s, not
# longer than max_gap_h (where the forward scan restarts), and not inside the noise floor between feeds.
.rb_evidence_cols <- function(has_feed, local = FALSE) {
  c(
    "ping_flag_side",
    if (local) "ping_flag_speed_kn",
    if (has_feed) "ping_flag_prev_feed",
    "ping_flag_prev_dt_s",
    "ping_flag_prev_dist_m",
    if (has_feed) "ping_flag_next_feed",
    "ping_flag_next_dt_s",
    "ping_flag_next_dist_m"
  )
}
.rb_evidence_init <- function(d, has_feed, local = FALSE) {
  for (nm in .rb_evidence_cols(has_feed, local)) {
    if (!nm %in% names(d)) {
      d[[nm]] <- if (grepl("_(side|feed)$", nm)) {
        rep(NA_character_, nrow(d))
      } else {
        rep(NA_real_, nrow(d))
      }
    }
  }
  d
}
.rb_flag_evidence <- function(
  d,
  new,
  kn_max,
  min_dt_s,
  max_gap_h,
  noise_m,
  has_feed,
  local = NULL
) {
  d <- .rb_evidence_init(d, has_feed, !is.null(local))
  if (!any(new)) {
    return(d)
  }
  o <- order(d$vid, as.numeric(d$time), d$lon, d$lat)
  n <- length(o)
  idx <- seq_len(n)
  v <- d$vid[o]
  t <- as.numeric(d$time[o])
  lo <- d$lon[o]
  la <- d$lat[o]
  fd <- if (has_feed) as.character(d$feed[o]) else NULL
  kept <- is.na(d$ping_flag[o])
  # the nearest kept ping before and after each row (0 / n + 1: none), then none if it is another vessel's
  pk <- c(0L, cummax(ifelse(kept, idx, 0L)))[idx]
  nk <- c(rev(cummin(rev(ifelse(kept, idx, n + 1L)))), n + 1L)[idx + 1L]
  pk[pk == 0L | v[pmax(pk, 1L)] != v] <- NA_integer_
  nk[nk > n | v[pmin(nk, n)] != v] <- NA_integer_
  r <- which(new[o])
  ms_max <- kn_max * 0.514444
  leg <- function(j) {
    # the leg from each flagged row r to its neighbour j (a row of the sorted track, NA: none)
    ok <- !is.na(j)
    dt <- dist <- rep(NA_real_, length(r))
    fj <- rep(NA_character_, length(r))
    if (any(ok)) {
      jj <- j[ok]
      rr <- r[ok]
      dist[ok] <- rb_calc_distance(lo[rr], la[rr], lo[jj], la[jj])
      dt[ok] <- abs(t[rr] - t[jj])
      if (has_feed) {
        fj[ok] <- fd[jj]
      }
    }
    in_noise <- if (has_feed) {
      !is.na(fj) & !is.na(fd[r]) & fj != fd[r] & dist <= noise_m
    } else {
      rep(FALSE, length(r))
    }
    too_far <- if (is.null(local)) {
      dist / pmax(dt, min_dt_s) > ms_max
    } else {
      # "local": the limit at the speed the ping was judged by, the fixed one where none
      v <- d$ping_flag_speed_kn[o[r]] * 0.514444
      lim <- ifelse(
        is.na(v),
        ms_max,
        local$factor * v + local$margin_kn * 0.514444
      )
      dist > local$margin_m + lim * pmax(dt, min_dt_s)
    }
    bad <- ok &
      !is.na(dist) &
      dt <= max_gap_h * 3600 &
      too_far &
      !in_noise
    list(dt = dt, dist = dist, feed = fj, bad = bad)
  }
  p <- leg(pk[r])
  q <- leg(nk[r])
  side <- ifelse(
    p$bad & q$bad,
    "both",
    ifelse(p$bad, "prev", ifelse(q$bad, "next", "none"))
  )
  rows <- o[r]
  d$ping_flag_side[rows] <- side
  d$ping_flag_prev_dt_s[rows] <- p$dt
  d$ping_flag_prev_dist_m[rows] <- p$dist
  d$ping_flag_next_dt_s[rows] <- q$dt
  d$ping_flag_next_dist_m[rows] <- q$dist
  if (has_feed) {
    d$ping_flag_prev_feed[rows] <- p$feed
    d$ping_flag_next_feed[rows] <- q$feed
  }
  d
}

#' Tag pings with the port polygon they lie in
#'
#' Step 0 of the fishing-activity flow: each ping gets the id of the port polygon that holds it
#' (`port_id`, NA at sea). Port pings are labelled and stay; nothing is dropped. A ping inside two
#' polygons gets the smaller one (the more specific port), then the lowest `port_id`, so there is one row per ping.
#' Runs in DuckDB (spatial extension): a data frame is registered and collected, a lazy table stays lazy. A ping's port
#' depends on its position alone, so each distinct position is joined to the polygons once and the pings take its port
#' by an equality join: no window runs over the pings, and the memory grows with the positions in port, not with the
#' pings' columns.
#'
#' @param pings A data frame or lazy DuckDB table with `lon`, `lat`.
#' @param ports An sf object of port polygons with `port_id`.
#' @param keep Other columns of `ports` to add to the pings (e.g. a second code).
#' @param lon,lat The position columns, if named otherwise.
#'
#' @return `pings` with `port_id` and the `keep` columns: the same type, the same rows.
#' @family flag pings
#' @export
rb_flag_ping_port <- function(pings, ports, keep = NULL, lon = lon, lat = lat) {
  .rb_check_input(
    pings,
    "rb_flag_ping_port",
    cols = c(
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    types = c(lon = "numeric", lat = "numeric")
  )
  map <- .rb_cols(lon = {{ lon }}, lat = {{ lat }})
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  .rb_scope()
  .rb_extension(con, "spatial")
  given <- pings
  if (!lazy_in) {
    pings <- .rb_with_rid(pings)
  }
  p <- .rb_as_lazy(.rb_std_in(pings, map), con)
  hb <- sf::st_transform(ports, 4326)
  # WKB keeps the coordinates bit for bit; area (in degrees) only ranks two polygons that hold one ping
  hw <- data.frame(
    sf::st_drop_geometry(hb)[, c("port_id", keep), drop = FALSE],
    rb_area = as.numeric(sf::st_area(sf::st_set_crs(sf::st_geometry(hb), NA)))
  )
  hw$rb_wkb <- lapply(sf::st_as_binary(sf::st_geometry(hb)), as.raw)
  nm <- .rb_register(con, hw, "rb_harb_")
  kp <- paste0(
    sprintf(
      ', first(h."%s" ORDER BY h.rb_area, h.port_id) AS "%s"',
      keep,
      keep
    ),
    collapse = ""
  )
  # each position once against the polygons (the smallest, then the lowest port_id), the pings joined back on it; until
  # 2026.10.28.3 a row number and a window over every ping and column did this, which took 15 GB for a year of 37
  # million pings under DuckDB 2.0 (fishycode plan 049)
  sql <- sprintf(
    "
WITH p AS NOT MATERIALIZED (%s),
h AS (SELECT * EXCLUDE (rb_wkb), ST_GeomFromWKB(rb_wkb) AS rb_g FROM %s),
q AS (SELECT DISTINCT lon, lat FROM p WHERE lon IS NOT NULL AND lat IS NOT NULL),
m AS (SELECT q.lon AS rb_lon, q.lat AS rb_lat, first(h.port_id ORDER BY h.rb_area, h.port_id) AS port_id%s
      FROM q JOIN h ON ST_Intersects(h.rb_g, ST_Point(q.lon, q.lat)) GROUP BY q.lon, q.lat)
SELECT p.*, m.* EXCLUDE (rb_lon, rb_lat) FROM p LEFT JOIN m ON p.lon = m.rb_lon AND p.lat = m.rb_lat",
    .rb_sql(p),
    nm,
    kp
  )
  out <- .rb_std_out(dplyr::tbl(con, dplyr::sql(sql)), map)
  if (lazy_in) {
    return(.rb_hold(out))
  }
  out <- dplyr::collect(out)
  .rb_unregister(con)
  .rb_as_given(out, given, ".rb_rid")
}
