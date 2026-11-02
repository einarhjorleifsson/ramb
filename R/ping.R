# flag_ping.R — step 0 of the fishing-activity flow: flag pings that are not usable positions.
#
# The three flaggers share one label column, `ping_flag`: NA for a usable ping, otherwise the reason
# ("invalid", "duplicate", or the impossible-position stage that caught it). Each tests only rows not yet
# flagged, so a chain applies them in order:
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
.rb_flag_init <- function(x) {
  if ("ping_flag" %in% colnames(x)) {
    x
  } else {
    dplyr::mutate(x, ping_flag = NA_character_)
  }
}

# Lazy tables: label in batches of whole vessels, join the labels back -------------------
# For rules that are sequential along a track and so cannot run in DuckDB itself. Each batch is
# collected, labelled in R by `fun` (a data-frame function that updates `ping_flag`), and its labels are
# written to a temporary parquet file; the result is the input joined to those files, still lazy. A row is
# found again by its number within its vessel, in `order_by` order, numbered the same way in DuckDB and
# in the batch. The files live until the R session ends. `extra` names more columns that `fun` adds to
# the batch (the evidence columns of rb_flag_ping_impossible()); they are joined back with the label.
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
  batches <- split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  dir <- tempfile("rb_labels_")
  dir.create(dir)
  for (i in seq_along(batches)) {
    g <- batches[[i]]
    d <- x |>
      dplyr::filter(vid %in% g) |>
      dplyr::select(dplyr::all_of(unique(c("vid", cols, "ping_flag")))) |>
      dplyr::group_by(vid) |>
      dbplyr::window_order(!!!ob) |>
      dplyr::mutate(.rb_rn = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::collect() |>
      dplyr::arrange(vid, !!!ob)
    lab <- fun(d)[, c("vid", ".rb_rn", "ping_flag", extra)]
    lab$.rb_rn <- as.numeric(lab$.rb_rn)
    # Register, write with COPY on the same connection, release: a frame left registered in a loop
    # accumulates (fishycode R/write_parquet.R: 48 GB spilled before that was found).
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
    dplyr::select(-"ping_flag") |>
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
#' @return `x` with `ping_flag` added or updated: the same type, the same rows.
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
  x <- dplyr::mutate(
    x,
    ping_flag = dplyr::case_when(
      !is.na(ping_flag) ~ ping_flag,
      !!eq & lon == lat ~ "invalid",
      !!oor & (abs(lon) > 180 | abs(lat) > 90) ~ "invalid",
      !!aft & !is.na(.rb_after) & .rb_after > !!ahead_s ~ "after receipt",
      TRUE ~ NA_character_
    )
  )
  x <- dplyr::select(x, -".rb_after")
  .rb_std_out(x, map)
}

#' Flag a second feed's copy of a ping
#'
#' Step 0 of the fishing-activity flow. When several feeds (e.g. a national AIS network and a commercial
#' one) report the same fix, the lower-priority feed's copy is flagged `"duplicate"` in `ping_flag`: a
#' ping of the same vessel from a higher-priority feed lies within `max_dt_s` seconds and the distance
#' `kn_max` covers in that time, among the `n_neighbours` neighbours on each side in time order. Only rows not yet
#' flagged are tested, and they are not used as anchors. No row is dropped.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon`, `lat` and the feed column.
#' @param priority The feeds, highest priority first. A feed not listed ranks last. No default: the
#'   order is a judgement about the feeds.
#' @param feed The column naming the feed.
#' @param max_dt_s,kn_max,n_neighbours Seconds apart, speed in knots that sets the distance, neighbours compared.
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` added or updated: the same type, the same rows.
#' @family flag pings
#' @export
rb_flag_ping_duplicate <- function(
  x,
  priority,
  feed = provider,
  max_dt_s = 10,
  kn_max = 25,
  n_neighbours = 3,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  batch_pings = 5e6
) {
  .rb_check_input(
    x,
    "rb_flag_ping_duplicate",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = c(feed = rlang::as_name(rlang::enquo(feed))),
    types = c(time = "time", lon = "numeric", lat = "numeric")
  )
  .rb_warn_unharmonised(x, "rb_flag_ping_duplicate")
  if (missing(priority)) {
    stop(
      "`priority` is required: the feeds, highest priority first.",
      call. = FALSE
    )
  }
  map <- .rb_cols(
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }},
    feed = {{ feed }}
  )
  x <- .rb_flag_init(.rb_std_in(x, map))
  fun <- function(d) {
    i <- which(is.na(d$ping_flag))
    if (length(i)) {
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
    }
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(
      x,
      fun,
      cols = c("time", "lon", "lat", "feed"),
      order_by = c("time", "lon", "lat", "feed"),
      batch_pings = batch_pings,
      what = "rb_flag_ping_duplicate()"
    )
  } else {
    fun(x)
  }
  .rb_std_out(x, map)
}

#' Flag positions no vessel could have reached
#'
#' Step 0 of the fishing-activity flow. A position or jump that implies an impossible speed is flagged
#' in `ping_flag` with the stage that caught it. Only rows not yet flagged are tested, so invalid and
#' duplicate pings never serve as anchors. No row is dropped.
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
#'   no neighbour on that side). The columns are `NA` for pings this call did not flag.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon` and `lat`.
#' @param method The rule:
#'   * `"clean"` (default): the recommended two-stage filter: speed-distance-angle ("vmask", "spike"),
#'     which judges each ping from both sides, then a forward scan against the last kept ping
#'     ("forward"), which resolves runs and interleaved streams.
#'   * `"sda"`: the speed-distance-angle stage alone ("vmask", "spike"); the algorithm of
#'     `argosfilter::sdafilter()` (Freitas et al. 2008), re-implemented in C++.
#'   * `"forward"`: the forward scan alone ("forward"): each ping is judged against the last
#'     *retained* ping, so consecutive clusters are caught.
#'   * `"fwdbwd"`: a ping too fast both from the previous and to the next ("fwdbwd"): isolated
#'     spikes only; consecutive clusters pass.
#'   * `"sequential"`: the cluster-aware sequential filter ("sequential"): iteratively removes
#'     the first too-fast segment and recomputes, in-memory only.
#' @param kn_max Speed threshold, knots.
#' @param min_dt_s A time step shorter than this counts as this many seconds.
#' @param max_gap_h For `"clean"` and `"forward"`: after a gap longer than this the scan restarts.
#' @param ... Passed to the method (e.g. `ang`, `distlim` for `"sda"`).
#' @param feed The column naming the feed of each ping, for the noise floor `noise_nm`. Default `NULL`: no
#'   floor. A ping with no feed (`NA`) never takes part in it. For the methods `"clean"`, `"sda"` and
#'   `"forward"`.
#' @param noise_nm The noise floor between feeds, nautical miles (1852 m). Only with `feed`.
#' @param evidence `TRUE` adds the evidence columns, see Details. Default `FALSE`.
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` added or updated: the same type, the same rows; with `evidence = TRUE`, the
#'   evidence columns too.
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
  method = c("clean", "sda", "forward", "fwdbwd", "sequential"),
  kn_max = 25,
  min_dt_s = 10,
  max_gap_h = 4,
  ...,
  feed = NULL,
  noise_nm = 0.3,
  evidence = FALSE,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  batch_pings = 5e6
) {
  feed_quo <- rlang::enquo(feed)
  has_feed <- !rlang::quo_is_null(feed_quo)
  .rb_check_input(
    x,
    "rb_flag_ping_impossible",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = if (has_feed) c(feed = rlang::as_name(feed_quo)) else character(0),
    types = c(time = "time", lon = "numeric", lat = "numeric")
  )
  .rb_warn_unharmonised(x, "rb_flag_ping_impossible")
  method <- match.arg(method)
  if (has_feed && !method %in% c("clean", "sda", "forward")) {
    stop(
      "`feed` (the noise floor between feeds) is used by the methods \"clean\", \"sda\" and \"forward\", not \"",
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
  x <- .rb_flag_init(.rb_std_in(x, map))
  dots <- list(...)
  ev_cols <- if (evidence) .rb_evidence_cols(has_feed) else character(0)
  fun <- function(d) {
    if (evidence) {
      d <- .rb_evidence_init(d, has_feed)
    }
    i <- which(is.na(d$ping_flag))
    if (!length(i)) {
      return(d)
    }
    s <- dplyr::ungroup(d[
      i,
      setdiff(names(d), c("ping_flag", ev_cols)),
      drop = FALSE
    ])
    s$.rb_i <- i
    if (has_feed) {
      s$.rb_feed <- s$feed
    }
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
    d$ping_flag[as.integer(names(stage))] <- unname(stage)
    if (evidence) {
      new <- logical(nrow(d))
      new[as.integer(names(stage))[!is.na(stage)]] <- TRUE
      d <- .rb_flag_evidence(
        d,
        new,
        kn_max = kn_max,
        min_dt_s = min_dt_s,
        max_gap_h = max_gap_h,
        noise_m = noise_m,
        has_feed = has_feed
      )
    }
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(
      x,
      fun,
      cols = c("time", "lon", "lat", if (has_feed) "feed"),
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

# Evidence of a flag (rb_flag_ping_impossible(evidence = TRUE)) -------------------------
# For each ping flagged in this call, the legs to the nearest ping on each side that is not flagged (by this
# call or an earlier one: the track that is left), in the order the filters use, (vid, time, lon, lat).
# The legs are the audit trail of a flag, and are measured on the track that is left, so a run of flagged
# pings is judged against the pings that stay, not against each other. `ping_flag_side` names the side(s)
# whose leg is impossible on the rule of the call: faster than kn_max on a step of at least min_dt_s, not
# longer than max_gap_h (where the forward scan restarts), and not inside the noise floor between feeds.
.rb_evidence_cols <- function(has_feed) {
  c(
    "ping_flag_side",
    if (has_feed) "ping_flag_prev_feed",
    "ping_flag_prev_dt_s",
    "ping_flag_prev_dist_m",
    if (has_feed) "ping_flag_next_feed",
    "ping_flag_next_dt_s",
    "ping_flag_next_dist_m"
  )
}
.rb_evidence_init <- function(d, has_feed) {
  for (nm in .rb_evidence_cols(has_feed)) {
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
  has_feed
) {
  d <- .rb_evidence_init(d, has_feed)
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
    bad <- ok &
      !is.na(dist) &
      dt <= max_gap_h * 3600 &
      dist / pmax(dt, min_dt_s) > ms_max &
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
