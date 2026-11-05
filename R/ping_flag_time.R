#' Flag a ping whose written time is off, keeping its position
#'
#' Step 0 of the fishing-activity flow, between the copies and the impossible positions: a ping written late (or early)
#' makes its step in too long and its step out too short for the distance its recorded speed covers, and every speed
#' rule then blames the next ping's position (the case study's 08:23 ping, written a minute late, fishycode TODO 190).
#' This flag labels the time instead: `ping_flag` `"time"`, basis `"recorded"`, its position kept and set aside as an
#' anchor, so [rb_flag_ping_impossible()], run after, judges the leg past it and blames no position.
#'
#' Within a stream, a ping is judged with the nearest pings on each side not yet flagged, where the three record a
#' steady speed (over `min_kn` at each, the two of each step within `steady` of their mean) and both steps are at
#' most `max_step_s`. Each step's residual is its written time less the distance over its mean recorded speed; a time off
#' by `e` adds `e` to the step in and takes it from the step out. The ping's time error, `ping_time_error_s` (+ late),
#' is half the difference of the two residuals, and half their sum the error of the chord past it. A ping is flagged
#' where its time error is over the allowance and the chord's is within it, the worst first: a ping beside one of a
#' larger time error waits, and the pings left are judged again without the ones flagged, until none is. The
#' allowance is what an on-time ping may show: the clock's grain (from `limits`: a time written to the minute is up to
#' a minute off against its neighbours' as written), twice the dataset's noise over the speed, and half of `steady` of
#' the longer step. A clock's spread beyond the grain (jitter, `limits`' `time_error_s`) is not allowed: those are the
#' times flagged.
#'
#' On `dansk_truth` (fishycode plan 065 step 4): of the 4,425 pings jittered more than 10 s, 1,572 flagged, the time
#' error estimated within a median 2 s (Spearman 0.92); 155 good pings flagged of 140,556 (110 on the jittered clock,
#' between two late pings); 3 of the 15 late stamps, the rest at under 2 knots or a speed changing by more than 20 %
#' (at 10 %: 1,056 jittered pings and 2 late stamps, 123 good pings).
#'
#' An along-track error of the position reads the same as a time error, the one ping displaced along its own track:
#' over the allowance at speed it is tens of metres or more, rare for a fix. A ping between two pings written late
#' itself reads early; a wrong position beside a ping moves the chord and the ping is not judged.
#'
#' @param x A data frame or a lazy DuckDB table with `vid`, `time`, `lon`, `lat` and the recorded speed.
#' @param limits The table of [rb_create_ping_limits()] for these pings (with the same `stream`, where one is given):
#'   each stream's grain and the dataset's noise.
#' @param speed The recorded speed over ground, knots (below 0 or 102.3 or more counts as none). A speed derived from
#'   the positions is no witness.
#' @param stream The stream column: pings are judged within it. Default `NULL`: one stream.
#' @param min_kn,steady,max_step_s The speed a ping and its neighbours must record, the share of their mean by which
#'   the two recorded speeds of a step may differ, and the longest step judged: over an hour a recorded speed at both
#'   ends does not hold between them.
#' @param vid,time,lon,lat The columns, if named otherwise.
#' @param batch_pings For a lazy table: pings per batch.
#'
#' @return `x` with `ping_flag` and `ping_flag_basis` added or updated, and `ping_time_error_s` (`NA` where not
#'   judged): the same type, the same rows.
#' @family flag pings
#' @export
#' @examples
#' t0 <- as.POSIXct("2024-01-01 00:00:07", tz = "UTC")
#' # 10 knots east, a ping a minute (times to the second), the fourth written 30 s late
#' d <- data.frame(vid = 1, time = t0 + 60 * 0:7, lat = 64, speed = 10)
#' d$lon <- -20 + 10 * 1852 / 3600 * 60 * 0:7 / (6371000 * pi / 180 * cos(64 * pi / 180))
#' lim <- rb_create_ping_limits(d, speed = speed, min_recorded = 3, min_moving = 3)
#' d$time[4] <- d$time[4] + 30
#' rb_flag_ping_time(d, lim)[, c("time", "ping_flag", "ping_time_error_s")]
rb_flag_ping_time <- function(
  x,
  limits,
  speed = speed,
  stream = NULL,
  min_kn = 2,
  steady = 0.2,
  max_step_s = 600,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat,
  batch_pings = 5e6
) {
  stream_quo <- rlang::enquo(stream)
  has_stream <- !rlang::quo_is_null(stream_quo)
  speed_nm <- rlang::as_name(rlang::enquo(speed))
  .rb_check_input(
    x,
    "rb_flag_ping_time",
    cols = c(
      vid = rlang::as_name(rlang::enquo(vid)),
      time = rlang::as_name(rlang::enquo(time)),
      lon = rlang::as_name(rlang::enquo(lon)),
      lat = rlang::as_name(rlang::enquo(lat))
    ),
    also = c(speed = speed_nm, if (has_stream) c(stream = rlang::as_name(stream_quo))),
    types = c(time = "time", lon = "numeric", lat = "numeric", speed = "numeric")
  )
  for (a in list(list("min_kn", min_kn), list("steady", steady), list("max_step_s", max_step_s))) {
    if (!is.numeric(a[[2]]) || length(a[[2]]) != 1 || is.na(a[[2]]) || a[[2]] <= 0) {
      stop(sprintf("`%s` must be one number over 0.", a[[1]]), call. = FALSE)
    }
  }
  map <- .rb_cols(vid = {{ vid }}, time = {{ time }}, lon = {{ lon }}, lat = {{ lat }})
  map <- c(map, speed = speed_nm)
  if (has_stream) {
    map <- c(map, stream = rlang::as_name(stream_quo))
  }
  lim <- .rb_limits_in(limits, map, colnames(x))
  x <- .rb_flag_init(.rb_std_in(x, map))
  if (!"ping_time_error_s" %in% colnames(x)) {
    x <- .rb_na_col(x, "ping_time_error_s", "DOUBLE")
  }
  KN <- 1852 / 3600
  fun <- function(d) {
    d$ping_time_error_s <- as.numeric(d$ping_time_error_s)
    pl <- .rb_limits_rows(d, lim)
    i <- which(is.na(d$ping_flag) & !is.na(d$vid) & !is.na(d$time) & !is.na(d$lon) & !is.na(d$lat))
    if (length(i) < 3) {
      return(d)
    }
    g <- if (has_stream) paste(d$vid[i], d$stream[i], sep = "\r") else as.character(d$vid[i])
    o <- i[order(g, as.numeric(d$time[i]), d$lon[i], d$lat[i])]
    sp_all <- as.numeric(d$speed)
    sp_all[!is.na(sp_all) & (sp_all < 0 | sp_all >= 102.3)] <- NA_real_
    grp <- if (has_stream) paste(d$vid, d$stream, sep = "\r") else as.character(d$vid)
    t_all <- as.numeric(d$time)
    is_steady <- function(u, w) !is.na(u) & !is.na(w) & u > min_kn & w > min_kn & abs(u - w) <= steady * (u + w) / 2
    # each ping of `o` (rows of d, sorted within their stream) judged with its neighbours in `o`
    judge <- function(o) {
      n <- length(o)
      g <- grp[o]
      a <- c(NA_integer_, o[-n])
      b <- c(o[-1], NA_integer_)
      a[c(TRUE, g[-1] != g[-n])] <- NA_integer_
      b[c(g[-n] != g[-1], TRUE)] <- NA_integer_
      s0 <- sp_all[a]
      s1 <- sp_all[o]
      s2 <- sp_all[b]
      dt_in <- t_all[o] - t_all[a]
      dt_out <- t_all[b] - t_all[o]
      ok <- !is.na(a) & !is.na(b) & is_steady(s0, s1) & is_steady(s1, s2) & dt_in > 0 & dt_out > 0 &
        dt_in <= max_step_s & dt_out <= max_step_s
      ok[is.na(ok)] <- FALSE
      r_in <- dt_in - .rb_haversine(d$lon[a], d$lat[a], d$lon[o], d$lat[o]) / ((s0 + s1) / 2 * KN)
      r_out <- dt_out - .rb_haversine(d$lon[o], d$lat[o], d$lon[b], d$lat[b]) / ((s1 + s2) / 2 * KN)
      e <- (r_in - r_out) / 2
      chord <- (r_in + r_out) / 2
      grain <- pl$grain[o]
      noise <- pl$noise[o]
      allow <- ifelse(is.na(grain), 0, grain) + 2 * ifelse(is.na(noise), 0, noise) / (s1 * KN) +
        steady / 2 * pmax(dt_in, dt_out)
      ok <- ok & !is.na(e)
      hit <- ok & abs(e) > allow & abs(chord) <= allow
      # the worst first: a hit beside a hit of a larger time error waits for the next round, judged without it (a
      # good ping between two pings written late reads early)
      ae <- ifelse(hit, abs(e), -Inf)
      worse_prev <- c(-Inf, ae[-n]) > ae & c(FALSE, g[-1] == g[-n])
      worse_next <- c(ae[-1], -Inf) >= ae & c(g[-n] == g[-1], FALSE)
      list(e = ifelse(ok, e, NA_real_), hit = hit & !worse_prev & !worse_next)
    }
    repeat {
      j <- judge(o)
      if (!any(j$hit)) {
        d$ping_time_error_s[o] <- j$e
        break
      }
      d$ping_time_error_s[o[j$hit]] <- j$e[j$hit]
      d$ping_flag[o[j$hit]] <- "time"
      d$ping_flag_basis[o[j$hit]] <- "recorded"
      o <- o[!j$hit]
      if (length(o) < 3) break
    }
    d
  }
  x <- if (inherits(x, "tbl_lazy")) {
    .rb_label_lazy(
      x,
      fun,
      cols = unique(c("time", "lon", "lat", "speed", if (has_stream) "stream", setdiff(lim$keys, "vid"))),
      order_by = c("time", "lon", "lat"),
      batch_pings = batch_pings,
      what = "rb_flag_ping_time()",
      extra = character(0)
    )
  } else {
    fun(x)
  }
  .rb_std_out(x, map)
}
