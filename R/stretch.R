# stretch.R — step 2: towing stretches cut from the track, and their ordered, one-to-one pairing with the
# declared tows.
#
# Ported from fishycode's TODO 56 prototype ($FISHYDATA/archive/2026-10-todo56/q2all.R; fishycode decision 090,
# plan 013 phase 4, TODO 59). The windowed speed runs in DuckDB; the cut and the pairing are sequential, so they
# run in R one vessel at a time, whole vessels collected in batches (tier 4, as the "jepol" method of
# rb_create_voyages()).

#' Create towing stretches from vessel tracks
#'
#' A step-2 builder of the fishing-activity flow: the stretches of a track at towing speed, the track's own
#' account of where the tows were. [rb_link_record_stretches()] pairs them with the declared tows.
#'
#' A stretch is a run of pings at sea whose speed, the median over `window_s` seconds either side
#' ([rb_calc_speed_smooth()]), lies in the ping's speed range. It is broken where the speed leaves the range and
#' at a gap of more than `max_gap_s` between pings. Stretches less than `dip_s` apart are joined, and then those
#' shorter than `min_dur_s` are dropped.
#'
#' With `records`, the declared tows are a prior. A stretch that holds two or more of them (each with at least
#' half its duration inside it) is cut between each tow and the next, at the slowest ping (its own speed) from
#' `margin_s` before the earlier tow's end to `margin_s` after the later one's start. The pings either side of
#' it become the ends of the two pieces. Without the prior, two tows with a short turn between them make one
#' stretch, and only one of them can pair with it: on the Icelandic towed gear of June 2023 the prior raised the
#' tows paired from 95.0 to 99.0 % (fishycode decision 090).
#'
#' Pings without a speed are left out. The windowed speed counts the pings in port; the stretches do not.
#'
#' @param pings A data frame or lazy DuckDB table with `vid`, `time`, the speed and speed range columns, and
#'   `port_call` if given.
#' @param records Optional: the declared tows, a data frame or lazy DuckDB table with `vid` and the `start` and
#'   `end` columns. Records without both, or that do not end after they start, are not used, and records with
#'   the same times (twins) count once.
#' @param window_s Half the speed window, in seconds; 0 uses each ping's own speed.
#' @param max_gap_s A gap between pings longer than this, in seconds, breaks a stretch.
#' @param min_dur_s Stretches shorter than this, in seconds, are dropped.
#' @param dip_s Stretches this close, in seconds, are joined; 0 joins none.
#' @param margin_s How far, in seconds, the prior's cut may reach into the tows on either side of a turn.
#' @param start,end The records' start and end columns.
#' @param port_call A ping column that is not `NA` while the vessel is in port (e.g. `port_call_id` from
#'   [rb_add_voyages()]); `NULL` when every ping is at sea.
#' @param speed,speed_min,speed_max The ping speed and the speed range of a towing ping. The range is the
#'   caller's: the gear's, the vessel's commonest, or the trip's.
#' @param batch_pings About this many pings are collected at a time, whole vessels.
#'
#' @return One row per stretch: `vid`, `stretch_id` (numbered per vessel in time), `s_start` and `s_end` (the
#'   times of its first and last ping), `n_pings`, `opened_by` and `closed_by`: `"speed"` (the speed entered or
#'   left the range), `"gap"` (a gap of more than `max_gap_s`, or the end of the data) or `"record"` (the
#'   prior's cut). A data frame for data frame input, a lazy table for lazy input.
#' @export
rb_create_record_stretches <- function(
  pings,
  records = NULL,
  window_s = 600,
  max_gap_s = 1800,
  min_dur_s = 900,
  dip_s = 0,
  margin_s = 900,
  start = "t2",
  end = "t3",
  port_call = NULL,
  speed = "speed",
  speed_min = "speed_min",
  speed_max = "speed_max",
  batch_pings = 5e7
) {
  .rb_check_input(
    pings,
    "rb_create_record_stretches",
    cols = c(speed = speed),
    types = c(time = "time", speed = "numeric")
  )
  need <- unique(c("vid", "time", speed, speed_min, speed_max, port_call))
  miss <- setdiff(need, colnames(pings))
  if (length(miss)) {
    stop("pings lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  if (!is.null(records)) {
    miss <- setdiff(c("vid", start, end), colnames(records))
    if (length(miss)) {
      stop("records lacks: ", paste(miss, collapse = ", "), call. = FALSE)
    }
  }
  for (x in list(window_s, max_gap_s, min_dur_s, dip_s, margin_s)) {
    stopifnot(is.numeric(x), length(x) == 1, x >= 0)
  }
  lazy_in <- inherits(pings, "tbl_lazy")
  con <- .rb_con_for(pings)
  p <- .rb_as_lazy(dplyr::select(pings, dplyr::all_of(need)), con)
  q <- function(x) .rb_q(con, x)
  port <- if (is.null(port_call)) {
    "FALSE"
  } else {
    sprintf("%s IS NOT NULL", q(port_call))
  }
  vn <- dplyr::collect(dplyr::count(p, vid))
  vn <- vn[!is.na(vn$vid), ]
  vn <- vn[order(-vn$n, vn$vid), ]
  batches <- if (nrow(vn)) {
    split(vn$vid, cumsum(as.numeric(vn$n)) %/% batch_pings)
  } else {
    list()
  }
  out <- lapply(batches, function(g) {
    sql <- sprintf(
      "
WITH p AS (SELECT vid, time, %s AS rb_s, %s AS rb_lo, %s AS rb_hi, %s AS rb_port FROM (%s)
           WHERE time IS NOT NULL AND %s IS NOT NULL),
w AS (SELECT *, %s AS rb_w FROM p)
SELECT vid, time, rb_s, rb_w, rb_lo, rb_hi FROM w WHERE NOT rb_port",
      q(speed),
      q(speed_min),
      q(speed_max),
      port,
      .rb_sql(dplyr::filter(p, vid %in% !!g)),
      q(speed),
      .rb_speed_smooth_sql("rb_s", window_s)
    )
    d <- as.data.frame(dplyr::collect(dplyr::tbl(con, dplyr::sql(sql))))
    d <- d[order(d$vid, d$time, d$rb_s), ]
    r <- NULL
    if (!is.null(records)) {
      r <- dplyr::select(records, dplyr::all_of(c("vid", start, end)))
      r <- as.data.frame(dplyr::collect(dplyr::filter(r, vid %in% !!g)))
      names(r) <- c("vid", "rb_a", "rb_b")
      # twin records (the same times) are one tow to the prior: there is no turn between them to cut at
      r <- unique(r[
        !is.na(r$rb_a) & !is.na(r$rb_b) & r$rb_b > r$rb_a,
      ])
      r <- r[order(r$vid, r$rb_a, r$rb_b), ]
    }
    dv <- split(seq_len(nrow(d)), d$vid)
    rv <- if (is.null(r)) list() else split(seq_len(nrow(r)), r$vid)
    res <- lapply(names(dv), function(v) {
      i <- dv[[v]]
      j <- rv[[v]]
      s <- .rb_stretch_one(
        as.numeric(d$time[i]),
        d$rb_s[i],
        d$rb_w[i],
        d$rb_lo[i],
        d$rb_hi[i],
        as.numeric(r$rb_a[j]),
        as.numeric(r$rb_b[j]),
        max_gap_s,
        min_dur_s,
        dip_s,
        margin_s
      )
      if (is.null(s)) {
        return(NULL)
      }
      cbind(data.frame(vid = d$vid[i[1]]), s)
    })
    do.call(rbind, res)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(
      vid = vn$vid[0],
      stretch_id = integer(0),
      a = numeric(0),
      b = numeric(0),
      n_pings = integer(0),
      opened_by = character(0),
      closed_by = character(0)
    )
  }
  out <- data.frame(
    vid = out$vid,
    stretch_id = out$stretch_id,
    s_start = as.POSIXct(out$a, origin = "1970-01-01", tz = "UTC"),
    s_end = as.POSIXct(out$b, origin = "1970-01-01", tz = "UTC"),
    n_pings = out$n_pings,
    opened_by = out$opened_by,
    closed_by = out$closed_by
  )
  out <- out[order(out$vid, out$s_start), ]
  rownames(out) <- NULL
  if (!lazy_in) {
    .rb_unregister(con)
    return(tibble::as_tibble(out))
  }
  dplyr::tbl(con, .rb_register(con, out, "rb_str_"))
}

# One vessel's stretches, times as seconds. tt, s, w, lo, hi: the vessel's pings at sea with a speed, in time
# order (s the own speed, w the windowed one, lo/hi the range); ra, rb: its records' starts and ends, in order.
# Returns stretch_id, a, b (the first and last ping's time), n_pings, opened_by, closed_by, or NULL.
.rb_stretch_one <- function(
  tt,
  s,
  w,
  lo,
  hi,
  ra,
  rb,
  max_gap_s,
  min_dur_s,
  dip_s,
  margin_s
) {
  n <- length(tt)
  ok <- !is.na(w) & !is.na(lo) & !is.na(hi) & w >= lo & w <= hi
  if (!any(ok)) {
    return(NULL)
  }
  gap <- diff(tt) > max_gap_s
  g <- cumsum(c(TRUE, !ok[-n] | gap))[ok]
  i <- which(ok)
  first <- i[!duplicated(g)]
  last <- i[!duplicated(g, fromLast = TRUE)]
  a <- tt[first]
  b <- tt[last]
  op <- ifelse(c(TRUE, gap)[first], "gap", "speed")
  cl <- ifelse(c(gap, TRUE)[last], "gap", "speed")
  if (dip_s > 0 && length(a) > 1) {
    g2 <- cumsum(c(TRUE, a[-1] - b[-length(b)] > dip_s))
    f <- !duplicated(g2)
    l <- !duplicated(g2, fromLast = TRUE)
    a <- a[f]
    op <- op[f]
    b <- b[l]
    cl <- cl[l]
  }
  k <- b - a >= min_dur_s
  if (!any(k)) {
    return(NULL)
  }
  a <- a[k]
  b <- b[k]
  op <- op[k]
  cl <- cl[k]
  # the prior: a stretch holding two or more records is cut at the slowest ping between each and the next
  if (length(ra) >= 2) {
    # the records that can overlap the stretch start after a[j] - (the longest record) and before b[j]
    dmax <- max(rb - ra)
    A <- B <- numeric(0)
    OP <- CL <- character(0)
    for (j in seq_along(a)) {
      lo_r <- findInterval(a[j] - dmax, ra) + 1L
      hi_r <- findInterval(b[j], ra)
      cand <- if (lo_r <= hi_r) lo_r:hi_r else integer(0)
      inside <- cand[
        .rb_overlap(ra[cand], rb[cand], a[j], b[j]) >=
          0.5 * (rb[cand] - ra[cand])
      ]
      ca <- a[j]
      cb <- numeric(0)
      if (length(inside) >= 2) {
        for (r in inside[-length(inside)]) {
          i1 <- max(
            findInterval(rb[r] - margin_s, tt, left.open = TRUE),
            findInterval(a[j], tt)
          ) +
            1L
          i2 <- min(
            findInterval(ra[r + 1] + margin_s, tt),
            findInterval(b[j], tt, left.open = TRUE)
          )
          if (i1 > i2) {
            next
          }
          i0 <- i1 - 1L + which.min(s[i1:i2])
          if (i0 <= 1 || i0 >= n) {
            next
          }
          cb <- c(cb, tt[i0 - 1])
          ca <- c(ca, tt[i0 + 1])
        }
      }
      A <- c(A, ca)
      B <- c(B, cb, b[j])
      OP <- c(OP, op[j], rep("record", length(ca) - 1))
      CL <- c(CL, rep("record", length(cb)), cl[j])
    }
    k <- B > A
    a <- A[k]
    b <- B[k]
    op <- OP[k]
    cl <- CL[k]
    o <- order(a, b)
    a <- a[o]
    b <- b[o]
    op <- op[o]
    cl <- cl[o]
  }
  data.frame(
    stretch_id = seq_along(a),
    a = a,
    b = b,
    n_pings = findInterval(b, tt) - findInterval(a, tt, left.open = TRUE),
    opened_by = op,
    closed_by = cl
  )
}

# Seconds two intervals overlap, 0 when they do not.
.rb_overlap <- function(a1, b1, a2, b2) {
  pmax(0, pmin(b1, b2) - pmax(a1, a2))
}

#' Pair declared tows with towing stretches, in order and one to one
#'
#' A step-2 link of the fishing-activity flow. Each declared tow (a logbook record) is paired with at most one
#' stretch of the track at towing speed ([rb_create_record_stretches()]), and each stretch with at most one tow, so
#' no ping can belong to two tows. The pairs keep the order of both: per vessel, a later tow pairs with a later
#' stretch. Of all such pairings, the one with the most overlap between the stretches and their tows, each tow
#' widened by `slack_s` on both sides, is taken. A pair needs some overlap.
#'
#' On the Icelandic towed gear of 2007-2026 (1.29 M tow operations) 87.3 % of tows paired, and bottom trawl of
#' 2015-2025 98-99 %, its start within 15 / 30 minutes of the declared one for 55-60 / 77-80 % (fishycode
#' decision 090). Pairing each tow with its best stretch on its own agreed as well, but gave 27,863 stretches to
#' two tows. Where one speed range does not describe a gear's phases (Danish seine, pelagic trawl), or one stretch
#' holds several short tows (dredges), the pairing is poorer.
#'
#' When two pairings tie, the later tow takes the stretch, and of two records with the same start and end the
#' one with the lower key. Twin records (one tow entered twice) so compete for its stretch: give them as one
#' record if they are one tow.
#'
#' @param records A data frame or lazy DuckDB table of declared tows: the `key` columns, `vid`, `start` and `end`.
#' @param stretches Stretches from [rb_create_record_stretches()].
#' @param key The columns that identify a record.
#' @param start,end The records' start and end columns.
#' @param slack_s How far, in seconds, a tow is widened on either side when it is scored against a stretch.
#' @return One row per record: the key, `vid`, `stretch_id`, `s_start` and `s_end` (`NA` when unpaired),
#'   `overlap_s` (the paired stretch's overlap with the widened tow), `dt_start_s` and `dt_end_s` (`s_start -
#'   start` and `s_end - end`, seconds), `n_candidates` (the stretches that overlap the widened tow) and
#'   `stretch_lost` (`TRUE` when it has candidates but is unpaired: they went to its neighbours). A record with no
#'   start or end, or one that does not end after it starts, has `n_candidates` 0. A data frame for data frame
#'   input, a lazy table for lazy input.
#' @export
rb_link_record_stretches <- function(
  records,
  stretches,
  key = c(".sid", "schema"),
  start = "t2",
  end = "t3",
  slack_s = 1800
) {
  miss <- setdiff(c(key, "vid", start, end), colnames(records))
  if (length(miss)) {
    stop("records lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  miss <- setdiff(
    c("vid", "stretch_id", "s_start", "s_end"),
    colnames(stretches)
  )
  if (length(miss)) {
    stop("stretches lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  }
  stopifnot(is.numeric(slack_s), length(slack_s) == 1, slack_s >= 0)
  lazy_in <- inherits(records, "tbl_lazy")
  con <- .rb_con_for(records)
  r <- as.data.frame(dplyr::collect(dplyr::select(
    records,
    dplyr::all_of(unique(c(key, "vid", start, end)))
  )))
  s <- as.data.frame(dplyr::collect(dplyr::select(
    stretches,
    dplyr::all_of(c("vid", "stretch_id", "s_start", "s_end"))
  )))
  ra <- as.numeric(r[[start]])
  rb <- as.numeric(r[[end]])
  sa <- as.numeric(s$s_start)
  sb <- as.numeric(s$s_end)
  ok <- !is.na(r$vid) & !is.na(ra) & !is.na(rb) & rb > ra
  pair <- rep(NA_integer_, nrow(r))
  ov <- rep(NA_real_, nrow(r))
  ncand <- integer(nrow(r))
  rv <- split(which(ok), r$vid[ok])
  sv <- split(seq_len(nrow(s)), s$vid)
  for (v in intersect(names(rv), names(sv))) {
    # in time, and on a full tie the lower key last: the later row wins a tie (.rb_align), as the lower key keeps
    # a shared window in rb_create_record_windows()
    ri <- rv[[v]]
    ri <- ri[do.call(
      base::order,
      c(
        list(ra[ri], rb[ri]),
        unname(as.list(r[ri, key, drop = FALSE])),
        list(
          decreasing = c(FALSE, FALSE, rep(TRUE, length(key))),
          method = "radix"
        )
      )
    )]
    si <- sv[[v]]
    si <- si[order(sa[si], sb[si])]
    # clusters of tows and stretches that overlap, directly or through others: a pairing in order is the best
    # pairing of each cluster on its own
    beg <- c(ra[ri] - slack_s, sa[si])
    fin <- c(rb[ri] + slack_s, sb[si])
    o <- order(beg, -fin)
    cl <- integer(length(beg))
    cl[o] <- cumsum(c(TRUE, beg[o][-1] > cummax(fin[o])[-length(o)]))
    cr <- cl[seq_along(ri)]
    cs <- cl[length(ri) + seq_along(si)]
    for (k in intersect(cr, cs)) {
      i <- ri[cr == k]
      j <- si[cs == k]
      sc <- outer(
        seq_along(i),
        seq_along(j),
        function(x, y) {
          .rb_overlap(
            ra[i[x]] - slack_s,
            rb[i[x]] + slack_s,
            sa[j[y]],
            sb[j[y]]
          )
        }
      )
      sc2 <- outer(
        seq_along(i),
        seq_along(j),
        function(x, y) .rb_overlap(ra[i[x]], rb[i[x]], sa[j[y]], sb[j[y]])
      )
      ncand[i] <- rowSums(sc > 0)
      p <- .rb_align(sc, sc2)
      pair[i] <- j[p]
      ov[i] <- sc[cbind(seq_along(i), p)]
    }
  }
  out <- r[, unique(c(key, "vid")), drop = FALSE]
  out$stretch_id <- s$stretch_id[pair]
  out$s_start <- s$s_start[pair]
  out$s_end <- s$s_end[pair]
  out$overlap_s <- ov
  out$dt_start_s <- sa[pair] - ra
  out$dt_end_s <- sb[pair] - rb
  out$n_candidates <- ncand
  out$stretch_lost <- is.na(pair) & ncand > 0
  out <- out[order(out$vid, ra), ]
  rownames(out) <- NULL
  if (!lazy_in) {
    .rb_unregister(con)
    return(tibble::as_tibble(out))
  }
  dplyr::tbl(con, .rb_register(con, out, "rb_pair_"))
}

# The pairing in order with the most total score: a monotone alignment of rows (tows) with columns (stretches),
# a pair only where the score `sc` is positive. Totals are compared on `sc`, then on `sc2` (the overlap with the
# tow itself, not widened): of two tows whose widened windows both hold a stretch, the one that overlaps it
# keeps it. On a full tie, a pair is preferred to skipping the row, and skipping the row to skipping the column
# (as the prototype). Returns the column paired with each row, or NA.
.rb_align <- function(sc, sc2 = 0 * sc) {
  n <- nrow(sc)
  m <- ncol(sc)
  D <- E <- matrix(0, n + 1, m + 1)
  B <- matrix(0L, n + 1, m + 1)
  ge <- function(a, a2, b, b2) a > b || (a == b && a2 >= b2)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      if (sc[i, j] > 0) {
        c3 <- D[i, j] + sc[i, j]
        e3 <- E[i, j] + sc2[i, j]
      } else {
        c3 <- -Inf
        e3 <- 0
      }
      o1 <- D[i, j + 1]
      e1 <- E[i, j + 1]
      o2 <- D[i + 1, j]
      e2 <- E[i + 1, j]
      if (ge(c3, e3, o1, e1) && ge(c3, e3, o2, e2)) {
        D[i + 1, j + 1] <- c3
        E[i + 1, j + 1] <- e3
        B[i + 1, j + 1] <- 3L
      } else if (ge(o1, e1, o2, e2)) {
        D[i + 1, j + 1] <- o1
        E[i + 1, j + 1] <- e1
        B[i + 1, j + 1] <- 1L
      } else {
        D[i + 1, j + 1] <- o2
        E[i + 1, j + 1] <- e2
        B[i + 1, j + 1] <- 2L
      }
    }
  }
  pair <- rep(NA_integer_, n)
  i <- n
  j <- m
  while (i > 0 && j > 0) {
    k <- B[i + 1, j + 1]
    if (k == 3L) {
      pair[i] <- j
      i <- i - 1
      j <- j - 1
    } else if (k == 1L) {
      i <- i - 1
    } else {
      j <- j - 1
    }
  }
  pair
}
