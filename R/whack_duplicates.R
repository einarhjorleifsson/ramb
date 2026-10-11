# whack_duplicates.R — near-duplicate pings across AIS feeds.
#
# Internal engine behind rb_flag_ping_duplicate() (flag_ping.R); not exported.
# The exported rb_whack_duplicates() wrapper was removed 2026-10-11.
#
# Flags the second copy of a position report that two feeds both received:
# same vessel, a few seconds apart, metres apart (e.g. a national receiver
# network and a commercial feed). The copies share neither timestamp nor
# position exactly, so they survive a match on (vid, time). In the Icelandic
# record 2013-2026 they were 1.2 % of all pings, with a median gap of 1-4 s
# and a median distance of 0.1-0.4 m by feed pair.
#
# Rule: a ping is a near-duplicate when a ping of the same vid from a feed
# ranked higher in `priority` lies within max_dt_s seconds and within the
# distance kn_max allows in max_dt_s (128.6 m at the defaults). Only the n
# nearest pings on each side, in time order, are compared. The top-ranked
# feed is never flagged. Feeds not named in priority rank last and equal, so
# two of them never flag each other; nor do two pings of the same feed.
#
# Returns a logical vector, TRUE for the copy to drop, in input order.
.rb_whack_duplicates <- function(vid, time, lon, lat, feed,
                                 priority = c("stk", "astd", "astdB", "emodnet"),
                                 max_dt_s = 10, kn_max = 25, n = 3) {
  max_m <- kn_max * 1852 / 3600 * max_dt_s
  pr <- match(feed, priority); pr[is.na(pr)] <- length(priority) + 1L
  t  <- as.numeric(time)
  o  <- order(vid, t, pr, lon, lat)
  v <- vid[o]; t <- t[o]; x <- lon[o]; y <- lat[o]; p <- pr[o]
  m <- length(o); dup <- logical(m)
  if (m < 2L) return(dup)
  for (k in seq_len(min(n, m - 1L))) {
    i <- seq_len(m - k); j <- i + k
    near <- v[i] == v[j] & abs(t[j] - t[i]) <= max_dt_s & rb_calc_distance(x[i], y[i], x[j], y[j]) <= max_m
    near[is.na(near)] <- FALSE
    dup[j[near & p[i] < p[j]]] <- TRUE
    dup[i[near & p[j] < p[i]]] <- TRUE
  }
  dup[order(o)]
}
