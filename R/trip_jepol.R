# trip_jepol.R — the ICES datacall's trip rule (define_trips_pol), recoded in base R from rb_trip_jepol()'s
# data.table body (plan 013: recoded, not copied). Sequential (the split loop), so tier 4: one vessel at a time.
#
# A trip runs from the first ping at sea to the first ping back in harbour. Trips of min_dur hours or less are
# dropped; with split, a trip over max_dur hours is cut at a long gap between its pings, again and again. Two
# defects of the original are kept so that it is reproduced exactly; they are logic questions in fishycode's
# plan 013: the cut falls one ping before the longest gap, and the "gap under 3 minutes" test reads the gap
# before that ping. Where the original stopped with an error (a long trip with fewer than two pings inside it),
# the trip is left whole, as the original does for a gap under 3 minutes.

.rb_jepol_one <- function(time, inh, vessel, min_dur, max_dur, split) {
  o <- order(time); time <- time[o]; inh <- inh[o]
  n <- length(time)
  none <- data.frame(vessel = vessel[0], trip = character(0), depart = time[0], arrival = time[0])
  intv <- c(NA, diff(as.numeric(time)) / 60)
  ev <- c(0, -diff(inh))
  ev[ev == -1] <- 2
  if (all(ev == 0)) return(none)
  if (inh[1] == 0) ev[1] <- 1
  if (inh[n] == 0) ev[n] <- 2
  dep <- time[ev == 1]; arr <- time[ev == 2]
  if (length(dep) != length(arr) && length(dep) != 1 && length(arr) != 1)
    stop("Vessel ", vessel, ": departures and arrivals do not pair.", call. = FALSE)
  k <- max(length(dep), length(arr))
  tr <- data.frame(trip = paste0(vessel, "_", seq_len(k)), depart = rep(dep, length.out = k), arrival = rep(arr, length.out = k))
  hrs <- function(d) as.numeric(difftime(d$arrival, d$depart, units = "hours"))
  tr$dur <- hrs(tr)
  tr <- tr[tr$dur > min_dur, ]
  if (split) {
    while (any(tr$dur > max_dur)) {
      i <- which(tr$dur > max_dur)[1]
      inside <- which(time > tr$depart[i] & time < tr$arrival[i])
      j <- if (length(inside) >= 2) which.max(intv[inside][-1]) else integer(0)
      g <- inside[j]
      if (!length(g) || intv[g] < 3) { tr$dur[i] <- max_dur; next }
      nt <- data.frame(trip = paste(tr$trip[i], 1:2, sep = "_"), depart = c(tr$depart[i], time[g]),
                       arrival = c(time[g - 1], tr$arrival[i]))
      nt$dur <- hrs(nt)
      tr <- rbind(tr[-i, ], nt)
      tr <- tr[order(tr$depart), ]
    }
  }
  tr <- tr[tr$dur != 0, ]
  if (any(tr$dur < min_dur)) {
    warning("At least one trip for ", vessel, " is shorter than min_dur after splitting at max_dur.", call. = FALSE)
    tr <- tr[tr$dur > min_dur, ]
  }
  if (!nrow(tr)) return(none)
  data.frame(vessel = vessel, trip = tr$trip, depart = tr$depart, arrival = tr$arrival)
}

# All vessels: the trip table, and each ping's trip (NA outside every trip).
.rb_jepol <- function(vessel, time, inh, min_dur = 0.5, max_dur = 72, split = TRUE) {
  if (anyNA(inh)) stop("The in-harbour indicator has missing values.", call. = FALSE)
  idx <- split(seq_along(vessel), vessel)
  trips <- do.call(rbind, lapply(names(idx), function(v) {
    ii <- idx[[v]]; .rb_jepol_one(time[ii], inh[ii], vessel[ii][1], min_dur, max_dur, split)
  }))
  trip <- rep(NA_character_, length(vessel))
  if (!is.null(trips) && nrow(trips)) {
    for (v in unique(trips$vessel)) {
      ii <- idx[[as.character(v)]]; t <- trips[trips$vessel == v, ]
      k <- findInterval(as.numeric(time[ii]), as.numeric(t$depart))
      hit <- k > 0 & as.numeric(time[ii]) <= as.numeric(t$arrival[pmax(k, 1)])
      trip[ii[hit]] <- t$trip[k[hit]]
    }
  }
  list(trips = trips, trip = trip)
}
