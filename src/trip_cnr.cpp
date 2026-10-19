// trip_cnr.cpp — compiled core of rb_cut_trip_voyages(method = "cnr"): CNR-IRBIM's trip finder.
//
// After create_fishing_trip() by A. Galdelli, J. Pulcinella, A.N. Tassetti and E. Armelloni (CNR-IRBIM,
// R4AIS v1.0.1, doi 10.5281/zenodo.4757505; WKSSFGEO dev_branch, R-dev/galdelli_pulcinella_tassetti/R/
// global_functions.R, GNU GPL 3). Recoded, not copied: the R function walks a data frame row by row and grows
// its trip table with rbind(); this is one pass per vessel. Checked against the R function, trip for trip, on its
// own Italian tracks (wkssfgeo_cnr_ais), on the Danish tracks, on perturbed copies and on Icelandic AIS; see NEWS.md.
//
// Two passes, with the spatial work in R between them (only the trip-end pings need a port or the coast):
//   cnr_cut_cpp():  the state machine. A vessel is in port after n_in consecutive pings in a port polygon
//                   at a mean speed under speed_kn; a trip runs from its last such ping (one later if the
//                   next ping is over shift_h away) to the ping before the n_in-th port ping of the next
//                   such run. A silence over gap_h at sea also ends a trip, and the next starts after it.
//   cnr_join_cpp(): two trips split by a silence, the first with no arrival port and the second with no
//                   departure port, are joined if they touch (no ping between them), unless both ends lie
//                   in the coastal zone, then only if together they last under join_h. Otherwise each open
//                   end is flagged for the closest port (CNR's closest_port()).
//
// Kept from the R function on purpose, so the two agree ping for ping: the first n_in pings are tested
// together (port run, or first-to-n_in-th ping over gap_h); a departure port is read n_in - 1 pings before
// the trip's start; the gap test is strict (> gap_h); a vessel of fewer than min_n pings has no trips; and a
// silence that ends inside a port starts a trip there that runs on through the stay (the vessel only arrives
// after a run of n_in pings all at sea), so a port call reached through a silence is not found.
// Different on purpose: a missing speed makes no ping run slow (R stops on it).
#include <Rcpp.h>
#include <vector>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame cnr_cut_cpp(IntegerVector grp, NumericVector time, IntegerVector inport, NumericVector speed,
                      int n_in, double speed_kn, double gap_h, double shift_h, int min_n) {
  const int N = grp.size();
  std::vector<int> o_grp, o_s, o_e, o_dep;
  int a = 0;
  while (a < N) {
    int b = a + 1;
    while (b < N && grp[b] == grp[a]) ++b;
    const int n = b - a;
    if (n >= min_n && n >= n_in + 1) {
      // 1-based row r of this vessel is index a + r - 1, as rowid in the R function
      auto T = [&](int r) { return time[a + r - 1]; };
      auto hours = [&](int r1, int r2) { return (T(r2) - T(r1)) / 3600.0; };
      // the queue: the last n_in pings up to row j, all in port? and slow on average?
      auto all_in = [&](int j) {
        for (int r = j - n_in + 1; r <= j; ++r) if (inport[a + r - 1] != 1) return false;
        return true;
      };
      auto none_in = [&](int j) {
        for (int r = j - n_in + 1; r <= j; ++r) if (inport[a + r - 1] == 1) return false;
        return true;
      };
      auto slow = [&](int j) {
        double s = 0;
        for (int r = j - n_in + 1; r <= j; ++r) s += speed[a + r - 1];
        return s / n_in < speed_kn;  // false if any speed is NA
      };
      int start = 0, inport_s = 0, pos = n_in - 1, buffer = 0, inizio = 0, delta = 0, check = 1;
      int last_end = -1;  // endid of the last trip built, -1 before the first
      auto build = [&](int j) {
        // the start moves one ping on if that ping is over shift_h later, but never past the trip's end
        if (hours(start, start + 1) > shift_h) start = start + 1;
        if (start > j - 1) start = start - 1;
        o_grp.push_back(grp[a]);
        o_s.push_back(a + start - 1);
        o_e.push_back(a + j - 2);
        o_dep.push_back(start - pos >= 1 ? a + start - pos - 1 : -1);
        last_end = j - 1;
      };

      if (all_in(n_in)) {
        if (slow(n_in)) {
          start = n_in; inport_s = 1; buffer = 1; inizio = 1;
        }
      } else if (hours(1, n_in) > gap_h) {
        start = n_in; inport_s = 1; inizio = 1;
      }

      for (int j = n_in + 1; j <= n; ++j) {
        const bool full = all_in(j), empty = none_in(j);
        if (start == 0 && inport_s == 0) {
          if (full && slow(j)) {
            start = j; inport_s = 1; buffer = 1; inizio = 1;
          } else if (hours(j - 1, j) > gap_h) {
            start = j; inport_s = 1; inizio = 1;
          }
          continue;
        }
        if (inizio == 1) {
          if (buffer == 1 && inport_s == 1) {
            if (full) start = j;
            else if (empty) { inport_s = 0; buffer = 0; }
          } else if (empty) {
            inport_s = 0;
          }
        }
        if (inizio == 0) {
          if (full && inport_s == 0) {
            if (slow(j)) { start = j; inport_s = 1; buffer = 1; inizio = 1; delta = 0; }
          } else if (delta == 1) {
            start = j; inport_s = 1; buffer = 1; inizio = 1; delta = 0;
          } else if (buffer == 0 && inport_s == 0 && hours(j - 1, j) > gap_h) {
            start = j; inport_s = 1; inizio = 1;
          }
        } else if (buffer == 0) {
          // at sea on a trip: back in port, or a silence
          if (full && inport_s == 0 && slow(j)) {
            build(j);
            start = j; inport_s = 1; inizio = 1; buffer = 1; delta = 0;
          }
          if (inport_s == 0) {
            if (last_end >= 0) check = (last_end != j) ? 1 : 0;
            if (check == 1 && hours(j - 1, j) > gap_h) {
              build(j);
              start = j; delta = 1; inizio = 1; inport_s = 1;
            }
          }
        }
      }
    }
    a = b;
  }
  const int m = o_grp.size();
  IntegerVector g(m), s(m), e(m), d(m);
  for (int i = 0; i < m; ++i) {
    g[i] = o_grp[i];
    s[i] = o_s[i] + 1;
    e[i] = o_e[i] + 1;
    d[i] = o_dep[i] < 0 ? NA_INTEGER : o_dep[i] + 1;
  }
  return DataFrame::create(_["grp"] = g, _["s"] = s, _["e"] = e, _["dep"] = d);
}

// Trips as cut above, in order within each vessel, with each end's port (NA where none) and how many
// coastal-zone polygons hold each end's ping. Rows are 1-based ping indices; ping times are needed for join_h.
// CNR's test is that the two counts do not sum to 2: on one (unioned) zone, that not both ends lie in it. On a
// zone of overlapping polygons, as the R function reads one, a ping in two of them alone sums to 2.
// [[Rcpp::export]]
DataFrame cnr_join_cpp(IntegerVector grp, IntegerVector s, IntegerVector e, IntegerVector port_from,
                       IntegerVector port_to, IntegerVector coast_s, IntegerVector coast_e,
                       NumericVector time, double join_h) {
  const int M = grp.size();
  struct Trip { int g, s, e, from, to, joined, cs, ce; bool from_closest, to_closest; };
  std::vector<Trip> out;
  out.reserve(M);
  int a = 0;
  while (a < M) {
    int b = a + 1;
    while (b < M && grp[b] == grp[a]) ++b;
    std::vector<Trip> tt;
    for (int i = a; i < b; ++i)
      tt.push_back({grp[i], s[i], e[i], port_from[i], port_to[i], 0, coast_s[i], coast_e[i], false, false});
    size_t i = 0;
    while (i + 1 < tt.size()) {
      Trip &x = tt[i], &y = tt[i + 1];
      const bool open = (y.from == NA_INTEGER && !y.from_closest) && (x.to == NA_INTEGER && !x.to_closest);
      if (!open) { ++i; continue; }
      bool join;
      int code;
      if (y.s - x.e > 1) {
        join = false; code = -2;
      } else if (y.cs + x.ce != 2) {
        join = true; code = 0;
      } else {
        join = (time[y.e - 1] - time[x.s - 1]) / 3600.0 < join_h; code = -1;
      }
      if (join) {
        x.e = y.e; x.to = y.to; x.ce = y.ce; x.to_closest = y.to_closest; x.joined += 1;
        tt.erase(tt.begin() + i + 1);
      } else {
        x.to_closest = true; y.from_closest = true; x.joined = code; y.joined = code;
        ++i;
      }
    }
    out.insert(out.end(), tt.begin(), tt.end());
    a = b;
  }
  const int m = out.size();
  IntegerVector g(m), so(m), eo(m), fr(m), to(m), jn(m);
  LogicalVector fc(m), tc(m);
  for (int k = 0; k < m; ++k) {
    g[k] = out[k].g; so[k] = out[k].s; eo[k] = out[k].e; fr[k] = out[k].from; to[k] = out[k].to;
    jn[k] = out[k].joined; fc[k] = out[k].from_closest; tc[k] = out[k].to_closest;
  }
  return DataFrame::create(_["grp"] = g, _["s"] = so, _["e"] = eo, _["port_from"] = fr, _["port_to"] = to,
                           _["joined"] = jn, _["from_closest"] = fc, _["to_closest"] = tc);
}
