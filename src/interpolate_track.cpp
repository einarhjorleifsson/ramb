// interpolate_track.cpp — compiled core of rb_interpolate_track()'s data.frame
// path. Replaces a per-group approx() call (still O(groups) R-level function
// call overhead even after pre-splitting track once) with a single compiled
// pass: binary search per event within its group's (pre-sorted, tie-
// collapsed) track slice, then linear interpolation done directly.
//
// Preconditions, enforced by the R wrapper before this is called:
//   - trk_time is sorted ascending WITHIN each group, and has no duplicate
//     times within a group (R side collapses ties via ties=mean first,
//     matching approx(..., ties = mean)'s semantics).
//   - trk_code/x_code are 1-based contiguous group ids, both encoded off the
//     SAME level set, so a group id means the same group in both.
//   - grp_start has length (max group id) + 1: grp_start[g-1]..grp_start[g]
//     (0-indexed, half-open) is group g's slice of the (sorted) track
//     arrays. A group with no track rows has grp_start[g-1] == grp_start[g].
//
// Semantics matched exactly (verified against both the original R
// group_modify() logic and the pure-R rb_interpolate_track_df() path on the
// same synthetic cases, incl. exact-time matches, before/after-range
// queries, single-point groups, and groups absent from the track):
//   - rule = 1 (no extrapolation): a query outside its group's track time
//     range is NA.
//   - An exact time match returns that track point's value(s) with
//     dt_sec = 0.
//   - Otherwise dt_sec is the gap (seconds) between the bracketing points.
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List interpolate_track_cpp(IntegerVector trk_code, NumericVector trk_time,
                            NumericMatrix trk_vals, IntegerVector grp_start,
                            IntegerVector x_code, NumericVector x_time) {
  int n = x_code.size();
  int nval = trk_vals.ncol();
  int ngroups = grp_start.size() - 1;

  NumericMatrix out(n, nval);
  NumericVector dt_sec(n);
  std::fill(out.begin(), out.end(), NA_REAL);
  std::fill(dt_sec.begin(), dt_sec.end(), NA_REAL);

  for (int i = 0; i < n; i++) {
    if (IntegerVector::is_na(x_code[i]) || NumericVector::is_na(x_time[i])) continue;
    int g = x_code[i];
    if (g < 1 || g > ngroups) continue;

    int s = grp_start[g - 1], e = grp_start[g];
    if (e - s < 2) continue;  // fewer than 2 track points: cannot interpolate

    double tq = x_time[i];

    // First index in [s, e) with trk_time >= tq (std::lower_bound by hand,
    // restricted to this group's slice).
    int lo = s, hi = e;
    while (lo < hi) {
      int mid = (lo + hi) / 2;
      if (trk_time[mid] < tq) lo = mid + 1; else hi = mid;
    }

    if (lo == s) {
      if (trk_time[s] == tq) {
        for (int v = 0; v < nval; v++) out(i, v) = trk_vals(s, v);
        dt_sec[i] = 0;
      }
      continue;  // else: before the track's range — NA (rule = 1)
    }
    if (lo == e) continue;  // after the track's range — NA (rule = 1)

    int i0 = lo - 1, i1 = lo;
    double t0 = trk_time[i0], t1 = trk_time[i1];

    if (t1 == tq) {
      for (int v = 0; v < nval; v++) out(i, v) = trk_vals(i1, v);
      dt_sec[i] = 0;
    } else {
      double frac = (tq - t0) / (t1 - t0);
      for (int v = 0; v < nval; v++)
        out(i, v) = trk_vals(i0, v) + frac * (trk_vals(i1, v) - trk_vals(i0, v));
      dt_sec[i] = t1 - t0;
    }
  }

  return List::create(_["values"] = out, _["dt_sec"] = dt_sec);
}
