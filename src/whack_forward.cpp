// whack_forward.cpp — compiled core of whack_forward()'s forward scan.
//
// The R version in whackies.R (.fwd(), pre-2026-09-23) is a plain R for-loop:
// correct, but ~1000x slower than this port on realistic track sizes, because
// the recurrence is inherently sequential (the reference point is the last
// ACCEPTED ping, not a fixed lag) and so cannot be expressed as a vectorised
// R op or a fixed-offset SQL window function the way whack_fwdbwd() is.
//
// This is a line-for-line port, including one behaviour that looks like a bug
// but is reproduced deliberately for now: if a single NA lon/lat poisons
// `prev`, every later point compared against it gets an NA distance, and
// `ISNAN(d)` (matching the R side's `is.na(d)`) forces flag = TRUE rather than
// advancing `prev` — so the poisoning cascades until max_gap_h forces a reset.
// Verified byte-identical against the R implementation on whacks1 and on
// synthetic stress cases (isolated spikes, runs of 2/3/5, a >4h gap, and an
// injected NA). Flagged for the maintainer to decide on separately; not
// changed here.
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector whack_forward_cpp(NumericVector lon, NumericVector lat,
                                 NumericVector time,
                                 double ms_max, double max_gap_sec, double min_dt,
                                 IntegerVector feed = IntegerVector::create(),
                                 double noise_m = 0) {
  int n = lon.size();
  LogicalVector flag(n, false);
  if (n < 2) return flag;
  // no feed codes (the default) is no noise floor; otherwise one code per ping
  const bool floor_on = noise_m > 0 && feed.size() == n;

  // Haversine, R = 6,371,000 m — the rb_calc_distance() sphere (R/track.R is the reference
  // of record); the same formula lives in whack_sda.cpp and in the SQL of .rb_whack_fwdbwd (R/whack.R).
  const double r = 6371000.0;
  int prev = 0;

  for (int i = 1; i < n; i++) {
    if (NumericVector::is_na(lon[i]) || NumericVector::is_na(lat[i]) ||
        NumericVector::is_na(time[i])) {
      prev = i;
      continue;
    }

    double dt = time[i] - time[prev];
    if (ISNAN(dt) || dt > max_gap_sec) {
      prev = i;
      continue;
    }

    double phi1 = lat[prev] * M_PI / 180.0;
    double phi2 = lat[i]    * M_PI / 180.0;
    double dphi = (lat[i] - lat[prev]) * M_PI / 180.0;
    double dlam = (lon[i] - lon[prev]) * M_PI / 180.0;

    double s = std::sqrt(std::sin(dphi / 2) * std::sin(dphi / 2) +
                          std::cos(phi1) * std::cos(phi2) *
                          std::sin(dlam / 2) * std::sin(dlam / 2));
    if (!ISNAN(s) && s > 1.0) s = 1.0;
    double d = 2 * r * std::asin(s);

    // A step shorter than min_dt counts as min_dt (was 1e-6 s until 2026-10-08, which
    // flagged a duplicate report in the same second, 0.4 m away).
    double dt2 = dt;
    if (dt2 < min_dt) dt2 = min_dt;

    // The optional noise floor between feeds (rb_flag_ping_impossible(feed = , noise_nm = )): a leg between
    // two pings of different known feeds (-1 = unknown) no longer than noise_m is never impossible, so the
    // ping is good and becomes the reference. noise_m = 0 is off. A NaN distance is not a short one.
    if (floor_on && feed[i] >= 0 && feed[prev] >= 0 && feed[i] != feed[prev] && d <= noise_m) {
      prev = i;
      continue;
    }

    if (ISNAN(d) || d / dt2 > ms_max) {
      flag[i] = true;   // bad — do NOT advance prev
    } else {
      prev = i;          // good — advance reference
    }
  }

  return flag;
}
