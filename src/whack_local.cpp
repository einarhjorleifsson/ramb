// whack_local.cpp — compiled core of rb_flag_ping_impossible(method = "local"): each leg judged against the
// vessel's own speed, from its recorded speed over ground, with no heading (fishycode plan 062 step 3, the prototype
// `local_sog` of its study's scripts/local_speed.R; built in step 4).
//
// The local speed of a ping is the median of the recorded speeds of the K pings on each side (its own left out, and
// the pings without one). A leg is impossible when it is longer than margin_m + (factor x v + margin_ms) x its time
// step (at least min_dt), v the local speed; where no ping near has a recorded speed, v is unknown and the limit is
// the fixed one, margin_m + kn_max x the time step. Per vessel (the input sorted by vessel, time, lon, lat):
//   1. a ping whose two legs are both impossible while the leg that skips it is not is flagged (code 1): a real turn
//      keeps its legs at the vessel's speed, a bad position makes both too long. A track's first ping, or the first
//      after a gap: its leg out impossible and the next leg possible; the last likewise. Repeated on the pings left,
//      the local speeds taken again, until no more.
//   2. a forward scan from the last ping kept, with the same limit (v the larger of the two pings'), restarting after
//      a gap (code 2): the runs of bad pings. With anchor = 1 (rb_flag_ping_impossible(anchor = "vote"), fishycode
//      plan 066) the pings after a jump may find the anchor the wrong end (scan_vote.h), and it is flagged (code 3).
// Each flag's basis: 1 where the ping's local speed came from recorded speeds, 2 where the fixed limit judged it.
//
// Each vessel's own limits (fishycode plan 065, rb_create_ping_limits()): whack_local_v_cpp() takes, per ping, the
// fixed limit kn_v (m/s, the vessel's ceiling), the margin m0_v (m, twice the position noise) and the time error te_v
// (s, the larger of the clock's grain and its measured error), and a leg is impossible when longer than
//   m0 + (factor x v + margin_ms) x (max(dt, min_dt) + te)   or, v unknown,   m0 + kn x (max(dt, min_dt) + te),
// each the larger of the leg's two ends'. whack_local_cpp() is the same with kn_max, margin_m and no time error on
// every ping, as before. A flag the ceiling judged has basis 3.
//
// The optional noise floor between feeds is the other engines' (whack_forward.cpp): a leg between pings of two known
// feeds no longer than noise_m counts as no distance. A ping with no position or time is not judged and is no ping's
// neighbour. The prototype in R gives the same flags on ramb's planted faults (fishycode's plan 062 step 4 study).
#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include "scan_vote.h"
using namespace Rcpp;

namespace {

// Haversine, R = 6,371,000 m: the filters' sphere (R/track.R's rb_calc_distance() is the reference of record).
inline double hav(double lon1, double lat1, double lon2, double lat2) {
  const double r = M_PI / 180.0;
  double sp = std::sin((lat2 - lat1) * r / 2), sl = std::sin((lon2 - lon1) * r / 2);
  double a = sp * sp + std::cos(lat1 * r) * std::cos(lat2 * r) * sl * sl;
  return 2 * 6371000.0 * std::asin(std::sqrt(std::min(1.0, a)));
}

// the median of the finite values in v (NaN when none), as R's median(na.rm = TRUE)
inline double median_of(std::vector<double>& v) {
  if (v.empty()) return NAN;
  size_t n = v.size(), h = n / 2;
  std::nth_element(v.begin(), v.begin() + h, v.end());
  double hi = v[h];
  if (n % 2) return hi;
  double lo = *std::max_element(v.begin(), v.begin() + h);
  return (lo + hi) / 2;
}

struct Rule {
  double min_dt, gap_s, factor, margin_ms, noise_m;
  int K;
  bool floor_on;
  // the longest a leg of time step dt may be, at local speed v (NaN: unknown, the fixed limit kn), with the margin m0
  // and the time error te of its ends
  double limit(double dt, double v, double kn, double m0, double te) const {
    double t = std::max(dt, min_dt) + te;
    return std::isnan(v) ? m0 + kn * t : m0 + (factor * v + margin_ms) * t;
  }
};

}  // namespace

// [[Rcpp::export]]
List whack_local_v_cpp(NumericVector lon, NumericVector lat, NumericVector time, IntegerVector vessel,
                       NumericVector sog_ms, IntegerVector feed, NumericVector kn_v, NumericVector m0_v,
                       NumericVector te_v, LogicalVector own, double min_dt, double gap_s, int K, double factor,
                       double margin_ms, double noise_m, int anchor = 0, int vote_n = 5, double detour_m = 50) {
  const int n = lon.size();
  IntegerVector code(n, 0), basis(n, 0);
  NumericVector judged(n, NA_REAL);  // the local speed (m/s) a flagged ping was judged by, NA where the fixed limit
  Rule R{min_dt, gap_s, factor, margin_ms, noise_m, K, noise_m > 0 && feed.size() == n};
  // a leg's limits: the larger of its two ends'
  auto lim = [&](int i, int j, double dt, double v) {
    return R.limit(dt, v, std::max(kn_v[i], kn_v[j]), std::max(m0_v[i], m0_v[j]), std::max(te_v[i], te_v[j]));
  };
  // a flag's basis: 1 a recorded speed judged it, 3 the vessel's own ceiling, 2 the fixed limit
  auto basis_of = [&](int i, double v) { return !std::isnan(v) ? 1 : (own[i] ? 3 : 2); };
  // the distance of a leg, within the noise floor between feeds none
  auto dist = [&](int i, int j) {
    double d = hav(lon[i], lat[i], lon[j], lat[j]);
    if (R.floor_on && feed[i] >= 0 && feed[j] >= 0 && feed[i] != feed[j] && d <= R.noise_m) return 0.0;
    return d;
  };
  // the local speed of each ping in `k` (indices into the input), from the pings of `k` around it
  auto local = [&](const std::vector<int>& k) {
    const int m = k.size();
    std::vector<double> v(m), w;
    w.reserve(2 * R.K);
    for (int q = 0; q < m; q++) {
      w.clear();
      for (int o = -R.K; o <= R.K; o++) {
        int r = q + o;
        if (o == 0 || r < 0 || r >= m) continue;
        double s = sog_ms[k[r]];
        if (!std::isnan(s)) w.push_back(s);
      }
      v[q] = median_of(w);
    }
    return v;
  };
  // a leg's speed: the mean of its two ends' local speeds that are known
  auto both = [](double a, double b) {
    if (std::isnan(a)) return b;
    if (std::isnan(b)) return a;
    return (a + b) / 2;
  };

  int a = 0;
  while (a < n) {
    int b = a;
    while (b < n && vessel[b] == vessel[a]) b++;
    std::vector<int> keep;
    for (int i = a; i < b; i++) {
      if (!std::isnan(lon[i]) && !std::isnan(lat[i]) && !std::isnan(time[i])) keep.push_back(i);
    }
    // a vessel of two pings or fewer: which one is wrong is not in the data
    if (keep.size() < 3) {
      a = b;
      continue;
    }
    // 1. both legs impossible, the leg that skips the ping possible; repeated on the pings left
    while (true) {
      const int m = keep.size();
      if (m < 3) break;
      std::vector<double> v = local(keep);
      std::vector<double> dt(m - 1);
      std::vector<char> bad(m, 0);  // bad[q]: leg q -> q + 1 (the last has none)
      for (int q = 0; q < m - 1; q++) {
        dt[q] = time[keep[q + 1]] - time[keep[q]];
        bad[q] = dist(keep[q], keep[q + 1]) > lim(keep[q], keep[q + 1], dt[q], both(v[q], v[q + 1]));
      }
      auto gapin = [&](int q) { return q == 0 || dt[q - 1] > R.gap_s; };
      auto gapout = [&](int q) { return q == m - 1 || dt[q] > R.gap_s; };
      std::vector<char> hit(m, 0);
      for (int q = 1; q < m - 1; q++) {
        if (gapin(q) || gapout(q) || !bad[q - 1] || !bad[q]) continue;
        double t2 = time[keep[q + 1]] - time[keep[q - 1]];
        hit[q] = dist(keep[q - 1], keep[q + 1]) <= lim(keep[q - 1], keep[q + 1], t2, v[q]);
      }
      for (int q = 0; q < m; q++) {
        // a first ping: its leg out impossible, the next possible (and not the track's last leg)
        if (gapin(q) && !gapout(q) && q + 1 < m - 1) hit[q] = bad[q] && !bad[q + 1] && !gapout(q + 1);
        // a last ping: its leg in impossible, the one before possible
        if (gapout(q) && !gapin(q) && q >= 2) hit[q] = bad[q - 1] && !bad[q - 2] && !gapin(q - 1);
      }
      std::vector<int> left;
      bool any = false;
      for (int q = 0; q < m; q++) {
        if (hit[q]) {
          code[keep[q]] = 1;
          basis[keep[q]] = basis_of(keep[q], v[q]);
          if (!std::isnan(v[q])) judged[keep[q]] = v[q];
          any = true;
        } else {
          left.push_back(keep[q]);
        }
      }
      if (!any) break;
      keep.swap(left);
    }
    // 2. a forward scan from the last ping kept, with the same limit
    if (keep.size() >= 2 && anchor == 1) {
      std::vector<double> v = local(keep);
      const int m = keep.size();
      auto vv = [&](int x, int y) {
        return std::isnan(v[y]) ? v[x] : (std::isnan(v[x]) ? v[y] : std::max(v[y], v[x]));
      };
      auto gap = [&](int x, int y) { return std::fabs(time[keep[y]] - time[keep[x]]) > R.gap_s; };
      auto ok = [&](int x, int y) {
        if (x > y) std::swap(x, y);
        return dist(keep[x], keep[y]) <= lim(keep[x], keep[y], time[keep[y]] - time[keep[x]], vv(x, y));
      };
      auto len = [&](int x, int y) { return hav(lon[keep[x]], lat[keep[x]], lon[keep[y]], lat[keep[y]]); };
      std::vector<int> by;
      std::vector<int> c = ramb_scan::vote(m, gap, ok, len, vote_n, detour_m, by);
      for (int q = 0; q < m; q++) {
        if (!c[q]) continue;
        double s = vv(q, by[q]);
        code[keep[q]] = c[q];
        basis[keep[q]] = basis_of(keep[q], s);
        if (!std::isnan(s)) judged[keep[q]] = s;
      }
    } else if (keep.size() >= 2) {
      std::vector<double> v = local(keep);
      int j = 0;
      for (int q = 1; q < (int)keep.size(); q++) {
        double dt = time[keep[q]] - time[keep[j]];
        double vv = std::isnan(v[q]) ? v[j] : (std::isnan(v[j]) ? v[q] : std::max(v[q], v[j]));
        if (dt > R.gap_s || dist(keep[j], keep[q]) <= lim(keep[j], keep[q], dt, vv)) {
          j = q;
        } else {
          code[keep[q]] = 2;
          basis[keep[q]] = basis_of(keep[q], vv);
          if (!std::isnan(vv)) judged[keep[q]] = vv;
        }
      }
    }
    a = b;
  }
  return List::create(_["code"] = code, _["basis"] = basis, _["v"] = judged);
}

// [[Rcpp::export]]
List whack_local_cpp(NumericVector lon, NumericVector lat, NumericVector time, IntegerVector vessel,
                     NumericVector sog_ms, IntegerVector feed, double kn_max_ms, double min_dt, double gap_s,
                     int K, double factor, double margin_ms, double margin_m, double noise_m) {
  const int n = lon.size();
  return whack_local_v_cpp(lon, lat, time, vessel, sog_ms, feed, NumericVector(n, kn_max_ms), NumericVector(n, margin_m),
                           NumericVector(n, 0.0), LogicalVector(n, false), min_dt, gap_s, K, factor, margin_ms,
                           noise_m, 0, 5, 50);
}
