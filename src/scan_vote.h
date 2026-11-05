// scan_vote.h — the forward scan of rb_flag_ping_impossible(anchor = "vote"), shared by whack_local.cpp (stage 2 of
// "local") and whack_forward.cpp ("forward", and "clean"'s second stage) (fishycode plan 066).
//
// Today's scan ("kept") keeps the last ping it kept as its anchor j; at a jump, a ping q that j cannot reach, it flags
// q and every ping after until one is reachable from j, so a wrong anchor costs the good pings after it. Here the pings
// after the jump decide which end is wrong, once per anchor:
//   the vote    of the vote_n pings after q (none past a gap), n_q can be reached from q and not from j, n_j the
//               reverse;
//   the detour  where the vote ties (none voting included), with p the ping kept before j and r the ping after q:
//               j's detour d(p,j) + d(j,r) - d(p,r) against q's d(p,q) + d(q,r) - d(p,r);
// and j is flagged in q's place (code 3) when n_q > n_j, or the vote ties and j's detour is more than twice q's plus
// detour_m, provided the leg that skips j is possible (from p; or j is the first of its stretch). q is then judged
// again from the ping before j. Otherwise q is flagged, as today (code 2).
//
// On ramb's dansk_truth and on Iceland's hand-judged cases (fishycode's study 2026-11-05-forward-anchor, plan 066) it
// caught more wrong positions and flagged fewer good pings than today's scan for every method; its cost, a run of
// consistent wrong pings before a silence long enough that the leg skipping a good anchor covers it (planted
// relocations at 6 min). The constants are hand-set: 5, 3 and 10 pings gave the same within two pings.
#ifndef RAMB_SCAN_VOTE_H
#define RAMB_SCAN_VOTE_H

#include <vector>

namespace ramb_scan {

// m candidates in time order. gap(a, b): the time between them is over the scan's gap; ok(a, b): the leg possible;
// dist(a, b): its length, m. Returns a code per candidate (0 kept, 2 flagged, 3 an anchor flagged by the pings after),
// and in `by` the candidate each flag was judged against (q's anchor; the anchor's q).
template <class Gap, class Ok, class Dist>
std::vector<int> vote(int m, Gap gap, Ok ok, Dist dist, int vote_n, double detour_m, std::vector<int>& by) {
  std::vector<int> code(m, 0);
  by.assign(m, -1);
  if (m < 2) return code;
  auto pass = [&](int a, int b) { return gap(a, b) || ok(a, b); };
  std::vector<int> kept;
  kept.reserve(m);
  std::vector<char> voted(m, 0);
  kept.push_back(0);
  for (int q = 1; q < m; q++) {
    if (kept.empty()) {
      kept.push_back(q);
      continue;
    }
    int j = kept.back();
    if (pass(j, q)) {
      kept.push_back(q);
      continue;
    }
    if (!voted[j]) {
      voted[j] = 1;
      const bool has_p = kept.size() >= 2 && !gap(kept[kept.size() - 2], j);
      const int p = has_p ? kept[kept.size() - 2] : -1;
      int nq = 0, nj = 0;
      for (int r = q + 1, c = 0; r < m && c < vote_n; r++, c++) {
        if (gap(r - 1, r)) break;
        const bool from_j = pass(j, r), from_q = pass(q, r);
        if (from_q && !from_j) nq++;
        if (from_j && !from_q) nj++;
      }
      const bool skip = has_p ? pass(p, q) : true;
      bool flip = skip && nq > nj;
      if (!flip && skip && has_p && nq == nj && q + 1 < m && !gap(q, q + 1)) {
        const int r = q + 1;
        const double dj = dist(p, j) + dist(j, r) - dist(p, r), dq = dist(p, q) + dist(q, r) - dist(p, r);
        flip = dj > 2 * dq + detour_m;
      }
      if (flip) {
        code[j] = 3;
        by[j] = q;
        kept.pop_back();
        if (kept.empty() || pass(kept.back(), q)) {
          kept.push_back(q);
          continue;
        }
        j = kept.back();
      }
    }
    code[q] = 2;
    by[q] = j;
  }
  return code;
}

}  // namespace ramb_scan

#endif
