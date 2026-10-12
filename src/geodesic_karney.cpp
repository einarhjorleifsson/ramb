// geodesic_karney.cpp — Rcpp entry point to the vendored GeographicLib geodesic.c
// (Karney's algorithm, WGS84): exact everywhere, including nearly antipodal pairs, with no
// convergence failure. The engine behind rb_calc_distance(method = "geodesic"); the reference
// of record for the ellipsoid, as rb_calc_distance(method = "haversine") is for the sphere.
//
// Note the argument order: ramb speaks (lon, lat), geod_inverse() speaks (lat, lon) — the
// swap happens here and nowhere else.

#include <Rcpp.h>
#include "geodesic.h"

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector geod_karney(NumericVector lon1, NumericVector lat1,
                          NumericVector lon2, NumericVector lat2) {
  R_xlen_t n = lon1.size();
  if (lat1.size() != n || lon2.size() != n || lat2.size() != n) {
    stop("all four coordinate vectors must have the same length");
  }
  NumericVector out(n);
  geod_geodesic g;
  geod_init(&g, 6378137.0, 1 / 298.257223563); // WGS84
  for (R_xlen_t i = 0; i < n; i++) {
    if (NumericVector::is_na(lon1[i]) || NumericVector::is_na(lat1[i]) ||
        NumericVector::is_na(lon2[i]) || NumericVector::is_na(lat2[i])) {
      out[i] = NA_REAL;
      continue;
    }
    double s12;
    geod_inverse(&g, lat1[i], lon1[i], lat2[i], lon2[i], &s12, nullptr,
                 nullptr);
    out[i] = s12;
  }
  return out;
}
