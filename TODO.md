# TODO

## Track-interpolation join is O(groups x track_rows), not O(n)

Noticed 2026-09-23 while looking at `fishycode`'s `curate/logbook_sensor_gps.R`,
which interpolates lon/lat for the afli GPS logger from the merged AIS track
(`curated/ais/ping`), bounded per voyage:

```r
inside |>
  group_by(vid, trip) |>
  group_modify(function(g, key) {
    a <- tk[tk$vid == key$vid & tk$trip == key$trip, ]   # re-scans the WHOLE
    ...                                                    # track table, per group
    g$lon <- approx(an, a$lon, tn, rule = 1, ties = mean)$y
    g$lat <- approx(an, a$lat, tn, rule = 1, ties = mean)$y
    ...
  })
```

That inner filter re-scans all of `tk` (a year's whole merged track, can be
millions of rows) once per `(vid, trip)` group (thousands of trips/year) -
O(groups x track_rows), not O(track_rows). Not yet measured how much of that
script's wall time this actually costs (do that before touching anything -
same rule as everywhere else here: measure, don't assume).

This isn't a `whack_forward`-shaped problem (no Rcpp needed) - "find the
track point immediately before/after a given timestamp, within the same
voyage" is a nearest-neighbour / ASOF-join problem, which vectorises or
SQL-ifies cleanly, unlike `whack_forward`'s genuinely sequential recurrence.

Two candidate fixes, if/when this becomes a `ramb` function rather than
inline script logic:

1. Pre-split `tk` by `(vid, trip)` once (`split()` or a keyed `data.table`)
   instead of re-filtering per group. Cheap, no compiled code, same spirit as
   `whack_fwdbwd`'s existing vectorised approach.
2. Push the whole interpolation into DuckDB lazily - `ASOF JOIN` (or
   `LAG`/`LEAD` over a window) to find the bracketing track points per logger
   fix, then the linear-interpolation formula in SQL. Bigger win (no
   `collect()` of the track at all) but needs the same care given to
   verifying `whack_forward`'s port: exact `rule = 1` (no extrapolation) and
   `dt_sec` before/after semantics have to match, not just look close.

Not implemented. Left here as a candidate `ramb` function (e.g.
`rb_interpolate_track()`) rather than touched in place, since it currently
lives as inline logic in `fishycode`, not in this package.
