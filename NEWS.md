# ramb (development version)

## Review fixes (2026-10-13)

From a review of the 2026-10-09..12 changes against fishycode and the fishydata store:

- `rb_cap_outliers(method = "miller")`: the cap no longer comes from outside the function. The fallback `if (!exists("limit"))` also searched the enclosing environments, so any object called `limit` on the search path was returned as the cap (with `limit <- 0.5` in the global environment, `rb_cap_outliers(c(1, 2, 3), "miller")` gave `0.5 0.5 0.5`). Each branch now returns; on 20,000 random vectors the caps are identical to before when no such object exists.
- `rb_flag_ping_impossible(method = "forward" | "fwdbwd" | "sequential")`: the flags no longer depend on the input's row order. These engines sorted by `(vid, time)` only, and the `fwdbwd` SQL ordered its window by `time` only, so pings of one second at two places took whatever order they came in — and parquet reads come back in a different order from run to run. `lon` and `lat` now break the tie everywhere, as `.rb_whack_sda()` always did. On one month of Icelandic AIS (June 2023, 3.3 M pings, 8,638 same-second pairs) reshuffling the rows changed 22-25 `"forward"` flags and 1-4 `"fwdbwd"` flags before, none after; the lazy `fwdbwd` gave 1,698-1,701 flags over four runs before, 1,697 every time after, equal to the data-frame branch. `method = "clean"` (sda first) was already stable and is unchanged: fishycode's production chain gives the same flags on every ping. The only order effect left is which of two byte-identical rows gets `"vmask"` and which `"spike"`.
- `rb_cut_trip_voyages(method = "jepol")`: a vessel whose last ping is itself a departure (the ping before it in port) no longer breaks the vessel's voyages. The rule, inherited unchanged from DTU Aqua's `define_trips()`, overwrote that departure with an arrival, leaving one departure short: after one voyage the departures were recycled into a voyage spanning the port stay; after two or more the whole call stopped with "departures and arrivals do not pair". With `by_year = TRUE` this is any vessel-year whose last ping is a departure: 791 of the 34,286 Icelandic vessel-years with port pings, and 2023 of the Icelandic data stopped at the first of them. That trailing one-ping voyage now has no duration and is dropped like any other under `min_dur_h`; the voyages before it are untouched. On 2023 (33.1 M pings, 1,821 vessels) the method now runs in 0.7 min: 106,890 voyages, none overlapping. `dansk` has no such vessel, so the pinned fixture is unchanged.
- `rb_calc_distance()`: the named vector form now needs all four coordinates. The defaults `lon1 = lon`, `lat1 = lat` exist for the data-frame form, but with `lon1` (or `lat1`) left out of a named vector call they were evaluated as ordinary variables: an error when nothing called `lon` was in reach, and a wrong distance without complaint when one was (`lon <- 5` in the global environment turned `rb_calc_distance(lat1 = 64, lon2 = -21, lat2 = 64)` into 1,258,560 m). It is now an error either way. Positional calls and the data-frame form are unchanged.
- `rb_calc_speed()` and `rb_calc_step_acceleration()` default to `method = "geodesic"` again, as before 2026-10-12; `"haversine"` stays available. The data decided it. On 35,501 steady straight legs of Icelandic STK VMS (June 2023: 5-15 min, 6-15 kn, speed within 0.3 kn and heading within 2 degrees at both ends), the haversine speed shows its own error against the recorded speed over ground: east-west legs read 0.085 % lower than north-south ones (95 % bootstrap interval 0.058-0.115 %). The geodesic speed shows no such difference (0.019 %, interval -0.011 to 0.047 %), and its median ratio to SOG is 1.0011 against 0.9978. A speed over ground does not depend on heading, so this difference is the sphere's. Derived speeds share a column, and the gears' speed windows, with recorded ones, so they belong on the same scale; on fishycode's emodnet 2017 track (19.2 M legs) the haversine default had moved 0.49 % of pings across one of the gear speed bounds. The impossible-position filters stay on the sphere (their 25 kn threshold does not care about 0.3 %), and so does the `rb_calc_distance()` default, which the filters share. Karney costs 0.74 s per million legs.

## Review fixes (2026-10-12)

From the flagged-issues review; behaviour changes are characterised exactly:

- `rb_plot_trip()`: the `col` argument is now honoured (the points are coloured by it, default `speed`), and the trip-path tooltip follows the `tid` argument instead of the hardcoded `"tid"`.
- `rb_cap_outliers(method = "winsorize")`: an `NA` in `x` no longer turns the whole vector `NA` — the limit is now computed with `na.rm = TRUE`, matching `method = "iqr"`. Output is unchanged when `x` has no `NA`. (fishycode's `products/exports/buseful_2025-05-05.R` caps `effort` this way; affected only if `effort` contains `NA`.)
- `rb_link_trip_logbook()`: the unconditional "Departure and arrival columns will be removed" warning is gone (the drop is documented in `@return`); the function now warns only when `ve` itself has columns named like the departure/arrival columns — a collision dplyr resolves by suffixing (`.x`/`.y`), so the logbook side then survives the drop.
- `rb_keep_in_polygons()` and `rb_drop_in_polygons()`: for an `sfc` input they now return the filtered geometries (inside / outside) instead of a logical mask; `rb_detect_in_polygons()` remains the mask function. `sf` data-frame input is unchanged.
- `rb_summarise_track()`: output columns renamed to snake_case and the `n.tips` typo fixed — `pings`, `t_min`, `t_max`, `s_min`, `s_med`, `s_max`, `di_min`, `di_med`, `di_max`, `du_min`, `du_med`, `du_max`, `n_trips`, `n_harbours`; the values are unchanged.
- `rb_pad_harbour()`: documentation only — the padded id is a port id (e.g. the `port_id` column from `rb_flag_ping_port()`); the name and arguments are kept, as plan 013 recorded.
- Deliberately left alone: `rb_calc_step_acceleration()` stays distance/dt² (documented as such; no consumer in ramb, fishycode or fishymodel), and `.rb_get_ices_gears()` keeps its misnamed `target` column for now.

## Argument renames (2026-10-12)

Sixteen argument names across twelve functions, for consistency and against base-name shadowing — hard renames, no aliases:

- `rb_calc_step_time(units)` → `unit` (matching `rb_calc_duration()`).
- `rb_calc_track_time(date, fill)` → `time`, `fill_ends`.
- `rb_encode_ices_rectangle(safe, useI)` → `snap_to_midpoint`, `use_i`.
- `rb_flag_ping_duplicate(n)` → `n_neighbours`.
- `rb_plot_trail(no_lines = TRUE)` → `path = FALSE` — note the flip: `no_lines = FALSE` becomes `path = TRUE`.
- `rb_peek(criteria)` → `threshold`.
- `rb_sum_across(start_name)` → `prefix`.
- `rb_get_gear_vocabulary(list)` → `code_list`.
- `rb_predict_gear_width(length)` → `length_m`.
- `rb_read_logbook_mfri(VIDs, YEARS)` → `vids`, `years`.
- `rb_share_catch(trails)` → `pings`, including the internals and error messages.
- `rb_detect_in_polygons()`, `rb_keep_in_polygons()` and `rb_drop_in_polygons(x, y)` → `points`, `polygons`.

fishycode call sites to migrate: `curate/ais_ping_clean.R` (`n = DUP_K` → `n_neighbours`), `curate/ais_voyage.R` and `docs/stk_data-inspection.qmd` (`no_lines = FALSE` → `path = TRUE`, 5 sites), `products/ices_vms_datacall/build.R` (`safe = FALSE` → `snap_to_midpoint = FALSE`), and the unevaluated `curate/ramb-articles/ais-workflow.Rmd` (`YEARS = YEARS` → `years`).

## Bug fixes (2026-10-12)

- `rb_cap_outliers(method = "miller")`: an all-zero vector came back as all `-Inf` (the limit was assigned to a misspelled variable, so `max()` of an empty vector was used, with a warning); it is now returned unchanged.
- The internal `.rb_read_trails()` is removed — legacy from the old MFRI workflow, unused in the package, and broken (`nrow(ais > 0)`); `rb_read_trail_mfri()` is the reader. One unevaluated historical sketch in fishycode (`curate/ramb-articles/ais-workflow.Rmd`) still calls the pre-rename name.

## Documentation reviewed, articles now Quarto (2026-10-12)

- The roxygen documentation of every exported function was reviewed for consistency and precision. Development history and sister-repo references moved out of the help pages into plain code comments, internal dot-functions now carry plain comments instead of `@noRd` pages, and the methods are properly referenced: Karney (2013) for `rb_calc_distance(method = "geodesic")`, Freitas et al. (2008) for `rb_flag_ping_impossible(method = "sda")`, Hintzen et al. (2012) for the `"jepol"` voyage rule, `rb_link_trip_logbook()` and `rb_encode_ices_rectangle(method = "vmstools")`, Eigaard et al. (2016) for `rb_predict_gear_width()` and the `benthis_*` data.
- The five articles are now Quarto (`.qmd`) and link profusely, both to the function reference and to external sources (the WGSFD, the data-call methods, DuckDB, mapview). In `.qmd`, pkgdown does not expand the rmarkdown-style `[rb_x()]` shortcuts, so the articles link explicitly: `[rb_x()](../reference/rb_x.html)`.

## Distance: a `method` argument and Karney's geodesic; jepol splits at the longest gap (2026-10-12)

- `rb_cut_trip_voyages(method = "jepol")`: fixed the off-by-one inherited from `rb_trip_jepol()` — a voyage over `max_dur_h` was split one ping before its longest gap, and the three-minute test read the gap before that ping. The cut now falls at the longest gap. Counted on `dansk`: no cut moves at `max_dur_h = 72`, two voyages of 92 at 48, three of 93 at 24; no voyage is gained or lost (at 24, the moved cut re-times two segments across the `min_dur_h` threshold, flipping two pings' at-sea status). Without splits the output is unchanged.

- `rb_calc_distance()` gains a `method` argument: `"haversine"` (default, unchanged — the filters' sphere) and `"geodesic"` (Karney's algorithm on WGS84, via GeographicLib's `geodesic.c` vendored unchanged in `src/`; exact everywhere, including nearly antipodal pairs). It also gains a data-frame form, `rb_calc_distance(d, lon2 = port_lon, lat2 = port_lat)`: first argument a data frame, the coordinate columns tidy-eval arguments (`lon1`/`lat1` default to `lon`/`lat`). The vector forms (positional or named) are unchanged; lazy DuckDB tables are not supported.
- `rb_calc_speed()` and `rb_calc_step_acceleration()` gain the same `method` argument, defaulting to `"haversine"` — one sphere across distance, speed and the impossible-position filters, and about 40-50x faster than the ellipsoid. Their previous behaviour was `"geodesic"`; pass that explicitly where the old values are needed. Speeds shift by a systematic, latitude- and bearing-dependent amount within ~0.6 %; at Icelandic latitudes (63-67° N) the haversine speed is always the lower, by 0.24 % (north-south legs) to 0.40 % (east-west); `method = "geodesic"` values additionally shift by up to ~1 micrometre per leg from the engine swap (Vincenty → Karney), and near-antipodal pairs are now exact instead of a warned Haversine fallback.

## Articles reorganised (2026-10-11)

The articles are now a small user-facing set explaining the essence of the package, all self-contained (package data only, no undeclared dependencies):

- `flow`: the fishing-activity flow, the naming conventions and the basis vocabulary — the get-started article (named `flow`, not `ramb`: pkgdown reserves the package name for a root vignette).
- `trips`: extended with step 0 (flagging invalid and impossible pings, tagging port pings with `rb_flag_ping_port()`) and closes with `rb_assign_trip()`. Interactive `mapview` maps with toggleable layers illustrate the cut: two clean trips (`EX_3`, Sep 2020) where the rules agree, the afternoon where `"runs"` and `"jepol"` disagree (`EX_10`, Jul 2014, two six-minute mini-voyages), and a morning of 33-km impossible-position spikes (`EX_10`, Oct 2013) with raw-vs-clean tracks. Diagnostic time-series panels (log-scale implied speed, turn angle) and a technical side point show the subtler failure mode — two interleaved emission streams on `EX_10`/`EX_11` — where `rb_flag_ping_duplicate()` must run before `rb_flag_ping_impossible()`.
- `gear`: the former `benthis` article, now self-contained (no `vmstools` needed for example data) and widened to métier extraction and the BENTHIS lookup; the hotlinked journal figure is gone.
- `catch`: new — sharing catch and value over fishing pings with `rb_share_catch()`.
- `tools`: new — the step-independent toolbox (track calculations, interpolation, polygons, ICES rectangles, binning, capping, timestamps, MMSI).
- `case_example01` moved to fishymodel (`vignettes/articles/mendo2019.Rmd`), where the `fm_*` models it demonstrates live.
- `technicalities` removed — a developer note on `rb_bin_midpoint()`'s floating-point floor; the user-facing fact (works on DuckDB lazy tables) is now in the `tools` article and the implementation note survives in git history.

## Renames for consistency with the verb glossary (2026-10-11)

- `dc_spread_cash_and_catch()` is now `rb_share_catch()` — "share" is the glossary verb for distributing a quantity while conserving the total, and the last export without the `rb_` prefix is gone. The placeholder arguments `ignore_Einstein` and `go_nuclear` are now `raise_unallocated = FALSE` and `raise_unmatched = FALSE` (still reserved, not yet implemented).
- `rb_add_tripid()` is now `rb_link_trip_logbook()` — a VMS-to-logbook crosswalk, "link" in the glossary, and no longer confusable with `rb_assign_trip()`.
- `rb_grade()` is removed; `rb_bin_midpoint()` is the same binning with the floating-point floor fixed.
- Internal helpers now follow the dot-prefix policy and lost their stray help pages: `.rb_iqr()`, `.rb_winsor()`, `.rb_miller()` (cap.R), `.rb_mobile_vid()`, `.rb_stk_trail()`, `.rb_read_trails()` (read_trails.R), `.rb_lump_gears()` (logbook_mfri.R) and `.rb_numbers_only()` (utils.R).
- The `R/` sources are reorganised from 40 topically-named files into 22 named after their stage or function (`trip.R`, `track.R`, `time.R`, `gear.R`, `catch.R`, ...); test files mirror them. No behaviour change.
- The articles are user-facing again: six developer-note sketches (two of which sourced `TOPSECRET.R`) moved out to fishycode (`curate/ramb-articles/`); `trips.Rmd` was rewritten for the one-flow API (`rb_cut_trip_voyages()`), `case_example01.Rmd` now calls the `fm_*` models in fishymodel, and the four remaining articles appear in the pkgdown navbar.

## Old model functions resurrected in fishymodel (2026-10-11)

The trip-model wrappers removed on 2025-09-10 (commit f9d4039) — `rb_gaussian()`, `rb_gaussian_binary_clustering()`, `rb_hidden_markov_step()` and `rb_hidden_markov_step_and_turn()`, plus the `tidy_*` wrappers — now live in the sister package `fishymodel` (`../fishymodel`), renamed with an `fm_` prefix.

## Raster and rayshade functions move to omb (2026-10-11)

The multibeam-bathymetry family is now its own package, `omb`, renamed with an `om_` prefix: `rb_create_base_raster()` → `omb::om_create_base_raster()`, `rb_create_bbox()` → `om_create_bbox()`, `rb_calc_xyz_extent()` → `om_calc_xyz_extent()`, `rb_rasterize_xyz()` → `om_rasterize_xyz()`, `rb_rayshade_raster()` → `om_rayshade_raster()`, `rb_rayshade_raster_rgb()` → `om_rayshade_raster_rgb()`, `rb_rayshade_xyz_dynamic()` → `om_rayshade_xyz_dynamic()`, `rb_convert_rayshade_to_rgb()` → `om_convert_rayshade_to_rgb()`. Nothing called these functions in fishycode or the other packages. With them go the `terra`, `rayshader` and `scales` dependencies; the never-used `ggnewscale` leaves Suggests too.

## Aliases removed, filters internal, one rectangle encoder (2026-10-11)

The old names kept on 2026-10-08 are removed, three days after they were introduced; the package exports only the current names. This breaks old code loudly (`could not find function`), not silently.

- `rb_encode_ices_rectangle()` gains `method`: `"robust"` (default, the algorithm to date) and `"vmstools"`, a faithful reproduction of `vmstools::icesRectangle()` kept for compatibility with vmstools-based workflows; the two diverge on exact 10-degree longitudes (e.g. `lon = -10`: `54E0` vs `54D9`). The unexported `rb_d2ir_vmstools()` is absorbed and deleted.
- Fixed a load-time bug: the `stopifnot` block embedded in `R/rb_d2ir.R` expected wrong rectangles (`43E1` for a position whose rectangle is `54D9`) and failed package load in interactive sessions. It is replaced by `tests/testthat/test-rb_d2ir.R`, which covers both methods and the 10-degree boundaries.
- The ping filters are now internal engines of `rb_flag_ping_impossible(method = ...)` and `rb_flag_ping_duplicate()`: `rb_whack_clean()`, `rb_whack_sda()`, `rb_whack_forward()`, `rb_whack_fwdbwd()`, `rb_whack_sequential_fast()`, `rb_whack_duplicates()` and the bare `whack_*` names are no longer exported, and the originals `rb_whacky_speed()`, `rb_whacky_speed_mendo()`, `rb_whacky_speed_trip()` and `rb_whacky_distance()` are gone.
- `rb_trip()` and `rb_trip_jepol()` are removed; `rb_cut_trip_voyages(method = "runs" | "jepol")` is the function.
- The 46 rename aliases of 2026-10-08 (`rb_speed()`, `rb_d2ir()`, `dc_*()`, `mb_*()`, `read_is_*()`, `rb_flag_ping_harbour()`, and the rest) are removed.
- `rb_rms_tbl()` and `rb_st_remove()` are removed; `geo` leaves Suggests.
- `R/rb_workflow.R` was misnamed, not trial code: it holds the MFRI logbook reader and curation helpers and is now `R/logbook_mfri.R`.

Fix list for fishycode (all calls found there on 2026-10-11):

- `products/exports/astd_iseez.R:201`: `ramb::rb_trip()` → `rb_cut_trip_voyages(method = "runs")`.
- `curate/checks/` (~110 calls in 13 files): `rb_whack_clean()` / `rb_whack_forward()` / `rb_whack_fwdbwd()` / `rb_whack_sda()` → `rb_flag_ping_impossible(method = "clean" / "forward" / "fwdbwd" / "sda")`.
- `vignettes/articles/` (ais-workflow, interpolation, trips) still references removed names; these are unevaluated historical sketches, left as-is for now.

## rb_calc_duration (2026-10-11)

- `rb_calc_duration()`: the elapsed time between two datetimes as plain numeric in a chosen `unit` (`"secs"`, `"mins"`, `"hours"`, `"days"`). One function for three inputs: a lazy table (computed in the database via `epoch()`), a data frame, or a `POSIXt` vector inside `mutate()`. On tables it adds a column and refuses to overwrite an existing one.

## "port", not "harbour" (2026-10-09)

The place a trip starts and ends is a port throughout (fishycode plan 014, decision 075).

- `rb_flag_ping_harbour()` is now `rb_flag_ping_port(pings, ports)`, adding `port_id`. The old name warns, and still
  takes `harbours` and returns `harbour_id`.
- `rb_find_trip_stays(pings, ports, ...)`: ports carry `port_id`, and so do the stays and the events. The rule
  `gap_near_harbour` is `gap_near_port`.
- `rb_cut_trip_voyages()`: `port_id` (for `"runs"`), and `port_from`/`port_to` on the voyages.
- `rb_assign_trip()`: `stay_port_id`, and the stay's key `stay_id` where the stays carry one. A ping can then hold
  the key and not the stay's values.
- Not renamed: the `dansk_harbours` data, `rb_read_harbours_mfri()` (it reads a file of that name) and the
  superseded `rb_trip_jepol(in_harbour)`.

## Step 2 and the window method of step 4 (2026-10-09)

Ported from fishycode (`R/duration_limit.R`, `curate/station_window.R`, `R/trail_sql.R`; fishycode plan 014
phase 2). Each port gives fishycode's built outputs exactly: 7,496,440 record windows, 7,239,778 judged durations,
and one month of the trail (4,864,265 fixes, 1,098,145 matches). All run on data frames and lazy DuckDB tables.

- `rb_limit_record_duration()`: a duration above the gear's limit becomes `NA`, the raw value is kept.
- `rb_build_record_windows()`: one window per (record, phase): tow, set, haul, jig, or the whole day; capped at
  the vessel's next recorded event; `win_basis` says where each came from.
- `rb_link_record_pings()`: every (ping, record) match; day windows claim only unclaimed pings.
- `rb_assign_record()`: `n_records`, and the record where exactly one holds the ping.
- `rb_assign_fishing(method = "window")`: in a window that can be fishing and inside its speed range.

## Step 1: three ways to cut voyages (2026-10-08)

- `rb_cut_trip_voyages(method = "runs")`: every run of harbour-tagged pings is a stay (was `rb_trip()`).
- `rb_cut_trip_voyages(method = "jepol")`: the ICES VMS datacall rule (was `rb_trip_jepol()`), recoded in base R
  from its `data.table` body. On the `dansk` data it gives the old trips exactly, with and without splits. Two
  defects are kept for now so that it does: the split cuts one ping before the longest gap, and the 3-minute
  test reads the gap before that ping. Where the old function stopped with an error (a long trip with fewer than
  two pings inside), the trip is now left whole.
- `rb_trip()` and `rb_trip_jepol()` are superseded and warn; `rb_trip_jepol()` now runs the recode, and
  `data.table` is no longer imported. The unexported `rb_interval_id()` (unused, results in sorted order, not
  input order) is removed.

## Dependencies (2026-10-08)

- `traipse` is recoded, not imported: step distances use Vincenty's formula on the WGS84 ellipsoid, within a
  micrometre of `traipse::track_distance()` (geodist's geodesic) on track steps. `traipse` stays in Suggests for
  the test that compares them.
- `glue` is replaced by a base-R filler for the two SQL templates that used it.
- `terra`, `mapdeck`, `scales` and `icesVocab` move to Suggests. The raster, map and vocabulary functions that need
  them say which package to install.
- `rlang`, already called directly and installed with `dplyr`, is declared in Imports.

## Step 0: the harbour tag (2026-10-08)

- `rb_flag_ping_harbour()`: each ping gets the harbour polygon it lies in (`harbour_id`, plus any columns in
  `keep`); a ping inside two polygons takes the smaller one, so no ping is doubled. Harbour pings are labelled,
  not flagged: they stay in the trail.
- Ported from fishycode's `curate/ais_ping_tag.R`: on the whole Icelandic union (543,776,104 pings, 2007-2026)
  the same polygon for every ping.
- The pkgdown reference lists the step functions by stage; the superseded `rb_whack_*` / `rb_whacky_*` are in
  their own group.

## Step 1: voyages and the trip of each ping (2026-10-08)

- `rb_cut_trip_voyages()`: the sea between two harbour stays, per vessel and year, with the harbours either side;
  voyages under `min_pings` dropped after numbering. `rb_assign_trip()`: each ping's stay, voyage, `trip_id` and
  `trip_basis` (`"reconstructed"` / `"none"`); declared trips and landings as sources follow.
- Ported from fishycode's `curate/ais_trip.R`: on the Icelandic data the same 1,372,656 voyages and, for all
  535,613,643 pings, the same stay and voyage in every year.
- A call on data frames now releases the frames it registered in DuckDB once it has its result.

## Step 1 builder: harbour stays (2026-10-08)

- `rb_find_trip_stays()`: harbour stays measured in elapsed time (rules `tag_then_gap`, `gap_near_harbour`,
  `tag_span`, and `event_pair` from optional harbour events), with a minimum stay per harbour, an arrival and
  a per-harbour departure radius, and a window for chunked runs. Runs in DuckDB; lazy in, lazy out.
- Ported from fishycode's `curate/harbour_stay.R`: on the Icelandic data (2007-2026) it gives the same
  1,551,604 stays, row for row.
- `DBI`, `duckdb` and `duckdbfs` move to Imports: lazy DuckDB tables are the default.

## Step 0 of the flow: flag pings (2026-10-08)

- `rb_flag_ping_invalid()`, `rb_flag_ping_duplicate()` and `rb_flag_ping_impossible(method = ...)` label
  pings in one column, `ping_flag` (NA = usable, else the reason). Each tests only rows not yet flagged, so
  a chain applies them in order. Data frames and lazy DuckDB tables give the same labels.
- `rb_flag_ping_impossible()`'s methods are the existing filters: `"clean"` (was `rb_whack_clean()`), `"sda"`,
  `"forward"`, `"fwdbwd"`, `"sequential"`.
- Superseded, old bodies kept, warning once per session from user code: `rb_whack_clean()`, `rb_whack_sda()`,
  `rb_whack_forward()`, `rb_whack_fwdbwd()`, `rb_whack_sequential_fast()`, `rb_whack_duplicates()`,
  `rb_whacky_speed()`, `rb_whacky_speed_mendo()`, `rb_whacky_speed_trip()`.
- Checked on real data: on all 69,815,643 pings of one year of the Icelandic AIS union, the chain gives the
  same label as the procedure it replaces for every ping.

# ramb 2026.10.08.1

## Function names follow one rule (2026-10-08)

Every function is now named verb first: `rb_<verb>_<stage>[_<detail>]` for a step of the fishing-activity
flow (vessel, ping, trip, record, gear, fishing, effort, catch) and `rb_<verb>_<object>` for tools. Functions
that did the same task are one function with a `method`, `part`, `list` or `from`/`to` argument. The old names
still work, warn once per session, and will be removed in a future version. See the Naming section of
`?ramb` and fishycode's `curate/plans/013-ramb-one-flow.md`.

| Old name | New name |
|---|---|
| `rb_speed()` | `rb_calc_speed()` |
| `rb_distance()` | `rb_calc_distance()` |
| `rb_st()` | `rb_calc_step_time()` |
| `rb_sa()` | `rb_calc_step_acceleration()` |
| `rb_track_time()` | `rb_calc_track_time()` |
| `rb_event()` | `rb_number_runs()` |
| `rb_d2ir()` | `rb_encode_ices_rectangle()` |
| `rb_midpoint()` | `rb_bin_midpoint()` |
| `rb_points_in_polygons()` | `rb_detect_in_polygons()` |
| `rb_st_keep()` | `rb_keep_in_polygons()` |
| `rb_st_drop()` | `rb_drop_in_polygons()` |
| `rb_benthis_width()` | `rb_predict_gear_width()` |
| `rb_gearwidth_proxy()` | `rb_fill_gear_width()` |
| `rb_std_meshsize()` | `rb_standardise_gear_meshsize()` |
| `rb_mmsi_category()` | `rb_classify_mmsi()` |
| `rb_mmsi_flag()` | `rb_lookup_mmsi_country()` |
| `rb_check_crosses_year()` | `rb_detect_crosses_year()` |
| `dc_check_eflalo_crosses_year()` | `rb_check_trip_crosses_year()` |
| `dc_create_timestamp()` | `rb_create_timestamp()` |
| `rb_mapdeck()` | `rb_plot_trail()` |
| `rb_md_trip()` | `rb_plot_trip()` |
| `rb_summary()` | `rb_summarise_track()` |
| `rb_logbook()` | `rb_read_logbook_mfri()` |
| `rb_trail()` | `rb_read_trail_mfri()` |
| `read_is_harbours()` | `rb_read_harbours_mfri()` |
| `read_is_survey_stations()` | `rb_read_survey_stations_mfri()` |
| `read_is_survey_tracks()` | `rb_read_survey_tracks_mfri()` |
| `mb_base_raster()` | `rb_create_base_raster()` |
| `mb_bb()` | `rb_create_bbox()` |
| `mb_rashade_xyz_dynamic()` | `rb_rayshade_xyz_dynamic()` |
| `mb_rasterize_xyz()` | `rb_rasterize_xyz()` |
| `mb_rayshade_raster()` | `rb_rayshade_raster()` |
| `mb_rayshade_raster_rgb()` | `rb_rayshade_raster_rgb()` |
| `mb_rayshade_to_rgb()` | `rb_convert_rayshade_to_rgb()` |
| `mb_xyz_extent()` | `rb_calc_xyz_extent()` |
| `rb_ms2kn()` | `rb_convert_speed(from = "ms", to = "kn")` |
| `rb_kn2ms()` | `rb_convert_speed(from = "kn", to = "ms")` |
| `rb_gear_from_metier()` | `rb_extract_metier(part = "gear")` |
| `rb_target_from_metier()` | `rb_extract_metier(part = "target")` |
| `rb_met5_from6()` | `rb_extract_metier(part = "metier5")` |
| `rb_get_ices_gears()` | `rb_get_gear_vocabulary(list = "gears")` |
| `rb_get_ices_target()` | `rb_get_gear_vocabulary(list = "target")` |
| `rb_get_ices_metier5()` | `rb_get_gear_vocabulary(list = "metier5")` |
| `rb_get_ices_metier6()` | `rb_get_gear_vocabulary(list = "metier6")` |
| `rb_get_ices_metier5_benthis_lookup()` | `rb_get_gear_vocabulary(list = "metier5_benthis_lookup")` |
| `rb_cap_iqr()` | `rb_cap_outliers(method = "iqr")` |
| `rb_cap_miller()` | `rb_cap_outliers(method = "miller")` |
| `rb_cap_winsorize()` | `rb_cap_outliers(method = "winsorize")` |

Other changes in the same release:

- `rb_predict_gear_width()` gains `method = "benthis"`.
- Duplicate definitions removed: the second `rb_st_keep()` (`points_in_not_in_polygons_filter.R`) and the first
  `mb_rasterize_xyz()`; the surviving bodies are the ones R was already using.
- `Depends: R (>= 4.1.0)` (the code uses `|>`); Roxygen markdown is on; `duckdbfs`, `geo`, `omar` and `trip`,
  used but undeclared, are in Suggests.
