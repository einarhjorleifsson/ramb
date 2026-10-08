# ramb (development version)

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
