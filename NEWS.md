# ramb 2026.10.27.1

ramb opens its own DuckDB connection (the maintainer, 2026-10-27).

- A call on data frames runs on one in-memory DuckDB per session, ramb's own, opened with nothing installed. Until
  now it ran on duckdbfs's cached connection, which installs the spatial and h3 extensions as it opens, so a user who
  could not download them (behind an institutional VPN) failed at the first call, spatial or not; it also turned
  automatic extension installs on and set `options(duckdbfs_conn)`. duckdbfs moves from Imports to Suggests.
- A lazy table brings its own connection, as before: one from `duckdbfs::open_dataset()` works in every function, on
  duckdbfs's connection, with data frames of the same call registered there. Every function now takes the connection
  from any of its tables that is lazy, not only the first: pings in memory with lazy voyages (`rb_add_voyages()`),
  matches, windows, records, port calls, vessels, units or landings run on the lazy table's connection and come back
  as the data frame came in. Before, that worked only because everything was on duckdbfs's one connection.
- ramb's instance uses at most a quarter of the machine's memory, or `options(ramb.memory_limit = "8GB")`. DuckDB
  gives every in-memory instance 80 % of it by default, knowing nothing of the others in the session.
- Extensions (spatial, httpfs) are loaded where a function needs them, and installed if they are not there: from
  `options(ramb.extension_repository = <folder or URL>)` when set. Where the download fails, the error names the file
  to fetch and the folder to copy it into (a copied file counts as installed), and that DuckDB's downloads go through
  `HTTP_PROXY`. Where extensions are kept follows duckdb's own setting (`DUCKDB_R_HOME`, `options(duckdb.home)`,
  `~/.duckdb`). `rb_map_track()` loads spatial itself; it relied on duckdbfs having loaded it.
- `rb_read_parquet()` (new): a lazy table over parquet files, a folder (hive-partitioned or not), a glob, or remote
  files (`https://`, `s3://`); over http(s) the files are listed, since DuckDB cannot list a folder there. Only the
  columns and row groups (and partitions) a query needs are read, when it runs.
- `rb_read_example()` takes `con`, to open an example on the connection of the caller's other tables: the lazy
  tables of one call must share a connection.
- `.onUnload()` closes ramb's connection.

# ramb 2026.10.27

The maintainer's two rules for every function (2026-10-27), now in `AGENTS.md` and the package help ("What comes
back"): a function that takes a table returns the construct it was given, rows in the order they came in; and it
leaves the caller's session as it found it, on error too. `tests/testthat/test-etiquette.R` checks both over every
exported function that takes a table (data.frame, tibble and data.table input with its rows shuffled, sf and lazy
tables where taken), and the session alone over the vector tools, `rb_read_example()` and the map viewer.

Breaking, for code that relied on the old class or order:

- Functions that add columns now give the rows back in the order they came in and in the input's class (a
  data.frame stays a data.frame, a tibble a tibble, a data.table a data.table, sf sf): `rb_flag_ping_port()`,
  `rb_add_voyages()`, `rb_add_trip_time()`, `rb_add_record()`, `rb_add_fishing()`, `rb_add_effort()`,
  `rb_add_catch_units()`, `rb_share_catch(method = "eflalo")` and `rb_cap_effort()`. Before, they sorted the rows by
  vessel and time (or vessel and `t1`), and returned a tibble for a data.frame or a data.frame for a tibble. The
  rows are numbered on the way in and put back by that number, in place of the sort by vessel and time: on 4.0
  million Danish pings, `rb_add_fishing()` took 0.24 s (0.21 s before) and `rb_add_effort()` 0.42 s (as before),
  and `rb_flag_ping_port()` on 0.4 million pings and 942 harbours 0.14 s (0.12 s). A lazy table is not reordered.
- Functions that make a table of their own return it in the input's construct: `rb_create_port_calls()`,
  `rb_create_voyages()` (before, a tibble from `"dwell"` and `"runs"`, a data.frame from `"jepol"`, `"cnr"` and
  `"iapesca"`, whatever came in), `rb_link_trip_voyages()`, `rb_link_trip_landings()`, `rb_create_record_windows()`,
  `rb_link_record_pings()`, `rb_link_record_events()`, `rb_create_record_pings()`, `rb_link_record_sets()`,
  `rb_create_record_stretches()`, `rb_link_record_stretches()`, `rb_link_vessel_tracks()`, `rb_share_catch()` and
  `rb_summarise_track()` (a data.table gave a data.frame). Made from sf pings, such a table has no point geometry and
  comes as the sf object's data frame or tibble.
- A table of its own no longer depends on the input's row order: `rb_link_trip_voyages()` sorts by vessel and trip
  key (it came in an order that followed the input), `rb_link_record_stretches()` breaks a tie in start by the key,
  and `rb_share_catch()` sorts the species within a ping and unit.
- `rb_link_trip_landings()` gives a lazy table for lazy `trips` (it collected). `rb_add_trip_time()` follows
  `trips` alone: trips in memory with lazy witnesses give a collected result (it was lazy).

The session:

- Tables on the DuckDB connection are named by number, not from `sample()`: a call on data frames moved the
  caller's random stream, and in a fresh session created `.Random.seed` in the global environment.
  `rb_limit_record_duration()` on a lazy table registers its limits instead of `copy_to()`, which drew too.
- `rb_read_example()` reads its parquet with a query, not through `duckdbfs::open_dataset()`, which named a view
  from R's random numbers and left it on the connection.
- Data frames registered for a call, and its temporary tables, belong to that call: released when it returns or
  fails, unless its lazy result reads them, which then keeps them. Before, a call on data frames released every
  registered frame, so a lazy result made earlier that reads a table given in memory (`rb_flag_ping_port()`'s
  ports, `rb_add_voyages()`'s voyages, `rb_add_record()`'s matches) could no longer be read after any later call; and a call that failed left its frames behind.
  `rb_map_pings()`, `rb_map_track()` and `rb_map_tiles_table()` release theirs when they fail.

Documentation: `rb_create_voyages(method = "iapesca")` and `rb_link_record_sets()` say that they follow iapesca 0.1
(2022), the GitHub copy since deprecated, and that the package lives at
<https://gitlab.ifremer.fr/iapesca/r-packages_iapesca> (version 1.3), with how its trip and set finders now differ.
DESCRIPTION: data.table and withr in Suggests, for the tests.

# ramb 2026.10.25.5

- `rb_map_pings()`, `rb_map_track()`: grouped pings, `group_by(trip)` or `group_by(trip, gear)`, or `choose =`, give
  the page a control per group column above the layers, each value with its number of pings: for a column of 12
  values or fewer, chips that tick on and off, so several show at once (setting and hauling together); for one of
  more, a list to show one at a time; and buttons that step to the previous and next. A choice shows those groups on
  every layer that has the column (a filter in the browser, nothing fetched again), fits the view to them (to the
  combination where every column has one value chosen), and goes into the address under the column's name, several
  values separated by commas. The groups cut the track too, so each group's line is its own. The
  choices are set by the call that starts the map.
- `rb_map()`: `choose`, the map's choices, as the layer functions make them.

# ramb 2026.10.25.4

- `rb_map_pings()` (new) and `rb_map_track()` (changed): the quick look as layers that compose,
  `pings |> rb_map_track() |> rb_map_pings()`. Given pings, each starts a map; given a map from either, it adds its
  layer over the earlier ones, from the map's pings and settings (`lon`, `lat`, `time`, `by`) unless `data` gives
  others. `rb_map_pings()` draws the points, coloured by one column or chosen among several ("Colour by");
  `rb_map_track()` draws each vessel's line, one colour or coloured by runs of one column; a second call adds a second
  track, by another column, each turned on and off in the page. `rb_map_track()`'s points and
  its `path`, `serve` arguments of 2026.10.25.2-.3 are gone: the points are `rb_map_pings()`'s, the line's column its
  `col`.
- `print()` of an `rb_map` serves it at an interactive console (on the port its first layer function set, else 8790)
  and prints its title and layers elsewhere; a chain is shown once, at its end.
- `rb_map_serve()` and `rb_map_stop()` drop the temporary tables of the map they replace or stop, unless a map on
  another port uses them; `rb_map_tiles_table()`'s tile function makes its table again if it was dropped, so a map
  shown again still draws.

# ramb 2026.10.25.3

- `rb_map_track()`: `path` may name a column, such as `fishing_flow`: the line is cut into runs of equal value as well
  as at silences, each run coloured by its value (the points' colours when they are coloured by the same column) and
  ending at the next run's first fix, so the line stays joined. On a trawler's year in the Icelandic trail, 3,507 runs,
  every end joined but at the five silences of over an hour and the year's end.

# ramb 2026.10.25.2

- `rb_map_track()` (new): the quick look at pings, on the map viewer's engine. The pings are piped in (a data frame, a
  lazy DuckDB table or sf points) and drawn as points coloured by a column: a numeric one on a continuous palette
  (`grDevices::hcl.colors()`, inferno by default) over its 1st to 99th percentile, any other a colour per value. A
  point's radius is in metres, so it grows with the zoom. The track can go under the points as a line per vessel,
  broken at silences of over `gap_h` hours. The map opens at once, on a dark base map, in RStudio's Viewer when the
  session is interactive, and a call on the same port replaces the last map and drops its tables. It keeps what the
  maintainer liked in the retired `rb_mapdeck()` (now `rb_plot_trail()`, on mapdeck): the pipe, the coloured points
  with an optional line, and a map at once.
- `rb_map_tiles_table()`: its temporary table's name is its tile function's `table` attribute.
- `rb_map_layer()`: `variants`, for a vector or geojson layer, are styles to choose between in the page (a "Colour
  by" control under the layer): each option sets paint properties on the layer's styles and may bring its own legend.
  The choice goes into the address as `<id>_by`, and nothing is fetched again. `rb_map_track()` uses it when `col`
  names several columns, `c(speed, course, behaviour)`.

# ramb 2026.10.25.1

- `rb_map_tiles_table()`: its temporary DuckDB tables are numbered. They were named from R's random numbers, which
  repeat after `set.seed()`, so a second table could take the first one's name and fail (found by R CMD check, which
  ran two examples from the same seed); drawing them also moved the user's random numbers.
- `rb_map_layer()`, `rb_map_group()`, `rb_map_serve()`: examples. `rb_map_serve()`'s makes a whole map and runs by
  hand only, so `example(rb_map_serve)` opens it in RStudio's Viewer.

# ramb 2026.10.24.3

- `rb_map_tiles_table()`: a property of a type a vector tile cannot hold is cast to one it can (a factor, date or time
  to text, a small integer to an integer, a decimal to a double). DuckDB's `ST_AsMVT()` refused it, and the layer was
  blank (a factor, found on iapesca's example data).
- `rb_map_serve()`: a tile function that fails replies 500 with the first line of its error, which the page shows.
- Four example maps as scripts in `inst/examples/map/` (named in `?rb_map`): the Danish pings by cell, the same with
  switches (cell size, a sample, the interpolation), the Italian tracks' voyages by the `cnr` and `iapesca` rules, and
  iapesca's example vessel.

# ramb 2026.10.24.2

- `rb_map_serve()`: a vector layer's tiles carry the server's start in their address, and a geojson layer's data are
  not cached, so a map served again on the same port with other data (another voyage) is not drawn from the browser's
  cache of the last one (found by fishycode plan 046 step 7).

# ramb 2026.10.24.1

## A map viewer (fishycode plan 046)

- `rb_map_layer()`, `rb_map_group()`, `rb_map()` (new): a map declared in R, its layers in groups. Three kinds of layer: `"grid"` (vector tiles of cells coloured in the page by a numeric field on a log scale, with palette, stretch, floor, cap and histogram, and filters whose values go into the tile address), `"vector"` (vector tiles drawn by MapLibre styles) and `"geojson"`. Texts are keys of the map's `texts`, in any number of languages; the page's own texts are English.
- `rb_map_serve()`, `rb_map_stop()` (new): the map served from this R session on `127.0.0.1`, by a small httpuv server with fixed routes (`/`, `/config.json`, `/meta`, `/tiles/<id>/{z}/{x}/{y}.pbf`, `/data/<id>.geojson`). A grid's query parameters are checked against its filters before its tile function sees them. The page (`inst/map/index.html`) holds the whole view in its address, so a link gives the same view, and works on a phone.
- `rb_map_tiles_table()` (new): a table of points, lines or areas (a lazy DuckDB table or a data frame) as vector tiles, cut by DuckDB's `ST_AsMVT()`, with a point on every feature as a second tile layer if asked.
- `rb_map_palettes()`, `rb_map_basemaps()` (new): the default palettes (viridisLite and tidyterra, nine colours each) and base maps (Carto's vector styles, Esri's raster tiles, none).
- httpuv and jsonlite are new in Suggests, needed to serve a map only.

# ramb 2026.10.22.2

- `rb_add_trip_time()`: a witness may carry `day` (`TRUE` where its time is a date only, a landing day or a logbook that records the day). Such a witness, when it ranks first, gives way to the best lower-ranked witness with a time on that day, and it agrees with any time on its day. A new column `_day` marks values known only to the day. Without `day` the results are as before (fishycode decision 174: the return, `T2`, from the same ranking as the departure).

# ramb 2026.10.22.1

## A trip's departure (or return) from ranked witnesses (fishycode TODO 121)

- `rb_add_trip_time()` (new, step 1): several sources witness a trip's departure, the track (`rb_link_trip_voyages()`), the logbook, a notification system. Each trip takes its time from the best-ranked witness that holds one and its place (a port) from the best-ranked witness that holds a place, each on its own, with the witness it came from and its basis. Every trip falls in one outcome per field (`"none"`, `"one"`, `"agree"`, `"differ"`; times agree within `tolerance`, 1 hour by default), and `_spread_s` gives the largest distance to another witness's time. `name = "T1"` and `place_name = "pid1"` by default, the departure; `"T2"` and `"pid2"` for the return. It is the step `docs/one_flow.qmd` in fishycode planned as `rb_reconcile_time`, named by its output shape.

# ramb 2026.10.19.3

## Port calls at any ping rate (fishycode plan 031, step 3)

- `rb_create_port_calls(method = "dwell")` gains a time budget, `speed_kn`. Two rules scale the minimum dwell with the ping interval:
  - `gap_budget`: a silence that begins within `radius_in` of a port, where the next ping may lie anywhere;
  - `span_budget`: a run of pings inside a port, too short for `tag_span`, measured from the ping before it to the ping after.

  Each is a port call when the time between the pings, less the sailing time to and from the port, is at least the minimum dwell. The speed is `speed_kn` knots, or with `"vessel"` each vessel's own: the upper quartile of its leg speeds between 2 and 30 knots, else `speed_fallback_kn` (6). Distances are in metres in `crs_m`. With sparse pings a fixed departure radius rejects a silence in port whose next ping is already further out; the budget accepts it. Default `NULL`: the rules and results are unchanged.
- `min_dwell_h = "valley"` sets the minimum dwell from the data (the maintainer, plan 031 ask a). It is the low point of the density of the candidate calls' log lengths, between the pass-throughs (mode under an hour) and the calls (mode over two hours). The value is returned as the attribute `min_dwell_h`. On all fourteen `dansk` vessels it is 0.99 h.
- `rb_create_voyages()`, methods `"dwell"` and `"runs"`: new columns `open_start`, `open_end` (no port call before or after the voyage) and `short` (fewer than `short_pings`, 10, pings). With `min_pings = 1`, short voyages are kept and labelled, not dropped (plan 031 §4.7). Defaults unchanged.

# ramb 2026.10.19.2

## Names by output shape, and port calls (fishycode plan 032)

A function's verb now says what it returns, after tidyverse's `count()`/`add_count()` (the maintainer, 2026-10-19): `rb_create_*()` a table of its own, `rb_add_*()` the input with columns added; `rb_flag_*()` and `rb_link_*()` (a table of pairs) keep their verbs. A vessel's time in port is a **port call**, no longer a "stay".

| old | new |
|---|---|
| `rb_find_trip_stays()` | `rb_create_port_calls()` |
| `rb_cut_trip_voyages()` | `rb_create_voyages()` |
| `rb_cut_record_stretches()` | `rb_create_record_stretches()` |
| `rb_build_record_windows()` | `rb_create_record_windows()` |
| `rb_build_record_pings()` | `rb_create_record_pings()` |
| `rb_assign_trip()` | `rb_add_voyages()` |
| `rb_assign_record()` | `rb_add_record()` |
| `rb_assign_fishing()` | `rb_add_fishing()` |
| `rb_assign_effort()` | `rb_add_effort()` |
| `rb_link_trip_logbook()` | `rb_add_logbook_trip()` |
| `rb_build_catch_units()` | `rb_add_catch_units()` |
| `rb_create_timestamp()` | `rb_add_timestamp()` |

- Each old name is kept for this release only, as an alias (`?ramb-renamed`) that warns once per session, takes the old argument names and gives the old result: `rb_find_trip_stays()` without the new `year` and `port_call_id`, `rb_assign_trip()` with `stay_port_id` and `stay_id`, `rb_create_timestamp()` a vector. Tested in `test-renamed.R`.
- `rb_create_port_calls()` takes the methods of `rb_create_voyages()`: port calls and voyages are the two halves of a track. `"dwell"` (the default) is the old stay rule, in port for at least `min_dwell_h` (was `min_stay_h`; the per-port table `min_dwell`, was `min_stay`, has a column `min_dwell_h`). For `"runs"`, `"jepol"`, `"cnr"` and `"iapesca"` a port call is the time in port between two of the method's voyages, from the end of one to the start of the next, at the port the first ended in, else the port the second began from; where neither voyage reaches a port (a jepol split, a silence at sea) there is none. Their arguments go in `...`; `ports` is needed only by `"dwell"` and `"iapesca"`.
- Port calls carry `year` (of `T_in`) and the key `port_call_id`, numbered per vessel by `T_in`, not per year: a port call is not cut at New Year, so a ping in one over New Year finds it by `vid` and `port_call_id` alone (the maintainer, 2026-10-19). Voyages keep `voyage_id` per vessel-year, since they are cut at New Year.
- `rb_create_voyages()`: `method = "stays"` is `"dwell"`, and the argument `stays` is `port_calls`.
- `rb_add_voyages()`: the argument `stays` is `port_calls`; the pings get `port_call_port_id` (was `stay_port_id`) and `port_call_id` (was `stay_id`) where the port calls carry the key, which those of `rb_create_port_calls()` now always do.
- `rb_create_record_stretches()`: the argument `stay` is `port_call`.
- `rb_add_timestamp(data, date, time, name = "time")` returns `data` with the timestamp column added, never overwriting one; `rb_create_timestamp()` returned a vector.
- Not renamed: `rb_limit_record_duration()`, `rb_share_catch()`, `rb_interpolate_track()` (their verbs say what they do), `rb_pad_harbour()` and `rb_read_harbours_mfri()`, and the methods `"runs"`, `"jepol"`, `"cnr"`, `"iapesca"` with their settings lists `cnr` and `iapesca`.
- The step registry has the new names; its test checks that a step's verb is `add` or `flag`, and that step 1 names its objects (`voyages`, `port_calls`) where the other steps name their stage.

Fix list for fishycode (all calls found there on 2026-10-19):

- `curate/port_stay.R:103`: `rb_find_trip_stays(…, min_stay_h = G_H, …)` → `rb_create_port_calls(…, min_dwell_h = G_H, …)` (it returns `year` and `port_call_id` too).
- `curate/ais_voyage.R:109`: `rb_cut_trip_voyages(clean, st_g, …)` → `rb_create_voyages(clean, st_g, …)` (positional `port_calls`).
- `curate/ais_voyage.R:119`: `rb_assign_trip(clean, v_b, stays = st_g)` → `rb_add_voyages(clean, v_b, port_calls = st_g)`, with the key column `port_call_id` (was `stay_id`) in `st_g` and on the pings.
- `curate/station_window.R:102`: `rb_build_record_windows()` → `rb_create_record_windows()`.
- `curate/ais_catch.R:133`: `rb_build_catch_units()` → `rb_add_catch_units()`.
- `R/trail_sql.R:93-97`: `rb_assign_fishing()`, `rb_assign_effort()`, `rb_assign_record()` → `rb_add_fishing()`, `rb_add_effort()`, `rb_add_record()`.
- `curate/checks/survey_tow_times.R:32`: `rb_cut_record_stretches(…, stay = "stay", …)` → `rb_create_record_stretches(…, port_call = "stay", …)`.

# ramb 2026.10.19.1

- `rb_cut_trip_voyages(method = "iapesca")`: new, Ifremer's trip finder (`Detect_FishingTrips()` of iapesca 0.1, J. Rodriguez, GPL; its harbour mode), recoded as one DuckDB query for fishycode plan 031's voyage bench (the maintainer, 2026-10-19). Its rule is vectorised, so unlike `"cnr"` it needs no C++: data frames and lazy tables (parquet) run the same query, with no batching. Pings duplicated in time and place are dropped, then up to five times the first of each run of pings reached at 25 knots or more; a ping is in port within a buffer of the `ports` polygons (2 km, or 1 % of the track's diagonal up to 2 nautical miles, grown when no ping falls inside); a voyage is a run of pings outside it, cut at a run in port only if an hour has passed since the previous voyage end. New argument `iapesca` (a list overriding `max_speed_kn`, `max_pass`, `buf_start_m`, `buf_max_m`, `min_stop_s`, `crs_m`). The geometry is planar in a metric CRS, by default the UTM zone of the pings' mean position, as the R function is with projected input; it never reads `sf::sf_use_s2()`. Added: `port_from` and `port_to`, the nearest port of the in-port ping before and after the voyage.
  - Checked against the installed iapesca (1b7248c), voyage by voyage (first and last ping at sea, number of pings at sea), each in the same CRS: its own example 9 of 9, `wkssfgeo_cnr_ais` 79 of 79, `dansk` 109 of 109, 12 Icelandic 2018 vessels 566 of 566, 30 perturbed copies of the Italian tracks 2,271 of 2,271. The Icelandic sample (481,143 pings) takes 0.98 s against 13.8 s for the R function; `dansk` 3.7 s against 14.8 s. Prototypes in C++ gave the same trips, but their spatial step (sf) cost more than the whole query.
  - Kept from the R function so the two agree: a ping is a duplicate when its time and its position each occurred before, matched separately; the hour (`min_stop_s`) is the time since the previous voyage end, not the length of the stop, so a short voyage merges with the next across a long stay, and a long voyage is cut by a short one; the buffer is 1 % of the diagonal only between 2 km and the cap, and 2 km above it. Not kept: the R function's rows of NA when a duplicate and a spurious speed meet, and its parsing of times at midnight. The quirks, with examples and remedies, are drafted as issues for iapesca.
  - Test fixture `inst/testdata/iapesca_italy_trips.csv`, made by `data-raw/iapesca_trips.R` from the installed package; iapesca's code and data are not copied into ramb.

# ramb 2026.10.19

- `rb_cut_trip_voyages(method = "cnr")`: new, CNR-IRBIM's trip finder (`create_fishing_trip()` of R4AIS, Galdelli et al. 2019; WKSSFGEO `dev_branch`, GNU GPL 3), recoded in C++ (`src/trip_cnr.cpp`) for fishycode plan 031's voyage bench (the maintainer, 2026-10-19). A vessel is in port after 4 pings in a port at a mean recorded speed under 6 knots; a silence over 4 h at sea also ends a voyage; voyages split by a silence with no port either side are joined, unless both ends lie in a coastal zone and together they last 24 h or more; an end with no port gets the closest port, or the other end's port when it is among the 5 closest within 50 km. New arguments: `ports` (sf polygons, for the ports of the ends), `coast` (sf, the coastal zone), `cnr` (a list overriding CNR's values, including `distance`), and the columns `speed`, `lon`, `lat`. Data frames and lazy tables (parquet) give the same voyages: a lazy table is collected in batches of whole vessels (`batch_pings`), the port of each ping tested in DuckDB beforehand by `rb_flag_ping_port()`, and only the voyage ends come into sf.
  - Checked against the R function as published (WKSSFGEO `dev_branch` efd28a8), run from the repository, trip by trip (start, end, departure port, arrival port): CNR's own Italian tracks (`wkssfgeo_cnr_ais`) with its Mediterranean harbours and coastal zone, 72 of 72 identical, and 70 of 70 with no zone; the Adriatic passive-gear vessel (`wkssfgeo_adriatic_passive`) with the same ports and zone, 8 of 8 (its provider drew 10); the Danish tracks (`dansk`) with CNR's `ices_med_harbours` and northern coastal zone, 90 of 90. The 94 trips CNR saved (`wkssfgeo_cnr_dansk_trips`, method "galdelli") are the R function run once on all 14 vessels together, which it sorts by time alone, so a trip can span two vessels: run that way it gives the same 94 trips, 92 with the same ports (the saved table leaves two arrivals NA that the published code fills); 50 perturbed copies of the Italian tracks (thinned, silences cut in, speeds scaled, pings moved), 3,134 of 3,134, meeting every join and recovery branch; 12 Icelandic vessels of 2018 (fishycode's AIS, 519 to 182,604 pings each), 598 of 598. On the sphere instead of the ellipsoid, the Italian ports are the same. 3.9 s in R becomes 0.5 s on the Italian tracks, 48 s becomes 0.95 s on the Danish ones. The R function runs at about 4,700 pings a second on one core (182,604 pings in 61 s), about 1.8 h for Iceland's 39.1 M pings of 2018; ramb does that year from parquet in 57 s (20 s for the ports in DuckDB, 36 s for the voyages).
  - Kept from the R function so the two agree: the first four pings tested together; the departure port read three pings before the voyage's first; the port buffer of 0.001 degrees on the plane (about 110 m north-south; the R code's comments say 1 km), used only for the port of a voyage end, while "in port" is the polygon itself; a vessel of under 10 pings has no voyages. And a weakness: a silence that ends inside a port starts a voyage there that runs on through the stay, since the vessel arrives only after four pings all at sea, so a port call reached through a silence is not found (a made-up track A, sea, silence, 10 pings in port B, sea, A gives one voyage A to A, in the R function too).
  - Different on purpose: a missing speed never counts as slow (the R function stops); ports are matched by id, not by name (5 names occur twice in the Mediterranean list); the coastal test is "both ends in the zone" (the R function sums the zone's polygons that hold each end, so on a zone of overlapping polygons one end in two of them counts as both; feeding it those counts gives the R function's voyages, also on the Danish tracks); distances to ports are on the ellipsoid by default (`distance = "geodesic"`, as the R code ran sf with s2 off; needs lwgeom) or on ramb's sphere (`"sphere"`, s2), whatever `sf::sf_use_s2()` says. Suggests: `lwgeom`, `s2`.
  - Test fixture `inst/testdata/cnr_italy_trips.csv`, made by `data-raw/cnr_trips.R`, which runs the R function from the repository; its code is not copied into ramb.

# ramb 2026.10.16.1

**Breaking.** All of ramb's data are parquet files in `inst/extdata`, tracked with Git LFS and read with `rb_read_example()` (the maintainer, 2026-10-16); `data/` is gone.

- Nine more datasets: the rest of the ICES WKSSFGEO and WKSSFGEO2 example data (the maintainer, 2026-10-19), made by `data-raw/wkssfgeo_examples.R` from each repository at a fixed commit, each with its entry in `data-dict.yaml`, its help page and its sources. Most carry the provider's own trips, so a trip cutter can be scored against trips someone else drew.
  - `wkssfgeo_adriatic_passive`: one Adriatic vessel with passive gear, AIS, 10 trips.
  - `wkssfgeo_denmark`: Denmark's land, one polygon on WGS 84 (from UTM 32N), a coastline for `dansk`.
  - From CNR-IRBIM's folder on WKSSFGEO's `dev_branch` (GNU GPL 3; Galdelli et al. 2019): `wkssfgeo_cnr_ais`, its six-vessel Adriatic test AIS; `wkssfgeo_cnr_dansk_trips` and `wkssfgeo_cnr_dansk_ping_trips`, the Danish tracks (`dansk`) cut into trips by CNR-IRBIM's method (94) and by jepol (90), per trip and per ping; `wkssfgeo_cnr_med_harbours`, 1,415 Mediterranean harbour polygons. The folder's other list, `ices_med_harbours`, is these and `dansk_harbours`, so it is not kept twice.
  - From WKSSFGEO2 (MIT), in the columns of the labelled tracks: `wkssfgeo2_irl_pots` (one Irish pot vessel, labelled, 6 trips), `wkssfgeo2_irl_gillnets` (four Irish gillnet vessels, inshore VMS, 136 trips, no label) and `wkssfgeo2_pt_gps` (eight Portuguese pot and dredge vessels, 30 s, labelled).
  - vmstools' and iapesca's data are not copied (the maintainer).

- The eight datasets that were lazy-loaded are now parquet, unchanged (same rows, order and columns; `data-raw/extdata.R` converts each `.rda` as it stood at d693c1b): `dansk`, `dansk_harbours`, `creel`, `trawlsurvey`, `trawlsurveytows`, `whacks1`, `benthis_parameters`, `metier5_benthis_lookup`. Fix: `ramb::dansk` becomes `rb_read_example("dansk")`, a lazy table; add `dplyr::collect()` for a data frame. Their help moved to `?rb_read_example`.
- `dansk_harbours` and `trawlsurveytows` keep their shapes as DuckDB's own geometry, which DuckDB 1.5 writes as GeoParquet and reads back as geometry with no extension. They no longer come as sf: a collected `geometry` is WKB, and sf where needed is `sf::st_sf(x[names(x) != "geometry"], geometry = sf::st_as_sfc(structure(x$geometry, class = "WKB"), crs = 4326))`. Fix for `rb_detect_in_polygons(geometry, dansk_harbours)` and `rb_flag_ping_port(pings, dansk_harbours)`: make the sf as above first (the articles show it).
- `rb_predict_gear_width()` reads its BENTHIS lookups from the parquet files; results unchanged.
- Every dataset is described in one data dictionary, `inst/extdata/data-dict.yaml` (the data-dict 0.1.0 spec, <https://data-dict.tidyverse.org/>; it passes the `data-dict` validator against the spec, the column types and the data), with its sources as BibTeX in `inst/REFERENCES.bib`, verified against the publishers on 2026-10-16. From it: each dataset has its own help page again (`?dansk`, `?ird_mg_gillnet`, ...: what a row is, its columns, its sources and its licence); each parquet file carries its own entry, its sources (text and BibTeX) and its licence in its metadata, so a copied file still says what it is; and `rb_read_example()` with no name returns that catalogue as a tibble (`name`, `label`, `description`, `rows`, `licence`, `cite`, `bibtex`) instead of the names. Suggests: `yaml`, `rbibutils`, only to write the help pages.
- `benthis_parameters` gains the uncertainty of its gear-width models: `se_a`, `se_b` (standard errors of `a` and `b`) and `n_obs`, from Table 4 of Eigaard et al. (2016). Its `a`, `b` and model match that table for all 14 groups. `gear_width` is in km, the model at the group's average vessel; for OT_DMF, OT_MIX_CRU, OT_CRU and TBB_DMF it differs from the model at the stored averages by 2 to 28 %. The standard deviations of the average vessel size and speed (Table 5) are not added.
- The IRD gillnet data are cited as Etienne (2020), DataSuds, with Behivoke et al. (2021) as their article; 2026.10.16 had them as Behivoke et al.
- The test fixture of the jepol trip cutter is `inst/testdata/pre_jepol.parquet` (was `tests/testthat/fixtures/pre_jepol.rds`), so the tests of an installed ramb find it.
- Parquet is written at zstd level 19: `dansk` is 6.6 MB (5.9 MB as xz `.rda`).
- fishycode: `curate/checks/whack_demo.R` reads `whacks1`; it needs `whacks1 <- dplyr::collect(rb_read_example("whacks1"))`.

# ramb 2026.10.16

Co-location and the static-gear set finder share one proximity engine; example data ship as parquet under Git LFS.

- `rb_link_vessel_tracks()`: new, the vessel stage. Links the ids of two position feeds (a tracking unit and an MMSI, an MMSI and a registry number) where their moving pings meet: per id of `x`, id of `y` and `period`, the `hits` (x pings with a y ping within `dt_s` and `dist_m`), the `opportunity` (x pings with any ping of that id within `dt_s`) and `score = hits / opportunity`; the ids are ranked, and every row keeps the best other id and its score, so a partner (pair trawler, tender, net buoy) shows instead of hiding. `pairs` scores a key's own pairs whatever their hits. Generalised from fishycode's W1 witness (`audit_w1_month()`, decision 047) and the plan 022 run by MMSI, whose defaults it keeps (5 min, 2 km, 1 knot; `strong` at 20 opportunities and 0.8). Unlike both, it has no lat/lon box (filter first), measures on ramb's sphere instead of a flat 111.2 km per degree, and takes the block widths from each latitude instead of from 65° N, where a hit across a block edge further north could be missed. Tested against a plain computation over every pair of pings.
- `rb_link_record_sets()`: three arguments, defaults unchanged.
  - `method`: where the haul's line comes from: `"pings"` (as before), `"line"` (the haul's pings joined), `"record"` (the haul's declared start and end, `lon1`...`lat2`), or several, tried in order per haul. New column `set_method`.
  - `skip_approach`: leave out the pass that leads straight into the haul.
  - `max_pass_pings`: the cap on a pass's length in its score (iapesca's 4; `Inf` for none).
  - With the defaults the output is the same as 2026.10.15.1's for all 129 labelled hauls at all five ping rates below, plus the new column.
- Measured on the labelled tracks now shipped (article "Finding the set of a static-gear haul"): sets found right, of 56 pot hauls (one Italian, two Portuguese vessels; soak median 44 h) and 72 drive-in gillnet hauls (four Malagasy fishers; soak median 1.8 h), at the tracks' own rate (60 s or less), 10 and 30 min:

  | variant | pots | gillnets |
  |---|---|---|
  | defaults (iapesca) | 13, 31, 9 | 70, 66, 26 |
  | `max_pass_pings = Inf` | 33, 31, 9 | 67, 64, 26 |
  | `skip_approach = TRUE, max_pass_pings = Inf` | 40, 32, 9 | 2, 3, 0 |
  | `method = "line"` | 13, 19, 6 | 70, 45, 4 |
  | `method = "record"` | 12, 32, 12 | 69, 67, 41 |
  | `method = c("pings", "record")` | 13, 31, 11 | 70, 68, 42 |

  On dense pot tracks the defaults take the run in to the buoy for the set (43 of 56 wrong); the approach guard fixes that and breaks the gillnets, whose set, fish drive and haul are one pass. At 30 min many hauls hold no ping, and the declared positions give the line back.
- Measured on the Icelandic logbook (fishycode, read-only; stations and trail as built 2026-10-14): the 2023 static-gear hauls whose logbook declares the set time, on tracks a median 6 min apart; right when the declared set time is within an hour of the found pass. Longlines (7,721; soak median 4.5 h): 2,030 right with the defaults, 3,674 with `max_pass_pings = Inf`, 3,752 with `skip_approach = TRUE` as well. Gillnets (6,352; soak median 22 h): 1,069, 1,040, 1,413. Pots (496): 5 in every variant. `method = "record"` does worse here (899 and 682 right) and only helps as a fallback. As on the labelled pots, most wrong picks with the defaults end just before the haul (longlines: 4,456 of 5,299). A limit of the rule itself: 22 % of the gillnets' and 18 % of the pots' declared sets fall inside another haul's window of the same vessel (hauled and set again in one pass), and haul windows are never candidates. The defaults stay as they were: the maintainer decides.
- `rb_read_example()`: new. ramb's example data are parquet files in `inst/extdata`, tracked with Git LFS and read lazily. First three labelled tracks: `wkssfgeo2_it_pots`, `wkssfgeo_pt_gps` (ICES WKSSFGEO2 and WKSSFGEO, MIT) and `ird_mg_gillnet` (Behivoke et al., doi:10.23708/IBZJGD, CC BY 4.0), made by `data-raw/labelled_tracks.R`; licences in `COPYRIGHTS`. The WKSSFGEO2 file holds trip 40 twice (trip 41 is the same trip an hour later), so trip 41 is left out. A package installed from an archive without its LFS objects holds pointers instead of data, and `rb_read_example()` says so.
- A function given lazy tables on two different DuckDB connections stops with a message (`rb_link_record_sets()`, `rb_link_vessel_tracks()`); a lazy `pings` with data-frame `hauls` now works.

# ramb 2026.10.15.1

- `rb_pad_harbour()`: the arguments are `voyage_id` and `port_id`, where they were `tid` and `hid` (fishycode TODO 53). The id is a voyage's, as `rb_assign_trip()` puts it on the pings, and the port id is the ports' own key (`port_id` from `rb_flag_ping_port()`), not a source's port code: the same renames as `rb_summarise_track()` in 2026.10.13.3. A call by position is unchanged; a call by name needs the new names. The name of the function is kept.
- `rb_pad_harbour()`: a ping in port with no voyage (`NA`, as `rb_assign_trip()` leaves it) now counts as outside the voyage. Before, the comparison with `NA` was itself `NA`, so on `rb_assign_trip()`'s output the function padded nothing; only the older convention (an id of 0 or less in port) worked. Where the neighbour has no port id, the end ping now keeps its own instead of taking `NA`. Nothing in fishycode calls it.

# ramb 2026.10.14.14

- `rb_build_catch_units()`: an event with no `start` now starts where the vessel's latest event that ends strictly earlier ends. Two events ending together (two landings in a day, one per buyer) get the same start and share a unit. Before, they were ordered by an arbitrary tie, which could differ between two evaluations of the same query; and the function evaluated its islands twice (for the key links, then for the result), so an event could fall in two units and its catch be counted twice (fishycode decision 102: 1,751 t too many in 2007, stopped by fishycode's kilograms check). The islands are now computed once, as a temporary table.

# ramb 2026.10.14.13

The step registry, item 0 of fishycode plan 013, now covers the flow's step functions.

- The ping flaggers, `rb_find_trip_stays()`, `rb_cut_trip_voyages()`, `rb_assign_trip()`, `rb_link_record_pings()`, `rb_assign_record()`, `rb_link_record_events()` and `rb_cut_record_stretches()` check their input against the registry before any work, as the effort, fishing, catch and record builders already do. A missing column or a time that is not a date-time stops with a message that names the column under the user's name and, where a step adds it, that step. A lazy table is checked by its schema only. Messages changed: e.g. `rb_cut_record_stretches()` without speed said "pings lacks: speed".
- A shared misuse test runs every checked step function on a table without its columns.

# ramb 2026.10.14.12

- `rb_link_record_sets()`: new, a step-2 builder for static gear (fishycode plan 013, item 8). Each haul's set is the vessel's latest earlier pass along the haul's line: pings within `max_back_h` (240 h) before the haul, outside every haul window, within the smallest buffer from 330 m up to 550 m that finds any; passes are runs of such pings, scored by lateness and length. Gives the set's start and end, the soak and `set_basis`. Recoded from the method of Ifremer's iapesca (`Retrieve_SettingOperations`), not copied; the distance is to the nearest haul ping, not to a line geometry.

# ramb 2026.10.14.11

- `rb_link_trip_landings()`: new, a step-1 builder (fishycode plan 013, item 5). Trips (declared, or voyages cut from the track) are linked to landings by a cascade of steps, strictest first (same day and port, then a day apart, then any port, then two days); a step accepts a pair only when it is the sole candidate both ways, so an ambiguous match is left for a looser step or unmatched. With `since_previous_days`, a last step links each trip still unmatched to the vessel's next landing, so one landing can cover several trips (a weekly landing); `n_trips` and `n_landings` count the links.

# ramb 2026.10.14.10

Records that no track covers (fishycode plan 013, item 7).

- `rb_build_record_pings()`: new, a step-2 builder. A record with no pings (the years before vessel tracking, a vessel the tracking does not reach) gets positions every `interval_s` over its window, in a straight line from its start to its end position, or at its start where it has no end; `pos_basis = "synthesised"`, and `speed` is the straight line over the window. They run through `rb_assign_effort()` and `rb_share_catch()` like any pings; whether they are fishing is the record's to say. The distance is a new SQL copy of the haversine on `rb_calc_distance()`'s sphere, pinned to it by a test.
- The step registry's message no longer says a basic input (`vid`, `time`, a position, `speed`) "is added by" a builder that happens to make it.

# ramb 2026.10.14.9

- Lazy results compose: a step function's lazy result is now a query wrapped in `SELECT * FROM (...)`, so two of them can be combined with `dplyr::union_all()`. Before, dbplyr put the second query after `UNION ALL` as it was, and DuckDB refused its leading `WITH` (found sharing catch under two fishing approaches in fishycode).

# ramb 2026.10.14.8

Step 4 at any data resolution (fishycode plan 013, item 3): the track decides where the record is missing.

- `rb_assign_fishing()`: new `method = "speed"`, the speed range on the ping itself (`ping_speed_min`, `ping_speed_max`: the gear's range, e.g. from the voyage's trip or landing), with no record needed. `method` may now name both, as a cascade tried in order for each ping: `c("window", "speed")` takes the record's window where one holds the ping and the speed range elsewhere. With `"speed"` in `method`, `<name>_basis` says which decided (`"window"`, `"speed"` or `"none"`). New `name`, so each approach can stand as a column of its own. `method = "window"` alone is unchanged.
- `rb_assign_fishing()` is the second function to check its input against the step registry: a missing speed column, a time that is not a date-time, a column it would overwrite, or `"window"` without matches stop with a message that says what to run.

# ramb 2026.10.14.7

Step 6 rewritten to the grammar (fishycode plan 013, item 2): catch is shared at the finest level the data can separate, at any resolution, and no kilogram is lost.

- `rb_build_catch_units()`: new, a step-6 builder. The events that carry catch (record windows, trips, landings over a trip, a week or a month) are grouped into units: events of one vessel and one `level` whose intervals overlap, or that share a key (a record's set and haul), are one unit, because the track cannot tell them apart. A timed haul is a unit of its own; a day's records without times are one unit. A landing with only a date reaches back to the vessel's previous landing (`start_filled`). `match_by` keeps units apart by, e.g., ICES rectangle.
- `rb_share_catch()`: **breaking.** The default is now `method = "units"`: `rb_share_catch(pings, units, catch, key = )` shares each unit's catch over its fishing pings by `hours` ([rb_assign_effort()]), species by species. A coarser unit's catch goes to its pings no finer unit took, or to all of them if none is left. Catch whose unit has no fishing ping moves up to the next level that overlaps it in time, then to the vessel's fishing pings that month; what still finds none is kept with no time and `catch_basis = "none"`, so the kilograms out equal the kilograms in. The fishing column is an argument and is named in `approach`, so the catch can be shared under each approach and compared. The result is one row per (fishing ping, unit, species). The ICES VMS data call's EFLALO sharing is `method = "eflalo"` (`rb_share_catch(pings, events, method = "eflalo")`); the old signature without it no longer works.
- On the Icelandic logbook of June 2023 (fishycode decision 100): 35,580 t in and out; 98 % shared in its own record's unit, 1.6 % moved to the vessel's month, 0.4 % with no fishing ping that month.
- Data frames whose date-times or dates are stored as integers (as `seq()` makes them) were registered in DuckDB as INTEGER, and any function given one failed; they are now registered as times.

# ramb 2026.10.14.6

Step 5 of the flow, and the first piece of the step registry (fishycode plan 013, items 0 and 1).

- `rb_assign_effort()`: new, step 5. Every ping gets the time it stands for (`hours`: the interval to the vessel's next ping, or since its previous one), `kw_hours` with the engine power from a vessel table or from the pings, and, where a towed gear's width is given, the area it sweeps (`pressure`, km²). It is done for every ping, fishing or not, so each approach to "is it fishing?" can stand as a column of its own and a summary picks one by filtering on it. Intervals longer than `max_interval_s` (2 h) are capped or dropped (`long`), shorter than `min_interval_s` dropped; `by` keeps an interval inside a voyage. `interval_flag` says which pings were capped, dropped or at a group's edge, and `effort_basis` is `"derived"`, or `"none"` where there are no hours. Without engine power or width, `kw_hours` and `pressure` are `NA`: the same columns at every data resolution.
- A step registry (internal): one row per step function of the flow, saying what it needs from its input, what it adds and which step normally comes before it. `rb_assign_effort()` is the first function to check its input against it: a missing column, a time that is not a date-time, or a second run on the same table stops with a message that names the column and, where another step adds it, that step. A lazy table is checked by its schema only. The other step functions keep their own checks until they are moved onto it.

# ramb 2026.10.14.5

- `rb_link_record_pings()`: new argument `window_s`, as in `rb_assign_fishing()`. The `fishing` of each match then tests the median speed over the pings given within `window_s` either side, so the matches and `rb_assign_fishing()` agree when both get the same pings and window. The default 0 keeps each ping's own speed; results are unchanged unless it is set.

# ramb 2026.10.14.4

The track's own account of the tows, paired with the declared ones (fishycode TODO 59; the prototype and its numbers are fishycode decision 090). Ported from that prototype and checked against it on the Icelandic data: June 2023's 4,237 tow operations pair tow for tow as there, 99.0 % of them, with the start within 15 / 30 minutes for 54.4 / 76.8 % and the end for 51.1 / 66.1 %.

- `rb_calc_speed_smooth()`: new. The median speed within `window_s` seconds either side of each position (default ±10 minutes). On the Icelandic AIS of 2007-2026 it called 92.7 % of the fixes inside recorded tows fishing, against 89.2 % on their own speed, and lost 0.4 points far from any tow.
- `rb_assign_fishing()`: new argument `window_s`, the same median in DuckDB. The default 0 keeps each ping's own speed, so results are unchanged unless it is set.
- `rb_cut_record_stretches()`: new, step 2. The stretches of a track at towing speed (the windowed speed inside the ping's speed range), broken at a gap of more than 30 minutes and dropped below 15. With the declared tows as a prior, a stretch that holds two of them is cut at the slowest ping between them; on June 2023 the prior raised the tows paired from 95.0 to 99.0 %. Twin records count once in the prior. The prototype collapsed them before it ran, and a twin otherwise cut its own tow in two.
- `rb_link_record_stretches()`: new, step 2. Each tow is paired with at most one stretch and each stretch with at most one tow, in order, maximising the overlap with the tows widened by 30 minutes, so no ping belongs to two tows. Where two pairings tie on that overlap, the one with more overlap with the tows themselves is taken. The prototype left such ties to the order in which it filled its table. On all of 2023 (61,464 tow operations) this changed 627 pairings and no vessel's total overlap or number of pairs; for those tows, the start within 15 minutes rose from 7.5 to 51.4 %. On a full tie, the lower key keeps the stretch.

# ramb 2026.10.14.3

- `rb_link_record_events()`: a tie between two phases of one record now always goes the same way. When both phases want a ping at the same distance and from the same event time, the earlier phase keeps it. A set and a haul of one record that start at the same time (`t1 = t3`) left the choice to chance: on fishycode's data 1,069 event rows of 532 records came out differently in two runs, 531 of the records with `t1 = t3`.

# ramb 2026.10.14.2

A ping belongs to at most one logbook record, and to one phase of it (fishycode's maintainer, 2026-10-14; fishycode decisions 090 and 091). Counted on the Icelandic logbooks and AIS (fishycode, all years 2007-2026; fixes for 2023).

- `rb_build_record_windows()`: the windows of a vessel other than `day` no longer overlap (47,515 of 5.06 M overlapped before, none now). They are put in one order (start, end, then the record, the lower key last), and a window ends where a later start that may cut it begins:
  - the recorded start of a tow, haul or jig cuts any window: jig windows are now cut too (2,107), where before they never were;
  - a set's start (`t1`) cuts only an earlier set. It never cuts a haul, because a gillnetter may set one net and go on hauling another. Cutting the hauls instead would have left 30.7k haul fixes of 2023 in no window, two thirds of them at hauling speed;
  - a window that starts inside one it may not cut starts at that window's end, and so does a start that is not a logbook time (the typical haul before `t4`, a jig `duration_m` before `t4`). The new column `win_opened_by` says `"previous_event"` (27,598 windows, 26,745 of them sets inside a haul, tow or jig);
  - of two windows with the same start, the one that ends first is cut to nothing; of identical windows (twin records), the one with the higher key. The row is kept with `w_end = w_start`. In all 34,776 windows are cut to nothing: 11,720 of twins, 16,248 other sets inside a haul, and 6,808 more, 6,722 of them same-start ties.
  `day` windows now end at midnight (`date + 1 day`), not one second before, because windows are half-open.
- `rb_link_record_pings()`: windows are half-open, `[w_start, w_end)`, so a ping at a window's end is no longer in it. With the windows above, 2023's fixes in two or more tier-1 windows went from 98,955 to none, 1,120 of 5.17 M fixes lost their window, and 521 of 4.14 M fishing fixes were no longer fishing.
- `rb_link_record_events()`: a ping now bounds the events of at most one record and one phase (`starts[i]` and `ends[i]` bound phase `i`, so a phase of one ping keeps both its events). When two would share a ping, the event nearer in time keeps it, then the earlier event time, then the lower key. The others get `time` and `dt_s` `NA` and the new column `ping_lost` `TRUE`; their `gap_s` is unchanged. Exclusivity holds within one call. On fishycode's 10.99 M event times (`max_dt_s` 1 h), 79,860 lost their ping (0.7 %).

# ramb 2026.10.14.1

- `rb_link_record_events()`: an event now takes the ping that bounds its phase, not the nearest one. `starts` (default `t1`, `t3`) take the next ping and `ends` (`t2`, `t4`) the previous, so the pings from a start's ping to its end's are the setting or the hauling, and the pings strictly between a `t2` and a `t3` are a tow; the `events` argument is gone (fishycode's maintainer, 2026-10-14). The nearest ping was the one before the event for half of all events, so on one year of Icelandic data (2023) it fell inside the record's own recorded tow or haul for 49-50 % of them; the bounding ping does for 97-99 %, the rest being phases shorter than the gap between pings. `dt_s` is now at least 0 for a start and at most 0 for an end.

# ramb 2026.10.14

- `rb_link_record_events()`: new, step 2. For every record and every event time it carries (`t1`-`t4` by default), the vessel's ping nearest in time, `dt_s` (that ping's time minus the event's, signed) and `gap_s` (the time between the pings on either side of the event, 0 when one falls on it, `NA` when the track does not reach both sides), with `max_dt_s` to leave out pings too far away. A missing event time gives no row; on a tie the ping before the event wins. Lazy DuckDB tables and data frames give the same result. It is the first building block of reconciling declared times with the track (fishycode plan 013, phase 4).

# ramb 2026.10.13.3

- `rb_summarise_track()`: counts voyages, not trips, by name. The column it counts is `voyage_id`, as `rb_assign_trip()` puts it on the pings, where it was `tid`; the output `n_trips` is now `n_voyages`. A voyage is any movement from one port stay to the next; a trip is a fishing trip (`trip_id`), and the function never counted those.

# ramb 2026.10.13.2

- `rb_summarise_track()`: `n_ports` now counts `port_id`, the port each ping lies in as `rb_flag_ping_port()` gives it, where it counted `hid`. Pass the ports' own key, not a source's port code: in fishycode, `pid`, not the logbook's or landings' `hid`, of which one port can have several.

# ramb 2026.10.13.1

- `rb_summarise_track()`: the output column `n_harbours` is now `n_ports`, following the "port, not harbour" rule of 2026-10-09; it still counts distinct `hid`. Values unchanged.

# ramb 2026.10.13

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
