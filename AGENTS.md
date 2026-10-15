# ramb — contributor notes (for humans and AI assistants)

ramb is an R package of tools for vessel-track (AIS/VMS) data and the ICES
fishing-activity flow. Functions are typically drafted in fishycode
(`~/stasi/fishycode`) and promoted here once they are general enough.

## Naming

- Every exported function is named verb first: `rb_<verb>_<stage>[_<detail>]`
  for a step of the fishing-activity flow, `rb_<verb>_<object>` for a tool
  that serves every step (`rb_calc_distance()`, `rb_encode_ices_rectangle()`).
- Stage words: vessel, ping, trip, record, gear, fishing, effort, catch.
  Example: `rb_assign_trip()` is step 1, `rb_find_trip_stays()` a step-1
  builder.
- One function per task: variants are a `method`, `part`, `list` or
  `from`/`to` argument, never a new name
  (`rb_flag_ping_impossible(method = "sda")`,
  `rb_encode_ices_rectangle(method = "vmstools")`). A grouping is a `by`
  argument.
- Internal functions are dot-prefixed (`.rb_*`, `.whack_*`), are not
  exported, and carry no roxygen docs — explain them in plain comments.
- Source files are named after their stage or function, without the `rb_`
  prefix (`trip.R`, `track.R`, `gear.R`, `bin_midpoint.R`); one family per
  file, no grab-bags (the old `toolbox.R` was dissolved). Test files mirror
  them (`test-trip.R` tests `trip.R`).

## What belongs here — and what does not

- Generic track / spatial / time tools: yes.
- ICES-domain workflow steps (the WGSFD one-flow): yes.
- MFRI/Iceland-specific code (Oracle readers, Icelandic gear-id rules):
  kept, but only in clearly-named files (`*_mfri`, `logbook_mfri.R`) with
  their packages in Suggests. A future split into a separate package should
  be mechanical — do not add MFRI-only functions to generic files.
- Multibeam/bathymetry rasters and rayshading: NO — that is the sibling
  package `omb` (`~/R/Pakkar/omb`, functions `om_*`), split out 2026-10-11
  with the terra/rayshader/scales dependencies.
- Movement-behaviour models (Gaussian mixtures, EMbC, HMM step/turn):
  NO — that is the sibling package `fishymodel`
  (`~/R/Pakkar/fishymodel`, functions `fm_*`), resurrected 2026-10-11.

Keep dependencies light: heavy or single-purpose packages go in Suggests
with an `.rb_need()` guard, never in Imports.

## Compatibility policy

- No long-lived aliases. A renamed function's old name may survive one
  release as a warning alias, then it is removed (done wholesale on
  2026-10-11). fishycode is expected to migrate; NEWS.md carries the fix
  list.
- Bump `Version` (`YYYY.MM.DD`, `.N` for a second the same day) at every
  user-facing breaking change, and give NEWS.md's top section that number.
  A week of hard renames shipped as 2026.10.08.1, and old and new API could
  not be told apart until 2026.10.13.
- Data frames and lazy DuckDB tables should give the same result; the tests
  check that they do.
- Ping filters break ties in time by lon, lat: every engine sorts by
  `(vid, time, lon, lat)` and the fwdbwd SQL window orders the same way.
  Parquet reads come back in a different order from run to run, and
  same-second pings at two places otherwise changed flags (2026-10-13,
  d0fba61).
- One distance sphere: Haversine on R = 6,371,000 m is the canonical distance
  basis; `rb_calc_distance()` ([R/track.R](R/track.R)) is the reference of
  record and the same formula is hand-copied into `src/whack_forward.cpp`,
  `src/whack_sda.cpp` and the SQL of `.rb_whack_fwdbwd()` (each backend needs
  it in its own inner loop — do not "deduplicate" these). Keep all four in
  sync; the tests pin them to each other. `method = "geodesic"` is Karney via
  vendored GeographicLib C (`src/geodesic.c/h`, unchanged, MIT/X11 — plan
  013's recorded exception to the recode rule) behind
  `geod_karney()` (`src/geodesic_karney.cpp`; note it swaps ramb's
  (lon, lat) order to the C library's (lat, lon)). A lazy geodesic is
  possible — DuckDB spatial's `ST_Distance_Spheroid` (plan 013 measured
  0.12 s per million legs) — but is not built; the filters and every lazy
  path stay Haversine-only for now.
- DuckDB's `LEAST()`/`GREATEST()` ignore NULLs: never use them as an
  `ASIN`-domain clamp in SQL — NULL coordinates would silently become the
  antipodal distance. Unclamped, NULL propagates correctly.
- `rb_calc_speed()` and `rb_calc_step_acceleration()` default to
  `"geodesic"`, not to the filters' sphere. A speed kept as a value sits
  beside recorded speed over ground, and against SOG the haversine speed
  errs by heading (35,501 STK legs; fishycode decision 080, 2026-10-13). Do
  not move a default without a measurement on real data.
- `rb_calc_distance()`'s named vector form needs all four coordinates:
  `lon1 = lon`, `lat1 = lat` are the data-frame form's column defaults, and
  evaluated in the vector form they picked up a global `lon`.

## Working on the package

- `devtools::test()`, `devtools::document()`, `devtools::check()`.
- Keep chatty builds quiet: `pkgdown::build_site()` and friends relay a
  subprocess stream that `capture.output()` does not catch. Sink both
  streams to a file (`sink(f); sink(f, type = "message")`) and report
  only the pass/fail lines, or run via `callr::r(..., show = FALSE)`.
- Do not stage or commit while articles are being drafted — the
  maintainer commits when an iteration is ready.
- Commit each finished piece before starting the next: roxygen and code
  edits pile up in the same files within a session and entangle fast —
  on 2026-10-12 separating four finished pieces took comment-vs-code
  hunk splitting and per-commit verification.
- Format with `air format .` (binary at `~/.local/bin/air`; settings are
  the committed `air.toml`, i.e. explicit defaults — keep diffs free of
  formatting noise). Note: the CRAN package called `air` is an unrelated
  OpenAI client.
- Re-running `devtools::test()` in the same R session can fail on leftover
  tables in duckdbfs's cached connection; run
  `duckdbfs::close_connection()` first.
- To test what users get, install a clean `git archive HEAD` with
  `R CMD INSTALL --preclean` (to a scratch library if need be) and run
  `testthat::test_dir("tests/testthat", package = "ramb", load_package =
  "installed")`. `devtools::test()` leaves -O0 objects in `src/` that a later
  `R CMD INSTALL .` reuses (2.7x slower C++).
- One test file run alone (`test_dir(filter = )`) can error on lazy-table
  tests: duckdb's `dbWriteTable` S4 method is not registered until an
  earlier file has loaded duckdb. Judge by the full suite.
- Run `roxygen2::roxygenise()` in a UTF-8 locale; otherwise the ö in the
  maintainer's name is written as `<U+00F6>` in `man/ramb-package.Rd`.
- `devtools::check()` shows one known, harmless warning from R's own
  `Boolean.h` (`-Wfixed-enum-extension`, toolchain artifact) — not ours.
- `docs/` is gitignored; `pkgdown::build_site()` output is not committed.
- Every user-facing change gets a bullet in NEWS.md.
- Open work is tracked in fishycode `curate/plans/013-ramb-one-flow.md`,
  not in this repo's TODO.md.
- fishycode (`~/stasi/fishycode`) is read-only: read its plans, checks and
  shims freely, but deliver anything meant for it (decision records, plan
  annotations) in chat for the maintainer to place. The parquet store under
  `$FISHYDATA` (`~/stasi/fishydata`, set in fishycode's `.Renviron`) may be
  read — never written — for scale tests against real data.
- Fixture tests pin pre-recode outputs (`tests/testthat/fixtures/`). When a
  deliberate logic fix deviates from the pinned output, do not drop the
  fixture: pin the counted difference instead, with the count in the test
  comment and in NEWS.md (the `jepol` block in `test-trip.R` is the model —
  the port was verified against `pre_jepol.rds`, then the off-by-one fix
  moved 0/2/3 cuts at `max_dur_h` 72/48/24, and the test pins exactly that).
- Articles are for package users: they must run against the public API,
  with no local paths, internal (`:::`) calls, or credentials. The six
  developer-note sketches that violated this moved to fishycode
  `curate/ramb-articles/` on 2026-10-11. Articles are Quarto (`.qmd`);
  pkgdown does not expand rmarkdown-style `[rb_x()]` shortcuts in `.qmd`,
  so link explicitly (`[rb_x()](../reference/rb_x.html)`). Building
  articles leaves a `vignettes/.quarto` freeze dir — git- and
  build-ignored, safe to delete.
- Known stale corners (do not "fix" blindly): `TOPSECRET.R` and
  `zgarbage/` sit in the repo root, build-ignored.
- Deliberate keeps from the 2026-10-12 review (do not re-flag):
  `rb_calc_step_acceleration()` is distance/dt², not Δspeed/Δt
  (documented as such; no consumer in ramb, fishycode or fishymodel);
  `.rb_get_ices_gears()`' key column is misnamed `target` (it holds gear
  codes); `rb_pad_harbour()` keeps its name per plan 013 — its
  arguments are `voyage_id` and `port_id` since 2026.10.15.1.
