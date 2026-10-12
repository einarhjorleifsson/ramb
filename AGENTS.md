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
- Data frames and lazy DuckDB tables should give the same result; the tests
  check that they do.

## Working on the package

- `devtools::test()`, `devtools::document()`, `devtools::check()`.
- Keep chatty builds quiet: `pkgdown::build_site()` and friends relay a
  subprocess stream that `capture.output()` does not catch. Sink both
  streams to a file (`sink(f); sink(f, type = "message")`) and report
  only the pass/fail lines, or run via `callr::r(..., show = FALSE)`.
- Do not stage or commit while articles are being drafted — the
  maintainer commits when an iteration is ready.
- Format with `air format .` (binary at `~/.local/bin/air`; settings are
  the committed `air.toml`, i.e. explicit defaults — keep diffs free of
  formatting noise). Note: the CRAN package called `air` is an unrelated
  OpenAI client.
- Re-running `devtools::test()` in the same R session can fail on leftover
  tables in duckdbfs's cached connection; run
  `duckdbfs::close_connection()` first.
- Every user-facing change gets a bullet in NEWS.md.
- Open work is tracked in fishycode `curate/plans/013-ramb-one-flow.md`,
  not in this repo's TODO.md.
- Articles are for package users: they must run against the public API,
  with no local paths, internal (`:::`) calls, or credentials. The six
  developer-note sketches that violated this moved to fishycode
  `curate/ramb-articles/` on 2026-10-11.
- Known stale corners (do not "fix" blindly): `TOPSECRET.R` and
  `zgarbage/` sit in the repo root, build-ignored.
