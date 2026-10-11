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

## What belongs here

- Generic track / spatial / time tools: yes.
- ICES-domain workflow steps (the WGSFD one-flow): yes.
- MFRI/Iceland-specific code (Oracle readers, Icelandic gear-id rules):
  kept, but only in clearly-named files (`*_mfri`, `logbook_mfri.R`) with
  their packages in Suggests. A future split into a separate package should
  be mechanical — do not add MFRI-only functions to generic files.

## Compatibility policy

- No long-lived aliases. A renamed function's old name may survive one
  release as a warning alias, then it is removed (done wholesale on
  2026-10-11). fishycode is expected to migrate; NEWS.md carries the fix
  list.
- Data frames and lazy DuckDB tables should give the same result; the tests
  check that they do.

## Working on the package

- `devtools::test()`, `devtools::document()`, `devtools::check()`.
- Re-running `devtools::test()` in the same R session can fail on leftover
  tables in duckdbfs's cached connection; run
  `duckdbfs::close_connection()` first.
- Every user-facing change gets a bullet in NEWS.md.
- Open work is tracked in fishycode `curate/plans/013-ramb-one-flow.md`,
  not in this repo's TODO.md.
