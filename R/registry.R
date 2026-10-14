# registry.R — the step registry: one row per step function of the fishing-activity flow.
#
# fishycode plan 013, "robustness to misuse" (maintainer, 2026-10-09; the skeleton built 2026-10-14). A row says
# what a function needs from its main input, what it adds, and the step that normally comes before it. State is
# read from columns, never from classes or attributes, because those do not survive parquet or a lazy DuckDB
# query. The check helper below reads the registry, so a missing column is reported with the step that adds it.
#
# Kinds: "step" adds columns to the pings and passes every ping on; "builder" returns a table of its own (stays,
# voyages, windows, links); "tool" serves a step but is not one (plots, vocabularies, the MFRI logbook helpers).
# Column names are the grammar's; a function whose columns can be renamed passes the user's names to the check.
# `paths` is the per-unit path a step serves (plan 013, "paths are branches in the code"); "all" until the path
# column exists.
#
# Only rb_assign_effort() calls the check so far. The other rows are data until each function is retrofitted.

.rb_step <- function(
  fn,
  stage,
  kind,
  input,
  needs,
  adds,
  after = NA_character_,
  paths = "all"
) {
  list(
    fn = fn,
    stage = stage,
    kind = kind,
    input = input,
    needs = needs,
    adds = adds,
    after = after,
    paths = paths
  )
}

.rb_steps <- list(
  # step 0: ping flags
  .rb_step(
    "rb_flag_ping_invalid",
    "ping",
    "step",
    "x",
    c("lon", "lat"),
    "ping_flag"
  ),
  .rb_step(
    "rb_flag_ping_duplicate",
    "ping",
    "step",
    "x",
    c("vid", "time", "lon", "lat"),
    "ping_flag",
    after = "rb_flag_ping_invalid"
  ),
  .rb_step(
    "rb_flag_ping_impossible",
    "ping",
    "step",
    "x",
    c("vid", "time", "lon", "lat"),
    "ping_flag",
    after = "rb_flag_ping_duplicate"
  ),
  .rb_step(
    "rb_flag_ping_port",
    "ping",
    "step",
    "pings",
    c("lon", "lat"),
    "port_id",
    after = "rb_flag_ping_impossible"
  ),
  # step 1: trips
  .rb_step(
    "rb_find_trip_stays",
    "trip",
    "builder",
    "pings",
    c("vid", "time", "lon", "lat"),
    c("port_id", "T_in", "T_out", "dur_h", "rule", "basis"),
    after = "rb_flag_ping_port"
  ),
  .rb_step(
    "rb_cut_trip_voyages",
    "trip",
    "builder",
    "pings",
    c("vid", "time"),
    c("voyage_id", "T1", "T2", "port_from", "port_to", "n_pings"),
    after = "rb_find_trip_stays"
  ),
  .rb_step(
    "rb_link_trip_voyages",
    "trip",
    "builder",
    "trips",
    c("vid", "T1", "T2"),
    c(
      "voyage_id",
      "T1_voyage",
      "T2_voyage",
      "overlap_s",
      "overlap_frac",
      "n_candidates",
      "basis"
    ),
    after = "rb_cut_trip_voyages"
  ),
  .rb_step(
    "rb_link_trip_logbook",
    "trip",
    "step",
    "ve",
    c("vessel_id", "time"),
    "trip_id"
  ),
  .rb_step(
    "rb_assign_trip",
    "trip",
    "step",
    "pings",
    c("vid", "time"),
    c("stay_port_id", "voyage_id", "trip_id", "trip_basis"),
    after = "rb_cut_trip_voyages"
  ),
  .rb_step(
    "rb_check_trip_crosses_year",
    "trip",
    "tool",
    "eflalo",
    character(0),
    character(0)
  ),
  .rb_step(
    "rb_plot_trip",
    "trip",
    "tool",
    "data",
    c("lon", "lat"),
    character(0)
  ),
  # step 2: logbook records
  .rb_step(
    "rb_limit_record_duration",
    "record",
    "builder",
    "records",
    character(0),
    "duration_source"
  ),
  .rb_step(
    "rb_build_record_windows",
    "record",
    "builder",
    "records",
    c("vid", "gear", "gear_class", "t1", "t2", "t3", "t4", "date"),
    c(
      "phase",
      "w_start",
      "w_end",
      "win_basis",
      "win_closed_by",
      "win_opened_by",
      "n_events",
      "events"
    ),
    after = "rb_limit_record_duration"
  ),
  .rb_step(
    "rb_link_record_pings",
    "record",
    "builder",
    "pings",
    c("vid", "time"),
    c("phase", "phase_frac", "w_start", "w_end", "fishing"),
    after = "rb_build_record_windows"
  ),
  .rb_step(
    "rb_link_record_events",
    "record",
    "builder",
    "pings",
    c("vid", "time"),
    c("event", "event_time", "dt_s", "gap_s", "ping_lost"),
    after = "rb_build_record_windows"
  ),
  .rb_step(
    "rb_cut_record_stretches",
    "record",
    "builder",
    "pings",
    c("vid", "time", "speed"),
    c("stretch_id", "s_start", "s_end", "n_pings", "opened_by", "closed_by"),
    after = "rb_link_record_pings"
  ),
  .rb_step(
    "rb_link_record_stretches",
    "record",
    "builder",
    "records",
    c("vid"),
    c(
      "stretch_id",
      "overlap_s",
      "dt_start_s",
      "dt_end_s",
      "n_candidates",
      "stretch_lost"
    ),
    after = "rb_cut_record_stretches"
  ),
  .rb_step(
    "rb_assign_record",
    "record",
    "step",
    "pings",
    c("vid", "time"),
    c("n_records", "phase", "phase_frac"),
    after = "rb_link_record_pings"
  ),
  # step 3: gear
  .rb_step(
    "rb_predict_gear_width",
    "gear",
    "tool",
    "metier",
    character(0),
    character(0)
  ),
  .rb_step("rb_fill_gear_width", "gear", "tool", "lb", "gid", "gear.width"),
  .rb_step(
    "rb_get_gear_vocabulary",
    "gear",
    "tool",
    "code_list",
    character(0),
    character(0)
  ),
  .rb_step(
    "rb_standardise_gear_meshsize",
    "gear",
    "tool",
    "lb",
    c("gid", "mesh"),
    "mesh.std"
  ),
  # step 4: fishing
  .rb_step(
    "rb_assign_fishing",
    "fishing",
    "step",
    "pings",
    c("vid", "time", "speed"),
    "fishing",
    after = "rb_link_record_pings"
  ),
  # step 5: effort
  .rb_step(
    "rb_cap_effort",
    "effort",
    "tool",
    "lb",
    c("vid", "gid", "effort", "t1", "t2"),
    character(0)
  ),
  .rb_step(
    "rb_assign_effort",
    "effort",
    "step",
    "pings",
    c("vid", "time"),
    c(
      "hours",
      "kw_hours",
      "pressure",
      "pressure_unit",
      "interval_flag",
      "effort_basis"
    ),
    after = "rb_assign_fishing"
  ),
  # step 6: catch (EFLALO-shaped until it is rewritten to the grammar, plan 013 item 2)
  .rb_step(
    "rb_share_catch",
    "catch",
    "step",
    "pings",
    c(".pid", "VE_COU", "VE_REF", "time", "FT_REF", "ir", "state"),
    c("LE_KG", "LE_EURO", ".how"),
    after = "rb_assign_effort"
  )
)
names(.rb_steps) <- vapply(.rb_steps, `[[`, "", "fn")

# The registry as a table, one row per function; needs, adds and paths are list columns.
.rb_registry <- function() {
  tibble::tibble(
    fn = names(.rb_steps),
    stage = unname(vapply(.rb_steps, `[[`, "", "stage")),
    kind = unname(vapply(.rb_steps, `[[`, "", "kind")),
    input = unname(vapply(.rb_steps, `[[`, "", "input")),
    needs = unname(lapply(.rb_steps, `[[`, "needs")),
    adds = unname(lapply(.rb_steps, `[[`, "adds")),
    after = unname(vapply(.rb_steps, `[[`, "", "after")),
    paths = unname(lapply(.rb_steps, `[[`, "paths"))
  )
}

# The steps that add a column, for the message that names the step to run first. A builder's columns are on a
# table of its own, so builders are named only when no step adds the column.
.rb_added_by <- function(col) {
  adds <- function(kind) {
    hit <- vapply(
      .rb_steps,
      function(s) s$kind == kind && col %in% s$adds,
      logical(1)
    )
    names(.rb_steps)[hit]
  }
  by <- adds("step")
  if (length(by) == 0) adds("builder") else by
}

# Check a step function's main input against its registry row, before any work is done.
#   x       the input (a data frame or a lazy table; a lazy table is checked by its schema only)
#   fn      the calling function's name, a key of .rb_steps
#   cols    the user's names for the grammar's columns, e.g. c(vid = "vessel_id"); unnamed needs are as listed
#   also    columns needed only in this call (an optional input the user switched on), named as `cols`
#   types   expected classes for some grammar columns: "time" (POSIXct) or "numeric"
#   fresh   TRUE: the columns this step adds must not be there yet (it has already run on this table)
#   call    the frame the error is reported from (the step function's)
.rb_check_input <- function(
  x,
  fn,
  cols = character(0),
  also = character(0),
  types = character(0),
  fresh = FALSE,
  call = rlang::caller_env()
) {
  row <- .rb_steps[[fn]]
  if (is.null(row)) {
    stop(
      sprintf("internal: %s() has no row in the step registry", fn),
      call. = FALSE
    )
  }
  arg <- row$input
  if (!(is.data.frame(x) || inherits(x, "tbl_lazy"))) {
    rlang::abort(
      c(
        sprintf("`%s` must be a data frame or a lazy DuckDB table.", arg),
        x = sprintf("It is a %s.", class(x)[1])
      ),
      call = call
    )
  }
  # a 0-row frame holds the column types of a lazy table without scanning it
  proto <- if (inherits(x, "tbl_lazy")) {
    dplyr::collect(utils::head(x, 0))
  } else {
    x[0, , drop = FALSE]
  }
  have <- names(proto)
  cols <- c(cols, also)
  user <- function(g) if (g %in% names(cols)) cols[[g]] else g
  needs <- unique(c(row$needs, names(also)))
  miss <- needs[!vapply(needs, function(g) user(g) %in% have, logical(1))]
  if (length(miss) > 0) {
    g <- miss[1]
    by <- .rb_added_by(g)
    hint <- if (length(by) > 0) {
      c(
        i = sprintf(
          "It is added by %s.",
          paste0("`", by, "()`", collapse = " or ")
        )
      )
    } else if (g %in% names(cols)) {
      c(i = sprintf("Name the column that holds it with the `%s` argument.", g))
    } else {
      c(i = "It is an input column: supply it, or rename yours to it.")
    }
    rlang::abort(
      c(
        sprintf("`%s` must have a column `%s`.", arg, user(g)),
        x = sprintf(
          "%s %s missing.",
          paste0("`", vapply(miss, user, ""), "`", collapse = ", "),
          if (length(miss) == 1) "is" else "are"
        ),
        hint
      ),
      call = call
    )
  }
  for (g in names(types)) {
    u <- user(g)
    if (!u %in% have) {
      next
    }
    ok <- switch(
      types[[g]],
      time = inherits(proto[[u]], "POSIXct"),
      numeric = is.numeric(proto[[u]]),
      TRUE
    )
    if (!ok) {
      rlang::abort(
        c(
          sprintf(
            "`%s` must be %s.",
            u,
            if (types[[g]] == "time") "a date-time (POSIXct)" else "numeric"
          ),
          x = sprintf("It is %s.", class(proto[[u]])[1])
        ),
        call = call
      )
    }
  }
  if (fresh) {
    again <- intersect(row$adds, have)
    if (length(again) > 0) {
      rlang::abort(
        c(
          sprintf(
            "`%s` already has %s.",
            arg,
            paste0("`", again, "`", collapse = ", ")
          ),
          i = sprintf(
            "%s() has probably been run on it already; drop those columns to run it again.",
            fn
          )
        ),
        call = call
      )
    }
  }
  invisible(proto)
}
