# renamed.R — the old names of fishycode plan 032 (2026-10-19), kept for one release as aliases that warn.
#
# Each alias warns once per session, so a loop over batches warns once, not thousands of times, and then calls
# the new function. The aliases take the old argument names (`stays`, `min_stay_h`, `min_stay`, method "stays")
# and give the old result: rb_find_trip_stays() without `year` and `port_call_id`, rb_assign_trip() with
# `stay_port_id` and `stay_id`, rb_create_timestamp() a vector. tests/testthat/test-renamed.R checks each against
# its new name.

.rb_warned <- new.env(parent = emptyenv())

.rb_renamed <- function(old, new, also = NULL) {
  if (!isTRUE(.rb_warned[[old]])) {
    assign(old, TRUE, envir = .rb_warned)
    warning(
      sprintf(
        "`%s()` is now `%s()`%s. The old name still works but will be removed in the next release.",
        old,
        new,
        if (is.null(also)) "" else paste0(" (", also, ")")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

# For tests: forget which old names have warned this session.
.rb_reset_renamed <- function() rm(list = ls(.rb_warned), envir = .rb_warned)

# Old name = new name, for the tests and the registry check.
.rb_renamed_from <- c(
  rb_find_trip_stays = "rb_create_port_calls",
  rb_cut_trip_voyages = "rb_create_voyages",
  rb_cut_record_stretches = "rb_create_record_stretches",
  rb_build_record_windows = "rb_create_record_windows",
  rb_build_record_pings = "rb_create_record_pings",
  rb_assign_trip = "rb_add_voyages",
  rb_assign_record = "rb_add_record",
  rb_assign_fishing = "rb_add_fishing",
  rb_assign_effort = "rb_add_effort",
  rb_link_trip_logbook = "rb_add_logbook_trip",
  rb_build_catch_units = "rb_add_catch_units",
  rb_create_timestamp = "rb_add_timestamp"
)

#' Old function names
#'
#' These names were changed on 2026-10-19 so that a function's verb says what it returns: `rb_create_*()` a table
#' of its own, `rb_add_*()` the input with columns added (see the Naming section of [ramb-package]). A port stay
#' is now a port call, and `method = "stays"` is `"dwell"`. The old names still work, with the old argument
#' names, warn once per session, and will be removed in the next release.
#'
#' | old | new |
#' |---|---|
#' | `rb_find_trip_stays()` | [rb_create_port_calls()] |
#' | `rb_cut_trip_voyages()` | [rb_create_voyages()] |
#' | `rb_cut_record_stretches()` | [rb_create_record_stretches()] |
#' | `rb_build_record_windows()` | [rb_create_record_windows()] |
#' | `rb_build_record_pings()` | [rb_create_record_pings()] |
#' | `rb_assign_trip()` | [rb_add_voyages()] |
#' | `rb_assign_record()` | [rb_add_record()] |
#' | `rb_assign_fishing()` | [rb_add_fishing()] |
#' | `rb_assign_effort()` | [rb_add_effort()] |
#' | `rb_link_trip_logbook()` | [rb_add_logbook_trip()] |
#' | `rb_build_catch_units()` | [rb_add_catch_units()] |
#' | `rb_create_timestamp()` | [rb_add_timestamp()] (now the data with a column added) |
#'
#' @param ... Passed to the new function.
#' @param pings,ports,radius_in,events,t_in_window,crs_m,voyages,by_year,vid,time,lon,lat As in the new
#'   function.
#' @param min_stay_h,min_stay Now `min_dwell_h` and `min_dwell` (with a column `min_dwell_h`).
#' @param stays Now `port_calls`.
#' @param stay Now `port_call`.
#' @param method As in the new function, where `"stays"` is now `"dwell"`.
#' @param date,tz,format As in `rb_create_timestamp()` of before: vectors of dates and times.
#' @name ramb-renamed
#' @keywords internal
NULL

#' @rdname ramb-renamed
#' @export
rb_find_trip_stays <- function(
  pings,
  ports,
  min_stay_h = 4,
  min_stay = NULL,
  radius_in = 2000,
  events = NULL,
  t_in_window = NULL,
  crs_m = 3575,
  vid = vid,
  time = time,
  lon = lon,
  lat = lat
) {
  .rb_renamed(
    "rb_find_trip_stays",
    "rb_create_port_calls",
    "`min_stay_h` is now `min_dwell_h`"
  )
  if (!is.null(min_stay)) {
    min_stay <- dplyr::rename(min_stay, min_dwell_h = min_stay_h)
  }
  out <- rb_create_port_calls(
    pings,
    ports,
    method = "dwell",
    min_dwell_h = min_stay_h,
    min_dwell = min_stay,
    radius_in = radius_in,
    events = events,
    t_in_window = t_in_window,
    crs_m = crs_m,
    vid = {{ vid }},
    time = {{ time }},
    lon = {{ lon }},
    lat = {{ lat }}
  )
  dplyr::select(out, -year, -port_call_id)
}

#' @rdname ramb-renamed
#' @export
rb_cut_trip_voyages <- function(
  pings,
  stays = NULL,
  method = c("stays", "runs", "jepol", "cnr", "iapesca"),
  ...
) {
  .rb_renamed(
    "rb_cut_trip_voyages",
    "rb_create_voyages",
    "`stays` is now `port_calls`, method \"stays\" is \"dwell\""
  )
  method <- match.arg(method)
  if (method == "stays") {
    method <- "dwell"
  }
  rb_create_voyages(pings, port_calls = stays, method = method, ...)
}

#' @rdname ramb-renamed
#' @export
rb_assign_trip <- function(
  pings,
  voyages,
  stays = NULL,
  by_year = TRUE,
  vid = vid,
  time = time
) {
  .rb_renamed(
    "rb_assign_trip",
    "rb_add_voyages",
    "`stays` is now `port_calls`, `stay_port_id` and `stay_id` are `port_call_port_id` and `port_call_id`"
  )
  if (!is.null(stays) && "stay_id" %in% colnames(stays)) {
    stays <- dplyr::rename(stays, port_call_id = stay_id)
  }
  out <- rb_add_voyages(
    pings,
    voyages,
    port_calls = stays,
    by_year = by_year,
    vid = {{ vid }},
    time = {{ time }}
  )
  dplyr::rename(
    out,
    dplyr::any_of(c(
      stay_port_id = "port_call_port_id",
      stay_id = "port_call_id"
    ))
  )
}

#' @rdname ramb-renamed
#' @export
rb_cut_record_stretches <- function(..., stay = NULL) {
  .rb_renamed(
    "rb_cut_record_stretches",
    "rb_create_record_stretches",
    "`stay` is now `port_call`"
  )
  rb_create_record_stretches(..., port_call = stay)
}

#' @rdname ramb-renamed
#' @export
rb_build_record_windows <- function(...) {
  .rb_renamed("rb_build_record_windows", "rb_create_record_windows")
  rb_create_record_windows(...)
}

#' @rdname ramb-renamed
#' @export
rb_build_record_pings <- function(...) {
  .rb_renamed("rb_build_record_pings", "rb_create_record_pings")
  rb_create_record_pings(...)
}

#' @rdname ramb-renamed
#' @export
rb_assign_record <- function(...) {
  .rb_renamed("rb_assign_record", "rb_add_record")
  rb_add_record(...)
}

#' @rdname ramb-renamed
#' @export
rb_assign_fishing <- function(...) {
  .rb_renamed("rb_assign_fishing", "rb_add_fishing")
  rb_add_fishing(...)
}

#' @rdname ramb-renamed
#' @export
rb_assign_effort <- function(...) {
  .rb_renamed("rb_assign_effort", "rb_add_effort")
  rb_add_effort(...)
}

#' @rdname ramb-renamed
#' @export
rb_link_trip_logbook <- function(...) {
  .rb_renamed("rb_link_trip_logbook", "rb_add_logbook_trip")
  rb_add_logbook_trip(...)
}

#' @rdname ramb-renamed
#' @export
rb_build_catch_units <- function(...) {
  .rb_renamed("rb_build_catch_units", "rb_add_catch_units")
  rb_add_catch_units(...)
}

#' @rdname ramb-renamed
#' @export
rb_create_timestamp <- function(
  date,
  time,
  tz = "GMT",
  format = "%d/%m/%Y %H:%M:%S"
) {
  .rb_renamed(
    "rb_create_timestamp",
    "rb_add_timestamp",
    "which takes the data and adds the column"
  )
  if (missing(date) || missing(time)) {
    stop("Both 'date' and 'time' must be provided.", call. = FALSE)
  }
  .rb_timestamp(date, time, tz = tz, format = format)
}
