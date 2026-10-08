# renamed.R — old names, kept as aliases that warn they will be removed (fishycode plan 013, phase 1a).
#
# Each alias warns once per session, so a loop over batches warns once, not thousands of times, and
# then calls the new function. The bodies did not change; tests/testthat/test-renamed.R checks every
# alias and every new name against outputs saved with the ramb of before the renames.

.rb_warned <- new.env(parent = emptyenv())

.rb_renamed <- function(old, new) {
  if (!isTRUE(.rb_warned[[old]])) {
    assign(old, TRUE, envir = .rb_warned)
    warning(sprintf("`%s()` is now `%s`. The old name still works but will be removed in a future version.",
                    old, new), call. = FALSE)
  }
  invisible(NULL)
}

# A function superseded by a new one keeps its old body and warns once per session, unless a ramb
# function called it (the new functions use some of the old bodies).
.rb_superseded <- function(old, new) {
  if (identical(topenv(parent.frame(2)), asNamespace("ramb"))) return(invisible(NULL))
  if (!isTRUE(.rb_warned[[old]])) {
    assign(old, TRUE, envir = .rb_warned)
    warning(sprintf("`%s()` is superseded by `%s` and will be removed in a future version; until then it behaves as before.",
                    old, new), call. = FALSE)
  }
  invisible(NULL)
}

# For tests: forget which old names have warned this session.
.rb_reset_renamed <- function() rm(list = ls(.rb_warned), envir = .rb_warned)

#' Old function names
#'
#' These names were changed on 2026-10-08 so that every function is named verb first, with a word
#' for its step in the fishing-activity flow (see the Naming section of [ramb-package]). The old
#' names still work, warn once per session, and will be removed in a future version.
#'
#' @param ... Passed to the new function.
#' @name ramb-renamed
#' @keywords internal
NULL


#' @rdname ramb-renamed
#' @export
rb_speed <- function(...) { .rb_renamed("rb_speed", "rb_calc_speed()"); rb_calc_speed(...) }

#' @rdname ramb-renamed
#' @export
rb_distance <- function(...) { .rb_renamed("rb_distance", "rb_calc_distance()"); rb_calc_distance(...) }

#' @rdname ramb-renamed
#' @export
rb_st <- function(...) { .rb_renamed("rb_st", "rb_calc_step_time()"); rb_calc_step_time(...) }

#' @rdname ramb-renamed
#' @export
rb_sa <- function(...) { .rb_renamed("rb_sa", "rb_calc_step_acceleration()"); rb_calc_step_acceleration(...) }

#' @rdname ramb-renamed
#' @export
rb_track_time <- function(...) { .rb_renamed("rb_track_time", "rb_calc_track_time()"); rb_calc_track_time(...) }

#' @rdname ramb-renamed
#' @export
rb_event <- function(...) { .rb_renamed("rb_event", "rb_number_runs()"); rb_number_runs(...) }

#' @rdname ramb-renamed
#' @export
rb_d2ir <- function(...) { .rb_renamed("rb_d2ir", "rb_encode_ices_rectangle()"); rb_encode_ices_rectangle(...) }

#' @rdname ramb-renamed
#' @export
rb_midpoint <- function(...) { .rb_renamed("rb_midpoint", "rb_bin_midpoint()"); rb_bin_midpoint(...) }

#' @rdname ramb-renamed
#' @export
rb_points_in_polygons <- function(...) { .rb_renamed("rb_points_in_polygons", "rb_detect_in_polygons()"); rb_detect_in_polygons(...) }

#' @rdname ramb-renamed
#' @export
rb_st_keep <- function(...) { .rb_renamed("rb_st_keep", "rb_keep_in_polygons()"); rb_keep_in_polygons(...) }

#' @rdname ramb-renamed
#' @export
rb_st_drop <- function(...) { .rb_renamed("rb_st_drop", "rb_drop_in_polygons()"); rb_drop_in_polygons(...) }

#' @rdname ramb-renamed
#' @export
rb_benthis_width <- function(...) { .rb_renamed("rb_benthis_width", "rb_predict_gear_width()"); rb_predict_gear_width(...) }

#' @rdname ramb-renamed
#' @export
rb_gearwidth_proxy <- function(...) { .rb_renamed("rb_gearwidth_proxy", "rb_fill_gear_width()"); rb_fill_gear_width(...) }

#' @rdname ramb-renamed
#' @export
rb_std_meshsize <- function(...) { .rb_renamed("rb_std_meshsize", "rb_standardise_gear_meshsize()"); rb_standardise_gear_meshsize(...) }

#' @rdname ramb-renamed
#' @export
rb_mmsi_category <- function(...) { .rb_renamed("rb_mmsi_category", "rb_classify_mmsi()"); rb_classify_mmsi(...) }

#' @rdname ramb-renamed
#' @export
rb_mmsi_flag <- function(...) { .rb_renamed("rb_mmsi_flag", "rb_lookup_mmsi_country()"); rb_lookup_mmsi_country(...) }

#' @rdname ramb-renamed
#' @export
rb_check_crosses_year <- function(...) { .rb_renamed("rb_check_crosses_year", "rb_detect_crosses_year()"); rb_detect_crosses_year(...) }

#' @rdname ramb-renamed
#' @export
dc_check_eflalo_crosses_year <- function(...) { .rb_renamed("dc_check_eflalo_crosses_year", "rb_check_trip_crosses_year()"); rb_check_trip_crosses_year(...) }

#' @rdname ramb-renamed
#' @export
dc_create_timestamp <- function(...) { .rb_renamed("dc_create_timestamp", "rb_create_timestamp()"); rb_create_timestamp(...) }

#' @rdname ramb-renamed
#' @export
rb_mapdeck <- function(...) { .rb_renamed("rb_mapdeck", "rb_plot_trail()"); rb_plot_trail(...) }

#' @rdname ramb-renamed
#' @export
rb_md_trip <- function(...) { .rb_renamed("rb_md_trip", "rb_plot_trip()"); rb_plot_trip(...) }

#' @rdname ramb-renamed
#' @export
rb_summary <- function(...) { .rb_renamed("rb_summary", "rb_summarise_track()"); rb_summarise_track(...) }

#' @rdname ramb-renamed
#' @export
rb_logbook <- function(...) { .rb_renamed("rb_logbook", "rb_read_logbook_mfri()"); rb_read_logbook_mfri(...) }

#' @rdname ramb-renamed
#' @export
rb_trail <- function(...) { .rb_renamed("rb_trail", "rb_read_trail_mfri()"); rb_read_trail_mfri(...) }

#' @rdname ramb-renamed
#' @export
read_is_harbours <- function(...) { .rb_renamed("read_is_harbours", "rb_read_harbours_mfri()"); rb_read_harbours_mfri(...) }

#' @rdname ramb-renamed
#' @export
read_is_survey_stations <- function(...) { .rb_renamed("read_is_survey_stations", "rb_read_survey_stations_mfri()"); rb_read_survey_stations_mfri(...) }

#' @rdname ramb-renamed
#' @export
read_is_survey_tracks <- function(...) { .rb_renamed("read_is_survey_tracks", "rb_read_survey_tracks_mfri()"); rb_read_survey_tracks_mfri(...) }

#' @rdname ramb-renamed
#' @export
mb_base_raster <- function(...) { .rb_renamed("mb_base_raster", "rb_create_base_raster()"); rb_create_base_raster(...) }

#' @rdname ramb-renamed
#' @export
mb_bb <- function(...) { .rb_renamed("mb_bb", "rb_create_bbox()"); rb_create_bbox(...) }

#' @rdname ramb-renamed
#' @export
mb_rashade_xyz_dynamic <- function(...) { .rb_renamed("mb_rashade_xyz_dynamic", "rb_rayshade_xyz_dynamic()"); rb_rayshade_xyz_dynamic(...) }

#' @rdname ramb-renamed
#' @export
mb_rasterize_xyz <- function(...) { .rb_renamed("mb_rasterize_xyz", "rb_rasterize_xyz()"); rb_rasterize_xyz(...) }

#' @rdname ramb-renamed
#' @export
mb_rayshade_raster <- function(...) { .rb_renamed("mb_rayshade_raster", "rb_rayshade_raster()"); rb_rayshade_raster(...) }

#' @rdname ramb-renamed
#' @export
mb_rayshade_raster_rgb <- function(...) { .rb_renamed("mb_rayshade_raster_rgb", "rb_rayshade_raster_rgb()"); rb_rayshade_raster_rgb(...) }

#' @rdname ramb-renamed
#' @export
mb_rayshade_to_rgb <- function(...) { .rb_renamed("mb_rayshade_to_rgb", "rb_convert_rayshade_to_rgb()"); rb_convert_rayshade_to_rgb(...) }

#' @rdname ramb-renamed
#' @export
mb_xyz_extent <- function(...) { .rb_renamed("mb_xyz_extent", "rb_calc_xyz_extent()"); rb_calc_xyz_extent(...) }

#' @rdname ramb-renamed
#' @export
rb_ms2kn <- function(...) { .rb_renamed("rb_ms2kn", 'rb_convert_speed(from = "ms", to = "kn")'); rb_convert_speed(..., from = "ms", to = "kn") }

#' @rdname ramb-renamed
#' @export
rb_kn2ms <- function(...) { .rb_renamed("rb_kn2ms", 'rb_convert_speed(from = "kn", to = "ms")'); rb_convert_speed(..., from = "kn", to = "ms") }

#' @rdname ramb-renamed
#' @export
rb_gear_from_metier <- function(...) { .rb_renamed("rb_gear_from_metier", 'rb_extract_metier(part = "gear")'); rb_extract_metier(..., part = "gear") }

#' @rdname ramb-renamed
#' @export
rb_target_from_metier <- function(...) { .rb_renamed("rb_target_from_metier", 'rb_extract_metier(part = "target")'); rb_extract_metier(..., part = "target") }

#' @rdname ramb-renamed
#' @export
rb_met5_from6 <- function(...) { .rb_renamed("rb_met5_from6", 'rb_extract_metier(part = "metier5")'); rb_extract_metier(..., part = "metier5") }

#' @rdname ramb-renamed
#' @export
rb_get_ices_gears <- function(...) { .rb_renamed("rb_get_ices_gears", 'rb_get_gear_vocabulary(list = "gears")'); rb_get_gear_vocabulary(..., list = "gears") }

#' @rdname ramb-renamed
#' @export
rb_get_ices_target <- function(...) { .rb_renamed("rb_get_ices_target", 'rb_get_gear_vocabulary(list = "target")'); rb_get_gear_vocabulary(..., list = "target") }

#' @rdname ramb-renamed
#' @export
rb_get_ices_metier5 <- function(...) { .rb_renamed("rb_get_ices_metier5", 'rb_get_gear_vocabulary(list = "metier5")'); rb_get_gear_vocabulary(..., list = "metier5") }

#' @rdname ramb-renamed
#' @export
rb_get_ices_metier6 <- function(...) { .rb_renamed("rb_get_ices_metier6", 'rb_get_gear_vocabulary(list = "metier6")'); rb_get_gear_vocabulary(..., list = "metier6") }

#' @rdname ramb-renamed
#' @export
rb_get_ices_metier5_benthis_lookup <- function(...) { .rb_renamed("rb_get_ices_metier5_benthis_lookup", 'rb_get_gear_vocabulary(list = "metier5_benthis_lookup")'); rb_get_gear_vocabulary(..., list = "metier5_benthis_lookup") }

#' @rdname ramb-renamed
#' @export
rb_cap_iqr <- function(...) { .rb_renamed("rb_cap_iqr", 'rb_cap_outliers(method = "iqr")'); rb_cap_outliers(..., method = "iqr") }

#' @rdname ramb-renamed
#' @export
rb_cap_miller <- function(...) { .rb_renamed("rb_cap_miller", 'rb_cap_outliers(method = "miller")'); rb_cap_outliers(..., method = "miller") }

#' @rdname ramb-renamed
#' @export
rb_cap_winsorize <- function(...) { .rb_renamed("rb_cap_winsorize", 'rb_cap_outliers(method = "winsorize")'); rb_cap_outliers(..., method = "winsorize") }
