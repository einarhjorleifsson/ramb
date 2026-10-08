# The renames of fishycode plan 013, phase 1a: new names reproduce the outputs saved with the ramb of
# before the renames (fixtures/pre_rename.rds, made by fixtures/make_pre_rename.R), old names still
# work and warn once, and every export follows the naming rule.

snap <- readRDS(test_path("fixtures", "pre_rename.rds"))

x <- head(dplyr::filter(dansk, vessel_id == dansk$vessel_id[1]), 300)
pts <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
hb <- sf::st_transform(dansk_harbours, 4326)
met <- c("OTB_DEF_>=120_0_0", "GNS_DEF_100-119_0_0", "OTB_CRU_70-99_0_0", "FPO_CRU_0_0_0")
lb <- data.frame(gid = c(5L, 6L, 7L, 9L, 14L, 2L, 15L), sweeps = c(NA, 100, 300, 50, 40, NA, NA),
                 plow_width = c(NA, NA, NA, NA, NA, NA, 3), mesh = c(150, 130, 40, 80, 40, 180, NA),
                 mesh_min = c(NA, NA, 38, NA, NA, NA, NA))
v <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 50, 100)
t1 <- as.POSIXct(c("2022-12-31 23:50:00", "2023-01-01 10:00:00"), tz = "UTC")
t2 <- as.POSIXct(c("2023-01-01 00:30:00", "2023-01-01 12:00:00"), tz = "UTC")
eflalo <- data.frame(FT_DDAT = c("31/12/2022", "01/01/2023"), FT_DTIME = c("23:50:00", "10:00:00"),
                     FT_LDAT = c("01/01/2023", "01/01/2023"), FT_LTIME = c("00:30:00", "12:00:00"))

# old name, new call, the new call's result
cases <- list(
  rb_speed = quote(rb_calc_speed(x$lon, x$lat, x$time_stamp)),
  rb_distance = quote(rb_calc_distance(x$lon[-1], x$lat[-1], x$lon[-300], x$lat[-300])),
  rb_st = quote(rb_calc_step_time(x$time_stamp)),
  rb_sa = quote(rb_calc_step_acceleration(x$lon, x$lat, x$time_stamp)),
  rb_track_time = quote(rb_calc_track_time(x$time_stamp)),
  rb_ms2kn = quote(rb_convert_speed(v, from = "ms", to = "kn")),
  rb_kn2ms = quote(rb_convert_speed(v, from = "kn", to = "ms")),
  rb_event = quote(rb_number_runs(x$speed > 3)),
  rb_d2ir = quote(rb_encode_ices_rectangle(x$lon, x$lat)),
  rb_midpoint = quote(rb_bin_midpoint(x$lon, 0.05)),
  rb_points_in_polygons = quote(rb_detect_in_polygons(pts, hb)),
  rb_st_keep = quote(rb_keep_in_polygons(pts, hb)),
  rb_st_drop = quote(rb_drop_in_polygons(pts, hb)),
  rb_gear_from_metier = quote(rb_extract_metier(met, part = "gear")),
  rb_target_from_metier = quote(rb_extract_metier(met, part = "target")),
  rb_met5_from6 = quote(rb_extract_metier(met, part = "metier5")),
  rb_benthis_width = quote(rb_predict_gear_width(c("OT_DMF", "TBB_DMF", "DRB_MOL"), c(20, 30, 15), c(400, 600, 200))),
  rb_gearwidth_proxy = quote(rb_fill_gear_width(lb)),
  rb_std_meshsize = quote(rb_standardise_gear_meshsize(lb)),
  rb_mmsi_category = quote(rb_classify_mmsi(c(251123456, 111251123, 970123456, 992511234, 98251123, 2511234))),
  rb_cap_iqr = quote(rb_cap_outliers(v, method = "iqr")),
  rb_cap_miller = quote(rb_cap_outliers(v, method = "miller")),
  rb_cap_winsorize = quote(rb_cap_outliers(v, method = "winsorize")),
  rb_check_crosses_year = quote(rb_detect_crosses_year(t1, t2)),
  dc_create_timestamp = quote(rb_create_timestamp(c("31/12/2022", "01/01/2023"), c("23:50:00", "10:00:00")))
)

test_that("every new name reproduces the output saved before the renames", {
  for (old in names(cases)) {
    expect_equal(eval(cases[[old]]), snap[[old]], info = old)
  }
  expect_equal(tryCatch(rb_check_trip_crosses_year(eflalo), warning = function(w) conditionMessage(w)),
               snap$dc_check_eflalo_crosses_year)
})

test_that("an old name warns once per session, then gives the new name's result", {
  .rb_reset_renamed()
  expect_warning(old <- rb_speed(x$lon, x$lat, x$time_stamp), "rb_calc_speed")
  expect_equal(old, snap$rb_speed)
  expect_no_warning(rb_speed(x$lon, x$lat, x$time_stamp))
  .rb_reset_renamed()
  expect_warning(old <- rb_cap_iqr(v, multiplier = 2), "rb_cap_outliers")
  expect_equal(old, rb_cap_outliers(v, method = "iqr", multiplier = 2))
  .rb_reset_renamed()
  expect_warning(old <- rb_kn2ms(v), "will be removed")
  expect_equal(old, snap$rb_kn2ms)
  .rb_reset_renamed()
})

test_that("every old name is an alias and still works", {
  .rb_reset_renamed()
  for (old in names(cases)) {
    args <- as.list(cases[[old]])[-1]
    nm <- if (is.null(names(args))) rep("", length(args)) else names(args)
    args <- args[!nm %in% c("from", "to", "part", "method")]
    env <- environment()
    res <- suppressWarnings(do.call(old, lapply(args, eval, envir = env)))
    expect_equal(res, snap[[old]], info = old)
  }
  .rb_reset_renamed()
})

test_that("every export follows the naming rule, except names a later phase replaces", {
  verbs <- c("flag", "assign", "share", "find", "cut", "build", "link", "limit", "number", "detect",
             "calc", "encode", "extract", "convert", "bin", "interpolate", "fill", "predict", "classify",
             "lookup", "cap", "standardise", "sum", "summarise", "check", "get", "read", "plot",
             "rasterize", "rayshade", "register", "keep", "drop", "create", "peek", "pad")
  renamed <- ls(getNamespace("ramb"))
  renamed <- renamed[vapply(renamed, function(f) {
    fn <- get(f, envir = getNamespace("ramb"))
    if (!is.function(fn)) return(FALSE)
    b <- deparse(body(fn))
    any(grepl(".rb_renamed(", b, fixed = TRUE)) || any(grepl(".Deprecated(", b, fixed = TRUE)) || any(grepl(".rb_superseded(", b, fixed = TRUE))
  }, logical(1))]
  # replaced in plan 013 phases 2-5; each will become an alias that warns
  later <- c("rb_whack_clean", "rb_whack_sda", "rb_whack_forward", "rb_whack_fwdbwd",
             "rb_whack_sequential_fast", "rb_whack_duplicates", "rb_whacky_speed",
             "rb_whacky_speed_mendo", "rb_whacky_speed_trip", "rb_trip", "rb_trip_jepol",
             "rb_add_tripid", "rb_cap_effort", "rb_grade", "rb_st_remove", "rb_rms_tbl",
             "dc_spread_cash_and_catch")
  exports <- setdiff(getNamespaceExports("ramb"), c(renamed, later))
  rule <- paste0("^rb_(", paste(verbs, collapse = "|"), ")(_|$)")
  expect_equal(exports[!grepl(rule, exports)], character(0))
})
