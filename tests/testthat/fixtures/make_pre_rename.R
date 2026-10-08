# Snapshot of the outputs of every function renamed in plan 013 phase 1a, taken with the ramb
# installed BEFORE the renames (ea77b7c). tests/testthat/test-renamed.R checks the new names, and the
# aliases, against it. Re-running this after the renames would defeat the point; it is kept for provenance.
#   Rscript tests/testthat/fixtures/make_pre_rename.R   (from the package root, old ramb installed)
library(ramb)
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
snap <- list(
  rb_speed = rb_speed(x$lon, x$lat, x$time_stamp),
  rb_distance = rb_distance(x$lon[-1], x$lat[-1], x$lon[-300], x$lat[-300]),
  rb_st = rb_st(x$time_stamp),
  rb_sa = rb_sa(x$lon, x$lat, x$time_stamp),
  rb_track_time = rb_track_time(x$time_stamp),
  rb_ms2kn = rb_ms2kn(v), rb_kn2ms = rb_kn2ms(v),
  rb_event = rb_event(x$speed > 3),
  rb_d2ir = rb_d2ir(x$lon, x$lat),
  rb_midpoint = rb_midpoint(x$lon, 0.05),
  rb_points_in_polygons = rb_points_in_polygons(pts, hb),
  rb_st_keep = rb_st_keep(pts, hb), rb_st_drop = rb_st_drop(pts, hb),
  rb_gear_from_metier = rb_gear_from_metier(met), rb_target_from_metier = rb_target_from_metier(met),
  rb_met5_from6 = rb_met5_from6(met),
  rb_benthis_width = rb_benthis_width(c("OT_DMF", "TBB_DMF", "DRB_MOL"), c(20, 30, 15), c(400, 600, 200)),
  rb_gearwidth_proxy = rb_gearwidth_proxy(lb), rb_std_meshsize = rb_std_meshsize(lb),
  rb_mmsi_category = rb_mmsi_category(c(251123456, 111251123, 970123456, 992511234, 98251123, 2511234)),
  rb_cap_iqr = rb_cap_iqr(v), rb_cap_miller = rb_cap_miller(v), rb_cap_winsorize = rb_cap_winsorize(v),
  rb_check_crosses_year = rb_check_crosses_year(t1, t2),
  dc_check_eflalo_crosses_year = tryCatch(dc_check_eflalo_crosses_year(eflalo), warning = function(w) conditionMessage(w)),
  dc_create_timestamp = dc_create_timestamp(c("31/12/2022", "01/01/2023"), c("23:50:00", "10:00:00"))
)
saveRDS(snap, "tests/testthat/fixtures/pre_rename.rds")
cat(length(snap), "snapshots written\n")
