# iapesca_trips.R — the test fixture of rb_create_voyages(method = "iapesca"): Ifremer's iapesca, run as published
# on CNR-IRBIM's Italian tracks.
#
# Input:  Detect_FishingTrips() of the iapesca package (J. Rodriguez, Ifremer, CC BY 4.0), version 1.3, installed
#         from https://gitlab.ifremer.fr/iapesca/r-packages_iapesca at commit 1882892; the tracks and harbours as ramb
#         holds them (wkssfgeo_cnr_ais, wkssfgeo_cnr_med_harbours), projected to UTM zone 33N (EPSG:32633).
# Output: inst/testdata/iapesca_italy_trips.csv, one row per trip: vid, T1 and T2 (its first and last position),
#         n_pings (its positions: those at sea, and those in port during a stop of an hour or less).
#
# iapesca's code and data are not copied into ramb: its function is called from the installed package, on ramb's
# own data. Run from the package root, with ramb loaded. Takes about 2 s.
library(sf)
stopifnot(
  requireNamespace("iapesca", quietly = TRUE),
  identical(
    packageDescription("iapesca")$RemoteSha,
    "1882892c74f950155222367b00043311f684f4e0"
  )
)

crs <- 32633
h <- dplyr::collect(ramb::rb_read_example("wkssfgeo_cnr_med_harbours"))
harbours <- st_transform(
  st_as_sfc(structure(h$geometry, class = "WKB"), crs = 4326),
  crs
)
a <- as.data.frame(dplyr::collect(ramb::rb_read_example("wkssfgeo_cnr_ais")))

out <- list()
for (v in unique(a$MMSI)) {
  d <- a[a$MMSI == v, ]
  d <- d[order(d$datetime, d$longitude, d$latitude), ]
  x <- st_transform(
    st_as_sf(
      data.frame(
        DATE_TIME = format(d$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        lon = d$longitude,
        lat = d$latitude
      ),
      coords = c("lon", "lat"),
      crs = 4326
    ),
    crs
  )
  r <- suppressWarnings(suppressMessages(
    iapesca::Detect_FishingTrips(
      x,
      Harbours = st_sf(geometry = harbours),
      DetectCoastLine = FALSE
    )
  ))
  tp <- st_drop_geometry(r$trip.path)
  if (!nrow(tp)) {
    next
  }
  s <- split(tp$DATE_TIME, tp$FISHING_TRIP_FK)
  out[[length(out) + 1]] <- data.frame(
    vid = v,
    T1 = vapply(s, min, ""),
    T2 = vapply(s, max, ""),
    n_pings = lengths(s)
  )
}
out <- do.call(rbind, out)
out <- out[order(out$vid, out$T1), ]
f <- "inst/testdata/iapesca_italy_trips.csv"
writeLines(
  c(
    "# iapesca::Detect_FishingTrips() (https://gitlab.ifremer.fr/iapesca/r-packages_iapesca, 1.3, 1882892, CC BY 4.0),",
    "# run as published on wkssfgeo_cnr_ais with the harbours of wkssfgeo_cnr_med_harbours, both in EPSG:32633;",
    "# T1, T2: a trip's first and last position; n_pings: its positions. Made by data-raw/iapesca_trips.R."
  ),
  f
)
suppressWarnings(utils::write.table(
  out,
  f,
  sep = ",",
  row.names = FALSE,
  append = TRUE
))
message(nrow(out), " trips written to ", f)
