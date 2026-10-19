# wkssfgeo_examples.R — the rest of the ICES WKSSFGEO and WKSSFGEO2 example data, as parquet in inst/extdata.
#
# The workshops' example data that ramb did not yet ship (the maintainer, 2026-10-19): tracks with the provider's own
# trips, which test a trip cutter against trips someone else drew, and the trips that two published methods cut from
# the Danish tracks (`dansk`). Each file is read from its source at a fixed commit and written with as few changes as
# possible; every change is named in the file's entry of inst/extdata/data-dict.yaml. Licences in inst/COPYRIGHTS:
# the WKSSFGEO and WKSSFGEO2 repositories are MIT, except the CNR folder of WKSSFGEO's dev_branch, which carries its
# own licence, the GNU GPL version 3.
#
#   wkssfgeo_adriatic_passive       one Adriatic vessel with passive gear, AIS, with the provider's trips
#   wkssfgeo_denmark                Denmark's land, one polygon: a coastline for the Danish tracks
#   wkssfgeo_cnr_ais                CNR's test subset of Adriatic AIS, six vessels
#   wkssfgeo_cnr_dansk_trips        the Danish tracks cut into trips by CNR's method and by jepol, one row per trip
#   wkssfgeo_cnr_dansk_ping_trips   the same, one row per ping: the trip each method put it in
#   wkssfgeo_cnr_med_harbours       CNR's Mediterranean harbours, as polygons
#   wkssfgeo2_irl_pots              an Irish pot vessel, labelled, with its trips
#   wkssfgeo2_irl_gillnets          four Irish gillnet vessels (inshore VMS), with their trips
#   wkssfgeo2_pt_gps                Portuguese pot and dredge vessels, GPS, labelled, with their trips
#
# The tracks of the WKSSFGEO2 data format (IRL, PT) take the columns of data-raw/labelled_tracks.R (vid, trip, time,
# lon, lat, speed, gear, label, op); the AIS files keep their own. Times are as the source gives them, stored as UTC
# (no source states a zone). Shapes are DuckDB GEOMETRY on WGS 84 (GeoParquet), as in data-raw/extdata.R.
#
# Run from the package root, then data-raw/data_dict.R, then devtools::document().

library(DBI)
sf::sf_use_s2(FALSE)

# A file of an ices-eg repository at a commit: from a local clone where there is one, else from GitHub.
repo_file <- function(repo, sha, path) {
  f <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  local <- path.expand(file.path("~/stasi", repo))
  if (dir.exists(file.path(local, ".git"))) {
    system2(
      "git",
      c("-C", local, "show", sprintf("%s:%s", sha, path)),
      stdout = f
    )
  } else {
    utils::download.file(
      sprintf(
        "https://raw.githubusercontent.com/ices-eg/%s/%s/%s",
        repo,
        sha,
        path
      ),
      f,
      mode = "wb",
      quiet = TRUE
    )
  }
  stopifnot(file.size(f) > 0)
  f
}
wk1 <- "3da870dc1660c41351455f0ea01c24a7a6d15fb1" # WKSSFGEO main, as for dansk and wkssfgeo_pt_gps
wk1_dev <- "efd28a8bb16bdf6b17182474e222ee68aea61eb4" # WKSSFGEO dev_branch, the last commit of the CNR data folder
wk2 <- "f3e49fe9956b4597e4e8d83f4fe67d4b3b7eaf85" # WKSSFGEO2 main, as for wkssfgeo2_it_pots
cnr <- function(path) {
  repo_file(
    "WKSSFGEO",
    wk1_dev,
    file.path("R-dev/galdelli_pulcinella_tassetti/data", path)
  )
}

utc <- function(x) as.POSIXct(x, tz = "UTC")

# The speed column's unit, checked against the track: the median of reported speed over the speed from distance and
# time, on legs 10 s to 15 min long with both above 1 knot. About 1 means knots.
speed_ratio <- function(vid, time, lon, lat, speed) {
  o <- order(vid, time)
  vid <- vid[o]
  time <- time[o]
  lon <- lon[o]
  lat <- lat[o]
  speed <- speed[o]
  n <- length(vid)
  same <- vid[-1] == vid[-n]
  dt <- as.numeric(difftime(time[-1], time[-n], units = "secs"))
  r <- pi / 180 # haversine, on ramb's sphere
  a <- sin((lat[-1] - lat[-n]) * r / 2)^2 +
    cos(lat[-n] * r) * cos(lat[-1] * r) * sin((lon[-1] - lon[-n]) * r / 2)^2
  d <- 2 * 6371000 * asin(sqrt(a))
  kn <- d / dt * 3600 / 1852
  ok <- same & dt >= 10 & dt <= 900 & kn > 1 & !is.na(speed[-1]) & speed[-1] > 1
  stats::median(speed[-1][ok] / kn[ok])
}

## WKSSFGEO: one Adriatic vessel with passive gear, AIS ------------------------
ad <- read.csv(repo_file(
  "WKSSFGEO",
  wk1,
  "data-examples/passive_vessel_ais_adriatic_2017.csv"
))
ad$time_stamp <- utc(ad$time_stamp)
stopifnot(!anyDuplicated(ad[c("vessel_id", "time_stamp")]))
message(
  "adriatic speed/track ",
  round(speed_ratio(ad$vessel_id, ad$time_stamp, ad$lon, ad$lat, ad$speed), 2)
)

## WKSSFGEO: Denmark's land ----------------------------------------------------
dn <- readRDS(repo_file("WKSSFGEO", wk1, "data/Denmark.rds"))
stopifnot(sf::st_crs(dn)$epsg == 32632L)
dn <- sf::st_transform(dn, 4326)

## WKSSFGEO dev_branch, CNR: test AIS, trips on the Danish tracks, harbours -----
ca <- read.csv(cnr("datatest.csv"))
ca$datetime <- utc(ca$datetime)
stopifnot(!anyDuplicated(ca[c("MMSI", "datetime")]))
message(
  "cnr ais speed/track ",
  round(
    speed_ratio(ca$MMSI, ca$datetime, ca$longitude, ca$latitude, ca$speed),
    2
  )
)

gt <- readRDS(cnr("galdelli_trips.rData"))
jt <- as.data.frame(readRDS(cnr("jepol_trips.rData")))
trips <- rbind(
  data.frame(
    method = "galdelli",
    vessel_id = as.character(gt$MMSI),
    trip_id = paste0(gt$MMSI, "_", gt$trip),
    T1 = gt$start_timestamp,
    T2 = gt$end_timestamp,
    port_from = as.integer(as.character(gt$departure)),
    port_to = as.integer(as.character(gt$arrival))
  ),
  data.frame(
    method = "jepol",
    vessel_id = jt$vessel_id,
    trip_id = jt$trip_id,
    T1 = jt$depart,
    T2 = jt$return,
    port_from = NA_integer_,
    port_to = NA_integer_
  )
)
stopifnot(
  !anyDuplicated(trips[c("method", "trip_id")]),
  all(trips$T2 >= trips$T1)
)

gp <- readRDS(cnr("galdelli_pp_trips.rData"))
jp <- as.data.frame(readRDS(cnr("jepol_pp_trip.rData")))
ping_trips <- unique(rbind(
  data.frame(
    method = "galdelli",
    vessel_id = as.character(gp$MMSI),
    time_stamp = gp$datetime,
    trip_id = paste0(gp$MMSI, "_", gp$trip)
  ),
  data.frame(
    method = "jepol",
    vessel_id = jp$vessel_id,
    time_stamp = jp$time_stamp,
    trip_id = jp$trip_id
  )
))
# CNR's per-ping table has one trip its trip table lacks (trip 62, 5,165 pings); it is kept, as the source has it
orphan <- setdiff(ping_trips$trip_id, trips$trip_id)
o <- ping_trips[ping_trips$trip_id %in% orphan, ]
message(
  "per-ping trips with no trip row: ",
  paste(orphan, collapse = ", "),
  " (",
  nrow(o),
  " pings, ",
  format(min(o$time_stamp)),
  " to ",
  format(max(o$time_stamp)),
  ")"
)
stopifnot(length(orphan) == 1, all(o$method == "galdelli"))
# one trip per ping and method (a ping repeated in the source keeps one row)
stopifnot(!anyDuplicated(ping_trips[c("method", "vessel_id", "time_stamp")]))

# CNR's two harbour lists: `med_harb_gsa`, 1,415 Mediterranean harbours, and `ices_med_harbours`, the same 1,415 plus
# the 942 harbours of `dansk_harbours`, numbered by their SI_HARB in harbour, Country and area. That number is the
# port of CNR's Danish trips. So only med_harb_gsa is kept; the check below is what allows it.
hb <- readRDS(cnr("med_harb_gsa.rData"))
ih <- readRDS(cnr("ices_med_harbours.rData"))
stopifnot(
  identical(names(hb), c("harbour", "Country", "geometry", "SI_HARB", "area")),
  identical(names(ih), names(hb))
)
num <- !is.na(suppressWarnings(as.integer(ih$harbour)))
dh <- duckdbfs::open_dataset("inst/extdata/dansk_harbours.parquet") |>
  dplyr::collect()
dh <- sf::st_sf(
  SI_HARB = dh$SI_HARB,
  geometry = sf::st_as_sfc(structure(dh$geometry, class = "WKB"), crs = 4326)
)
m <- match(as.integer(ih$harbour[num]), dh$SI_HARB)
stopifnot(
  sum(num) == nrow(dh),
  !anyNA(m),
  all(ih$harbour[num] == ih$Country[num] & ih$harbour[num] == ih$area[num]),
  all(
    lengths(sf::st_equals(
      sf::st_geometry(ih)[num],
      sf::st_geometry(dh)[m],
      sparse = TRUE
    )) >
      0
  ),
  identical(
    sort(paste(
      ih$harbour,
      ih$Country,
      ih$area,
      sf::st_as_text(sf::st_geometry(ih))
    )[!num]),
    sort(paste(
      hb$harbour,
      hb$Country,
      hb$area,
      sf::st_as_text(sf::st_geometry(hb))
    ))
  )
)
hb <- hb[c("harbour", "Country", "SI_HARB", "area")]
stopifnot(sf::st_crs(hb)$epsg == 4326L, all(sf::st_is_valid(hb)))

## WKSSFGEO2: Irish pots and gillnets, Portuguese pots and dredges --------------
labelled <- function(vid, trip, time, lon, lat, speed, gear, label) {
  data.frame(
    vid = as.character(vid),
    trip = trip,
    time = utc(time),
    lon = lon,
    lat = lat,
    speed = speed,
    gear = gear,
    label = as.character(label), # all NA in IRL_KY22, which read.csv reads as logical
    op = NA_character_ # no label of these files names a set or a haul
  )
}
f1 <- read.csv(repo_file(
  "WKSSFGEO2",
  wk2,
  "data_examples/IRL_FPO_WKSSFGEO2_Anon.csv"
))
irl_pots <- with(
  f1,
  labelled(
    BoatID,
    TripID,
    Time,
    Longitude,
    Latitude,
    Speed,
    Gear,
    FishingOperation
  )
)
f2 <- read.csv(repo_file(
  "WKSSFGEO2",
  wk2,
  "data_examples/IRL_KY22_WKSSFGEO2_Anon.csv"
))
# the trip ids end in a space ("VE_000034 "), which is dropped
irl_gillnets <- with(
  f2,
  labelled(
    BoatID,
    trimws(TripID),
    Time,
    Longitude,
    Latitude,
    Speed,
    Gear,
    FishingOperation
  )
)
f3 <- read.csv(repo_file("WKSSFGEO2", wk2, "data_examples/PT_IPM_2017_GP2.csv"))
pt <- with(
  f3,
  labelled(
    BOAT_ID,
    as.integer(TRIP_ID),
    DATE_TIME,
    LONGITUDE,
    LATITUDE,
    SPEED,
    GEAR,
    STATUS
  )
)
# 4 times of one vessel appear twice in the source, a few metres apart; both rows are kept
for (nm in c("irl_pots", "irl_gillnets", "pt")) {
  x <- get(nm)
  message(
    nm,
    " speed/track ",
    round(speed_ratio(x$vid, x$time, x$lon, x$lat, x$speed), 2),
    ", repeated (vid, time): ",
    sum(duplicated(x[c("vid", "time")]))
  )
}

## Write -----------------------------------------------------------------------
con <- dbConnect(duckdb::duckdb())
dbExecute(con, "LOAD spatial")
write_parquet <- function(x, name, order_by, geo = FALSE) {
  if (geo) {
    g <- sf::st_geometry(x)
    x <- sf::st_drop_geometry(x)
    x$geometry <- blob::as_blob(unclass(sf::st_as_binary(g)))
  }
  duckdb::duckdb_register(con, "x", as.data.frame(x))
  sel <- if (geo) "* REPLACE (ST_GeomFromWKB(geometry) AS geometry)" else "*"
  dbExecute(
    con,
    sprintf(
      "COPY (SELECT %s FROM x ORDER BY %s) TO 'inst/extdata/%s.parquet' (FORMAT parquet, COMPRESSION zstd, COMPRESSION_LEVEL 19)",
      sel,
      order_by,
      name
    )
  )
  duckdb::duckdb_unregister(con, "x")
}
write_parquet(ad, "wkssfgeo_adriatic_passive", "vessel_id, time_stamp")
write_parquet(dn, "wkssfgeo_denmark", "OBJECTID", geo = TRUE)
write_parquet(ca, "wkssfgeo_cnr_ais", "MMSI, datetime")
write_parquet(trips, "wkssfgeo_cnr_dansk_trips", "method, vessel_id, T1")
write_parquet(
  ping_trips,
  "wkssfgeo_cnr_dansk_ping_trips",
  "method, vessel_id, time_stamp"
)
write_parquet(
  hb,
  "wkssfgeo_cnr_med_harbours",
  "Country, harbour, area",
  geo = TRUE
)
write_parquet(irl_pots, "wkssfgeo2_irl_pots", "vid, time, lon, lat")
write_parquet(irl_gillnets, "wkssfgeo2_irl_gillnets", "vid, time, lon, lat")
write_parquet(pt, "wkssfgeo2_pt_gps", "vid, time, lon, lat")
dbDisconnect(con, shutdown = TRUE)
