# labelled_tracks.R — the labelled vessel tracks in inst/extdata, read with rb_read_example().
#
# Three public datasets whose pings carry an observer's or an expert's label of what the vessel was doing, kept
# for testing and measuring ramb's step functions (first: rb_link_record_sets() on set and haul labels). Each is
# read from its source at a fixed version and written as it is, in the same columns:
#
#   vid     the vessel, as the source names it
#   trip    the source's trip number, where it has one
#   time    the time as the source gives it, stored as UTC (the sources do not state a zone)
#   lon     lat
#   speed   knots, as the source gives it (checked against the track: the IT file's first leg is 184 m in 60 s,
#           5.95 kn, against SPEED 5.94); the IRD file gives distance per minute, turned into knots
#   gear    the source's gear code
#   label   the source's label, verbatim
#   op      "set" or "haul" where the label says so, else NA: the set and haul labels harmonised
#
# The files are tracked with Git LFS. Licences in inst/COPYRIGHTS. Run from the package root.

library(DBI)

# A local copy is used when it is there, else the source itself.
src <- function(url, local) if (file.exists(local)) local else url

it_csv <- src(
  "https://raw.githubusercontent.com/ices-eg/WKSSFGEO2/f3e49fe9956b4597e4e8d83f4fe67d4b3b7eaf85/data_examples/IT_CNR_2023_GP.csv",
  path.expand("~/stasi/WKSSFGEO2/data_examples/IT_CNR_2023_GP.csv")
)
pt_csv <- src(
  "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/3da870dc1660c41351455f0ea01c24a7a6d15fb1/data-examples/example_data_GPS.csv",
  path.expand("~/stasi/WKSSFGEO/data-examples/example_data_GPS.csv")
)
ird_url <- "https://dataverse.ird.fr/api/access/datafile/4827" # doi:10.23708/IBZJGD, Supplementary material2
ird_md5 <- "d732ab135b3a841f97cb92ceada7b089"

utc <- function(x) as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = "UTC")

## WKSSFGEO2: one Italian pot vessel, 60 s -----------------------------------
it <- read.csv(it_csv)
# trip 41 is trip 40 an hour later: 370 of its 368 times match trip 40's plus 1 h at the same position, 363 with the
# same label. One trip entered twice, so the second copy is left out.
it <- it[it$TRIP_ID != 41, ]
it <- data.frame(
  vid = it$BOAT_ID,
  trip = as.integer(it$TRIP_ID),
  time = as.POSIXct(it$DATE_TIME, tz = "UTC"),
  lon = it$LONGITUDE,
  lat = it$LATITUDE,
  speed = it$SPEED,
  gear = it$GEAR,
  label = it$STATUS,
  op = c(setting = "set", hauling = "haul")[it$STATUS]
)

## WKSSFGEO: Portuguese pot and dredge vessels, GPS trackers -------------------
pt <- read.csv(pt_csv)
pt <- data.frame(
  vid = pt$vessel_id,
  trip = NA_integer_,
  time = as.POSIXct(pt$time, tz = "UTC"),
  lon = pt$lon,
  lat = pt$lat,
  speed = pt$speed,
  gear = pt$gear,
  label = pt$fish.val.full,
  op = c(set = "set", hauling = "haul")[pt$fish.val.full]
)

## IRD: four Malagasy drive-in gillnet fishers, 60 s ---------------------------
f <- tempfile(fileext = ".Rdata")
local_ird <- Sys.getenv("RAMB_IRD_RDATA")
if (nzchar(local_ird)) {
  file.copy(local_ird, f)
} else {
  utils::download.file(ird_url, f, mode = "wb")
}
stopifnot(unname(tools::md5sum(f)) == ird_md5)
e <- new.env()
load(f, envir = e)
g <- e$gillnet_dataset
class(g) <- "data.frame"
g <- data.frame(
  vid = g$pecheur_id,
  trip = as.integer(g$no_trajet),
  time = utc(g$date_heure),
  lon = g$longitude,
  lat = g$latitude,
  speed = g$distance / 60 * 3600 / 1852, # every duree is 00:01:00
  gear = g$code_engin,
  label = g$etat_detaille,
  op = c(
    Pose_filet = "set",
    Remonter_filet = "haul",
    Remonter_filet_collecte = "haul"
  )[g$etat_detaille]
)
stopifnot(all(e$gillnet_dataset$duree == "00:01:00"))

## Write -----------------------------------------------------------------------
con <- dbConnect(duckdb::duckdb())
dir.create("inst/extdata", showWarnings = FALSE)
for (nm in c("wkssfgeo2_it_pots", "wkssfgeo_pt_gps", "ird_mg_gillnet")) {
  x <- switch(
    nm,
    wkssfgeo2_it_pots = it,
    wkssfgeo_pt_gps = pt,
    ird_mg_gillnet = g
  )
  x$op <- unname(x$op)
  x <- x[order(x$vid, x$time, x$lon, x$lat), ]
  duckdb::duckdb_register(con, "x", x)
  dbExecute(
    con,
    sprintf(
      "COPY (SELECT * FROM x) TO 'inst/extdata/%s.parquet' (FORMAT parquet, COMPRESSION zstd)",
      nm
    )
  )
  duckdb::duckdb_unregister(con, "x")
}
dbDisconnect(con, shutdown = TRUE)
