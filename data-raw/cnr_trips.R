# cnr_trips.R — the test fixture of rb_create_voyages(method = "cnr"): CNR-IRBIM's own trip finder, run as
# published on its own Italian tracks.
#
# Input:  create_fishing_trip() and the functions it calls, from R-dev/galdelli_pulcinella_tassetti/R/
#         global_functions.R of WKSSFGEO's dev_branch (GNU GPL 3) at the commit of ramb's CNR data; the
#         Mediterranean harbours from the same folder; the tracks as ramb holds them (wkssfgeo_cnr_ais).
# Output: inst/testdata/cnr_italy_trips.csv, one row per trip: vid, T1, T2, from, to (harbour names; the R
#         function's "At_Sea" written as NA).
#
# The R function's code is run from the repository, never copied into ramb. Only the trip functions are taken
# from the file: it also loads a dozen packages and a random-forest model for its gear classes, which the trips do
# not use. No coastal zone, since ramb does not carry CNR's (its join rule is tested on its own in test-trip.R).
# Run from the package root, with ramb installed or loaded. Takes about 5 s.
library(sf)
library(dplyr)
sf::sf_use_s2(FALSE) # as the R function's workflow

wk1_dev <- "efd28a8bb16bdf6b17182474e222ee68aea61eb4" # WKSSFGEO dev_branch, as for wkssfgeo_cnr_*
cnr_dir <- "R-dev/galdelli_pulcinella_tassetti"
repo_file <- function(path) {
  f <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  local <- path.expand("~/stasi/WKSSFGEO")
  if (dir.exists(file.path(local, ".git"))) {
    system2(
      "git",
      c("-C", local, "show", sprintf("%s:%s/%s", wk1_dev, cnr_dir, path)),
      stdout = f
    )
  } else {
    url <- sprintf(
      "https://raw.githubusercontent.com/ices-eg/WKSSFGEO/%s/%s/%s",
      wk1_dev,
      cnr_dir,
      path
    )
    utils::download.file(url, f, mode = "wb", quiet = TRUE)
  }
  stopifnot(file.size(f) > 0)
  f
}

cnr <- new.env()
keep <- c(
  "create_fishing_trip",
  "build_trip",
  "find_inport",
  "closest_port",
  "closest_port_recovery",
  "get_port",
  "incoastal_ban_zone"
)
for (e in parse(repo_file("R/global_functions.R"))) {
  if (
    is.call(e) &&
      as.character(e[[1]]) %in% c("<-", "=") &&
      as.character(e[[2]]) %in% keep
  ) {
    eval(e, cnr)
  }
}
stopifnot(all(keep %in% ls(cnr)))
cnr$wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
cnr$`%ni%` <- Negate("%in%")

# as trip_ita_cnr_ais.R prepares its ports
ports <- readRDS(repo_file("data/med_harb_gsa.rData")) |> rename(GSA = area)
st_crs(ports) <- 4326
port_buf <- suppressWarnings(st_buffer(ports, 0.001))
st_crs(port_buf) <- 4326
no_zone <- ports[0, ]

a <- as.data.frame(collect(ramb::rb_read_example("wkssfgeo_cnr_ais")))
out <- list()
for (v in unique(a$MMSI)) {
  x <- a[a$MMSI == v, ]
  x <- x[order(x$datetime, x$longitude, x$latitude), ]
  x$datetime <- format(x$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  r <- NULL
  invisible(capture.output(
    r <- suppressWarnings(suppressMessages(
      cnr$create_fishing_trip(
        data = x,
        ports = ports,
        ports_buffer = port_buf,
        coastal_ban_zone = no_zone
      )
    ))
  ))
  if (!is.null(r) && nrow(r)) {
    out[[length(out) + 1]] <- data.frame(
      vid = v,
      T1 = format(r$start_timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      T2 = format(r$end_timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      from = unlist(r$departure),
      to = unlist(r$arrival)
    )
  }
}
out <- do.call(rbind, out)
out$from[out$from %in% "At_Sea"] <- NA
out$to[out$to %in% "At_Sea"] <- NA
f <- "inst/testdata/cnr_italy_trips.csv"
writeLines(
  c(
    sprintf(
      "# CNR-IRBIM create_fishing_trip() (WKSSFGEO dev_branch %s, %s, GNU GPL 3),",
      substr(wk1_dev, 1, 7),
      cnr_dir
    ),
    "# run as published on wkssfgeo_cnr_ais with the harbours of wkssfgeo_cnr_med_harbours and no coastal zone;",
    "# At_Sea written as NA. Made by data-raw/cnr_trips.R."
  ),
  f
)
suppressWarnings(utils::write.table(
  out,
  f,
  sep = ",",
  row.names = FALSE,
  append = TRUE,
  na = ""
))
message(nrow(out), " trips written to ", f)
