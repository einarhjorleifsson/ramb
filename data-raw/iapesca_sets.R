# iapesca_sets.R — the test fixture of rb_link_record_sets(): Ifremer's iapesca, run as published on labelled tracks.
#
# Input:  Retrieve_SettingOperations() of the iapesca package (J. Rodriguez, Ifremer, CC BY 4.0), version 1.3,
#         installed from https://gitlab.ifremer.fr/iapesca/r-packages_iapesca at commit 1882892; two labelled pot
#         tracks: iapesca's own example vessel (iapesca::positions, 8 trips, a ping every 15 min) and the Italian pot
#         vessel ramb holds (wkssfgeo2_it_pots, a ping a minute), each projected to its UTM zone.
# Output: inst/testdata/iapesca_sets.csv, one row per haul with a line and variant: the set iapesca finds, its soak
#         and speed.
#
# A haul is a run of pings labelled hauling, from its first ping to a second past its last; its net is the line
# through its pings, as rb_link_record_sets(method = "line") draws it, so the two are compared on the set finder
# alone (iapesca draws its own nets from labelled pings with Create_Nets(), which ramb does not). Variants: the
# defaults with the setting labels (`labels`), without them (`no_labels`), and the first search alone
# (`first_search`, Consolidate = FALSE). iapesca's code and data are not copied into ramb: its functions are called
# from the installed package. Run from the package root. Takes about 20 s.
library(sf)
stopifnot(
  requireNamespace("iapesca", quietly = TRUE),
  identical(
    packageDescription("iapesca")$RemoteSha,
    "1882892c74f950155222367b00043311f684f4e0"
  )
)

# the pings, one per time, and the hauls: runs labelled haul (the same in tests/testthat/test-record.R)
labelled <- function(p) {
  p <- p[order(p$vid, p$time, p$lon, p$lat), ]
  p <- p[!duplicated(p[c("vid", "time")]), ]
  p$op[is.na(p$op)] <- "other"
  r <- cumsum(c(TRUE, p$op[-1] != p$op[-nrow(p)] | p$vid[-1] != p$vid[-nrow(p)]))
  i <- which(p$op == "haul")
  h <- do.call(rbind, lapply(split(i, r[i]), function(j) {
    data.frame(
      vid = p$vid[j[1]],
      trip = p$trip[j[1]],
      start = min(p$time[j]),
      end = max(p$time[j]) + 1,
      n = length(j)
    )
  }))
  h <- h[order(h$vid, h$start), ]
  h$id <- seq_len(nrow(h))
  rownames(h) <- NULL
  list(pings = p, hauls = h)
}

ia_sets <- function(d, crs, labels, consolidate) {
  p <- d$pings
  h <- d$hauls[d$hauls$n >= 2, ]
  fop <- ifelse(
    p$op == "haul",
    "Hauling",
    ifelse(labels & p$op == "set", "Setting", "NotFishing")
  )
  x <- st_transform(
    st_as_sf(
      data.frame(
        VESSEL_FK = p$vid,
        FISHING_TRIP_FK = as.character(p$trip),
        DATE_TIME = format(p$time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        FishingOperation = fop,
        lon = p$lon,
        lat = p$lat
      ),
      coords = c("lon", "lat"),
      crs = 4326
    ),
    crs
  )
  k <- ave(seq_len(nrow(h)), h$trip, FUN = seq_along)
  lines <- lapply(seq_len(nrow(h)), function(j) {
    w <- p$time >= h$start[j] & p$time < h$end[j]
    st_linestring(cbind(p$lon[w], p$lat[w]))
  })
  nets <- st_transform(
    st_sf(
      Net = paste0(h$trip, "_", sprintf("%02d", k)),
      FISHING_TRIP_FK = as.character(h$trip),
      hauling.start = format(h$start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      hauling.end = format(h$end, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      geometry = st_sfc(lines, crs = 4326)
    ),
    crs
  )
  nets$length <- st_length(nets)
  r <- suppressWarnings(suppressMessages(iapesca::Retrieve_SettingOperations(
    x,
    list(nets),
    Setting.category = "Setting",
    Consolidate = consolidate
  )))$nets
  tm <- function(s) {
    out <- rep(NA_character_, length(s))
    ok <- !is.na(s)
    out[ok] <- format(iapesca:::Char2Time(s[ok]), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    out
  }
  data.frame(
    id = h$id[match(r$Net, nets$Net)],
    set_start = tm(r$setting.start),
    set_end = tm(r$setting.end),
    soak_h = as.numeric(r$soaking.time.hours),
    set_speed = as.numeric(r$setting.speed)
  )
}

ip <- iapesca::positions
tracks <- list(
  iapesca = list(
    d = labelled(data.frame(
      vid = ip$VESSEL_FK,
      trip = ip$FISHING_TRIP_FK,
      time = as.POSIXct(ip$DATE_TIME, tz = "UTC"),
      lon = ip$LONGITUDE,
      lat = ip$LATITUDE,
      op = c(Setting = "set", Hauling = "haul", NotFishing = NA)[as.character(
        ip$FishingOperation
      )]
    )),
    crs = 32628
  ),
  it_pots = list(
    d = labelled(as.data.frame(dplyr::collect(
      ramb::rb_read_example("wkssfgeo2_it_pots")
    ))[c("vid", "trip", "time", "lon", "lat", "op")]),
    crs = 32638
  )
)
variants <- list(
  labels = list(labels = TRUE, consolidate = TRUE),
  no_labels = list(labels = FALSE, consolidate = TRUE),
  first_search = list(labels = TRUE, consolidate = FALSE)
)
out <- list()
for (ds in names(tracks)) {
  for (v in names(variants)) {
    s <- ia_sets(
      tracks[[ds]]$d,
      tracks[[ds]]$crs,
      variants[[v]]$labels,
      variants[[v]]$consolidate
    )
    out[[length(out) + 1]] <- data.frame(ds = ds, variant = v, s)
  }
}
out <- do.call(rbind, out)
out <- out[order(out$ds, out$variant, out$id), ]
f <- "inst/testdata/iapesca_sets.csv"
writeLines(
  c(
    "# iapesca::Retrieve_SettingOperations() (https://gitlab.ifremer.fr/iapesca/r-packages_iapesca, 1.3, 1882892,",
    "# CC BY 4.0), run as published on iapesca::positions and wkssfgeo2_it_pots in their UTM zones, with nets drawn",
    "# through each labelled haul's pings. Made by data-raw/iapesca_sets.R."
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
message(nrow(out), " rows written to ", f)
