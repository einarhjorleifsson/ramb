# dansk_relief.R — the seabed and the land around the Danish tracks (dansk), a grid of elevations for maps.
#
# Input:  NOAA's ETOPO 2022 global relief model (15 arc-seconds, bed elevation), read through marmap's
#         getNOAA.bathy() at 30 arc-seconds for the box of the Danish pings (2.5-13 E, 54.5-58.5 N)
# Output: inst/extdata/dansk_relief.parquet: lon, lat (the cell's centre) and elevation (metres, negative below sea
#         level), one row per cell, 604,800 cells
#
# NOAA's image service samples the 15 arc-second model at the size asked for (nearest neighbour), so each cell is one
# of the model's cells, not their mean. marmap labels the cells from the box's west edge to its east edge, 10.5
# degrees over 1,259 steps, while the image's cells are 30 arc-seconds wide and centred half a cell in (checked against
# the service's GeoTIFF read with terra on 2026-11-09: the values agree to 0.013 m, the positions differ by up to half a
# cell); the centres are placed here from the box and the grid's size.
#
# Run from the package root (needs the internet), then data-raw/data_dict.R dansk_relief, which stamps the file with
# its entry of inst/extdata/data-dict.yaml and its sources (inst/REFERENCES.bib).

library(DBI)

box <- c(west = 2.5, east = 13, south = 54.5, north = 58.5) # the Danish pings: 2.73-12.86 E, 54.54-58.36 N
res_min <- 0.5 # minutes of arc

x <- marmap::getNOAA.bathy(
  lon1 = box[["west"]],
  lon2 = box[["east"]],
  lat1 = box[["south"]],
  lat2 = box[["north"]],
  resolution = res_min,
  keep = FALSE
)
nx <- nrow(x) # x[lon, lat], latitude rising
ny <- ncol(x)
step <- res_min / 60
stopifnot(
  nx == round((box[["east"]] - box[["west"]]) / step),
  ny == round((box[["north"]] - box[["south"]]) / step),
  !anyNA(x)
)
relief <- data.frame(
  lon = rep(box[["west"]] + (seq_len(nx) - 0.5) * step, times = ny),
  lat = rep(box[["south"]] + (seq_len(ny) - 0.5) * step, each = nx),
  elevation = as.vector(unclass(x))
)

con <- dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "relief", relief)
dbExecute(
  con,
  "COPY (SELECT round(lon, 6) AS lon, round(lat, 6) AS lat, CAST(elevation AS FLOAT) AS elevation
                      FROM relief ORDER BY lat, lon)
                TO 'inst/extdata/dansk_relief.parquet' (FORMAT parquet, COMPRESSION zstd, COMPRESSION_LEVEL 19)"
)
dbDisconnect(con, shutdown = TRUE)
