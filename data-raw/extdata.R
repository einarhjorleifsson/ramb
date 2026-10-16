# extdata.R — ramb's datasets as parquet in inst/extdata (and the test fixture in inst/testdata), tracked with Git LFS.
#
# Until 2026.10.15.1 the datasets were lazy-loaded .rda files in data/, made by data-raw/DATASET.R (the trawl survey
# from MFRI's Oracle, the others from public sources). The maintainer chose parquet for all of ramb's data
# (2026-10-16), so they are read like any track, with rb_read_example(). This converts each .rda as it stood at
# d693c1b, unchanged: same rows in the same order, same columns. A table with an sf geometry keeps it in
# `geometry` as DuckDB's own GEOMETRY type, which DuckDB (1.5 on, no extension needed) writes as GeoParquet and reads
# back as geometry; their CRS is EPSG:4326 in lon/lat order, GeoParquet's default, OGC:CRS84. In R a collected
# geometry is WKB: sf::st_as_sfc(structure(x$geometry, class = "WKB"), crs = 4326). Run from the package root, then
# data-raw/data_dict.R, which stamps each file with its entry of inst/extdata/data-dict.yaml and its sources.

library(DBI)

ref <- "d693c1b"
from_git <- function(path) {
  f <- tempfile(fileext = paste0(".", tools::file_ext(path)))
  system2("git", c("show", sprintf("%s:%s", ref, path)), stdout = f)
  f
}

con <- dbConnect(duckdb::duckdb())
write_parquet <- function(x, out, geo = FALSE) {
  duckdb::duckdb_register(con, "x", x)
  sel <- if (geo) "* REPLACE (ST_GeomFromWKB(geometry) AS geometry)" else "*"
  dbExecute(
    con,
    sprintf(
      "COPY (SELECT %s FROM x) TO '%s' (FORMAT parquet, COMPRESSION zstd, COMPRESSION_LEVEL 19)",
      sel,
      out
    )
  )
  duckdb::duckdb_unregister(con, "x")
}

dir.create("inst/extdata", showWarnings = FALSE)
for (nm in c(
  "dansk",
  "dansk_harbours",
  "creel",
  "trawlsurvey",
  "trawlsurveytows",
  "whacks1",
  "benthis_parameters",
  "metier5_benthis_lookup"
)) {
  e <- new.env()
  load(from_git(sprintf("data/%s.rda", nm)), envir = e)
  x <- e[[nm]]
  if (nm == "benthis_parameters") {
    # The uncertainty of the gear-width models, which the table compiled for ramb left out: the standard errors of a and
    # b and the number of observations, as printed in Eigaard et al. (2016), Table 4 (doi:10.1093/icesjms/fsv099).
    # Its a, b and model match this table's for all 14 groups (checked 2026-10-16).
    t4 <- utils::read.table(
      header = TRUE,
      text = "
      benthis_metier se_a se_b n_obs
      OT_CRU 1.8153 0.0598 124
      OT_DMF 3.9823 0.0676 39
      OT_MIX 6.6939 0.1044 94
      OT_MIX_CRU 10.6718 0.0450 271
      OT_MIX_DMF_BEN 1.6785 40.9298 48
      OT_MIX_DMF_PEL 2.6909 0.1261 190
      OT_MIX_CRU_DMF 0.9284 21.0229 53
      OT_SPF 0.2052 7.4518 19
      TBB_CRU 0.2784 0.0347 7
      TBB_DMF 0.1729 0.0389 42
      TBB_MOL 0.3157 0.1384 22
      DRB_MOL 0.1100 0.1061 33
      SDN_DMF 637.2515 0.0637 47
      SSC_DMF 1665.5023 0.1188 8"
    )
    stopifnot(setequal(t4$benthis_metier, x$benthis_metier))
    x <- dplyr::left_join(x, t4, by = "benthis_metier")
  }
  geo <- inherits(x, "sf")
  if (geo) {
    stopifnot(identical(sf::st_crs(x)$epsg, 4326L))
    g <- sf::st_geometry(x)
    x <- sf::st_drop_geometry(x)
    x$geometry <- blob::as_blob(unclass(sf::st_as_binary(g)))
  }
  write_parquet(as.data.frame(x), sprintf("inst/extdata/%s.parquet", nm), geo)
}

# the fixture of the jepol trip cutter's pre-recode output (test-trip.R), one column per list element
dir.create("inst/testdata", showWarnings = FALSE)
pj <- readRDS(from_git("tests/testthat/fixtures/pre_jepol.rds"))
write_parquet(as.data.frame(pj), "inst/testdata/pre_jepol.parquet")
dbDisconnect(con, shutdown = TRUE)
