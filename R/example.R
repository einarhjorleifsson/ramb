# example.R — ramb's data: parquet files in inst/extdata, tracked with Git LFS, read lazily.
#
# All of ramb's data are parquet since 2026.10.16.1 (the maintainer, 2026-10-16): the datasets that were lazy-loaded
# from data/ until then, made into parquet unchanged by data-raw/extdata.R, and the labelled tracks of
# data-raw/labelled_tracks.R. A geometry is DuckDB's own GEOMETRY, written by DuckDB as GeoParquet: DuckDB reads it
# as geometry, and sf where it is needed converts it from WKB.

#' Read an example dataset shipped with ramb
#'
#' ramb's data are parquet files, opened lazily with [duckdbfs::open_dataset()] like any other track; collect one
#' with [dplyr::collect()]. The two with a geometry (`dansk_harbours`, `trawlsurveytows`) hold it in `geometry` as
#' DuckDB's GEOMETRY (GeoParquet, lon/lat on WGS 84), which DuckDB's `ST_` functions work on. Collected into R it is
#' WKB; for sf: `x <- dplyr::collect(rb_read_example("dansk_harbours"))`, then
#' `sf::st_sf(x[names(x) != "geometry"], geometry = sf::st_as_sfc(structure(x$geometry, class = "WKB"), crs = 4326))`.
#'
#' Tracks:
#'
#' * `dansk`: 14 Danish vessels with 4 gears, 395,558 AIS positions, a median 10 s apart. `.rowid`, `vessel_id`,
#'   `time_stamp`, `lon`, `lat`, `speed` (knots), `course`, `gear`, `behaviour`. ICES WKSSFGEO, `example_data_AIS.csv`,
#'   MIT licence.
#' * `dansk_harbours`: 942 harbour polygons for `dansk`: `SI_HARB` (an id) and `geometry`. ICES WKSSFGEO.
#' * `creel`: 5 trips of 5 small-scale creel vessels, positions every 60 s with their behaviour: `id`, `rowid`,
#'   `time`, `x`, `y` (UTM 30N), `lon`, `lat`, `behaviour`. Mendo, Smout, Photopoulou and James (2019), Dryad,
#'   doi:10.5061/dryad.k80bp46, CC0.
#' * `trawlsurvey`, `trawlsurveytows`: positions and tows (lines) of the Icelandic spring bottom-trawl survey.
#'   MFRI.
#' * `whacks1`: a small fictional track for the impossible-position filters ([rb_flag_ping_impossible()]): `.rid`,
#'   `vid`, `tid`, `lon`, `lat`, `time`, and `whacks`, `TRUE` where a speed filter should flag.
#'
#' Labelled tracks (made by `data-raw/labelled_tracks.R`; columns `vid`, `trip`, `time`, `lon`, `lat`, `speed`
#' (knots), `gear`, `label` (the source's, verbatim) and `op` (`"set"` or `"haul"` where the label says so)):
#'
#' * `wkssfgeo2_it_pots`: one Italian pot vessel, pings every 60 s, labelled setting, hauling or not fishing; the
#'   positions and dates are anonymised. ICES WKSSFGEO2, `IT_CNR_2023_GP.csv` (CNR-IRBIM), MIT licence. The source
#'   holds trip 40 twice (its trip 41 is the same trip an hour later); trip 41 is left out.
#' * `wkssfgeo_pt_gps`: two Portuguese pot vessels and three dredgers, GPS trackers, some tracks validated by an
#'   expert. ICES WKSSFGEO, `example_data_GPS.csv`, MIT licence.
#' * `ird_mg_gillnet`: four drive-in gillnet fishers in south-west Madagascar, 2017, pings every 60 s, labelled by
#'   observers (net set, fish driven, net hauled, ...). Behivoke et al., doi:10.23708/IBZJGD, CC BY 4.0.
#'
#' Lookups:
#'
#' * `benthis_parameters`: the gear footprints of 14 towed gear groups (eight otter trawl, three beam trawl, two
#'   demersal seine, one dredge) and their vessel-size to gear-width models: `benthis_metier`, `a`, `b`, `model`
#'   (linear or power), `variable` (vessel length or kW), `subsurface_percentage`, and others; used by
#'   [rb_predict_gear_width()]. Eigaard et al. (2016), ICES Journal of Marine Science 73(suppl. 1): i27-i43,
#'   doi:10.1093/icesjms/fsv099.
#' * `metier5_benthis_lookup`: ICES metier level 5 (`metier5`) to the Benthis metier (`benthis_metier`).
#'
#' Licences and attributions are in the package's `COPYRIGHTS` file.
#'
#' @param name The dataset's name; `NULL` lists the names.
#' @return A lazy DuckDB table, or the names.
#' @export
#' @examples
#' rb_read_example()
#' rb_read_example("wkssfgeo2_it_pots")
rb_read_example <- function(name = NULL) {
  dir <- system.file("extdata", package = "ramb")
  have <- sub("\\.parquet$", "", list.files(dir, pattern = "\\.parquet$"))
  if (is.null(name)) {
    return(have)
  }
  if (!is.character(name) || length(name) != 1 || !name %in% have) {
    stop(
      sprintf(
        "`name` must be one of %s.",
        paste0("\"", have, "\"", collapse = ", ")
      ),
      call. = FALSE
    )
  }
  .rb_read_parquet(file.path(dir, paste0(name, ".parquet")), lazy = TRUE)
}

# One of ramb's parquet files, lazy or collected.
.rb_read_parquet <- function(f, lazy = FALSE) {
  if (.rb_is_lfs_pointer(f)) {
    stop(
      sprintf(
        "%s is a Git LFS pointer, not the data: install ramb from a clone with Git LFS, or from an archive that includes LFS objects.",
        basename(f)
      ),
      call. = FALSE
    )
  }
  x <- duckdbfs::open_dataset(f)
  if (lazy) x else dplyr::collect(x)
}

# A lookup that a function reads, once per session.
.rb_lookups <- new.env(parent = emptyenv())
.rb_extdata_df <- function(name) {
  if (is.null(.rb_lookups[[name]])) {
    f <- system.file("extdata", paste0(name, ".parquet"), package = "ramb")
    .rb_lookups[[name]] <- .rb_read_parquet(f)
  }
  .rb_lookups[[name]]
}

# A Git LFS pointer instead of the data: what a package built from an archive without its LFS objects holds.
.rb_is_lfs_pointer <- function(f) {
  head <- readBin(f, "raw", 23)
  identical(head, charToRaw("version https://git-lfs"))
}
