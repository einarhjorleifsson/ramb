# example.R — ramb's example and test data: parquet files in inst/extdata, tracked with Git LFS, read lazily.

#' Read an example dataset shipped with ramb
#'
#' ramb's example data are parquet files, opened lazily with [duckdbfs::open_dataset()] like any other track.
#'
#' Labelled tracks (made by `data-raw/labelled_tracks.R`; columns `vid`, `trip`, `time`, `lon`, `lat`, `speed`
#' (knots), `gear`, `label` (the source's, verbatim) and `op` (`"set"` or `"haul"` where the label says so)):
#'
#' * `wkssfgeo2_it_pots`: one Italian pot vessel, pings every 60 s, labelled setting, hauling or not fishing; the
#'   positions and dates are anonymised. ICES WKSSFGEO2, `IT_CNR_2023_GP.csv` (CNR-IRBIM), MIT licence.
#' * `wkssfgeo_pt_gps`: two Portuguese pot vessels and three dredgers, GPS trackers, some tracks validated by an
#'   expert. ICES WKSSFGEO, `example_data_GPS.csv`, MIT licence.
#' * `ird_mg_gillnet`: four drive-in gillnet fishers in south-west Madagascar, 2017, pings every 60 s, labelled by
#'   observers (net set, fish driven, net hauled, ...). Behivoke et al., doi:10.23708/IBZJGD, CC BY 4.0.
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
  f <- file.path(dir, paste0(name, ".parquet"))
  if (.rb_is_lfs_pointer(f)) {
    stop(
      sprintf(
        "%s is a Git LFS pointer, not the data: install ramb from a clone with Git LFS, or from an archive that includes LFS objects.",
        basename(f)
      ),
      call. = FALSE
    )
  }
  duckdbfs::open_dataset(f)
}

# A Git LFS pointer instead of the data: what a package built from an archive without its LFS objects holds.
.rb_is_lfs_pointer <- function(f) {
  head <- readBin(f, "raw", 23)
  identical(head, charToRaw("version https://git-lfs"))
}
