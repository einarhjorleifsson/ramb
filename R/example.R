# example.R — ramb's data: parquet files in inst/extdata, tracked with Git LFS, read lazily.
#
# All of ramb's data are parquet since 2026.10.16.1 (the maintainer, 2026-10-16): the datasets that were lazy-loaded
# from data/ until then, made into parquet by data-raw/extdata.R, and the labelled tracks of data-raw/labelled_tracks.R.
# What each is, its sources and its licence: inst/extdata/data-dict.yaml and inst/REFERENCES.bib, stamped into each
# file by data-raw/data_dict.R. A geometry is DuckDB's own GEOMETRY, written by DuckDB as GeoParquet: DuckDB reads it
# as geometry, and sf where it is needed converts it from WKB.

#' Read an example dataset shipped with ramb
#'
#' ramb's data are parquet files in its `extdata` folder, opened lazily with [duckdbfs::open_dataset()] like any
#' other track; collect one with [dplyr::collect()]. Each dataset has its own help page (`?dansk`, `?ird_mg_gillnet`,
#' ...): what a row is, its columns, its sources and its licence. The same is in the folder's data dictionary,
#' `data-dict.yaml` (<https://data-dict.tidyverse.org/>), and in each file's own parquet metadata, so a file copied
#' out of the package still says what it is. The sources are BibTeX entries in the package's `REFERENCES.bib`.
#'
#' The two datasets with shapes (`dansk_harbours`, `trawlsurveytows`) hold them in `geometry` as DuckDB's GEOMETRY
#' (GeoParquet, longitude and latitude on WGS 84), which DuckDB's `ST_` functions work on. Collected into R it is
#' WKB; for sf: `x <- dplyr::collect(rb_read_example("dansk_harbours"))`, then
#' `sf::st_sf(x[names(x) != "geometry"], geometry = sf::st_as_sfc(structure(x$geometry, class = "WKB"), crs = 4326))`.
#'
#' @param name The dataset's name; `NULL` for the catalogue of all of them.
#' @return A lazy DuckDB table; or, for `name = NULL`, a tibble with one row per dataset: `name`, `label`,
#'   `description`, `rows`, `licence`, `cite` (its sources as text) and `bibtex` (the same as BibTeX), read from the
#'   files' own metadata.
#' @export
#' @examples
#' rb_read_example()
#' rb_read_example("wkssfgeo2_it_pots")
#' cat(rb_read_example()$bibtex[1])
rb_read_example <- function(name = NULL) {
  dir <- system.file("extdata", package = "ramb")
  have <- sub("\\.parquet$", "", list.files(dir, pattern = "\\.parquet$"))
  if (is.null(name)) {
    return(.rb_catalogue(dir))
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

# The catalogue: what data-raw/data_dict.R stamped into each file's parquet metadata, one row per file.
.rb_catalogue <- function(dir) {
  files <- list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
  for (f in files) {
    .rb_read_parquet(f, lazy = TRUE) # stops on a Git LFS pointer, with a message that says so
  }
  con <- duckdbfs::cached_connection()
  glob <- file.path(dir, "*.parquet")
  kv <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT parse_filename(file_name, true) AS file, decode(key) AS key, decode(value) AS value
       FROM parquet_kv_metadata('%s') WHERE decode(key) LIKE 'ramb.%%'",
      glob
    )
  )
  n <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT parse_filename(file_name, true) AS file, num_rows FROM parquet_file_metadata('%s')",
      glob
    )
  )
  get <- function(file, key) {
    v <- kv$value[kv$file == file & kv$key == paste0("ramb.", key)]
    if (length(v) == 1) enc2utf8(v) else NA_character_
  }
  out <- tibble::tibble(name = sort(sub("\\.parquet$", "", basename(files))))
  for (k in c("label", "description")) {
    out[[k]] <- vapply(out$name, get, "", key = k, USE.NAMES = FALSE)
  }
  out$rows <- as.integer(n$num_rows[match(out$name, n$file)])
  for (k in c("licence", "cite", "bibtex")) {
    out[[k]] <- vapply(out$name, get, "", key = k, USE.NAMES = FALSE)
  }
  out
}

# Roxygen lines for one dataset's help page (@eval in R/datasets.R), from its entry in inst/extdata/data-dict.yaml and
# its sources in inst/REFERENCES.bib. It runs when the package is documented, never when it is used, so yaml and
# rbibutils are in Suggests.
.rb_dataset_rd <- function(name) {
  # read_yaml() re-encodes to the session's encoding and, outside a UTF-8 locale, stops at the first non-ASCII
  # character without an error, dropping the datasets after it
  dd <- yaml::yaml.load(readLines(
    system.file("extdata", "data-dict.yaml", package = "ramb"),
    encoding = "UTF-8"
  ))
  t <- Filter(function(x) identical(x$name, name), dd$tables)[[1]]
  bib <- rbibutils::readBib(
    system.file("REFERENCES.bib", package = "ramb"),
    direct = TRUE
  )
  for (i in seq_along(bib)) {
    if (!is.null(bib[[i]]$doi)) bib[[i]]$url <- NULL
  }
  # a labelled paragraph of `details` (**Cite:**, **Licence:**, **Changes:**), its lines joined
  line_of <- function(label) {
    para <- gsub("\\s*\\n\\s*", " ", strsplit(t$details, "\\n\\s*\\n")[[1]])
    l <- grep(sprintf("^\\s*\\*\\*%s:\\*\\*", label), para, value = TRUE)
    trimws(sub(sprintf("^\\s*\\*\\*%s:\\*\\*", label), "", l))
  }
  esc <- function(x) gsub("%", "\\\\%", trimws(x))
  keys <- regmatches(
    line_of("Cite"),
    gregexpr("(?<=@)[A-Za-z0-9_]+", line_of("Cite"), perl = TRUE)
  )[[1]]
  op <- options(useFancyQuotes = FALSE)
  on.exit(options(op))
  rows <- DBI::dbGetQuery(
    duckdbfs::cached_connection(),
    sprintf(
      "SELECT num_rows FROM parquet_file_metadata('%s')",
      system.file("extdata", t$source$parquet, package = "ramb")
    )
  )$num_rows
  col <- function(c) {
    what <- if (is.null(c$description)) {
      ""
    } else {
      esc(gsub("\n", " ", c$description))
    }
    if (!is.null(c$units)) {
      what <- paste0(what, " Unit: ", c$units, ".")
    }
    if (!is.null(c$values) && !is.null(names(c$values))) {
      what <- paste0(
        what,
        " Values: ",
        paste0("`", names(c$values), "` ", unlist(c$values), collapse = "; "),
        "."
      )
    } else if (!is.null(c$values)) {
      what <- paste0(
        what,
        " Values: ",
        paste0("`", unlist(c$values), "`", collapse = ", "),
        "."
      )
    }
    sprintf("* `%s`: %s", c$name, what)
  }
  c(
    paste("@title", t$label),
    paste("@name", name),
    paste0(
      "@description ",
      esc(gsub("\n", " ", t$description)),
      " Read it with `rb_read_example(\"",
      name,
      "\")`."
    ),
    paste0(
      "@format A parquet file of ",
      format(rows, big.mark = ","),
      " rows in the package's `extdata` folder, `",
      t$source$parquet,
      "`, with the columns:"
    ),
    "",
    vapply(t$columns, col, ""),
    "",
    paste("@details", esc(gsub("\n", " ", line_of("Changes")))),
    paste0(
      "@source ",
      paste(esc(format(bib[keys], style = "text")), collapse = "\n\n"),
      "\n\nLicence: ",
      esc(line_of("Licence"))
    ),
    "@family example data",
    "@keywords datasets",
    "@seealso [rb_read_example()]; the data dictionary `data-dict.yaml` in the package's `extdata` folder."
  )
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
