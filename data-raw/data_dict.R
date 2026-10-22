# data_dict.R — stamp each parquet file in inst/extdata with its own entry of the data dictionary.
#
# The dictionary (inst/extdata/data-dict.yaml, data-dict 0.1.0, https://data-dict.tidyverse.org/) is the one source
# of what each dataset is; its sources are BibTeX entries in inst/REFERENCES.bib, named on each table's **Cite:**
# line. This copies, into each file's parquet key-value metadata:
#
#   ramb.name, ramb.label, ramb.description   the table's own fields
#   ramb.licence                              its **Licence:** line
#   ramb.cite                                 its sources as text
#   ramb.bibtex                               its sources as BibTeX
#   ramb.origin                               the script that made it
#   ramb.data_dict                            its whole entry of the dictionary, as YAML
#
# so a file copied out of the package still says what it is and whom to cite. rb_read_example() reads them back.
# Run after data-raw/extdata.R and data-raw/labelled_tracks.R, from the package root; the rows are checked unchanged.

library(DBI)
options(useFancyQuotes = FALSE)

# read_yaml() truncates the file outside a UTF-8 locale
dd <- yaml::yaml.load(readLines("inst/extdata/data-dict.yaml", encoding = "UTF-8"))
bib <- rbibutils::readBib("inst/REFERENCES.bib", direct = TRUE)
for (i in seq_along(bib)) {
  if (!is.null(bib[[i]]$doi)) bib[[i]]$url <- NULL # the reader adds the DOI's URL, which would print it twice
}
# a labelled paragraph of `details` (**Cite:**, **Licence:**, **Changes:**), its lines joined
line_of <- function(details, label) {
  para <- gsub("\\s*\\n\\s*", " ", strsplit(details, "\\n\\s*\\n")[[1]])
  l <- grep(sprintf("^\\s*\\*\\*%s:\\*\\*", label), para, value = TRUE)
  stopifnot(length(l) == 1)
  trimws(sub(sprintf("^\\s*\\*\\*%s:\\*\\*", label), "", l))
}
sq <- function(x) gsub("'", "''", enc2utf8(x), fixed = TRUE)

con <- dbConnect(duckdb::duckdb())
for (t in dd$tables) {
  f <- file.path("inst/extdata", t$source$parquet)
  keys <- regmatches(
    line_of(t$details, "Cite"),
    gregexpr("(?<=@)[A-Za-z0-9_]+", line_of(t$details, "Cite"), perl = TRUE)
  )[[1]]
  stopifnot(length(keys) > 0, all(keys %in% names(bib)))
  refs <- bib[keys]
  kv <- c(
    ramb.name = t$name,
    ramb.label = t$label,
    ramb.description = trimws(t$description),
    ramb.licence = line_of(t$details, "Licence"),
    ramb.cite = paste(format(refs, style = "text"), collapse = "\n\n"),
    ramb.bibtex = paste(as.character(utils::toBibtex(refs)), collapse = "\n"),
    ramb.origin = t$origin,
    ramb.data_dict = yaml::as.yaml(t)
  )
  tmp <- tempfile(fileext = ".parquet")
  dbExecute(
    con,
    sprintf(
      "COPY (FROM read_parquet('%s')) TO '%s' (FORMAT parquet, COMPRESSION zstd, COMPRESSION_LEVEL 19, KV_METADATA {%s})",
      f,
      tmp,
      paste(sprintf("'%s': '%s'", names(kv), sq(kv)), collapse = ", ")
    )
  )
  diff <- dbGetQuery(
    con,
    sprintf(
      "SELECT (SELECT count(*) FROM (FROM read_parquet('%1$s') EXCEPT ALL FROM read_parquet('%2$s'))) +
              (SELECT count(*) FROM (FROM read_parquet('%2$s') EXCEPT ALL FROM read_parquet('%1$s'))) AS n",
      f,
      tmp
    )
  )$n
  stopifnot(diff == 0)
  file.copy(tmp, f, overwrite = TRUE)
  unlink(tmp)
}
dbDisconnect(con, shutdown = TRUE)
