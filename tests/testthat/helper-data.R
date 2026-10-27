# ramb's data for the tests: parquet in inst/extdata (rb_read_example()), collected into the data frames the tests
# were written on when they were lazy-loaded from data/ (until 2026.10.15.1).
dansk <- dplyr::arrange(dplyr::collect(rb_read_example("dansk")), .rowid)
whacks1 <- dplyr::arrange(dplyr::collect(rb_read_example("whacks1")), .rid)
dansk_harbours <- local({
  x <- dplyr::collect(rb_read_example("dansk_harbours"))
  sf::st_sf(
    x[names(x) != "geometry"],
    geometry = sf::st_as_sfc(structure(x$geometry, class = "WKB"), crs = 4326)
  )
})

# A data frame as a lazy table on ramb's own connection, where duckdbfs::as_dataset() put it on duckdbfs's until
# 2026.10.27. Numbered names: R's random numbers are the caller's.
.lazy <- new.env(parent = emptyenv())
.lazy$n <- 0L
as_lazy <- function(x, con = .rb_connection()) {
  repeat {
    .lazy$n <- .lazy$n + 1L
    nm <- paste0("test_lazy_", .lazy$n)
    if (!DBI::dbExistsTable(con, nm)) break
  }
  DBI::dbWriteTable(con, nm, as.data.frame(x), temporary = TRUE)
  dplyr::tbl(con, nm)
}
