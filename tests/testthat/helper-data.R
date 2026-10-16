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
