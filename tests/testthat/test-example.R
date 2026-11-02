# The example data (example.R): parquet in inst/extdata, read lazily; a Git LFS pointer is named as such.

test_that("the catalogue lists every dataset with its label, licence and sources", {
  cat <- rb_read_example()
  expect_s3_class(cat, "tbl_df")
  expect_named(
    cat,
    c("name", "label", "description", "rows", "licence", "cite", "bibtex")
  )
  files <- sub(
    "\\.parquet$",
    "",
    list.files(
      system.file("extdata", package = "ramb"),
      pattern = "\\.parquet$"
    )
  )
  expect_setequal(cat$name, files)
  expect_true(all(
    c("dansk", "whacks1", "wkssfgeo2_it_pots", "ird_mg_gillnet") %in% cat$name
  ))
  for (k in c("label", "description", "licence", "cite", "bibtex")) {
    expect_false(any(is.na(cat[[k]]) | cat[[k]] == ""), info = k)
  }
  expect_true(all(cat$rows > 0))
  expect_true(all(grepl("^@[A-Za-z]+\\{", cat$bibtex)))
  expect_match(
    cat$cite[cat$name == "ird_mg_gillnet"],
    "doi:10.23708/IBZJGD",
    fixed = TRUE
  )
  expect_equal(cat$licence[cat$name == "ird_mg_gillnet"], "CC BY 4.0.")
})

test_that("the labelled tracks are read lazily, in their columns", {
  for (n in c("wkssfgeo2_it_pots", "wkssfgeo_pt_gps", "ird_mg_gillnet")) {
    x <- rb_read_example(n)
    expect_s3_class(x, "tbl_lazy")
    d <- dplyr::collect(x)
    expect_named(
      d,
      c("vid", "trip", "time", "lon", "lat", "speed", "gear", "label", "op")
    )
    expect_s3_class(d$time, "POSIXct")
    expect_setequal(unique(stats::na.omit(d$op)), c("set", "haul"))
  }
  expect_error(rb_read_example("nope"), "must be one of")
})

test_that("the planted faults: one track per vessel and rate, every fault at every rate, wrong only where planted", {
  d <- dplyr::collect(rb_read_example("dansk_planted"))
  expect_setequal(unique(d$rate_s), c(10L, 60L, 360L, 3600L))
  expect_equal(d$vid, sprintf("%s_%ss", d$vessel, d$rate_s))
  faults <- c("spike", "run", "detour", "lon_eq_lat", "shifted_copy", "clock_offset", "summer_time", "near_copy",
              "tie", "fast")
  expect_setequal(stats::na.omit(unique(d$fault)), faults)
  expect_true(all(table(d$fault, d$rate_s) > 0))
  expect_false(any(d$wrong & is.na(d$fault)))
  expect_false(any(d$wrong & d$fault %in% c("near_copy", "fast")))
  expect_true(all(d$wrong[d$fault %in% c("spike", "run", "detour", "lon_eq_lat", "shifted_copy", "clock_offset",
                                         "summer_time")]))
  # a tie is two pings at one second, the planted one wrong
  tie <- d[d$fault %in% "tie", ]
  expect_true(all(table(paste(tie$vid, tie$fault_id)) == 2))
  expect_true(all(tapply(tie$wrong, paste(tie$vid, tie$fault_id), sum) == 1))
  expect_equal(d$lon[d$fault %in% "lon_eq_lat"], d$lat[d$fault %in% "lon_eq_lat"])
  expect_equal(d$.rid, seq_len(nrow(d)))
})

test_that("the data dictionary, the files and the references agree", {
  dir <- system.file("extdata", package = "ramb")
  dd_file <- file.path(dir, "data-dict.yaml")
  bib <- readLines(
    system.file("REFERENCES.bib", package = "ramb"),
    encoding = "UTF-8"
  )
  have_keys <- sub(
    "^@[A-Za-z]+\\{([^,]+),.*",
    "\\1",
    grep("^@[A-Za-z]+\\{", bib, value = TRUE)
  )
  # every cited key is in REFERENCES.bib (read as text: no BibTeX reader needed)
  dd_text <- readLines(dd_file, encoding = "UTF-8")
  cites <- grep("\\*\\*Cite:\\*\\*", dd_text, value = TRUE)
  keys <- unique(unlist(regmatches(
    cites,
    gregexpr("(?<=@)[A-Za-z0-9_]+", cites, perl = TRUE)
  )))
  expect_true(length(keys) > 0)
  expect_true(
    all(keys %in% have_keys),
    info = paste(setdiff(keys, have_keys), collapse = ", ")
  )
  skip_if_not_installed("yaml")
  dd <- yaml::yaml.load(dd_text) # read_yaml() truncates outside a UTF-8 locale
  files <- list.files(dir, pattern = "\\.parquet$")
  expect_setequal(vapply(dd$tables, function(t) t$source$parquet, ""), files)
  # each file carries its own entry of the dictionary, unchanged since it was stamped (data-raw/data_dict.R)
  con <- .rb_connection()
  for (t in dd$tables) {
    f <- file.path(dir, t$source$parquet)
    kv <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT decode(value) AS v FROM parquet_kv_metadata('%s') WHERE decode(key) = 'ramb.data_dict'",
        f
      )
    )$v
    expect_equal(yaml::yaml.load(enc2utf8(kv)), t, info = t$name)
    cols <- names(dplyr::collect(utils::head(rb_read_example(t$name), 0)))
    expect_setequal(vapply(t$columns, function(c) c$name, ""), cols)
  }
})

test_that("a Git LFS pointer is told from a parquet file", {
  f <- tempfile(fileext = ".parquet")
  writeLines(
    c(
      "version https://git-lfs.github.com/spec/v1",
      "oid sha256:0000",
      "size 1"
    ),
    f
  )
  expect_true(.rb_is_lfs_pointer(f))
  ex <- system.file("extdata", "wkssfgeo2_it_pots.parquet", package = "ramb")
  expect_false(.rb_is_lfs_pointer(ex))
})
