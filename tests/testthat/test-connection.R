# ramb's own DuckDB connection (connection.R): opened with nothing installed and the session left as found, a quarter
# of the memory, extensions installed offline, rb_read_parquet(), and lazy tables from other connections.

# a throwaway DuckDB whose extensions live in a temporary home, so nothing touches the user's
throwaway <- function(env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb(
    home = withr::local_tempdir(.local_envir = env)
  ))
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

test_that("ramb's connection opens with no extension loaded and leaves the session as it found it", {
  .rb_disconnect()
  g <- globalenv()
  state <- function() {
    list(
      options(),
      ls(g, all.names = TRUE),
      if (exists(".Random.seed", g, inherits = FALSE)) get(".Random.seed", g)
    )
  }
  before <- state()
  con <- .rb_connection()
  expect_identical(state(), before)
  expect_identical(.rb_connection(), con)
  loaded <- DBI::dbGetQuery(
    con,
    "SELECT extension_name FROM duckdb_extensions() WHERE loaded"
  )$extension_name
  expect_false(any(c("spatial", "h3", "httpfs") %in% loaded))
})

test_that("ramb's instance takes a quarter of the memory, or what options(ramb.memory_limit) says", {
  expect_equal(.rb_parse_size("19.1 GiB"), 19.1 * 2^30)
  expect_equal(.rb_parse_size("8GB"), 8e9)
  expect_true(is.na(.rb_parse_size("lots")))
  con <- throwaway()
  default <- .rb_parse_size(
    DBI::dbGetQuery(con, "SELECT current_setting('memory_limit') AS v")$v
  )
  expect_equal(
    .rb_parse_size(.rb_memory_limit(con)),
    default * 5 / 16,
    tolerance = 1e-3
  )
  withr::with_options(
    list(ramb.memory_limit = "2GB"),
    expect_equal(.rb_memory_limit(con), "2GB")
  )
  # set on ramb's own, as DuckDB rounds it
  set <- DBI::dbGetQuery(
    .rb_connection(),
    "SELECT current_setting('memory_limit') AS v"
  )$v
  expect_equal(.rb_parse_size(set), default * 5 / 16, tolerance = 0.02)
})

test_that("an extension installs offline: from a copied file or a folder, and the error names both ways", {
  # the smallest core extension already installed for the user, copied; skipped where there is none. ramb loads core
  # extensions only, and DuckDB 2.0 refuses a community one copied without its .info file
  have <- DBI::dbGetQuery(
    .rb_connection(),
    "SELECT extension_name, install_path FROM duckdb_extensions()
     WHERE installed AND install_path <> '' AND installed_from = 'core'"
  )
  have <- have[file.exists(have$install_path), ]
  skip_if(nrow(have) == 0, "no core DuckDB extension is installed to copy")
  pick <- have[which.min(file.size(have$install_path)), ]
  name <- pick$extension_name
  # nothing reachable: a download would fail
  withr::local_options(ramb.extension_repository = "http://127.0.0.1:9")

  con <- throwaway()
  version <- DBI::dbGetQuery(con, "SELECT version() AS v")$v
  platform <- DBI::dbGetQuery(con, "PRAGMA platform")$platform
  dir <- DBI::dbGetQuery(
    con,
    "SELECT current_setting('extension_directory') AS v"
  )$v
  err <- tryCatch(.rb_extension(con, name), error = identity)
  expect_s3_class(err, "error")
  expect_match(
    conditionMessage(err),
    paste0(name, ".duckdb_extension.gz"),
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    file.path(dir, version, platform),
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "HTTP_PROXY", fixed = TRUE)

  # a file copied into the extension folder counts as installed
  to <- file.path(dir, version, platform)
  dir.create(to, recursive = TRUE, showWarnings = FALSE) # the failed install made it
  file.copy(pick$install_path, file.path(to, paste0(name, ".duckdb_extension")))
  expect_true(.rb_extension(con, name))

  # a folder laid out as DuckDB's repository, named in the option
  repo <- withr::local_tempdir()
  dir.create(file.path(repo, version, platform), recursive = TRUE)
  file.copy(
    pick$install_path,
    file.path(repo, version, platform, paste0(name, ".duckdb_extension"))
  )
  withr::local_options(ramb.extension_repository = repo)
  con2 <- throwaway()
  expect_true(.rb_extension(con2, name))
  from <- DBI::dbGetQuery(
    con2,
    sprintf(
      "SELECT installed_from FROM duckdb_extensions() WHERE extension_name = '%s'",
      name
    )
  )$installed_from
  expect_equal(normalizePath(from), normalizePath(repo))
  # INSTALL ... FROM sets nothing on the connection
  expect_equal(
    DBI::dbGetQuery(
      con2,
      "SELECT current_setting('custom_extension_repository') AS v"
    )$v,
    ""
  )
})

test_that("rb_read_parquet() reads a hive-partitioned folder, files and globs, lazily", {
  dir <- withr::local_tempdir()
  d <- dplyr::collect(rb_read_example("whacks1"))
  d$part <- as.integer(d$.rid %% 2)
  w <- throwaway()
  DBI::dbWriteTable(w, "d", as.data.frame(d))
  DBI::dbExecute(
    w,
    sprintf("COPY d TO '%s' (FORMAT parquet, PARTITION_BY (part))", dir)
  )

  x <- rb_read_parquet(dir)
  expect_s3_class(x, "tbl_lazy")
  got <- dplyr::arrange(dplyr::collect(x), .rid)
  expect_equal(nrow(got), nrow(d))
  expect_equal(as.integer(got$part), d$part) # a folder name read as a number
  expect_equal(got$lon, d$lon)
  expect_false("part" %in% colnames(rb_read_parquet(dir, hive = FALSE)))
  files <- list.files(
    dir,
    pattern = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_equal(nrow(dplyr::collect(rb_read_parquet(files))), nrow(d))
  one <- dplyr::collect(rb_read_parquet(file.path(dir, "part=1", "*.parquet")))
  expect_equal(sort(one$.rid), d$.rid[d$part == 1])

  # files whose columns differ
  mix <- withr::local_tempdir()
  DBI::dbExecute(
    w,
    sprintf(
      "COPY (SELECT 1 AS a, 2 AS b) TO '%s' (FORMAT parquet)",
      file.path(mix, "ab.parquet")
    )
  )
  DBI::dbExecute(
    w,
    sprintf(
      "COPY (SELECT 3 AS a, 4 AS c) TO '%s' (FORMAT parquet)",
      file.path(mix, "ac.parquet")
    )
  )
  u <- dplyr::collect(rb_read_parquet(mix, union_by_name = TRUE))
  expect_setequal(names(u), c("a", "b", "c"))
  expect_equal(sum(is.na(u$b)), 1)

  # on a connection given
  y <- rb_read_parquet(dir, con = w)
  expect_identical(dbplyr::remote_con(y), w)
  expect_error(rb_read_parquet(character(0)), "paths or URLs")
  expect_error(rb_read_parquet(NA_character_), "paths or URLs")
})

test_that("rb_read_example() opens on the connection given, and ramb works on it", {
  other <- throwaway()
  x <- rb_read_example("whacks1", con = other)
  expect_identical(dbplyr::remote_con(x), other)
  out <- rb_flag_ping_invalid(x)
  expect_identical(dbplyr::remote_con(out), other)
  expect_equal(dplyr::pull(dplyr::count(out), n), nrow(whacks1))
})

test_that("a data frame and a lazy table on another connection run on that connection, the result as the data frame", {
  # the voyages of a few pings, lazy elsewhere; the pings in memory (fishycode passes duckdbfs tables so)
  p <- dplyr::filter(dansk, vessel_id == dansk$vessel_id[1])[1:2000, ] |>
    dplyr::transmute(vid = 1L, time = time_stamp, lon, lat, speed)
  p$port_id <- ifelse(p$speed < 0.5, "a", NA)
  v <- rb_create_voyages(p, method = "runs")
  other <- throwaway()
  DBI::dbWriteTable(other, "v", as.data.frame(v))
  got <- rb_add_voyages(p, dplyr::tbl(other, "v"))
  expect_s3_class(got, "tbl_df")
  expect_equal(got, rb_add_voyages(p, v))
})

test_that("a lazy table on duckdbfs's connection works as on any other", {
  skip_if_not_installed("duckdbfs")
  # duckdbfs installs spatial and h3 as it opens its connection: skipped where that fails
  dcon <- tryCatch(duckdbfs::cached_connection(), error = function(e) NULL)
  skip_if(is.null(dcon), "duckdbfs could not open its connection")
  x <- duckdbfs::as_dataset(whacks1)
  out <- rb_flag_ping_impossible(rb_flag_ping_invalid(x), method = "clean")
  expect_identical(dbplyr::remote_con(out), dcon)
  ref <- rb_flag_ping_impossible(
    rb_flag_ping_invalid(whacks1),
    method = "clean"
  )
  got <- dplyr::collect(out)
  expect_equal(got$ping_flag[order(got$.rid)], ref$ping_flag[order(ref$.rid)])
})
