# connection.R — ramb's own DuckDB connection, and the extensions it loads.
#
# A call on data frames runs on one in-memory DuckDB per R session, ramb's own (the maintainer, 2026-10-27): kept in
# this package, not in options(), and opened with nothing installed. A lazy table brings its own connection
# (duckdbfs's, the caller's or this one), which .rb_con_for() uses. Until 2026.10.27 ramb used duckdbfs's cached
# connection, which installs spatial and h3 as it opens, turns automatic installs on and sets options(duckdbfs_conn):
# a user who could not download failed at the first call, and the caller's session was changed.
#
# Where downloaded extensions are kept is for duckdb to resolve from the caller's choice (DUCKDB_R_HOME,
# options(duckdb.home), ~/.duckdb, else a per-session temporary directory; ?duckdb::duckdb_storage).

.rb_db <- new.env(parent = emptyenv())

.rb_connection <- function() {
  con <- .rb_db$con
  if (!is.null(con) && DBI::dbIsValid(con)) {
    return(con)
  }
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, sprintf("SET memory_limit = '%s'", .rb_memory_limit(con)))
  .rb_db$con <- con
  con
}

.rb_disconnect <- function() {
  con <- .rb_db$con
  .rb_db$con <- NULL
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  invisible()
}

.onUnload <- function(libpath) .rb_disconnect()

# The memory ramb's instance may use: options(ramb.memory_limit), a DuckDB size such as "8GB", else a quarter of the
# machine's memory. DuckDB gives every in-memory instance 80 % of it by default, knowing nothing of the others in the
# session (duckdbfs's, the caller's), and takes no percentage, so the quarter is 5/16 of that default.
.rb_memory_limit <- function(con) {
  limit <- getOption("ramb.memory_limit")
  if (!is.null(limit)) {
    return(limit)
  }
  default <- DBI::dbGetQuery(
    con,
    "SELECT current_setting('memory_limit') AS v"
  )$v
  bytes <- .rb_parse_size(default)
  if (is.na(bytes)) {
    return(default)
  }
  sprintf("%.0fMiB", bytes * 5 / 16 / 2^20)
}

# A DuckDB size ("19.1 GiB", "8GB", "512 MiB") in bytes; NA if it is not one.
.rb_parse_size <- function(x) {
  m <- regmatches(x, regexec("^\\s*([0-9.]+)\\s*([KMGT]i?B|bytes)?\\s*$", x))[[
    1
  ]]
  if (length(m) == 0) {
    return(NA_real_)
  }
  unit <- c(
    KiB = 2^10,
    MiB = 2^20,
    GiB = 2^30,
    TiB = 2^40,
    KB = 1e3,
    MB = 1e6,
    GB = 1e9,
    TB = 1e12,
    bytes = 1
  )
  as.numeric(m[2]) * if (m[3] == "") 1 else unit[[m[3]]]
}

# Load a DuckDB extension on `con`, installing it first if it is not there. Without access to DuckDB's repository
# there are three ways in, each tested on 2026-10-27: a file copied into the extension folder counts as installed;
# options(ramb.extension_repository) names a folder or URL laid out as DuckDB's, installed from with INSTALL ... FROM,
# which leaves the connection's settings as they were (SET custom_extension_repository would not, on a caller's
# connection); and DuckDB's downloads go through the web proxy in HTTP_PROXY.
.rb_extension <- function(con, name) {
  load <- function() {
    !inherits(
      try(DBI::dbExecute(con, sprintf("LOAD %s", name)), silent = TRUE),
      "try-error"
    )
  }
  if (load()) {
    return(invisible(TRUE))
  }
  repo <- getOption("ramb.extension_repository")
  sql <- if (is.null(repo)) {
    sprintf("INSTALL %s", name)
  } else {
    sprintf("INSTALL %s FROM %s", name, DBI::dbQuoteString(con, repo))
  }
  got <- try(DBI::dbExecute(con, sql), silent = TRUE)
  if (!inherits(got, "try-error") && load()) {
    return(invisible(TRUE))
  }
  version <- DBI::dbGetQuery(con, "SELECT version() AS v")$v
  platform <- DBI::dbGetQuery(con, "PRAGMA platform")$platform
  dir <- DBI::dbGetQuery(
    con,
    "SELECT current_setting('extension_directory') AS v"
  )$v
  why <- if (inherits(got, "try-error")) {
    conditionMessage(attr(got, "condition"))
  } else {
    "It installed but did not load."
  }
  rlang::abort(
    c(
      sprintf(
        "DuckDB's `%s` extension could not be installed and loaded.",
        name
      ),
      x = strsplit(why, "\n")[[1]][1],
      i = "Without access to DuckDB's extension repository, any one of these works:",
      "*" = sprintf(
        "Download http://extensions.duckdb.org/%s/%s/%s.duckdb_extension.gz on another machine, unpack it and copy it into %s.",
        version,
        platform,
        name,
        file.path(dir, version, platform)
      ),
      "*" = "Set the environment variable HTTP_PROXY to your network's web proxy, if it has one.",
      "*" = sprintf(
        "Set options(ramb.extension_repository = <folder or URL>), laid out as <repository>/%s/%s/%s.duckdb_extension.",
        version,
        platform,
        name
      )
    ),
    call = NULL
  )
}

#' Read a parquet dataset lazily
#'
#' A lazy DuckDB table over parquet files: one file or several, a folder (every `.parquet` file below it), a glob
#' (`"tracks/year=*/*.parquet"`), or remote files (`https://`, `s3://`). Nothing is read when the table is made: when
#' a query runs, DuckDB reads only the columns it needs and skips the row groups, and with `hive = TRUE` the
#' partitions, that its filters rule out. A hive-partitioned dataset (`year=2024/month=3/part-0.parquet`) gets its
#' keys as columns.
#'
#' Over http(s) DuckDB cannot list a folder, so give the files' URLs (a server's list of them does). Remote files need
#' DuckDB's `httpfs` extension, which is loaded, and installed first if need be. On a network that blocks the
#' download: copy the extension file into DuckDB's extension folder, set `HTTP_PROXY`, or set
#' `options(ramb.extension_repository = )` to a folder or URL that holds it; the error names the file and the folder.
#' Credentials for a private bucket are set on the connection (DuckDB's `CREATE SECRET`), so give it in `con`.
#'
#' @param sources Paths or URLs: files, folders or globs.
#' @param hive Read `key=value` folder names as columns.
#' @param union_by_name Combine files whose columns differ, by name; a column a file lacks is `NULL` there.
#' @param con A DuckDB connection to open the dataset on; ramb's own by default. The lazy tables of one ramb call
#'   must share a connection.
#'
#' @return A lazy DuckDB table.
#' @export
#' @examples
#' d <- rb_read_parquet(system.file("extdata", "whacks1.parquet", package = "ramb"))
#' dplyr::count(d)
rb_read_parquet <- function(
  sources,
  hive = TRUE,
  union_by_name = FALSE,
  con = NULL
) {
  if (!is.character(sources) || length(sources) == 0 || anyNA(sources)) {
    stop("`sources` must be paths or URLs.", call. = FALSE)
  }
  if (is.null(con)) {
    con <- .rb_connection()
  }
  remote <- grepl("^[a-z][a-z0-9+.-]*://", sources)
  if (any(remote)) {
    .rb_extension(con, "httpfs")
  }
  sources[!remote] <- path.expand(sources[!remote])
  folder <- !remote & dir.exists(sources)
  sources[folder] <- file.path(
    sub("/+$", "", sources[folder]),
    "**",
    "*.parquet"
  )
  sql <- sprintf(
    "SELECT * FROM read_parquet([%s], hive_partitioning = %s, union_by_name = %s)",
    paste(DBI::dbQuoteString(con, sources), collapse = ", "),
    tolower(isTRUE(hive)),
    tolower(isTRUE(union_by_name))
  )
  dplyr::tbl(con, dplyr::sql(sql))
}
