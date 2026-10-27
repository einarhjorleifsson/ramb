# The maintainer's two rules for a function that takes a table (2026-10-27), as checks:
# 1. it returns the construct it was given (data.frame, tibble, data.table, sf, a lazy DuckDB table), with the rows
#    in the order they came in;
# 2. it leaves the caller's session as it found it, on error too.
# test-etiquette.R runs them over every exported function that takes a table.

# Namespaces a function may load on first use, loaded here so that what their .onLoad sets is not counted as the
# function's doing.
for (pkg in c(
  "sf",
  "s2",
  "lwgeom",
  "data.table",
  "duckdb",
  "duckdbfs",
  "dbplyr",
  "withr"
)) {
  requireNamespace(pkg, quietly = TRUE)
}

# The session as a caller sees it. The tables of duckdbfs's cached connection are the caller's too: a call on data
# frames must not leave its registered copies there. Not counted: dbplyr's `dbplyr_subquery_name`, the counter it
# moves on every lazy query it renders, whoever asks for it (dbplyr 2.6.0).
.session_state <- function() {
  g <- globalenv()
  op <- options()
  list(
    options = op[setdiff(names(op), "dbplyr_subquery_name")],
    s2 = sf::sf_use_s2(),
    seed = if (exists(".Random.seed", g, inherits = FALSE)) {
      get(".Random.seed", g)
    } else {
      "none"
    },
    rng = RNGkind(),
    wd = getwd(),
    files = list.files(getwd(), recursive = TRUE, all.files = TRUE),
    global = setdiff(ls(g, all.names = TRUE), ".Random.seed"),
    env = as.list(Sys.getenv()),
    locale = Sys.getlocale(),
    search = search(),
    devices = grDevices::dev.list(),
    connections = rownames(showConnections()),
    duckdb = sort(DBI::dbListTables(duckdbfs::cached_connection()))
  )
}

# What differs between two states, by name ("options: digits", "seed").
.session_diff <- function(a, b, ignore = character(0)) {
  out <- character(0)
  for (k in setdiff(names(a), ignore)) {
    if (identical(a[[k]], b[[k]])) {
      next
    }
    if (k %in% c("options", "env")) {
      n <- union(names(a[[k]]), names(b[[k]]))
      n <- n[
        !vapply(n, function(i) identical(a[[k]][[i]], b[[k]][[i]]), logical(1))
      ]
      out <- c(out, paste0(k, " (", paste(n, collapse = ", "), ")"))
    } else if (k %in% c("files", "global", "duckdb")) {
      out <- c(
        out,
        paste0(
          k,
          " (",
          paste(
            c(
              paste0("+", setdiff(b[[k]], a[[k]])),
              paste0("-", setdiff(a[[k]], b[[k]]))
            ),
            collapse = ", "
          ),
          ")"
        )
      )
    } else {
      out <- c(out, k)
    }
  }
  out
}

# Evaluate `code` and fail if it changed the session.
expect_session_kept <- function(code, label, ignore = character(0)) {
  before <- .session_state()
  force(code)
  diff <- .session_diff(before, .session_state(), ignore)
  expect(
    length(diff) == 0,
    sprintf("%s changed the session: %s.", label, paste(diff, collapse = "; "))
  )
  invisible(diff)
}

# A tibble (or sf) as the construct `to`; pings without a geometry become sf points from their lon and lat.
.as_construct <- function(x, to) {
  as_sf <- function(d) {
    if (inherits(x, "sf")) {
      sf::st_as_sf(d)
    } else {
      sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    }
  }
  switch(
    to,
    data.frame = as.data.frame(x),
    tibble = tibble::as_tibble(x),
    data.table = data.table::as.data.table(x),
    sf = as_sf(as.data.frame(x)),
    sf_tibble = as_sf(tibble::as_tibble(x)),
    lazy = duckdbfs::as_dataset(x)
  )
}

# The constructs a case is run on: data.frame, tibble and data.table, and sf and lazy where it takes them; only sf
# where its main table must be sf.
.constructs <- function(case) {
  if (isTRUE(case$sf_only)) {
    return(c("sf", "sf_tibble"))
  }
  c(
    "data.frame",
    "tibble",
    "data.table",
    if (case$sf) c("sf", "sf_tibble"),
    if (case$lazy) "lazy"
  )
}

.same_table <- function(a, b) {
  a <- as.data.frame(a)
  b <- as.data.frame(b)
  rownames(a) <- NULL
  rownames(b) <- NULL
  isTRUE(all.equal(a, b))
}

# Run a case (test-etiquette.R) on its main table as each construct, its rows shuffled, and check both rules. The
# class that came in goes out (but for a "vector" or a "function", the shapes that return no table). The rows keep
# their order, by `.rid`: every row ("same"), the rows kept ("subset"), the rows of a table of links, one or more
# per input row ("links"), the elements of a "vector". A table of its own ("new") is the same whatever the input
# order. The caller's table is untouched (a function that writes into a data.table changes it by reference), and
# the session is as it was, after the call and after a call that errors.
expect_etiquette <- function(case) {
  f <- getExportedValue("ramb", case$fn)
  quiet <- function(args) suppressMessages(suppressWarnings(do.call(f, args)))
  ref <- if (case$shape %in% c("new", "vector")) quiet(case$args())
  for (to in .constructs(case)) {
    args <- case$args()
    x <- args[[case$main]]
    x <- x[withr::with_seed(1, sample(nrow(x))), ]
    x <- .as_construct(x, to)
    x0 <- if (to == "data.table") data.table::copy(x) else x
    args[[case$main]] <- x
    if (to == "lazy") {
      for (a in case$lazy_also) {
        args[[a]] <- duckdbfs::as_dataset(args[[a]])
      }
    }
    label <- sprintf(
      "%s(%s = <%s>)%s",
      case$fn,
      case$main,
      to,
      if (is.null(case$label)) "" else case$label
    )
    out <- NULL
    # a lazy result reads the tables it made; a tile function its tile table
    expect_session_kept(
      out <- quiet(args),
      label,
      ignore = if (to == "lazy" || case$shape == "function") "duckdb"
    )
    if (case$shape == "function") {
      next
    }
    if (to == "lazy") {
      expect(
        inherits(out, "tbl_lazy"),
        sprintf("%s returned a %s, not a lazy table.", label, class(out)[1])
      )
      if (case$shape == "same" && inherits(out, "tbl_lazy")) {
        rid <- dplyr::pull(out, .rid)
        expect(
          length(rid) == nrow(dplyr::collect(x)) &&
            setequal(rid, dplyr::pull(x, .rid)),
          sprintf("%s did not return every row.", label)
        )
      }
      next
    }
    expect(
      identical(as.data.frame(x), as.data.frame(x0)),
      sprintf("%s changed the caller's table.", label)
    )
    if (case$shape == "vector") {
      orig <- case$args()[[case$main]]
      expect(
        identical(out, ref[match(x$.rid, orig$.rid)]),
        sprintf(
          "%s did not return its values in the order the rows came in.",
          label
        )
      )
      next
    }
    # a table of its own made from sf pings has no geometry: it comes back as the sf object's data frame or tibble
    want <- if (case$shape == "new") setdiff(class(x), "sf") else class(x)
    expect(
      identical(class(out), want),
      sprintf(
        "%s returned <%s>, not <%s>.",
        label,
        paste(class(out), collapse = "/"),
        paste(want, collapse = "/")
      )
    )
    if (case$shape == "same") {
      expect(
        identical(out$.rid, x$.rid),
        sprintf("%s did not return the rows in the order they came in.", label)
      )
    } else if (case$shape %in% c("subset", "links")) {
      i <- match(out$.rid, x$.rid)
      expect(
        !anyNA(i) && !is.unsorted(i),
        sprintf("%s did not return its rows in the order they came in.", label)
      )
    } else if (case$shape == "new") {
      expect(
        .same_table(out, ref),
        sprintf("%s gave another table on shuffled input.", label)
      )
    }
  }
  # on error: the main table with nothing but its row id
  args <- case$args()
  args[[case$main]] <- args[[case$main]][, ".rid", drop = FALSE]
  expect_session_kept(
    try(quiet(args), silent = TRUE),
    sprintf("%s on error", case$fn)
  )
}
