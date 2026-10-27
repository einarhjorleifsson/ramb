# The maintainer's two rules (2026-10-27) over every exported function (helper-etiquette.R): a function that takes a
# table returns the construct it was given, rows in the order they came in; and no function changes the caller's
# session, on error too. The table functions are the cases below; the vector tools, the plots, the readers and the
# map viewer give no table back, so only the session is checked for them. The old names of renamed.R call the new ones.

# Functions that take vectors ------------------------------------------------------------------------------------

test_that("the vector tools leave the session as they found it", {
  t <- as.POSIXct("2024-12-31 23:00:00", tz = "UTC") + seq(0, 7200, by = 600)
  lon <- -20 + seq_along(t) / 100
  lat <- 64 + seq_along(t) / 200
  calls <- list(
    rb_bin_midpoint = quote(rb_bin_midpoint(c(1.2, 3.7, -0.4), 1)),
    rb_calc_distance = quote(rb_calc_distance(
      lon1 = lon[-1],
      lat1 = lat[-1],
      lon2 = lon[-13],
      lat2 = lat[-13]
    )),
    rb_calc_speed = quote(rb_calc_speed(lon, lat, t)),
    rb_calc_speed_smooth = quote(rb_calc_speed_smooth(
      seq_along(t),
      t,
      window_s = 1200
    )),
    rb_calc_step_acceleration = quote(rb_calc_step_acceleration(lon, lat, t)),
    rb_calc_step_time = quote(rb_calc_step_time(t)),
    rb_calc_track_time = quote(rb_calc_track_time(t)),
    rb_classify_mmsi = quote(rb_classify_mmsi(c(
      "251439000",
      "123",
      "970123456"
    ))),
    rb_convert_speed = quote(rb_convert_speed(
      c(1, 10),
      from = "kn",
      to = "ms"
    )),
    rb_detect_crosses_year = quote(rb_detect_crosses_year(t[1], t[13])),
    rb_encode_ices_rectangle = quote(rb_encode_ices_rectangle(lon, lat)),
    rb_interpolate = quote(rb_interpolate(c(1, NA, 3), c(1, 2, 3))),
    rb_lookup_mmsi_country = quote(
      rb_lookup_mmsi_country(
        c("251439000", "231000000"),
        tibble::tibble(MID = "251", flag = "IS")
      )
    ),
    rb_number_runs = quote(rb_number_runs(c(1, 1, 2, 2, 1))),
    rb_pad_harbour = quote(rb_pad_harbour(
      c(NA, 1, 1, 1, NA),
      c(7, NA, NA, NA, 9)
    )),
    rb_cap_outliers = quote(rb_cap_outliers(
      c(1, 2, 3, 1000),
      method = "miller"
    )),
    rb_extract_metier = quote(rb_extract_metier(
      "OTB_DEF_>=120_0_0",
      part = "metier5"
    )),
    rb_predict_gear_width = quote(rb_predict_gear_width("OTB_DEF", 30, 1000))
  )
  for (fn in names(calls)) {
    expect_session_kept(eval(calls[[fn]]), fn)
    expect_session_kept(
      try(eval(calls[[fn]][1:2]), silent = TRUE),
      paste(fn, "on error")
    )
  }
})

# Plots, readers and the map viewer --------------------------------------------------------------------------------

test_that("the plots leave the session as they found it", {
  skip_if_not_installed("mapdeck")
  d <- dplyr::filter(dansk, vessel_id == dansk$vessel_id[1])[1:200, ]
  d$tid <- 1L
  expect_session_kept(rb_plot_trail(d, path = TRUE), "rb_plot_trail")
  expect_session_kept(rb_plot_trip(d, tid), "rb_plot_trip")
  expect_session_kept(
    try(rb_plot_trail(d[, "speed"]), silent = TRUE),
    "rb_plot_trail on error"
  )
})


test_that("rb_read_example() leaves the session as it found it", {
  expect_session_kept(
    dplyr::collect(rb_read_example("whacks1")),
    "rb_read_example"
  )
  expect_session_kept(rb_read_example(), "rb_read_example()")
  expect_session_kept(
    try(rb_read_example("nope"), silent = TRUE),
    "rb_read_example on error"
  )
})

test_that("the map viewer's builders leave the session as they found them", {
  skip_if_not_installed("jsonlite")
  st <- list(list(id = "a-line", type = "line"))
  expect_session_kept(rb_map_basemaps(), "rb_map_basemaps")
  expect_session_kept(rb_map_palettes(), "rb_map_palettes")
  expect_session_kept(
    a <- rb_map_layer(
      "a",
      "vector",
      tiles = function(z, x, y) raw(0),
      style = st
    ),
    "rb_map_layer"
  )
  expect_session_kept(g <- rb_map_group("g", layers = list(a)), "rb_map_group")
  expect_session_kept(m <- rb_map(g), "rb_map")
  expect_session_kept(try(rb_map(list()), silent = TRUE), "rb_map on error")
  d <- dplyr::filter(dansk, vessel_id == dansk$vessel_id[1])[1:500, ]
  # a map's pings and tiles are tables on the connection, kept for the map until it is served and stopped
  expect_session_kept(
    rb_map_pings(d, speed, time = "time_stamp"),
    "rb_map_pings",
    ignore = "duckdb"
  )
  expect_session_kept(
    rb_map_track(d, speed, time = "time_stamp", by = vessel_id),
    "rb_map_track",
    ignore = "duckdb"
  )
  expect_session_kept(
    try(rb_map_pings(d, nope), silent = TRUE),
    "rb_map_pings on error"
  )
  skip_if_not_installed("httpuv")
  port <- httpuv::randomPort()
  # a served map is the point of these two; everything else about the session is kept
  expect_session_kept(
    {
      rb_map_serve(m, port = port, open = FALSE)
      rb_map_stop(port)
    },
    "rb_map_serve, rb_map_stop"
  )
})

# What a call registers on the connection --------------------------------------------------------------------------

test_that("a lazy result stays readable after a later call on data frames", {
  skip_if_not_installed("duckdbfs")
  sq <- sf::st_sf(
    port_id = "a",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      crs = 4326
    )
  )
  d <- tibble::tibble(
    vid = 1L,
    time = as.POSIXct("2024-01-01", tz = "UTC") + 0:1,
    lon = c(0.5, 2),
    lat = c(0.5, 2)
  )
  # the lazy result reads the ports registered for it; the call on a data frame used to release them
  lz <- rb_flag_ping_port(duckdbfs::as_dataset(d), sq)
  df <- rb_flag_ping_port(d, sq)
  # collected once: a lazy table has no row order
  lz <- dplyr::collect(lz)
  expect_equal(lz$port_id[order(lz$lon)], df$port_id)
  expect_equal(df$port_id, c("a", NA))
})

# Functions that take a table ------------------------------------------------------------------------------------

# One working call per function, and one per method where the methods run apart. The main table carries `.rid`, its
# row number in a sorted order; expect_etiquette() shuffles it and converts it to each construct. `shape` says what
# comes back: every row ("same"), some ("subset"), a table of links in the input rows' order ("links"), a table of
# its own ("new"), a value per row ("vector"), a tile function ("function"). `sf` and `lazy`: the main table may
# be sf points or a lazy DuckDB table; `sf_only`: it must be sf; `lazy_also`: other tables made lazy with it. The
# side tables (port calls, voyages, windows, matches, units, stretches) are made once, here; `args()` makes fresh
# copies. Not cases, since they take vectors: rb_cap_outliers(), rb_extract_metier(), rb_predict_gear_width(). Not a
# case, since it reads from the internet and takes no table: rb_get_gear_vocabulary().

etiquette_cases <- local({
  fresh <- function(x) unserialize(serialize(x, NULL))
  add_rid <- function(x) {
    x$.rid <- seq_len(nrow(x))
    x
  }
  utc <- function(x) as.POSIXct(x, tz = "UTC")

  # Danish pings (step 1): EX_4 (4 days, 6 port calls) and EX_1, every second ping, with the ports near them ------
  dk <- dansk |>
    dplyr::filter(vessel_id %in% c("EX_1", "EX_4")) |>
    dplyr::arrange(vessel_id, time_stamp) |>
    dplyr::filter(dplyr::row_number() %% 2 == 1, .by = vessel_id) |>
    dplyr::transmute(
      vid = ifelse(vessel_id == "EX_1", 1L, 2L),
      time = time_stamp,
      lon,
      lat,
      speed
    ) |>
    dplyr::arrange(vid, time)
  dk <- add_rid(dk)
  dk_ports <- local({
    p <- dplyr::mutate(
      sf::st_transform(dansk_harbours, 4326),
      port_id = as.character(SI_HARB)
    )
    box <- sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = min(dk$lon) - 0.2,
          ymin = min(dk$lat) - 0.2,
          xmax = max(dk$lon) + 0.2,
          ymax = max(dk$lat) + 0.2
        ),
        crs = 4326
      )
    )
    p[lengths(sf::st_intersects(p, box)) > 0, c("port_id", "SI_HARB")]
  })
  dk_calls <- rb_create_port_calls(
    dk[names(dk) != ".rid"],
    dk_ports,
    min_dwell_h = 2
  )
  dk_voyages <- rb_create_voyages(
    dk[names(dk) != ".rid"],
    dk_calls,
    min_pings = 10
  )
  dk_tagged <- local({
    t <- rb_flag_ping_port(dk, dk_ports["port_id"])
    t <- tibble::as_tibble(t[order(t$.rid), ])
    t[c("vid", "time", "lon", "lat", "port_id", ".rid")]
  })

  # Italian AIS (CNR-IRBIM, WKSSFGEO): vessels 5 and 6, renamed to the grammar; harbours as sf ---------------------
  it_ports <- local({
    h <- dplyr::collect(rb_read_example("wkssfgeo_cnr_med_harbours"))
    sf::st_sf(
      port_id = seq_len(nrow(h)),
      geometry = sf::st_as_sfc(structure(h$geometry, class = "WKB"), crs = 4326)
    )
  })
  it <- dplyr::collect(rb_read_example("wkssfgeo_cnr_ais")) |>
    dplyr::filter(MMSI %in% 5:6) |>
    dplyr::transmute(
      vid = MMSI,
      time = datetime,
      lon = longitude,
      lat = latitude,
      speed
    ) |>
    dplyr::arrange(vid, time)
  it <- add_rid(tibble::as_tibble(it))
  it_tagged <- local({
    t <- rb_flag_ping_port(it, it_ports["port_id"])
    tibble::as_tibble(t[
      order(t$.rid),
      c("vid", "time", "lon", "lat", "speed", "port_id", ".rid")
    ])
  })

  # Logbook records (test-record.R) --------------------------------------------------------------------------------
  t0 <- utc("2024-03-01 06:00:00")
  h <- function(x) t0 + x * 3600
  records <- function() {
    tibble::tibble(
      .sid = 1:6,
      schema = "a",
      vid = c(1L, 1L, 2L, 2L, 3L, 3L),
      gear = c("OTB", "OTB", "GNS", "GNS", "LHM", "OTB"),
      gear_class = c("towed", "towed", "static", "static", "jigged", "towed"),
      set_max_min = c(NA, NA, 60, 60, NA, NA),
      t1 = h(c(NA, NA, 0, 1, 0, NA)),
      t2 = h(c(0, 3, NA, NA, NA, NA)),
      t3 = h(c(4, 5, 10, NA, NA, NA)),
      t4 = h(c(NA, NA, 12, 14, 6, NA)),
      duration_m = c(240, 120, NA, NA, NA, NA),
      date = as.Date("2024-03-01"),
      s1 = c(2, 2, NA, NA, 0, 2),
      s2 = c(5, 5, NA, NA, 3, 5),
      .rid = 1:6
    )
  }
  rec <- records()[names(records()) != ".rid"]
  windows_all <- rb_create_record_windows(rec, min_hauls = 1)
  windows_tow <- rb_create_record_windows(rec[1, ], min_hauls = 1)
  pings_tier <- function() {
    tibble::tibble(
      vid = c(1L, 1L, 1L, 3L, 3L, 4L),
      time = h(c(1, 3.5, 20, 2, 17, 1)),
      speed = c(3, NA, 3, 1, 9, 3),
      .rid = 1:6
    )
  }
  matches_tier <- rb_link_record_pings(
    pings_tier()[1:3],
    windows_all,
    carry = "gear",
    speed_min = "s1",
    speed_max = "s2"
  )
  pings_tow <- function() {
    p <- tibble::tibble(
      vid = 1L,
      time = h(seq(0.5, 2.5, by = 1 / 60)),
      speed = 3
    )
    p$speed[61] <- 9
    p$speed[62] <- NA
    add_rid(p)
  }
  matches_tow <- rb_link_record_pings(
    pings_tow()[1:3],
    windows_tow,
    speed_min = "s1",
    speed_max = "s2"
  )

  # Towing stretches (test-stretch.R) ------------------------------------------------------------------------------
  m <- function(x) t0 + x * 60
  stretch_pings <- function() {
    mins <- c(0:390, 435:500)
    spd <- ifelse(
      mins < 60,
      10,
      ifelse(
        mins < 180,
        3.5,
        ifelse(
          mins < 185,
          1,
          ifelse(
            mins < 300,
            3.5,
            ifelse(
              mins < 360,
              10,
              ifelse(
                mins <= 390,
                3.5,
                ifelse(
                  mins <= 465,
                  3.5,
                  ifelse(mins < 480, 10, ifelse(mins <= 490, 3.5, 0.5))
                )
              )
            )
          )
        )
      )
    )
    spd[mins == 90] <- 8
    add_rid(tibble::tibble(
      vid = 1L,
      time = m(mins),
      speed = spd,
      speed_min = 2,
      speed_max = 5,
      port_call = ifelse(mins > 490, 7L, NA_integer_)
    ))
  }
  tows <- function() {
    tibble::tibble(
      .sid = 1:5,
      schema = "a",
      vid = 1L,
      t2 = m(c(65, 190, 362, 436, 436)),
      t3 = m(c(175, 295, 388, 464, 464)),
      .rid = 1:5
    )
  }
  stretches <- rb_create_record_stretches(
    stretch_pings()[1:6],
    records = tows()[1:5],
    port_call = "port_call"
  )

  # Catch units (test-catch.R) -------------------------------------------------------------------------------------
  hc <- function(x) utc("2024-03-04") + x * 3600
  catch_events <- function() {
    tibble::tibble(
      id = c("h1", "h2", "d1", "d1b", "t1"),
      vid = 1L,
      start = hc(c(0, 26, 0, 0, 0)),
      end = hc(c(4, 27, 24, 24, 50)),
      level = c(1L, 1L, 2L, 2L, 3L),
      resolution = c("haul", "haul", "day", "day", "trip")
    )
  }
  catch_units <- rb_add_catch_units(catch_events(), key = "id")

  # Co-location tracks (test-vessel.R) -----------------------------------------------------------------------------
  vt_track <- function(
    id,
    t0,
    n,
    lon0,
    lat0,
    step_s = 60,
    dlon = 0.002,
    speed = 5
  ) {
    tibble::tibble(
      id = id,
      time = t0 + (0:(n - 1)) * step_s,
      lon = lon0 + dlon * (0:(n - 1)) * step_s / 60,
      lat = lat0,
      speed = speed
    )
  }

  # Port polygons for rb_flag_ping_port (test-ping.R) --------------------------------------------------------------
  sq <- function(x0, y0, w) {
    sf::st_polygon(list(rbind(
      c(x0, y0),
      c(x0 + w, y0),
      c(x0 + w, y0 + w),
      c(x0, y0 + w),
      c(x0, y0)
    )))
  }
  sq_ports <- sf::st_sf(
    port_id = c("big", "small", "far"),
    code = c("B", "S", "F"),
    geometry = sf::st_sfc(
      sq(0, 0, 1),
      sq(0.2, 0.2, 0.1),
      sq(5, 5, 1),
      crs = 4326
    )
  )

  # sf points for the polygon functions: every 10th ping of EX_4, some in port 136 ---------------------------------
  dk_points <- local({
    p <- dk[dk$vid == 2L, ]
    p <- p[seq(1, nrow(p), by = 10), c("vid", "time", "lon", "lat")]
    p$.rid <- seq_len(nrow(p))
    sf::st_as_sf(p, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  })
  dk_polygons <- sf::st_geometry(dk_ports)

  # Icelandic logbook, toy (for the three MFRI helpers) ------------------------------------------------------------
  lb <- function() {
    tibble::tibble(
      vid = c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 5L),
      gid = c(6L, 6L, 6L, 7L, 7L, 2L, 14L, 5L, 15L),
      t1 = utc("2024-03-01 06:00") + 3600 * c(0, 5, 20, 0, 30, 0, 10, 0, 0),
      t2 = utc("2024-03-01 06:00") + 3600 * c(6, 15, 26, 25, 50, 4, 30, 3, 2),
      effort = c(6, NA, 14, 25, 20, NA, 20, 3, 2),
      mesh = c(135, 155, NA, NA, NA, 203, NA, 135, NA),
      mesh_min = c(NA, NA, NA, 40, 40, NA, NA, NA, NA),
      sweeps = c(100, 300, NA, 200, 300, NA, 60, NA, NA),
      plow_width = c(NA, NA, NA, NA, NA, NA, NA, NA, 3),
      .rid = 1:9
    )
  }

  # Deterministic interpolation fixture (test-interpolate.R's shape, no random numbers) ----------------------------
  interp <- local({
    t0 <- utc("2020-01-01")
    track <- do.call(
      rbind,
      lapply(1:4, function(g) {
        data.frame(
          vid = g %% 2,
          trip = g,
          time = t0 + g * 86400 + c(0, 120, 300, 420, 600, 900),
          lon = -25 + g / 10 + c(0, 0.01, 0.03, 0.04, 0.06, 0.09),
          lat = 62 + c(0, 0.005, 0.01, 0.02, 0.025, 0.03)
        )
      })
    )
    events <- do.call(
      rbind,
      lapply(1:5, function(g) {
        data.frame(
          vid = g %% 2,
          trip = g,
          time = t0 + g * 86400 + c(-60, 0, 60, 350, 600, 1000)
        )
      })
    )
    list(
      track = tibble::as_tibble(track),
      events = add_rid(tibble::as_tibble(events))
    )
  })

  # Two feeds of one vessel for the duplicate flag (test-ping.R, without the shuffle) ------------------------------
  two_feeds <- local({
    a <- dk[dk$vid == 2L, ][1:200, ] |>
      dplyr::transmute(vid = 1L, time, lon, lat, provider = "stk")
    b <- a[seq(1, nrow(a), by = 3), ] |>
      dplyr::mutate(time = time + 1, lon = lon + 1e-6, provider = "astd")
    d <- dplyr::bind_rows(a, b) |> dplyr::arrange(vid, time, provider)
    add_rid(d)
  })

  list(
    # Step 0: flag pings ----------------------------------------------------------------------------------------------
    list(
      fn = "rb_flag_ping_invalid",
      main = "x",
      args = function() {
        x <- data.frame(
          vid = 1,
          time = utc("2024-01-01") + 60 * 0:5,
          lon = c(-20, 64, -20.01, 5, -20.02, -20.03),
          lat = 64
        )
        list(x = add_rid(tibble::as_tibble(x)))
      },
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = ""
    ),
    list(
      fn = "rb_flag_ping_duplicate",
      main = "x",
      args = function() list(x = fresh(two_feeds), priority = c("stk", "astd")),
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = "feed column `provider`; 267 pings, every third copied 1 s later by a second feed."
    ),
    list(
      fn = "rb_flag_ping_impossible",
      main = "x",
      args = function() list(x = fresh(whacks1), method = "clean"),
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = "whacks1 (46 pings). Lazy input: labels are written to temporary parquet files that live until the R session ends."
    ),
    list(
      fn = "rb_flag_ping_impossible",
      main = "x",
      args = function() list(x = fresh(whacks1), method = "sda"),
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = "whacks1. Lazy input: temporary parquet label files, kept for the session."
    ),
    list(
      fn = "rb_flag_ping_impossible",
      main = "x",
      args = function() list(x = fresh(whacks1), method = "forward"),
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = "whacks1. Lazy input: temporary parquet label files, kept for the session."
    ),
    list(
      fn = "rb_flag_ping_impossible",
      main = "x",
      args = function() list(x = fresh(whacks1), method = "fwdbwd"),
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = "whacks1. Lazy input: also writes a temporary parquet label file, kept for the session."
    ),
    list(
      fn = "rb_flag_ping_impossible",
      main = "x",
      args = function() list(x = fresh(whacks1), method = "sequential"),
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = "whacks1. Lazy input: temporary parquet label files, kept for the session."
    ),
    list(
      fn = "rb_flag_ping_port",
      main = "pings",
      args = function() {
        d <- tibble::tibble(
          vid = 1L,
          time = utc("2024-01-01") + 60 * 0:3,
          lon = c(0.25, 0.8, 5.5, 9),
          lat = c(0.25, 0.8, 5.5, 9)
        )
        list(pings = add_rid(d), ports = fresh(sq_ports), keep = "code")
      },
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = "`ports` is sf (polygons); pings take lon/lat columns, not sf."
    ),

    # Step 1: port calls, voyages, trips -----------------------------------------------------------------------------
    list(
      fn = "rb_create_port_calls",
      main = "pings",
      args = function() {
        list(pings = fresh(dk), ports = fresh(dk_ports), min_dwell_h = 2)
      },
      shape = "new",
      lazy = TRUE,
      sf = TRUE,
      note = "dansk EX_1 and EX_4, every 2nd ping; `ports` is sf, cut to the ports near the tracks (all 942 take 7 s)."
    ),
    list(
      fn = "rb_create_voyages",
      main = "pings",
      args = function() {
        list(pings = fresh(dk), port_calls = fresh(dk_calls), min_pings = 10)
      },
      shape = "new",
      lazy = TRUE,
      sf = TRUE,
      note = "method dwell; `port_calls` from rb_create_port_calls() (may be lazy too)."
    ),
    list(
      fn = "rb_create_voyages",
      main = "pings",
      args = function() list(pings = fresh(dk_tagged), method = "runs"),
      shape = "new",
      lazy = TRUE,
      sf = TRUE,
      note = "method runs; port_id from rb_flag_ping_port()."
    ),
    list(
      fn = "rb_create_voyages",
      main = "pings",
      args = function() list(pings = fresh(dk_tagged), method = "jepol"),
      shape = "new",
      lazy = TRUE,
      sf = TRUE,
      note = "method jepol; port_id from rb_flag_ping_port()."
    ),
    list(
      fn = "rb_create_voyages",
      main = "pings",
      args = function() {
        list(
          pings = fresh(it_tagged),
          method = "cnr",
          ports = fresh(it_ports),
          by_year = FALSE
        )
      },
      shape = "new",
      lazy = TRUE,
      sf = TRUE,
      note = "method cnr on CNR's Italian AIS (vessels 5, 6); needs speed and port_id; `ports` sf; uses lwgeom."
    ),
    list(
      fn = "rb_create_voyages",
      main = "pings",
      args = function() {
        list(
          pings = fresh(it),
          method = "iapesca",
          ports = fresh(it_ports),
          by_year = FALSE,
          iapesca = list(crs_m = 32633)
        )
      },
      shape = "new",
      lazy = TRUE,
      sf = TRUE,
      note = "method iapesca on CNR's Italian AIS (vessels 5, 6); `ports` sf."
    ),
    list(
      fn = "rb_add_voyages",
      main = "pings",
      args = function() {
        list(
          pings = fresh(dk),
          voyages = fresh(dk_voyages),
          port_calls = fresh(dk_calls)
        )
      },
      shape = "same",
      lazy = TRUE,
      sf = TRUE,
      note = "`voyages` and `port_calls` may be lazy too."
    ),
    list(
      fn = "rb_link_trip_voyages",
      main = "trips",
      args = function() {
        voyages <- tibble::tibble(
          vid = c(1L, 1L, 2L),
          year = 2024,
          voyage_id = c(1, 2, 1),
          T1 = utc("2024-03-01 06:00") + 86400 * c(0, 2, 0),
          T2 = utc("2024-03-01 18:00") + 86400 * c(0, 2, 1),
          port_from = c("A", "A", "B"),
          port_to = c("A", "B", "B"),
          n_pings = c(100, 80, 200)
        )
        trips <- tibble::tibble(
          tid = 1:4,
          vid = c(1L, 1L, 2L, 3L),
          T1 = utc("2024-03-01 07:00") + 86400 * c(0, 2, 0, 0),
          T2 = utc("2024-03-01 17:00") + 86400 * c(0, 2, 1, 0),
          .rid = 1:4
        )
        list(trips = trips, voyages = voyages, keys = c("tid", ".rid"))
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "one row per linked trip (a subset): keys and voyage columns only (T1, T2 dropped); .rid is in `keys` so it comes out. Trip 4 has no voyage."
    ),
    list(
      fn = "rb_link_trip_landings",
      main = "trips",
      args = function() {
        trips <- tibble::tibble(
          tid = 1:5,
          vid = c(1L, 1L, 1L, 1L, 2L),
          port = c("A", "A", "B", "B", "C"),
          T2 = utc(c(
            "2024-03-01 18:00",
            "2024-03-03 20:00",
            "2024-03-05 23:30",
            "2024-03-06 17:00",
            "2024-03-01 12:00"
          )),
          .rid = 1:5
        )
        landings <- tibble::tibble(
          lid = c(10, 11, 12, 20, 21),
          vid = c(1L, 1L, 1L, 2L, 2L),
          port = c("A", "A", "B", "C", "C"),
          date = as.Date(c(
            "2024-03-01",
            "2024-03-04",
            "2024-03-07",
            "2024-03-01",
            "2024-03-01"
          ))
        )
        list(
          trips = trips,
          landings = landings,
          trip_key = "tid",
          landing_key = "lid",
          port_trip = "port",
          port_landing = "port",
          since_previous_days = 3
        )
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "links only (tid, lid, match_type, ...); unmatched trips and other trip columns are dropped. Accepts lazy tables but returns a collected tibble."
    ),
    list(
      fn = "rb_add_trip_time",
      main = "trips",
      args = function() {
        t0 <- utc("2024-03-01 06:00")
        w <- tibble::tibble(
          tid = c(1L, 1L, 2L, 2L, 4L, 4L),
          witness = c(
            "track",
            "logbook",
            "logbook",
            "notice",
            "notice",
            "track"
          ),
          time = t0 + c(0, 600, 0, 7200, 0, NA),
          place = c("A", "A", NA, "B", "C", "D")
        )
        list(
          trips = tibble::tibble(tid = 1:4, .rid = 1:4),
          witnesses = w,
          keys = "tid",
          rank = c("track", "logbook", "notice"),
          basis = c(
            track = "reconstructed",
            logbook = "recorded",
            notice = "recorded"
          )
        )
      },
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "`witnesses` may be lazy too."
    ),
    list(
      fn = "rb_add_logbook_trip",
      main = "ve",
      args = function() {
        ve <- tibble::tibble(
          vid = c("A", "A", "A", "B"),
          time = utc(c(
            "2022-01-01 10:00:00",
            "2022-01-01 15:00:00",
            "2022-01-02 08:00:00",
            "2022-01-01 12:00:00"
          )),
          .rid = 1:4
        )
        le <- tibble::tibble(
          vid = c("A", "A", "B"),
          tid = c(101, 102, 201),
          T1 = utc(c(
            "2022-01-01 09:00:00",
            "2022-01-02 07:00:00",
            "2022-01-01 11:00:00"
          )),
          T2 = utc(c(
            "2022-01-01 16:00:00",
            "2022-01-02 09:00:00",
            "2022-01-01 13:00:00"
          ))
        )
        list(ve = ve, le = le)
      },
      shape = "same",
      lazy = FALSE,
      sf = FALSE,
      note = "plain dplyr; column arguments are captured with substitute(), so leave them at their defaults. A lazy ve does not error: it warns that vid and time are missing (names() of a tbl_lazy) and returns ve unchanged."
    ),

    # Step 2: logbook records ----------------------------------------------------------------------------------------
    list(
      fn = "rb_limit_record_duration",
      main = "records",
      args = function() {
        list(
          records = tibble::tibble(
            gid = c(6, 6, 7, 9),
            duration_m = c(100, 900, NA, 5000),
            .rid = 1:4
          ),
          limits = data.frame(gid = c(6, 7), dur_max_min = c(600, 60))
        )
      },
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = ""
    ),
    list(
      fn = "rb_create_record_windows",
      main = "records",
      args = function() list(records = records(), min_hauls = 1),
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "one row per record and phase; .rid is not carried."
    ),
    list(
      fn = "rb_link_record_pings",
      main = "pings",
      args = function() {
        list(
          pings = pings_tier(),
          windows = fresh(windows_all),
          carry = "gear",
          speed_min = "s1",
          speed_max = "s2"
        )
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "one row per (ping, window) match; `windows` from rb_create_record_windows()."
    ),
    list(
      fn = "rb_add_record",
      main = "pings",
      args = function() {
        list(
          pings = pings_tier(),
          matches = fresh(matches_tier),
          carry = "gear"
        )
      },
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "`matches` from rb_link_record_pings()."
    ),
    list(
      fn = "rb_link_record_events",
      main = "pings",
      args = function() {
        pings <- tibble::tibble(
          vid = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L),
          time = h(c(-1, 0.25, 3.5, 6, 11, 11, 13, 0)),
          .rid = 1:8
        )
        list(pings = pings, records = records())
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "one row per record event time; vid 2 has two pings at the same time."
    ),
    list(
      fn = "rb_create_record_pings",
      main = "records",
      args = function() {
        r <- tibble::tibble(
          id = 1:3,
          vid = c(7L, 7L, 8L),
          start = h(c(0, 2, 0)),
          end = h(c(1, 2, 3)),
          lon1 = c(-22, -21, -20),
          lat1 = c(64, 64, 65),
          lon2 = c(-22.2, NA, -20),
          lat2 = c(64.05, NA, 65.3),
          .rid = 1:3
        )
        list(records = r, key = "id", interval_s = 900)
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "synthesised pings along each record's line."
    ),
    list(
      fn = "rb_link_record_sets",
      main = "hauls",
      args = function() {
        line <- function(t0, n, lat0 = 64, dlat = 0.05) {
          tibble::tibble(
            vid = 1L,
            time = t0 + (0:(n - 1)) * 120,
            lon = -22 + 0.0005 * (0:(n - 1)) %% 2,
            lat = lat0 + dlat * (0:(n - 1)) / (n - 1)
          )
        }
        d1 <- utc("2024-03-01 06:00")
        d2 <- utc("2024-03-02 12:00")
        steam <- tibble::tibble(
          vid = 1L,
          time = d1 + 3600 * (1:20),
          lon = -21,
          lat = 64.5
        )
        lone <- tibble::tibble(
          vid = 1L,
          time = d2 + 5 * 3600,
          lon = -19,
          lat = 63
        )
        pings <- dplyr::bind_rows(line(d1, 16), steam, line(d2, 31), lone)
        hauls <- tibble::tibble(
          id = 1:2,
          vid = 1L,
          start = c(d2, d2 + 5 * 3600 - 1),
          end = c(d2 + 3600, d2 + 5 * 3600 + 1),
          .rid = 1:2
        )
        list(hauls = hauls, pings = pings, key = c("id", ".rid"))
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "one row per haul, but only key, vid and the set columns (start and end are dropped): .rid is in `key` so it comes out and the order can be checked; same values as key = \"id\". Haul 1 finds its set, haul 2 none. `pings` may be lazy too."
    ),
    list(
      fn = "rb_create_record_stretches",
      main = "pings",
      args = function() {
        list(pings = stretch_pings(), records = tows(), port_call = "port_call")
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "457 pings, one a minute; `records` (tows) may be lazy too."
    ),
    list(
      fn = "rb_link_record_stretches",
      main = "records",
      args = function() {
        list(
          records = tows(),
          stretches = fresh(stretches),
          key = c(".sid", "schema", ".rid")
        )
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "one row per record, but only key, vid and the stretch columns (t2 and t3 are dropped): .rid is in `key` so it comes out and the order can be checked; same values as the default key. `stretches` from rb_create_record_stretches()."
    ),

    # Step 4: fishing ------------------------------------------------------------------------------------------------
    list(
      fn = "rb_add_fishing",
      main = "pings",
      args = function() {
        list(
          pings = pings_tow(),
          matches = fresh(matches_tow),
          speed_min = "s1",
          speed_max = "s2",
          window_s = 600
        )
      },
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "method window (the default), ±10 min speed; `matches` from rb_link_record_pings()."
    ),
    list(
      fn = "rb_add_fishing",
      main = "pings",
      args = function() {
        pings <- tibble::tibble(
          vid = 1L,
          time = h(c(1, 2, 12, 13, 14, 15)),
          speed = c(3, 9, 3, 9, 3, NA),
          lo = c(2, 2, 2, 2, NA, 2),
          hi = c(5, 5, 5, 5, NA, 5),
          .rid = 1:6
        )
        list(
          pings = pings,
          method = "speed",
          ping_speed_min = "lo",
          ping_speed_max = "hi"
        )
      },
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "method speed: no matches, the range on each ping."
    ),

    # Step 5: effort -------------------------------------------------------------------------------------------------
    list(
      fn = "rb_add_effort",
      main = "pings",
      args = function() {
        pings <- tibble::tibble(
          vid = c(1L, 1L, 1L, 1L, 2L, 2L),
          voyage = c(1L, 1L, 2L, 2L, 1L, 1L),
          time = t0 + c(0, 600, 1200, 12000, 0, 2),
          speed = c(3, 3, 4, 10, 2, 2),
          fishing = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
          width = c(60, 60, 60, 60, NA, NA),
          .rid = 1:6
        )
        list(
          pings = pings,
          vessels = tibble::tibble(vid = 1:2, kw = c(1000, 500)),
          by = "voyage",
          width = "width"
        )
      },
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "`vessels` may be lazy too."
    ),

    # Step 6: catch --------------------------------------------------------------------------------------------------
    list(
      fn = "rb_add_catch_units",
      main = "events",
      args = function() {
        events <- tibble::tibble(
          id = c("haul", "d1", "d2", "d2", "L", "r1", "r2"),
          vid = 1L,
          start = hc(c(6, 0, 0, 30, NA, 48, 48)),
          end = hc(c(9, 24, 24, 32, 48, 72, 72)),
          level = c(1L, 2L, 2L, 2L, 4L, 2L, 2L),
          resolution = c("haul", "day", "day", "day", "landing", "day", "day"),
          rect = c(NA, NA, NA, NA, NA, "54D9", "54E0"),
          .rid = 1:7
        )
        list(events = events, key = "id", match_by = "rect")
      },
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "a landing's missing start is filled (start_filled)."
    ),
    list(
      fn = "rb_share_catch",
      main = "pings",
      args = function() {
        pings <- tibble::tibble(
          vid = 1L,
          time = hc(c(1, 2, 3, 7, 8, 12, 13, 30, 31, 60)),
          fishing = c(
            TRUE,
            TRUE,
            FALSE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE,
            TRUE
          ),
          hours = c(1, 3, 1, 1, 1, 2, 2, 1, 1, 1),
          .rid = 1:10
        )
        catch <- tibble::tibble(
          id = c("h1", "h1", "h2", "d1", "d1b", "t1", "zz"),
          species = c("cod", "had", "cod", "cod", "cod", "cod", "cod"),
          kg = c(400, 40, 50, 60, 30, 10, 7)
        )
        list(
          pings = pings,
          units = fresh(catch_units),
          catch = catch,
          key = "id"
        )
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "method units: one row per (fishing ping, unit, species), plus catch with no ping (time NA); ping columns other than vid/time are dropped."
    ),
    list(
      fn = "rb_share_catch",
      main = "pings",
      args = function() {
        ep <- tibble::tibble(
          .pid = 1:4,
          VE_COU = "IS",
          VE_REF = "1",
          FT_REF = "T1",
          time = utc("2024-06-01 06:00") + (0:3) * 3600,
          ir = "54D9",
          state = c(1L, 1L, 0L, 1L),
          .rid = 1:4
        )
        ee <- tibble::tibble(
          .eid = 1L,
          VE_COU = "IS",
          VE_REF = "1",
          FT_REF = "T1",
          date = as.Date("2024-06-01"),
          LE_RECT = "54D9",
          LE_KG = 90,
          LE_EURO = 9
        )
        list(pings = ep, units = ee, method = "eflalo")
      },
      shape = "same",
      lazy = FALSE,
      sf = FALSE,
      note = "method eflalo: `units` is the eflalo events table; checks column types with pings$col, so data frames only."
    ),

    # Track utilities ------------------------------------------------------------------------------------------------
    list(
      fn = "rb_interpolate_track",
      main = "x",
      args = function() {
        list(x = fresh(interp$events), track = fresh(interp$track))
      },
      shape = "same",
      lazy = TRUE,
      lazy_also = "track",
      sf = FALSE,
      note = "`track` must match x's class: a lazy x needs a lazy track on the same connection (a data-frame track with a lazy x fails). Event group 5 has no track (NA)."
    ),
    list(
      fn = "rb_link_vessel_tracks",
      main = "x",
      args = function() {
        t0 <- utc("2024-03-01 06:00")
        x <- vt_track(1L, t0, 120, -22, 66.5)
        names(x)[1] <- "unit"
        y <- dplyr::bind_rows(
          vt_track(111, t0 + 20, 120, -22, 66.5 + 0.0005),
          vt_track(222, t0 + 30, 120, -22, 66.5 + 0.009),
          vt_track(444, t0, 6, -22, 66.5, step_s = 1200),
          vt_track(999, t0, 120, -22, 67)
        )
        names(y)[1] <- "mmsi"
        list(x = add_rid(x), y = y, id_x = "unit", id_y = "mmsi")
      },
      shape = "new",
      lazy = TRUE,
      sf = TRUE,
      note = "one row per (unit, period, mmsi) score; `y` may be lazy too."
    ),
    list(
      fn = "rb_calc_distance",
      main = "x",
      args = function() {
        d <- tibble::tibble(
          lon = c(-21.93, -22.50, NA, -21.0),
          lat = c(64.15, 63.90, 64.00, 64.5),
          port_lon = -21.94,
          port_lat = 64.15,
          .rid = 1:4
        )
        list(x = d, lon2 = quote(port_lon), lat2 = quote(port_lat))
      },
      shape = "vector",
      lazy = FALSE,
      sf = FALSE,
      note = "data-frame form returns a numeric vector (metres), one per row; refuses a lazy table by design."
    ),
    list(
      fn = "rb_calc_duration",
      main = "x",
      args = function() {
        d <- tibble::tibble(
          T1 = utc(c("2007-06-04 14:03:36", "2007-06-08 07:40:43", NA)),
          T2 = utc(c("2007-06-07 05:25:45", "2007-06-09 10:02:21", NA)),
          .rid = 1:3
        )
        list(x = d, from = quote(T1), to = quote(T2))
      },
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "table form; from/to are tidy-eval column names (passed as symbols)."
    ),
    list(
      fn = "rb_add_timestamp",
      main = "data",
      args = function() {
        d <- tibble::tibble(
          date = c("14/09/2025", "15/09/2025", "16/09/2025"),
          clock = c("15:30:00", "00:00:00", "23:59:59"),
          .rid = 1:3
        )
        list(data = d, date = quote(date), time = quote(clock), tz = "UTC")
      },
      shape = "same",
      lazy = FALSE,
      sf = FALSE,
      note = "refuses a lazy table by design (\"collect a lazy table first\")."
    ),
    list(
      fn = "rb_summarise_track",
      main = "d",
      args = function() {
        d <- tibble::tibble(
          time = utc("2022-01-01 00:00:00") + c(0, 600, 1200, 1800),
          speed = c(0, 5, 10, 4),
          dist = c(NA, 100, 200, 150),
          dura = c(NA, 600, 600, 600),
          voyage_id = c(1, 1, 2, 2),
          port_id = c(1, 1, 2, NA),
          .rid = 1:4
        )
        list(d = d)
      },
      shape = "new",
      lazy = TRUE,
      sf = FALSE,
      note = "one summary row (per group); drops every input column by design. Lazy: dbplyr warns once that NAs are removed."
    ),
    list(
      fn = "rb_peek",
      main = "d",
      args = function() {
        d <- tibble::tibble(
          .rid = 1:12,
          speed = c(3, 3, 4, 30, 3, 3, 3, 3, 3, 25, 3, 3)
        )
        list(d = d, what = quote(speed), threshold = 20)
      },
      shape = "subset",
      lazy = TRUE,
      sf = FALSE,
      note = "selects by .rid +-2 of the rows over the threshold, so .rid must be the row number in sorted order; adds `whacky`."
    ),

    # Eflalo / MFRI logbook helpers ----------------------------------------------------------------------------------
    list(
      fn = "rb_sum_across",
      main = "d",
      args = function() {
        d <- tibble::tibble(
          id = 1:3,
          LE_KG_COD = c(1, 2, 3),
          LE_KG_HAD = c(0, 1, 0),
          LE_KG_HER = c(2, 1, 2),
          .rid = 1:3
        )
        list(d = d, prefix = "LE_KG")
      },
      shape = "same",
      lazy = FALSE,
      sf = FALSE,
      note = "drops the summed columns (remove = TRUE)."
    ),
    list(
      fn = "rb_check_trip_crosses_year",
      main = "eflalo",
      args = function() {
        e <- tibble::tibble(
          FT_DDAT = c("30/12/2022", "01/01/2023", "05/01/2023"),
          FT_DTIME = c("23:50:00", "10:00:00", "06:00:00"),
          FT_LDAT = c("31/12/2022", "01/01/2023", "06/01/2023"),
          FT_LTIME = c("10:30:00", "12:00:00", "18:00:00"),
          .rid = 1:3
        )
        list(eflalo = e)
      },
      shape = "same",
      lazy = FALSE,
      sf = FALSE,
      note = "returns eflalo invisibly with FT_DDATIM and FT_LDATIM added; no trip crosses the year, so no warning. Refuses non-data-frames."
    ),
    list(
      fn = "rb_cap_effort",
      main = "lb",
      args = function() list(lb = lb()),
      shape = "same",
      lazy = FALSE,
      sf = FALSE,
      note = "Icelandic gids; sorts by (vid, t1) by design; lubridate on the times."
    ),
    list(
      fn = "rb_standardise_gear_meshsize",
      main = "lb",
      args = function() list(lb = lb()),
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "Icelandic gids; overwrites `mesh` with `mesh_min` where gid is 7."
    ),
    list(
      fn = "rb_fill_gear_width",
      main = "lb",
      args = function() list(lb = lb()),
      shape = "same",
      lazy = TRUE,
      sf = FALSE,
      note = "Icelandic gids; a left_join to a per-gear median."
    ),

    # Spatial --------------------------------------------------------------------------------------------------------
    list(
      fn = "rb_detect_in_polygons",
      main = "points",
      args = function() {
        list(points = fresh(dk_points), polygons = fresh(dk_polygons))
      },
      shape = "vector",
      lazy = FALSE,
      sf = TRUE,
      sf_only = TRUE,
      note = "main must be sf (or sfc) POINTs in the polygons' CRS; returns a logical vector, one per point."
    ),
    list(
      fn = "rb_keep_in_polygons",
      main = "points",
      args = function() {
        list(points = fresh(dk_points), polygons = fresh(dk_polygons))
      },
      shape = "subset",
      lazy = FALSE,
      sf = TRUE,
      sf_only = TRUE,
      note = "main must be sf (or sfc) POINTs; a plain data frame, data.table or lazy table is not meaningful."
    ),
    list(
      fn = "rb_drop_in_polygons",
      main = "points",
      args = function() {
        list(points = fresh(dk_points), polygons = fresh(dk_polygons))
      },
      shape = "subset",
      lazy = FALSE,
      sf = TRUE,
      sf_only = TRUE,
      note = "main must be sf (or sfc) POINTs; a plain data frame, data.table or lazy table is not meaningful."
    ),
    list(
      fn = "rb_map_tiles_table",
      main = "x",
      args = function() {
        x <- tibble::tibble(
          lon = c(12.5, 12.6, 12.7, 10.2),
          lat = c(56.0, 56.0, 56.1, 57.5),
          gear = c("OTB", "GNS", "OTB", "PTM"),
          .rid = 1:4
        )
        list(x = x, lon = "lon", lat = "lat", properties = c("gear", ".rid"))
      },
      shape = "function",
      lazy = TRUE,
      sf = TRUE,
      note = "returns a tile function (attributes count, table), not a table; leaves a TEMP table rb_tiles_<n> in the cached DuckDB connection and LOADs (or INSTALLs) the spatial extension."
    )
  )
})

for (case in etiquette_cases) {
  method <- case$args()$method
  case$label <- if (is.null(method)) "" else sprintf(" [method %s]", method[1])
  test_that(
    sprintf(
      "%s%s keeps the construct, the row order and the session",
      case$fn,
      case$label
    ),
    {
      expect_etiquette(case)
    }
  )
}
