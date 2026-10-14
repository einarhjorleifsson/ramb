# Towing stretches and their pairing with declared tows: a port of fishycode's TODO 56 prototype (q2all.R). The
# port was checked on the Icelandic data (fishycode TODO 59: June 2023 reproduced tow for tow); here, the rules
# on a small track, and data frame = lazy table.

t0 <- as.POSIXct("2024-03-01 06:00:00", tz = "UTC")
m <- function(x) t0 + x * 60
# one ping a minute: steaming 06:00-06:59; towing 07:00-08:59 with a spike at 07:30; a slow turn 09:00-09:04;
# towing 09:05-10:59; steaming 11:00-11:59; towing 12:00-12:30; nothing for 45 min; towing 13:15-13:45;
# steaming 13:46-13:59; towing 14:00-14:10 (too short); then 14:11-14:20 in port, slow
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
pings <- tibble::tibble(
  vid = 1L,
  time = m(mins),
  speed = spd,
  speed_min = 2,
  speed_max = 5,
  stay = ifelse(mins > 490, 7L, NA_integer_)
)
tows <- tibble::tibble(
  .sid = 1:5,
  schema = "a",
  vid = 1L,
  t2 = m(c(65, 190, 362, 436, 436)),
  t3 = m(c(175, 295, 388, 464, 464))
)

test_that("stretches: the windowed speed, gaps, the shortest dropped, the prior's cut; data frame = lazy table", {
  skip_if_not_installed("duckdbfs")
  s <- rb_cut_record_stretches(pings, records = tows, stay = "stay")
  sl <- dplyr::collect(rb_cut_record_stretches(
    duckdbfs::as_dataset(pings),
    records = duckdbfs::as_dataset(tows),
    stay = "stay"
  ))
  expect_equal(as.data.frame(sl), as.data.frame(s), ignore_attr = TRUE)
  expect_equal(s$s_start, m(c(60, 181, 360, 435)))
  expect_equal(s$s_end, m(c(179, 299, 390, 465)))
  expect_equal(s$opened_by, c("speed", "record", "speed", "gap"))
  expect_equal(s$closed_by, c("record", "speed", "gap", "speed"))
  expect_equal(s$n_pings, c(120L, 119L, 31L, 31L))
  # the slowest ping of the turn (09:00) is cut out; the spike at 07:30 does not break the tow
  expect_false(any(s$s_start <= m(180) & s$s_end >= m(180)))
})

test_that("stretches: own speed and no prior split at the spike and the turn", {
  skip_if_not_installed("duckdbfs")
  s <- rb_cut_record_stretches(pings, window_s = 0, stay = "stay")
  expect_equal(s$s_start, m(c(60, 91, 185, 360, 435)))
  expect_equal(s$s_end, m(c(89, 179, 299, 390, 465)))
  # without the prior, the windowed speed smooths the turn away: one stretch holds both tows
  s10 <- rb_cut_record_stretches(pings, stay = "stay")
  expect_equal(s10$s_start[1:2], m(c(60, 360)))
  expect_equal(s10$s_end[1], m(299))
  # dip_s joins stretches closer than it
  sd <- rb_cut_record_stretches(pings, window_s = 0, dip_s = 120, stay = "stay")
  expect_equal(sd$s_start[1:2], m(c(60, 185)))
})

test_that("pairing: in order, one to one; twins: the lower key keeps the stretch; data frame = lazy table", {
  skip_if_not_installed("duckdbfs")
  s <- rb_cut_record_stretches(pings, records = tows, stay = "stay")
  p <- rb_link_record_stretches(tows, s)
  pl <- dplyr::collect(rb_link_record_stretches(
    duckdbfs::as_dataset(tows),
    duckdbfs::as_dataset(s)
  ))
  pl <- pl[order(pl$.sid), names(p)]
  expect_equal(
    as.data.frame(pl),
    as.data.frame(p[order(p$.sid), ]),
    ignore_attr = TRUE
  )
  expect_equal(p$stretch_id, c(1L, 2L, 3L, 4L, NA))
  expect_equal(p$dt_start_s, c(-300, -540, -120, -60, NA))
  expect_equal(p$n_candidates, c(2L, 2L, 1L, 1L, 1L))
  expect_equal(p$stretch_lost, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_false(anyDuplicated(p$stretch_id[!is.na(p$stretch_id)]) > 0)
})

test_that("pairing: on equal widened overlap, the tow that overlaps the stretch keeps it", {
  # a 25-minute stretch inside the widened windows of two short tows; only the first overlaps it
  st <- tibble::tibble(vid = 1L, stretch_id = 1L, s_start = m(0), s_end = m(25))
  r <- tibble::tibble(
    .sid = 1:2,
    schema = "a",
    vid = 1L,
    t2 = m(c(1, 26)),
    t3 = m(c(6, 31))
  )
  p <- rb_link_record_stretches(r, st)
  expect_equal(p$stretch_id, c(1L, NA))
  expect_equal(p$overlap_s, c(1500, NA))
})

test_that("missing inputs: no stretches, no usable records, no pings in range", {
  skip_if_not_installed("duckdbfs")
  s0 <- rb_cut_record_stretches(transform(
    pings,
    speed_min = 20,
    speed_max = 30
  ))
  expect_equal(nrow(s0), 0)
  expect_named(
    s0,
    c(
      "vid",
      "stretch_id",
      "s_start",
      "s_end",
      "n_pings",
      "opened_by",
      "closed_by"
    )
  )
  p0 <- rb_link_record_stretches(tows, s0)
  expect_equal(nrow(p0), nrow(tows))
  expect_true(all(is.na(p0$stretch_id)))
  expect_true(all(p0$n_candidates == 0))
  bad <- transform(tows, t3 = t2)
  pb <- rb_link_record_stretches(bad, rb_cut_record_stretches(pings))
  expect_true(all(is.na(pb$stretch_id) & pb$n_candidates == 0))
  expect_error(rb_cut_record_stretches(pings[, -3]), "pings lacks: speed")
})
