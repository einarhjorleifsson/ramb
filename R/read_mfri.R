#' rb_read_harbours_mfri
#'
#' A conenient wrapper that read a dataframe of Icelandic harbours (hid) with
#' associated polygon geometry from an open ftp-site.
#'
#' @return An sf-tibble
#' @export
#'
rb_read_harbours_mfri <- function() {
  sf::read_sf("ftp://ftp.hafro.is/pub/data/shapes/harbours.gpkg")
}

#' rb_read_survey_tracks_mfri
#'
#' A conenient wrapper that read a dataframe of Icelandic trawl vessel ais/vms
#' data. The variables are \code{vid} (vessel idendification numner), \code{cruise_id}
#' (cruise identification number) and the usual ais/vms variables (\code{time},
#' \code{lon}, \code{lat}, \code{speed}, \code{heading}).
#'
#' @return A tibble
#' @export
#'
rb_read_survey_tracks_mfri <- function() {
  readr::read_csv(
    "ftp://ftp.hafro.is/pub/data/csv/is_survey-tracks.csv",
    show_col_types = FALSE
  )
}


#' rb_read_survey_stations_mfri
#'
#' A conenient wrapper that read a dataframe of Icelandic trawl vessel survey
#' tow data. The variables are \code{cruise_id} (cruise identification number),
#' \code{station_id} (unique station identification numner), \code{station_nr}
#' (a non-unique station number), \code{date} (date the tow was taken),
#' \code{vid} (vessel identification number), \code{sclass} (sample class, 30
#' is the annual spring survey, 35 the annual fall survey) and time-space
#' informations: start (\code{lon1}, \code{lat1}, \code{t1}) and end
#' (\code{lon2}, \code{lat2}, \code{t2}) of towing as reported by the vessel
#' captain.
#'
#' @return A tibble
#' @export
#'
rb_read_survey_stations_mfri <- function() {
  readr::read_csv(
    "ftp://ftp.hafro.is/pub/data/csv/is_survey-stations.csv",
    show_col_types = FALSE
  )
}
# Query: mapping between mobileid and vessel id
.rb_mobile_vid <- function(con) {
  omar::tbl_mar(con, "ops$einarhj.mobile_vid") |>
    dplyr::mutate(
      t1 = to_date(t1, "YYYY:MM:DD"),
      t2 = to_date(t2, "YYYY:MM:DD")
    )
}

# Query: the stk.trail table with decoded positions and ramb column names
.rb_stk_trail <- function(con, vid) {
  omar::tbl_mar(con, "stk.trail") |>
    dplyr::mutate(
      lon = poslon * 180 / pi,
      lat = poslat * 180 / pi,
      heading = heading * 180 / pi,
      speed = speed * 3600 / 1852
    ) |>
    dplyr::select(
      mid = mobileid,
      time = posdate,
      lon,
      lat,
      speed,
      heading,
      hid = harborid,
      io = in_out_of_harbor,
      rectime = recdate
    )
}

#' Read vessel trails
#'
#' @param con oracle connection
#' @param vid vessel id, if missing the function returns all trails
#'
#' @return a tibble
#' @export
#'
rb_read_trail_mfri <- function(con, vid) {
  if (!missing(vid)) {
    VID <- vid
    q <-
      .rb_mobile_vid(con) |>
      dplyr::filter(!is.na(pings) | link == "manual") |>
      dplyr::select(mid, vid, t1, t2) |>
      dplyr::filter(vid %in% VID) |>
      dplyr::left_join(.rb_stk_trail(con), by = "mid") |>
      dplyr::filter(time >= t1, time <= t2) |>
      dplyr::select(-c(t1, t2))
  } else {
    q <-
      .rb_stk_trail(con)
  }
  return(q)
}

# Read and merge stk, logbook and PAM trails for given vessels and years.
# Legacy: kept for the old MFRI workflow; rb_read_trail_mfri() is the reader.
.rb_read_trails <- function(con, VID, YEARS, use_PAM = FALSE) {
  D1 <- paste0(min(YEARS), "-01-01")
  D2 <- paste0(max(YEARS), "-12-31")

  stk <-
    omar::tbl_mar(con, "ops$einarhj.MID_VID_ICELANDIC_20220418") |>
    dplyr::filter(vid %in% VID) |>
    dplyr::select(mid, vid) |>
    dplyr::left_join(omar::stk_trail(con), by = "mid") |>
    dplyr::filter(
      time >= to_date(D1, "YYYY:MM:DD"),
      time <= to_date(D2, "YYYY:MM:DD")
    ) |>
    dplyr::collect(n = Inf) |>
    dplyr::select(vid, time, lon, lat, speed, heading) |>
    dplyr::mutate(source = "stk") |>
    dplyr::arrange(time) |>
    dplyr::distinct(time, .keep_all = TRUE)

  if (use_PAM) {
    pam <-
      PAM |>
      dplyr::filter(vid %in% VID) |>
      dplyr::filter(time >= lubridate::ymd(D1), time <= lubridate::ymd(D2)) |>
      dplyr::mutate(source = "pam") |>
      dplyr::distinct(time, .keep_all = TRUE)
  } else {
    pam <-
      omar::mmsi_icelandic_registry(con) |>
      dplyr::select(vid, mmsi) |>
      dplyr::filter(vid %in% VID) |>
      dplyr::left_join(omar::pame_trail(con), by = "mmsi") |>
      dplyr::filter(
        time >= to_date(D1, "YYYY:MM:DD"),
        time <= to_date(D2, "YYYY:MM:DD")
      ) |>
      dplyr::select(-c(mmsi, imo, vessel, flag)) |>
      dplyr::collect(n = Inf) |>
      # what happens if nrow == 0?
      dplyr::mutate(source = "pam") |>
      dplyr::mutate(speed = .rb_ms2kn(speed))
  }

  lbs <-
    omar::lb_trail(con) |>
    dplyr::filter(vid %in% VID) |>
    dplyr::filter(
      time >= to_date(D1, "YYYY:MM:DD"),
      time <= to_date(D2, "YYYY:MM:DD")
    ) |>
    dplyr::collect(n = Inf) |>
    dplyr::mutate(lon = NA_real_, lat = NA_real_, source = "lgs") |>
    dplyr::arrange(time) |>
    dplyr::distinct(time, .keep_all = TRUE) |>
    dplyr::mutate(source = "lbs")

  ais <-
    dplyr::bind_rows(stk, lbs, pam) |>
    dplyr::arrange(vid, time) |>
    dplyr::filter(dplyr::between(lon, -60, 60), dplyr::between(lat, 50, 80))

  if (nrow(ais > 0)) {
    ais <-
      ais |>
      dplyr::mutate(.rid = 1:dplyr::n()) |>
      dplyr::select(.rid, dplyr::everything())
  }

  return(ais)
}
