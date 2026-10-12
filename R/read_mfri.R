#' Read Icelandic harbour polygons
#'
#' Harbour polygons with harbour ids (`hid`), read from MFRI's public FTP
#' site.
#'
#' @return An sf tibble with `hid` and `geometry`.
#' @export
#'
rb_read_harbours_mfri <- function() {
  sf::read_sf("ftp://ftp.hafro.is/pub/data/shapes/harbours.gpkg")
}

#' Read MFRI survey tracks
#'
#' AIS/VMS tracks of the Icelandic trawl survey vessels, read from MFRI's
#' public FTP site: `vid` (vessel id), `cruise_id` (cruise id) and the usual
#' track variables (`time`, `lon`, `lat`, `speed`, `heading`).
#'
#' @return A tibble.
#' @export
#'
rb_read_survey_tracks_mfri <- function() {
  readr::read_csv(
    "ftp://ftp.hafro.is/pub/data/csv/is_survey-tracks.csv",
    show_col_types = FALSE
  )
}


#' Read MFRI survey stations
#'
#' The tows of the Icelandic trawl surveys, read from MFRI's public FTP site:
#' `cruise_id` (cruise id), `station_id` (unique station id), `station_nr`
#' (a non-unique station number), `date`, `vid` (vessel id), `sclass` (sample
#' class: 30 the spring survey, 35 the autumn survey) and the start (`lon1`,
#' `lat1`, `t1`) and end (`lon2`, `lat2`, `t2`) of each tow as reported by
#' the captain.
#'
#' @return A tibble.
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

#' Read vessel position trails from the MFRI database
#'
#' Decoded positions from the `stk.trail` table (longitude, latitude and
#' heading in degrees, speed in knots), for all vessels or, via the
#' mobile-id–vessel mapping, a given set.
#'
#' @param con An Oracle connection (see the `omar` package).
#' @param vid Vessel ids; if missing, all trails.
#'
#' @return A lazy table with `mid`, `time`, `lon`, `lat`, `speed`, `heading`,
#'   `hid`, `io`, `rectime`, and `vid` where `vid` was given.
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
