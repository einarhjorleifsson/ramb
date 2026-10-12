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
