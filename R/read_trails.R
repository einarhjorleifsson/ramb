#' Read all trails
#'
#' @param con oracle connection
#' @param VID vessel
#' @param YEARS years
#'
#' @return a tibble
#' @export
#'
rb_read_trails <- function(con, VID, YEARS) {
  
  D1 <- paste0(min(YEARS), "-01-01")
  D2 <- paste0(max(YEARS), "-12-31")
  
  stk <- 
    omar::tbl_mar(con, "ops$einarhj.MID_VID_ICELANDIC_20220418") |> 
    dplyr::filter(vid %in% VID) |> 
    dplyr::select(mid, vid) |> 
    dplyr::left_join(omar::stk_trail(con),
                     by = "mid") %>%
    dplyr::filter(
      time >= to_date(D1, "YYYY:MM:DD"),
      time <= to_date(D2, "YYYY:MM:DD")) %>% 
    dplyr::collect(n = Inf) %>%
    dplyr::select(vid, time, lon, lat, speed, heading) %>%
    dplyr::mutate(source = "stk") %>% 
    dplyr::arrange(time) %>% 
    dplyr::distinct(time, .keep_all = TRUE)
  
  pam <- 
    omar::mmsi_vessel(con) |> 
    dplyr::select(vid, mmsi) |> 
    dplyr::filter(vid %in% VID) |> 
    dplyr::left_join(omar::pame_trail(con),
                     by = "mmsi") |> 
    dplyr::filter(time >= to_date(D1, "YYYY:MM:DD"),
                  time <= to_date(D2, "YYYY:MM:DD")) %>% 
    dplyr::collect(n = Inf)
  if(nrow(pam) > 10) {
    pam <- 
      pam %>% 
      dplyr::select(vid, 
                    time,
                    lon,
                    lat) %>% 
      dplyr::arrange(time) %>% 
      dplyr::mutate(source = "pam",
                    speed = traipse::track_speed(lon, lat, time),
                    speed = rb_ms2kn(speed)) %>% 
      # first record has speed as NA
      tidyr::fill(speed, .direction = "up") |> 
      dplyr::distinct(time, .keep_all = TRUE)
  }
  
  
  lbs <- 
    omar::lb_trail(con) |> 
    dplyr::filter(vid %in% VID) |> 
    dplyr::filter(time >= to_date(D1, "YYYY:MM:DD"),
                  time <= to_date(D2, "YYYY:MM:DD")) |> 
    dplyr::collect(n = Inf) %>% 
    dplyr::mutate(lon = NA_real_,
                  lat = NA_real_,
                  source = "lgs") |> 
    dplyr::arrange(time) %>% 
    dplyr::distinct(time, .keep_all = TRUE)
  ais <-
    dplyr::bind_rows(stk,
                     lbs,
                     pam)
  if(nrow(ais) > 100) {
    ais <- 
      ais %>% 
    dplyr::arrange(vid, time) %>% 
    dplyr::mutate(.rid = 1:dplyr::n()) %>% 
    dplyr::select(.rid, dplyr::everything()) |> 
    dplyr::group_by(vid) %>% 
    # NA approximated, note not operating on a sphere, should not matter because distance small
    dplyr::mutate(lon = approx(time, lon, time, method = "linear", rule = 1, f = 0, ties = mean)$y,
                  lat = approx(time, lat, time, method = "linear", rule = 1, f = 0, ties = mean)$y) |> 
    dplyr::distinct(vid, time, .keep_all = TRUE) |> 
    dplyr::ungroup() |> 
    dplyr::filter(between(lon, -60, 60),
                  between(lat,  50, 80))
  }
  return(ais)
}
