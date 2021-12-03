# argosfilter functions - tease out algorithm, "export" more details


#' rb_rms
#' 
#' The root mean square of value and the 2 upstream and 2 downstream values
#'
#' rb_rms_tbl
#'
#' @param d A tibble containing lon, lat and time
#'
#' @return A tibble
#' @export
#'
rb_rms_tbl <- function(d) {
  d %>% 
    # distance
    mutate(.d_2 = geo::arcdist(lat, lon, lag(lat, 2),  lag(lon, 2),  scale = "km") * 1e3,
           .d_1 = geo::arcdist(lat, lon, lag(lat, 1),  lag(lon, 1),  scale = "km") * 1e3,
           .d1 =  geo::arcdist(lat, lon, lead(lat, 1), lead(lon, 1), scale = "km") * 1e3,
           .d2 =  geo::arcdist(lat, lon, lead(lat, 2), lead(lon, 2), scale = "km") * 1e3,
           .vd = sqrt((.d_2^2 + .d_1^2 + .d1^2 + .d2^2)/4)) %>% 
    # time steps
    mutate(.t_2 = as.numeric(difftime(time, lag(time, 2),  units = "secs") + 0.01),
           .t_1 = as.numeric(difftime(time, lag(time, 1),  units = "secs") + 0.01),
           .t1 =  as.numeric(difftime(time, lead(time, 1), units = "secs") + 0.01),
           .t2 =  as.numeric(difftime(time, lead(time, 2), units = "secs") + 0.01),
           .vt =  sqrt((.t_2^2 + .t_1^2 + .t1^2 + .t2^2)/4)) %>% 
    # speed
    mutate(.s_2 = .d_2 / .t_2,
           .s_1 = .d_1 / .t_1,
           .s1 = .d1 / .t1,
           .s2 = .d2 / .t2,
           .vs = sqrt((.s_2^2 + .s_1^2 + .s1^2 + .s2^2)/4)) %>% 
    select(-c(.d_2, .d_1, .d1, .d2, .t_2, .t_1, .t1, .t2, .s_2, .s_1, .s1, .s2))
}

# as a above, just speed


rb_rms_speed <- function(time, lon, lat) {
  rb_vmask0 <- function(time, lon, lat, laglead = 1L) {
    if(laglead < 0L) {
      laglead <- -laglead
      x <- 
        geo::arcdist( {{lat}}, {{lon}}, dplyr::lag( {{lat}} , laglead),  dplyr::lag( {{lon}}, laglead),  scale = "km") * 1e3 /
        as.numeric(difftime( {{time}} , dplyr::lag( {{time}}, laglead),  units = "secs") + 0.01)
    }
    if(laglead > 0L) {
      x <- 
        geo::arcdist(lat, lon, dplyr::lead(lat, laglead),  dplyr::lead(lon, laglead),  scale = "km") * 1e3 /
        as.numeric(difftime(time, dplyr::lead(time, laglead),  units = "secs") + 0.01)
    }
    return(x)
  }
  sqrt(
    (
      rb_vmask0( {{time}}, {{lon}}, {{lat}}, -2) +
        rb_vmask0( {{time}}, {{lon}}, {{lat}}, -1) +
        rb_vmask0( {{time}}, {{lon}}, {{lat}}, 1) +
        rb_vmask0( {{time}}, {{lon}}, {{lat}}, 2)
    ) / 4
  )
}
