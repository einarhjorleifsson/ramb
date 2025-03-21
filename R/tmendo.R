# https://academic.oup.com/icesjms/article/81/2/390/7516127

#' Yet another speed filter
#' 
#' The input dataframe has to contain the following variables: x, y, time. device_id and seq.
#' The x and the y are coordates in units of meters.
#' 
#' This function is fast but it chops off equal amount of valid points as invalid
#' points. Function rb_whacky_speed is more conservative, albeit slower. The latter
#' also does not filter the data.
#'
#' @param df A dataframe containing variables named 
#' @param speed_filter speed in knots
#'
#' @return A filtered dataframe with a bunch of stuff
#' @export
#'
rb_whacky_speed_mendo <- function(df, speed_filter = 25) {
  
  ms2knots = 1.9438 #(m/s)/knots
  
  df <- 
    df |> 
    dplyr::group_by(device_id) |> 
    dplyr::mutate(dx = c(0,abs(diff(x))),
                  dy = c(0,abs(diff(y)))) |> 
    dplyr::mutate(dt = c(as.numeric(time - dplyr::lag(time),units="secs")),
           dd = sqrt(dx^2 + dy^2),
           speed = dd / dt * ms2knots) |> 
    dplyr::ungroup()
  
  repeat {
    subset <-
      df |> 
      dplyr::filter(speed > speed_filter)
    sel <- factor(subset$seq)
    nrows <- length(sel)
    if (nrows==0) {
      break
    }  else {
      df <- df[!df$seq %in% sel,]
    }
    df <-
      df |> 
      dplyr::group_by(device_id) |> 
      dplyr::mutate(dx = c(0, abs(diff(x))),
                    dy = c(0, abs(diff(y)))) |> 
      dplyr::mutate(dt = c(as.numeric(time - dplyr::lag(time), units="secs"))) |> 
      dplyr::mutate(dd = sqrt(dx^2 + dy^2)) |> 
      dplyr::mutate(speed = dd / dt * ms2knots) |> 
      # add so data returned to user is 
      dplyr::ungroup()
  }
  return(df)
}