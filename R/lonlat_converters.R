#' Get ICES rectangles from coordinates
#' 
#' Get the ICES rectangle from any lon-lat position. If position outside
#' ICES region return NA's. 
#' 
#' @param lon Longitude vector
#' @param lat Latitude vector
#' @param safe Boolean (default TRUE), calculates mid-point first before calculating rectangle. Ensures consistency in binning.
#' @param useI Boolean (default FALSE), I is rarely used.
#'
#' @return A character vector of ICES rectangles
#' @export
#'
rb_d2ir <- function (lon, lat, safe = TRUE, useI = FALSE) {
  
  if (useI == TRUE) {
    lettersUsed <- LETTERS[1:12]
  } else {
    lettersUsed <- LETTERS[c(1:8, 10:13)]
  }
  
  d <- 
    tibble::tibble(lon = lon,
                   lat = lat) |> 
    dplyr::mutate(.row_id = 1:dplyr::n(),
                  outside = ifelse(lat < 36 | lat >= 85 | lon <= -44 | lon > 69,
                                   TRUE,
                                   FALSE))
  
  outside <- d |> dplyr::filter(outside == TRUE)
  
  
  inside  <- 
    d |> 
    dplyr::filter(outside == FALSE)
  
  if(safe == TRUE) {
    dx <- 1
    dy <- dx / 2
    inside <- 
      inside |> 
      dplyr::mutate(lon = lon %/% dx * dx + dx/2,
                    lat = lat %/% dy * dy + dy/2)
  }
  
  inside <- 
    inside |> 
    dplyr::mutate(y = floor(lat * 2) - 71,
                  y = ifelse(y < 10, paste("0", y, sep = ""), y),
                  x1 = dplyr::case_when(outside == TRUE ~ NA,
                                        .default = lettersUsed[(lon + 60)%/%10]),
                  x2 = dplyr::case_when(x1 == "A" ~ floor(lon %% 4),
                                        .default = floor(lon %% 10)),
                  ir = paste0(y, x1, x2))
  
  res <- 
    dplyr::bind_rows(inside, outside) |> 
    dplyr::arrange(.row_id) |> 
    dplyr::pull(ir)
  return(res)
}



#' Get ICES rectangle from coordinates
#' 
#' Get the ICES rectangle from any lon-lat position, given that this position 
#' is within the ICES region.
#' 
#' This function is the same as vmstools::icesRectangle but takes vectors
#' as input rather than a dataframe.
#' 
#' Use with caution, seems to give non-consistent results
#'
#' @param lon Longitude vector
#' @param lat Latitude vector
#'
#' @return Returns the rectangles as a vector 
#'
rb_d2ir_vmstools <- function(lon, lat) {
  
  dF <- data.frame(SI_LATI = lat,
                   SI_LONG = lon)
  rectChar1n2 <- sprintf("%02i",as.integer(2 * (dF[, "SI_LATI"] - 35.5)))
  rectChar3 <- ifelse(dF[, "SI_LONG"] > -50 & dF[, "SI_LONG"]<= -40, "A",
                      ifelse(dF[, "SI_LONG"] > -40 & dF[, "SI_LONG"]<= -30, "B",
                             ifelse(dF[, "SI_LONG"] > -30 & dF[, "SI_LONG"]<= -20, "C",
                                    ifelse(dF[, "SI_LONG"] > -20 & dF[, "SI_LONG"]<= -10, "D",
                                           ifelse(dF[, "SI_LONG"] > -10 & dF[, "SI_LONG"]<    0, "E",
                                                  #-Note that at 0 meridian the allocation of points at the meridian switch
                                                  ifelse(dF[, "SI_LONG"] >=  0 & dF[, "SI_LONG"]<  10, "F",
                                                         ifelse(dF[, "SI_LONG"] >= 10 & dF[, "SI_LONG"]<  20, "G",
                                                                ifelse(dF[, "SI_LONG"] >= 20 & dF[, "SI_LONG"]<  30, "H", "J"))))))))
  rectChar4  <- rep(NA,nrow(dF))
  idxlowzero <- which(dF[,"SI_LONG"] <  0)
  idxabozero <- which(dF[,"SI_LONG"] >= 0)
  if(length(idxlowzero)>0) rectChar4[idxlowzero] <- ceiling(dF[idxlowzero,"SI_LONG"] %% 10 -1 + 10)%%10
  if(length(idxabozero)>0) rectChar4[idxabozero] <- floor(dF[idxabozero,"SI_LONG"] %% 10)
  rectID <- paste(rectChar1n2, rectChar3, rectChar4, sep = "")
  
  
  return(rectID)
}