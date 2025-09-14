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


