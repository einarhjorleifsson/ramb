# argosfilter functions - tease out algorithm, "export" more details


#' rb_rms
#' 
#' The root mean square of value and the 2 upstream and 2 downstream values
#'
#' @param lon longitude
#' @param lat latitude
#' @param what name of variable
#'
#' @return
#' @export
#'
rb_rms <- function(lon, lat, time) {
  v <- res <- numeric(length(lat))
  for (i in 3:(length(lat)-2)) {
    v_2  <- distance_m(lat[i],lon[i],lat[i-2],lon[i-2]) / as.numeric(difftime(  time  [i],  time  [i-2],units = "secs")+1)
    v_1  <- distance_m(lat[i],lon[i],lat[i-1],lon[i-1]) / as.numeric(difftime(  time  [i],  time  [i-1],units = "secs")+1)
    v1   <- distance_m(lat[i],lon[i],lat[i+1],lon[i+1]) / as.numeric(difftime(  time  [i+1],  time  [i],units = "secs")+1)
    v2   <- distance_m(lat[i],lon[i],lat[i+2],lon[i+2]) / as.numeric(difftime(  time  [i+2],  time  [i],units = "secs")+1)
    v[i] <- sqrt(sum(v_2^2, v_1^2, v1^2, v2^2)/4)
    res[i] <- v[i]
  }
  return(res)
}


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


rb_rms_distance <- function(lon, lat, time) {
  v <- res <- numeric(length(lat))
  for (i in 3:(length(lat)-2)) {
    v_2  <- distance_m(lat[i],lon[i],lat[i-2],lon[i-2])
    v_1  <- distance_m(lat[i],lon[i],lat[i-1],lon[i-1])
    v1   <- distance_m(lat[i],lon[i],lat[i+1],lon[i+1])
    v2   <- distance_m(lat[i],lon[i],lat[i+2],lon[i+2])
    v[i] <- sqrt(sum(v_2^2, v_1^2, v1^2, v2^2)/4)
    res[i] <- v[i]
  }
  return(res)
}



# # these are functions from argosfilter
# vmask <- function(lat, lon, dtime, vmax) {
#   
#   row_id<-1:length(lat)
#   v<-numeric(length(lat))
#   dset<-data.frame(row_id,lat,lon,dtime,v,row.names = NULL)
#   
#   dset2<-dset
#   n_int=0
#   maxi=100
#   
#   ascending=TRUE
#   curr_peak=0
#   curr_null=0
#   n_peaks=0
#   pos_peak=0
#   
#   
#   while(maxi>vmax){
#     n_int=n_int+1 # number of iteractions
#     lat<-dset2$lat
#     lon<-dset2$lon
#     dtime<-dset2$dtime
#     
#     #-----------------------------------------------------
#     #calculate velocities v[i]
#     for (i in 3:(length(lat)-2)) {
#       v_2=distance_m(lat[i],lon[i],lat[i-2],lon[i-2])/as.numeric(difftime(dtime[i],dtime[i-2],units = "secs")+1)
#       v_1=distance_m(lat[i],lon[i],lat[i-1],lon[i-1])/as.numeric(difftime(dtime[i],dtime[i-1],units = "secs")+1)
#       v1=distance_m(lat[i],lon[i],lat[i+1],lon[i+1])/as.numeric(difftime(dtime[i+1],dtime[i],units = "secs")+1)
#       v2=distance_m(lat[i],lon[i],lat[i+2],lon[i+2])/as.numeric(difftime(dtime[i+2],dtime[i],units = "secs")+1)
#       v[i]=sqrt(sum(v_2^2, v_1^2, v1^2, v2^2)/4)
#       dset2$v[i]=v[i]
#     }
#     
#     #-----------------------------------------------------
#     # get the peaks in v[i]
#     ascending=TRUE
#     curr_peak=0
#     curr_null=0
#     n_peaks=0
#     pos_peak=0
#     for (i in 3:(length(lat)-2)) {
#       if (ascending) {                                
#         if (v[i]>curr_peak) curr_peak=v[i]	else {
#           ascending = FALSE;              
#           curr_null = v[i];
#           pos_peak=cbind(pos_peak,i-1);
#           n_peaks=n_peaks+1
#         }
#         
#       } else {
#         if (v[i] < curr_null) curr_null = v[i] else {
#           ascending = TRUE # previous point was a minimum, now going uphill again
#           curr_peak = v[i];
#         }
#       }
#       
#     }
#     # check last point, if still going uphill include it as a maximum
#     if (ascending) { pos_peak=cbind(pos_peak,i);n_peaks=n_peaks+1 }
#     
#     n_peaks
#     pos_peak
#     length(pos_peak)
#     pos_peak=pos_peak[-1]
#     #pos_peak
#     #length(pos_peak)
#     #v[1:30]
#     
#     #-----------------------------------------------------
#     # remove peaks where v[i]>vmax
#     #pos_peak
#     #length(pos_peak)
#     v[pos_peak]
#     if (max(v[pos_peak])>vmax){
#       peaks_to_remove<-pos_peak[which(v[pos_peak]>vmax)];peaks_to_remove
#       length(peaks_to_remove)
#       dset2<-dset2[peaks_to_remove*-1,]
#       dset2[1:12,]
#     }
#     maxi<-max(dset2$v);#maxi
#     
#   }
#   #n_int
#   #length(dset2$lat)
#   
#   # Find removed rows
#   not_rem<-match(dset2$row_id,dset$row_id);#not_rem
#   removed<-dset[not_rem*-1,];#removed
#   vmask<-sda_filter<-character(length(dset$lat));#vmask
#   vmask[not_rem*-1]<-"removed";#vmask
#   vmask[not_rem]<-"not";#vmask
#   extremes<-c(1,2,length(dset$lat)-1,length(dset$lat));#extremes
#   vmask[extremes]<-"end_location"
#   vmask
# }
# 
# 
# sdafilter <- function(lat, lon, dtime, lc, vmax=2,ang=c(15,25),distlim=c(2500,5000))
# {
#   row_id <- 1:length(lat)
#   original_data <- data.frame(row_id, lat, lon, dtime, lc)
#   
#   # calculate vmask ----------------------------------------------------------
#   mfilter <- vmask(lat, lon, dtime, vmax)
#   original_data <- cbind(original_data, mfilter)
#   
#   # remove Z positions and vmask="removed" (if dist>5000m to the prev pos) ---
#   dist <- dist.prev(lat,lon)
#   original_data <- cbind(original_data, dist)
#   rem <- which((mfilter == "removed" & dist > 5000) | lc == -9 | lc == "z" | lc == "Z" )
#   if(length(rem) != 0) filt_data <- original_data[-rem,] else filt_data <- original_data
#   
#   # remove angles <= angles in the list & dist > distances in the list -------
#   remx <- c(1, 2)
#   while (!is.na(remx[1])){
#     latx <- filt_data$lat
#     lonx <- filt_data$lon
#     
#     # angle to the previous and next position --------------------------------
#     angs <- internal.angles(latx, lonx)
#     # distance to the next position ------------------------------------------
#     dnext <- dist.next(latx, lonx)
#     # distance to the previous position --------------------------------------
#     dprevv <- vdist.prev(latx,lonx)
#     
#     rem_string = character(1)
#     for (i in 1:length(ang) ){
#       string <- paste("(angs<=",ang[i]," & dprev>",distlim[i]," & dnext>",distlim[i],")",sep="")
#       if (i==1) rem_string <- string else rem_string=paste(rem_string,"|",string)
#     }
#     
#     rem_string
#     eval(parse(text = rem_string))
#     remx <- which( eval(parse(text=rem_string)) )
#     if (!is.na(remx[1])) filt_data<-filt_data[-remx,]
#   }
#   
#   # Find removed rows --------------------------------------------------------
#   not_rem <- match(filt_data$row_id, original_data$row_id)
#   removed <- original_data[not_rem * -1,]
#   sda_filter <- character(length(original_data$lat))
#   sda_filter[not_rem * -1] <- "removed"
#   sda_filter[not_rem] <- "not"
#   extremes<-c(1,2,length(original_data$lat)-1,length(original_data$lat))
#   sda_filter[extremes]<-"end_location"
#   sda_filter
#   
# }

