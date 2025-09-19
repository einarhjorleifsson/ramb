#' Create trip
#'
#' The function 
#'
#' @param x A boolean vector
#'
#' @return An integer vector of the same length as input, providing unique trip number
#' @export
#'
rb_trip <- function(x) {
  tibble::tibble(x = x) |> 
    dplyr::mutate(tid = dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L),
                  tid = ifelse(x, -tid, tid))  |>  
    dplyr::group_by(x) |>  
    dplyr::mutate(tid = cumsum(tid))  |>  
    dplyr::ungroup(x) |> 
    dplyr::pull(tid)
}

#' define_trips_jepol
#'
#' Use the columns "SI_HARB" to determine when a vessel is on a trip. A trip is defined
#' from when it leaves the harbour till it returns
#'
#' @param vessel_id a vector containing vessel id
#' @param time a vector containing timestamp
#' @param in_harbour a binary vector indicating if vessel in harbour (1) or not (0)
#' @param min_dur the minimum trip length (hours)
#' @param max_dur the maximum trip length (hours)
#' @param split_trips If the trip is longer than the maximum hours, it will try to split
#' the trip into two or more trips, if there is long enough intervals between pings
#' 
#' @import data.table
#'
#' @return sequential numbers identifying trips, unique within each vessel
#' 
#' @export


rb_trip_jepol <- function(vessel_id, time, in_harbour, 
                          min_dur = 0.5, max_dur = 72, split_trips = TRUE) {
  org <-
    tibble::tibble(vessel_id = vessel_id,
                   time_stamp = time,
                   SI_HARB = in_harbour) |> 
    dplyr::mutate(.rid = 1:dplyr::n())
  
  x <- org
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  data.table::setDT(x)[, recid := 1:.N]
  if("SI_HARB" %!in% names(x) | any(is.na(x$SI_HARB)))
    stop("No harbour column in dataset, please add it (add_harbour)")
  
  
  out <- data.table::data.table()
  for(i in unique(x$vessel_id)){
    #progress(match(i, unique(x$vessel_id)),length(unique(x$vessel_id)))
    
    gps <- x[vessel_id == i]
    
    
    dss <- gps
    data.table::setorder(dss, time_stamp)
    dss[, INTV:=-as.numeric(difftime(data.table::shift(time_stamp, fill = NA, type = "lag"), time_stamp, units = "mins"))]
    
    dss[, id := 1:.N]
    
    ### Add harbour id and depart / return to dss
    
    dss[, SI_HARB2 := stats::approx((1:.N)[!is.na(SI_HARB)],stats::na.omit(SI_HARB),1:.N)$y]
    
    
    dss[is.na(SI_HARB), SI_HARB := pos2[dss[is.na(SI_HARB)], on = .(id = id), SI_HARB]]
    
    dss[, SI_HARB := SI_HARB[1], .(cumsum(!is.na(SI_HARB)))]
    
    dss[, HARB_EVENT := data.table::shift(SI_HARB, fill = NA, type = "lag") - SI_HARB]
    dss[HARB_EVENT == -1, HARB_EVENT := 2]
    dss[is.na(HARB_EVENT), HARB_EVENT := 0]
    
    #if there is no trips for the vessel, move to the next.
    if(all(dss$HARB_EVENT == 0))
      next
    
    #If the vessel is out of harbour at the start of the dataset, set the first point as a departure event
    if(dss[1]$SI_HARB == 0)
      dss[1, HARB_EVENT := 1]
    
    #If the vessel is out of harbour at the end of the dataset, set the last point as a return event
    if(dss[.N]$SI_HARB == 0)
      dss[.N, HARB_EVENT := 2]
    
    #Make data.table with timings of trip
    trip <- data.table::data.table(vessel_id = i,
                                   depart = dss[HARB_EVENT == 1]$time_stamp,
                                   arrival = dss[HARB_EVENT == 2]$time_stamp
    )
    
    trip[, trip_id2 := paste0(i, "_", 1:.N)]
    trip[, duration_hours := as.numeric(difftime(arrival, depart, units = "hours"))]
    
    
    #Only use trips longer than min_dur
    trip <- trip[duration_hours > min_dur]
    
    #Split trips longer than max_dur into smaller trips, based on the longest interval between pings
    if(split_trips == T)
      while (any(trip$duration_hours > max_dur)) {
        tls <- trip[duration_hours > max_dur][1,]
        cutp2 <- dss[time_stamp > tls$depart & time_stamp < tls$arrival][base::which.max(INTV[2:.N])]$id
        
        if(dss[id == cutp2]$INTV < 3){
          trip[trip_id2 == tls$trip_id2, duration_hours := max_dur]
          next
        }
        
        newtrips <- data.table::data.table(vessel_id = i,
                               depart = c(tls$depart, dss[id == cutp2]$time_stamp),
                               arrival = c(dss[id == cutp2-1]$time_stamp, tls$arrival),
                               trip_id2 = paste(tls$trip_id2, 1:2, sep = "_"))
        newtrips[, duration_hours := as.numeric(base::difftime(arrival, depart, units = "hours"))]
        trip <- data.table::rbindlist(list(trip[trip_id2 != tls$trip_id2], newtrips))
        data.table::setorder(trip, depart)
      }
    
    trip <- trip[duration_hours != 0]
    
    if(any(trip$duration_hours < min_dur)){
      print(trip[duration_hours < min_dur])
      warning(paste("At least one trip for", i, "is shorter than min_dur, after splitting up trips based on max_dur:") )
      trip <- trip[duration_hours > min_dur]
      
    }
    
    
    data.table::setkey(trip, vessel_id, depart, arrival)
    gps[ ,time_stamp2 := time_stamp]
    data.table::setkey(gps, vessel_id, time_stamp, time_stamp2)
    
    midi <- data.table::foverlaps(gps, trip, type="any", nomatch=NULL) 
    gps[, time_stamp2 := NULL]
    
    midi[!is.na(trip_id2), trip_id := trip_id2]
    midi[, `:=`(depart = NULL, arrival = NULL, duration_hours = NULL, time_stamp2 = NULL, trip_id2 = NULL)]
    out <- data.table::rbindlist(list(out, midi), fill = T)
    
    out <- data.table::rbindlist(list(out, gps[recid %!in% out$recid]), fill = T)
    
  }
  
  data.table::setorder(out, vessel_id, time_stamp)
  out[, recid := NULL]
  
 res <- 
    org |> 
    dplyr::select(.rid) |> 
    dplyr::left_join(out,
                     by = dplyr::join_by(.rid)) |> 
    dplyr::pull(trip_id)
 
 return(res)
  
}


