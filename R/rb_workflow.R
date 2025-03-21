# 2022-10-15 This is still trial and errror - not part of the workflow

#' Read in logbooks
#' 
#' A single tibble for both mobile and static gears for particular vessel(s) and 
#' year(s).
#'
#' @param con connection to Oracle
#' @param VIDs Vessel identification numbers
#' @param YEARS xxx
#'
#' @return A tibble with both mobile and static tows
#' @export
#'
rb_logbook <- function(con, VIDs, YEARS) {
  
  mb <- 
    omar::lb_mobile(con)
  if(!missing(VIDs)) {
    mb <- 
      mb |> 
      dplyr::filter(vid %in% VIDs)
  }
  mb <- 
    mb |> 
    dplyr::filter(year %in% YEARS) %>% 
    dplyr::left_join(omar::lb_catch(con) %>% 
                       dplyr::group_by(visir) %>% 
                       # check unit here, should be in kg
                       dplyr::summarise(catch = sum(catch, na.rm = TRUE),
                                        .groups = "drop"),
                     by = "visir") %>% 
    dplyr::collect(n = Inf)
  st <- omar::lb_static(con)
  if(!missing(VIDs)) {
    st <-
      st |> 
      dplyr::filter(vid %in% VIDs)
  }
  
  st <- 
    st %>% 
    dplyr::filter(year %in% YEARS) %>% 
    dplyr::left_join(omar::lb_catch(con) %>% 
                       dplyr::group_by(visir) %>% 
                       dplyr::summarise(catch = sum(catch, na.rm = TRUE),
                                        .groups = "drop"),
                     by = "visir") %>% 
    dplyr::collect(n = Inf)
  
  LGS <-
    dplyr::bind_rows(mb %>% dplyr::mutate(source = "mobile"), 
                     st %>% dplyr::mutate(source = "static")) %>%
    dplyr::mutate(date  =  lubridate::as_date(date),
                  datel = lubridate::as_date(datel),
                  gid   = as.integer(gid))
  return(LGS)
}


#' Cap effort
#' 
#' Replaces missing effort with median effort and caps effort to 12 hours (gid 6)
#' 24 hours (gid 7) and 15 hours (gid 14). If end of haul (t2) overlaps with
#' beginning of next haul (t1), cut the value of t2.
#'
#' @param lb logbooks
#'
#' @return a tibble, supposedly with same number of rows as the input
#' @export
#'
rb_cap_effort <- function(lb) {
  median.effort <- 
    lb %>% 
    dplyr::group_by(gid) %>% 
    dplyr::summarise(median = stats::median(effort, na.rm = TRUE),
                     .groups = "drop") %>% 
    tidyr::drop_na()
  lb <- 
    lb %>% 
    dplyr::left_join(median.effort, by = "gid") %>% 
    dplyr::mutate(effort = ifelse(!is.na(effort), effort, median)) %>% 
    dplyr::select(-median) %>% 
    # cap effort hours
    dplyr::mutate(effort = dplyr::case_when(effort > 12 & gid ==  6 ~ 12,
                                            effort > 24 & gid ==  7 ~ 24,
                                            effort > 15 & gid == 14 ~ 15,
                                            TRUE ~ effort)) %>% 
    # Cap on the t2 so not overlapping with next setting
    #    NOTE: Effort not adjusted accordingly
    dplyr::arrange(vid, t1) %>%
    dplyr::group_by(vid) %>%
    dplyr::mutate(overlap = dplyr::if_else(t2 > dplyr::lead(t1), TRUE, FALSE, NA),
                  t22 = dplyr::if_else(overlap,
                                       dplyr::lead(t1) - lubridate::minutes(1), # need to subtract 1 minute but get the format right
                                       t2,
                                       as.POSIXct(NA)),
                  t22 = lubridate::ymd_hms(format(as.POSIXct(t22, origin="1970-01-01", tz="UTC"))),
                  t2 = dplyr::if_else(overlap & !is.na(t22), t22, t2, as.POSIXct(NA))) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-t22) 
  
  return(lb)
}

#' Standardize mesh size
#' 
#' Crude correction of mesh size
#'
#' @param lb logbooks
#'
#' @export
#'
rb_std_meshsize <- function(lb) {
  lb <- 
    lb %>% 
    # "correct" mesh size
    dplyr::mutate(mesh = ifelse(gid == 7, mesh_min, mesh),
                  mesh.std = dplyr::case_when(gid ==  9 ~ 80,
                                              gid %in% c(7, 10, 12, 14) ~ 40,
                                              gid %in% c(5, 6) & (mesh <= 147 | is.na(mesh)) ~ 135,
                                              gid %in% c(5, 6) &  mesh >  147 ~ 155,
                                              gid %in% c(15, 38, 40) ~ 100,
                                              TRUE ~ NA_real_)) %>% 
    # gill net stuff
    dplyr::mutate(tommur = ifelse(gid == 2, round(mesh / 2.54), NA),
                  tommur = dplyr::case_when(tommur <= 66 ~ 60,
                                            tommur <= 76 ~ 70,
                                            tommur <= 87 ~ 80,
                                            tommur <= 100000 ~ 90,
                                            TRUE ~ NA_real_),
                  mesh.std = ifelse(gid == 2, round(tommur * 2.54), mesh.std),
                  mesh.std = ifelse(gid == 2 & is.na(mesh.std),  203, mesh.std)) |> 
    dplyr::select(-tommur)
  
  return(lb)
}


#' Gear width proxy
#'
#' The gear width for trawl is based on the sweeps, otherwise the plow width. 
#' If missing use the median value.
#'
#' @param lb logbooks
#'
#' @return a tibble
#' @export
#'
rb_gearwidth_proxy <- function(lb) {
  lb <- 
    lb %>% 
    dplyr::mutate(gear.width = dplyr::case_when(gid %in% c(6L, 7L, 9L, 14L) ~ as.numeric(sweeps),
                                                gid %in% c(15L, 38L, 40L) ~ as.numeric(plow_width),
                                                TRUE ~ NA_real_),
                  # cap gear width
                  gear.width = dplyr::case_when(gid ==  6L & gear.width > 250 ~ 250,
                                                gid ==  7L & gear.width > 250 ~ 250,
                                                gid ==  9L & gear.width >  75 ~  75,
                                                gid == 14L & gear.width >  55 ~  55,
                                                TRUE ~ gear.width))
  gear.width <- 
    lb %>% 
    dplyr::filter(gid %in% c(6, 7, 9, 14, 15, 37, 38, 40)) %>% 
    dplyr::group_by(gid) %>% 
    dplyr::summarise(gear.width.median = stats::median(gear.width, na.rm = TRUE),
                     .groups = "drop")
  # use median gear width by gear, if gear width is missing
  #   could also try to do this by vessels. time scale (year) may also matter here
  lb <- 
    lb %>% 
    dplyr::left_join(gear.width,
                     by = "gid") %>% 
    dplyr::mutate(gear.width = ifelse(gid %in% c(6, 7, 9, 14, 15, 37, 38, 40) & is.na(gear.width),
                                      gear.width.median,
                                      gear.width)) %>% 
    dplyr::select(-gear.width.median)
  
  # Put gear width of 5 as 500 - this is taken from thin air
  #   TODO: What gear width should be used
  lb <- 
    lb %>% 
    dplyr::mutate(gear.width = ifelse(gid == 5, 500, gear.width))
  
  return(lb)
}

rb_lump_gears <- function(lb) {
  lb %>% 
    dplyr::mutate(gid = dplyr::case_when(gid %in% c(10L, 12L) ~ 4L,       # purse seines
                                         gid %in% c(18L, 39L) ~ 18L,      # traps
                                         TRUE ~ gid)) %>% 
    # make the rest a negative number
    dplyr::mutate(gid = ifelse(is.na(gid), -666L, gid)) %>% 
    # "skip" these also in downstream processing
    dplyr::mutate(gid = ifelse(gid %in% c(4, 12, 42), -666, gid)) %>% 
    # lump dredges into one single gear
    dplyr::mutate(gid = ifelse(gid %in% c(15, 37, 38, 40), 15, gid))
}
