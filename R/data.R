#' Vessel tracks and behaviour of creel fisheries
#'
#' A dataset of movement data collected every 60 sec from 5 trips by 5 different
#' small scale fishing vessels using creels. (Mendo, Tania; Smout, Sophie; Photopoulou, Theoni; James, Mark (2019), Data
#' from: Identifying fishing grounds from vessel tracks: model-based inference
#' for small scale fisheries)
#'
#' @format A data frame with 2226 rows and 8 variables:
#' \describe{
#'   \item{id}{vessel identification}
#'   \item{rowid}{record number, unique for each vessel}
#'   \item{time}{time of position}
#'   \item{x}{Easting}
#'   \item{y}{Northing}
#'   \item{lon}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{behaviour}{Vessel activity}
#'   }
#'
#' @source Mendo, Tania; Smout, Sophie; Photopoulou, Theoni; James, Mark (2019), Data
#' from: Identifying fishing grounds from vessel tracks: model-based inference
#' for small scale fisheries (\url{https://doi.org/10.5061/dryad.k80bp46})
#'
"creel"

#' Vessel tracks and behaviour of some danish fishing vessels
#'
#' Example data of 14 vessels using 4 different gears with a median 
#' time interval of 10 sec.
#'
#' @format A data frame with 395558 rows and 9 variables:
#' \describe{
#'   \item{.rowid}{sequential numbers}
#'   \item{vessel_id}{vessel identification}
#'   \item{time_stamp}{time of position}
#'   \item{lon}{longitude}
#'   \item{lat}{latitude}
#'   \item{speed}{instantaneous speed in knots}
#'   \item{course}{true course?}
#'   \item{gear}{gear type deployed}
#'   \item{behaviour}{Vessel activity}
#'   }
#'
#' @source WKSSFGEO (\url{https://raw.githubusercontent.com/ices-eg/WKSSFGEO/main/data-examples/example_data_AIS.csv})
#'
"dansk"

#' Some harbour polygons used in association with the dansk-dataset
#'
#'
#' @format A data frame with 942 rows and 2 variables:
#' \describe{
#'   \item{SI_HARB}{sequential numbers denoting unique harbour id}
#'   \item{geometry}{harbour polygons}
#'   }
#'
#' @source WKSSFGEO
#'
"dansk_harbours"

#' Bottom trawl survey
#'
#' Icelandic spring bottom trawl survey.
#'
"trawlsurvey"

#' Bottom trawl survey tows
#'
#' Icelandic spring bottom trawl survey.
#'
"trawlsurveytows"

#' Fictional vessel tracks - set 1
#'
#' Some simple dataset to test speed filters
#' 
#' @format A data frame with 46 rows and 7 variables:
#' \describe{
#'   \item{.rid}{row number}
#'   \item{vid}{unique vessel identifier}
#'   \item{tid}{unique trip identifier}
#'   \item{lon}{Longitude}
#'   \item{lat}{latitude}
#'   \item{time}{time}
#'   \item{whacks}{Boolean, indication if vessel on track or not}
#'   }
#'
#'
"whacks1"



#' Benthis parameters
#' 
#' Gear “footprints” of 14 distinct towed gear groups (eight otter trawl groups, 
#' three beam trawl groups, two demersal seine groups, and one dredge group).
#' The footprint is defined as the relative contribution from individual larger 
#' gear components, such as trawl doors, sweeps, and groundgear, to the total 
#' area and severity of the gear's impact.
#' 
#' For each gear group, a vessel size–gear size relationship parameters can 
#' used to estimate gear width and sediment penetration from 
#' vessel size (length or power). 
#' 
#' https://doi.org/10.1093/icesjms/fsv099
#' 
#' @format A data frame with 14 observations (rows) and 12 variables (columns):
#' \describe{
#'   \item{benthis_metier}{Metier as defined in the Benthis project}
#'   \item{a}{First model parameter}
#'   \item{b}{Second model parameter}
#'   \item{model}{Model type, linear or power}
#'   \item{variable}{Dependent variable, overall vessel length or vessel kilowatts}
#'   \item{subsurface_percentage}{Subsurface impact}
#'   \item{id}{...}
#'   \item{av_kw}{...}
#'   \item{av_loa}{...}
#'   \item{av_fspeed}{...}
#'   \item{gear_width}{...}
#'   \item{contact_model}{...}
#'   }
#'
#' @source https://doi.org/10.1093/icesjms/fsv099
"benthis_parameters"


#' Benthis metier lookup
#' 
#' @format A data frame with 41 observations (rows) and 2 variables (columns):
#' \describe{
#'   \item{metier5}{Metier as defined by ICES}
#'   \item{benthis_metier}{Metier as defined in the benthis project}
#'   }
#'
#' @source https://doi.org/10.1093/icesjms/fsv099
"metier5_benthis_lookup"

