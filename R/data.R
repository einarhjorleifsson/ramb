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
#'   \item{behaviour}{Vessel activity}}
#'
#' @source Mendo, Tania; Smout, Sophie; Photopoulou, Theoni; James, Mark (2019), Data
#' from: Identifying fishing grounds from vessel tracks: model-based inference
#' for small scale fisheries (\url{https://doi.org/10.5061/dryad.k80bp46})
#'
"creel"

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
