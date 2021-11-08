#' Vessel tracks and behaviour of creel fisheries
#'
#' A dataset of movement data collected every 60 sec from 5 trips by 5 different
#' small scale fishing vessels using creels.
#'
#' Mendo, Tania; Smout, Sophie; Photopoulou, Theoni; James, Mark (2019), Data
#' from: Identifying fishing grounds from vessel tracks: model-based inference
#' for small scale fisheries, Dryad, Dataset, https://doi.org/10.5061/dryad.k80bp46
#'
#' @format A data frame with 2226 rows and 5 variables:
#' \describe{
#'   \item{id}{vessel identification <int>}
#'   \item{rowid}{record number for each vesssel <int>}
#'   \item{time}{time <dttm>}
#'   \item{x}{Longitude in meters, crs = 32630 <dbl>}
#'   \item{y}{Latitude in meters, crs = 32630 <dbl>}
#'   \item(lon}(Longitude <dbl>}
#'   \item{lat}{Latittude <dbl>}
#'   \item{behaviour}{Fishing activity, 'steaming', 'shooting' and 'hauling' <chr>}
#' }
#' @source \url{https://doi.org/10.5061/dryad.k80bp46}
"creel"


