
#' Derive metier 5 from metier 6
#' 
#' Basically trims all mesh etc. details from metier 6
#'
#' @param x A character vector
#'
#' @return A vector
#' @export
#'
rb_met5_from6 <- function(x) {
  sapply(strsplit(x, "_"), function(x) paste(x[1:2], collapse = "_")) 
}

#' Get ICES metier 5 table
#'
#' @param trim Boolean (default TRUE), return only key variables
#' @param valid Boolean (default TRUE), returns only metiers not depreciated.
#' Only actuelt if trim is TRUE.
#'
#' @return A tibble containing metier 5 and description
#' @export
#'
rb_get_ices_metier5 <- function(trim = TRUE, valid = TRUE) {
  res <- icesVocab::getCodeList("Metier5_FishingActivity")
  if(trim) {
    res <- 
      res |> 
      dplyr::select(met5 = Key,
                    description = Description,
                    deprecated = Deprecated)
    if(valid) {
      res <-
        res |> 
        dplyr::filter(deprecated == FALSE) |> 
        dplyr::select(-deprecated)
    }
  }
  return(res)
}

#' Get ICES metier 6 table
#'
#' @param trim Boolean (default TRUE), return only key variables
#' @param valid Boolean (default TRUE), returns only metiers not depreciated.
#' Only actuelt if trim is TRUE.
#'
#' @return A tibble containing metier 6 and description
#' @export
rb_get_ices_metier6 <- function(trim = TRUE, valid = TRUE) {
  res <- icesVocab::getCodeList("Metier6_FishingActivity")
  if(trim == TRUE) {
    res <- 
      res |> 
      dplyr::select(met6 = Key,
                    description = Description,
                    deprecated = Deprecated) |> 
      dplyr::mutate(description = stringr::str_remove(description, ", see the code reg. mesh size and selectivity device"))
    if(valid) {
      res <-
        res |> 
        dplyr::filter(deprecated == FALSE) |> 
        dplyr::select(-deprecated)
    }
  }
  return(res)
}

#' Get a table matching metier 5 and benthis metier
#' 
#' Extracts key variables from https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv
#'
#' @param trim Boolean (default TRUE), return only key variables
#' @param correct Boolean (default TRUE), not active
#'
#' @return
#' @export
rb_get_ices_metier5_benthis_lookup <- function(trim = TRUE, correct = TRUE) {
  res <- 
    "https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv" |> 
    read.csv()
  if(trim) {
    res <- 
      res |>  
      dplyr::mutate(met5 = rb_met5_from6(Metier_level6)) |> 
      dplyr::select(met5, benthis = Benthis_metiers) |> 
      tibble::as_tibble() |> 
      dplyr::filter(benthis != "") |> 
      dplyr::distinct()
    if(correct) {
    }
  }
  return(res)
}

