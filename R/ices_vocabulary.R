#' Derive gear from metier 5 or 6
#' 
#' Obtain gear type (sometimes wrongly called metier 4?) from metier5 or  6
#'
#' @param x A character vector
#'
#' @return A vector
#' @export
#'
rb_gear_from_metier <- function(x) {
  x |> stringr::str_split("_") |> purrr::map_chr(1)
}
#' Derive target from metier 5 or 6
#'
#' Not to be confused with metier 5
#' 
#' @param x A character vector
#'
#' @return A vector
#' @export
#'
rb_target_from_metier <- function(x) {
  x |> stringr::str_split("_") |> purrr::map_chr(2)
}
#' Derive metier 5 from metier 6
#'
#' Not to be confused with target assemblage
#' 
#' @param x A character vector
#'
#' @return A vector
#' @export
#'
rb_met5_from6 <- function(x) {
  sapply(strsplit(x, "_"), function(x) paste(x[1:2], collapse = "_"))
}


#' Get ICES metier level 5 table
#'
#' @param trim Boolean (default TRUE), return only key variables
#' @param valid Boolean (default TRUE), returns only metiers not depreciated.
#' Only done if trim is TRUE.
#'
#' @return A tibble containing metier 5 and description
#' @export
#'
rb_get_ices_metier5 <- function(trim = TRUE, valid = TRUE) {
  res <- icesVocab::getCodeList("Metier5_FishingActivity")
  if(trim) {
    res <- 
      res |> 
      dplyr::select(metier5 = Key,
                    description = Description,
                    deprecated = Deprecated) |> 
      dplyr::as_tibble()
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
      dplyr::as_tibble() |> 
      dplyr::mutate(description = stringr::str_remove(description, ", see.*device")) 
    if(valid) {
      res <-
        res |> 
        dplyr::filter(deprecated == FALSE) |> 
        dplyr::select(-deprecated)
    }
  }
  return(res)
}

#' Get ICES gear table
#'
#' @param trim Boolean (default TRUE), return only key variables
#' @param valid Boolean (default TRUE), returns only metiers not depreciated.
#' Only actuelt if trim is TRUE.
#'
#' @return A tibble containing target list and description
#' @export
rb_get_ices_gears <- function(trim = TRUE, valid = TRUE) {
  res <- icesVocab::getCodeList("GearType")
  if(trim == TRUE) {
    res <- 
      res |> 
      dplyr::select(target = Key,
                    description = Description,
                    deprecated = Deprecated) |> 
      dplyr::as_tibble()
    if(valid) {
      res <-
        res |> 
        dplyr::filter(deprecated == FALSE) |> 
        dplyr::select(-deprecated)
    }
  }
  return(res)
}


#' Get ICES target table
#'
#' @param trim Boolean (default TRUE), return only key variables
#' @param valid Boolean (default TRUE), returns only metiers not depreciated.
#' Only actuelt if trim is TRUE.
#'
#' @return A tibble containing target list and description
#' @export
rb_get_ices_target <- function(trim = TRUE, valid = TRUE) {
  res <- icesVocab::getCodeList("TargetAssemblage")
  if(trim == TRUE) {
    res <- 
      res |> 
      dplyr::select(target = Key,
                    description = Description,
                    deprecated = Deprecated) |> 
      dplyr::as_tibble()
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
#' @return A vector
#' @export
rb_get_ices_metier5_benthis_lookup <- function(trim = TRUE, correct = TRUE) {
  res <- 
    "https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv" |> 
    utils::read.csv()
  if(trim) {
    res <- 
      res |>  
      dplyr::mutate(metier5 = rb_met5_from6(Metier_level6)) |> 
      dplyr::select(metier5, benthis_metier = Benthis_metiers) |> 
      tibble::as_tibble() |> 
      dplyr::filter(benthis_metier != "") |> 
      dplyr::distinct()
    if(correct) {
    }
  }
  return(res)
}

