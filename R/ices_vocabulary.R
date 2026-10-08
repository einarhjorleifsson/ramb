#' Derive gear from metier 5 or 6
#' 
#' Obtain gear type (sometimes wrongly called metier 4?) from metier5 or  6
#'
#' @param x A character vector
#'
#' @return A vector
#' @noRd
#'
.rb_gear_from_metier <- function(x) {
  x |> stringr::str_split("_") |> purrr::map_chr(1)
}
#' Derive target from metier 5 or 6
#'
#' Not to be confused with metier 5
#' 
#' @param x A character vector
#'
#' @return A vector
#' @noRd
#'
.rb_target_from_metier <- function(x) {
  x |> stringr::str_split("_") |> purrr::map_chr(2)
}
#' Derive metier 5 from metier 6
#'
#' Not to be confused with target assemblage
#' 
#' @param x A character vector
#'
#' @return A vector
#' @noRd
#'
.rb_met5_from6 <- function(x) {
  sapply(strsplit(x, "_"), function(x) paste(x[1:2], collapse = "_"))
}


#' Get ICES metier level 5 table
#'
#' @param trim Boolean (default TRUE), return only key variables
#' @param valid Boolean (default TRUE), returns only metiers not depreciated.
#' Only done if trim is TRUE.
#'
#' @return A tibble containing metier 5 and description
#' @noRd
#'
.rb_get_ices_metier5 <- function(trim = TRUE, valid = TRUE) {
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
#' @noRd
.rb_get_ices_metier6 <- function(trim = TRUE, valid = TRUE) {
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
#' @noRd
.rb_get_ices_gears <- function(trim = TRUE, valid = TRUE) {
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
#' @noRd
.rb_get_ices_target <- function(trim = TRUE, valid = TRUE) {
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
#' @noRd
.rb_get_ices_metier5_benthis_lookup <- function(trim = TRUE, correct = TRUE) {
  res <- 
    "https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv" |> 
    utils::read.csv()
  if(trim) {
    res <- 
      res |>  
      dplyr::mutate(metier5 = .rb_met5_from6(Metier_level6)) |> 
      dplyr::select(metier5, benthis_metier = Benthis_metiers) |> 
      tibble::as_tibble() |> 
      dplyr::filter(benthis_metier != "") |> 
      dplyr::distinct()
    if(correct) {
    }
  }
  return(res)
}

#' Extract a part of an ICES metier code
#'
#' @param x A character vector of metier codes (level 5 or 6, e.g. `"OTB_DEF_>=120_0_0"`).
#' @param part `"gear"` (first field), `"target"` (second field) or `"metier5"` (gear and target).
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' rb_extract_metier("OTB_DEF_>=120_0_0", part = "metier5")
rb_extract_metier <- function(x, part = c("gear", "target", "metier5")) {
  part <- match.arg(part)
  switch(part,
         gear    = .rb_gear_from_metier(x),
         target  = .rb_target_from_metier(x),
         metier5 = .rb_met5_from6(x))
}

#' Get an ICES gear vocabulary table
#'
#' @param list Which table: `"gears"`, `"target"`, `"metier5"`, `"metier6"` or `"metier5_benthis_lookup"`.
#' @param ... Passed on: `trim`, `valid` (and `correct` for `"metier5_benthis_lookup"`).
#'
#' @return A tibble, fetched with icesVocab.
#' @export
rb_get_gear_vocabulary <- function(list = c("gears", "target", "metier5", "metier6", "metier5_benthis_lookup"), ...) {
  list <- match.arg(list)
  switch(list,
         gears   = .rb_get_ices_gears(...),
         target  = .rb_get_ices_target(...),
         metier5 = .rb_get_ices_metier5(...),
         metier6 = .rb_get_ices_metier6(...),
         metier5_benthis_lookup = .rb_get_ices_metier5_benthis_lookup(...))
}
