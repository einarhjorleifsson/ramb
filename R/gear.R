#' Predict gear width from vessel size
#'
#' The width (metres) of the gear a vessel would tow, from its metier and its
#' length or engine power, with the Benthis vessel-size–gear-width models.
#'
#' @param metier Metier level 5 codes, matched against
#'   [metier5_benthis_lookup].
#' @param length_m Vessel length overall, metres.
#' @param kw Vessel engine power, kW.
#' @param method Only `"benthis"` so far.
#'
#' @return A numeric vector of gear widths in metres; `NA` where the metier
#'   has no model, or the model is in the other size variable.
#'
#' @references Eigaard, O.R., Bastardie, F., Breen, M., et al. (2016).
#'   "Estimating seabed pressure from demersal trawls, seines, and dredges
#'   based on gear design and dimensions". ICES Journal of Marine Science,
#'   73(suppl. 1), i27–i43. \doi{10.1093/icesjms/fsv099}; the parameters are
#'   in [benthis_parameters].
#' @export
rb_predict_gear_width <- function(metier, length_m, kw, method = "benthis") {
  method <- match.arg(method)
  tibble::tibble(
    metier5 = {{ metier }},
    length_m = {{ length_m }},
    kw = {{ kw }}
  ) |>
    dplyr::left_join(metier5_benthis_lookup, by = dplyr::join_by(metier5)) |>
    dplyr::left_join(
      benthis_parameters |>
        dplyr::select(benthis_metier:variable),
      by = dplyr::join_by(benthis_metier)
    ) |>
    dplyr::mutate(
      width = dplyr::case_when(
        model == "power" & variable == "avg_kw" ~ a * kw^b,
        model == "power" & variable == "avg_oal" ~ a * length_m^b,
        model == "linear" & variable == "avg_oal" ~ a * length_m + b,
        .default = NA
      )
    ) |>
    dplyr::pull(width)
}
# The gear field of a metier 5 or 6 code
.rb_gear_from_metier <- function(x) {
  x |> stringr::str_split("_") |> purrr::map_chr(1)
}
# The target-assemblage field of a metier 5 or 6 code
.rb_target_from_metier <- function(x) {
  x |> stringr::str_split("_") |> purrr::map_chr(2)
}
# The metier 5 (gear_target) of a metier 6 code
.rb_met5_from6 <- function(x) {
  sapply(strsplit(x, "_"), function(x) paste(x[1:2], collapse = "_"))
}


# ICES Metier5_FishingActivity code list: metier5, description; valid drops the deprecated
.rb_get_ices_metier5 <- function(trim = TRUE, valid = TRUE) {
  .rb_need("icesVocab")
  res <- icesVocab::getCodeList("Metier5_FishingActivity")
  if (trim) {
    res <-
      res |>
      dplyr::select(
        metier5 = Key,
        description = Description,
        deprecated = Deprecated
      ) |>
      dplyr::as_tibble()
    if (valid) {
      res <-
        res |>
        dplyr::filter(deprecated == FALSE) |>
        dplyr::select(-deprecated)
    }
  }
  return(res)
}

# ICES Metier6_FishingActivity code list: met6, description
.rb_get_ices_metier6 <- function(trim = TRUE, valid = TRUE) {
  .rb_need("icesVocab")
  res <- icesVocab::getCodeList("Metier6_FishingActivity")
  if (trim == TRUE) {
    res <-
      res |>
      dplyr::select(
        met6 = Key,
        description = Description,
        deprecated = Deprecated
      ) |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        description = stringr::str_remove(description, ", see.*device")
      )
    if (valid) {
      res <-
        res |>
        dplyr::filter(deprecated == FALSE) |>
        dplyr::select(-deprecated)
    }
  }
  return(res)
}

# ICES GearType code list. NOTE: the key column is misnamed `target` (it holds gear codes).
.rb_get_ices_gears <- function(trim = TRUE, valid = TRUE) {
  .rb_need("icesVocab")
  res <- icesVocab::getCodeList("GearType")
  if (trim == TRUE) {
    res <-
      res |>
      dplyr::select(
        target = Key,
        description = Description,
        deprecated = Deprecated
      ) |>
      dplyr::as_tibble()
    if (valid) {
      res <-
        res |>
        dplyr::filter(deprecated == FALSE) |>
        dplyr::select(-deprecated)
    }
  }
  return(res)
}


# ICES TargetAssemblage code list: target, description
.rb_get_ices_target <- function(trim = TRUE, valid = TRUE) {
  .rb_need("icesVocab")
  res <- icesVocab::getCodeList("TargetAssemblage")
  if (trim == TRUE) {
    res <-
      res |>
      dplyr::select(
        target = Key,
        description = Description,
        deprecated = Deprecated
      ) |>
      dplyr::as_tibble()
    if (valid) {
      res <-
        res |>
        dplyr::filter(deprecated == FALSE) |>
        dplyr::select(-deprecated)
    }
  }
  return(res)
}

# Metier 5 to Benthis metier lookup, from the RCG reference list
# (RDB_ISSG_Metier_list.csv at github.com/ices-eg/RCGs). `correct` is a
# placeholder and does nothing.
.rb_get_ices_metier5_benthis_lookup <- function(trim = TRUE, correct = TRUE) {
  res <-
    "https://raw.githubusercontent.com/ices-eg/RCGs/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv" |>
    utils::read.csv()
  if (trim) {
    res <-
      res |>
      dplyr::mutate(metier5 = .rb_met5_from6(Metier_level6)) |>
      dplyr::select(metier5, benthis_metier = Benthis_metiers) |>
      tibble::as_tibble() |>
      dplyr::filter(benthis_metier != "") |>
      dplyr::distinct()
    if (correct) {}
  }
  return(res)
}

#' Extract a part of an ICES metier code
#'
#' @param x A character vector of metier codes (level 5 or 6, e.g.
#'   `"OTB_DEF_>=120_0_0"`; see [rb_get_gear_vocabulary()] for the code lists).
#' @param part `"gear"` (first field), `"target"` (second field) or `"metier5"` (gear and target).
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' rb_extract_metier("OTB_DEF_>=120_0_0", part = "metier5")
rb_extract_metier <- function(x, part = c("gear", "target", "metier5")) {
  part <- match.arg(part)
  switch(
    part,
    gear = .rb_gear_from_metier(x),
    target = .rb_target_from_metier(x),
    metier5 = .rb_met5_from6(x)
  )
}

#' Get an ICES vocabulary table
#'
#' @param code_list Which code list: `"gears"`, `"target"`, `"metier5"`,
#'   `"metier6"` (from [icesVocab::getCodeList()]) or
#'   `"metier5_benthis_lookup"` (from the RCG metier reference list).
#' @param ... Passed on: `trim` (key columns only) and `valid` (drop
#'   deprecated codes).
#'
#' @return A tibble.
#' @export
rb_get_gear_vocabulary <- function(
  code_list = c(
    "gears",
    "target",
    "metier5",
    "metier6",
    "metier5_benthis_lookup"
  ),
  ...
) {
  code_list <- match.arg(code_list)
  switch(
    code_list,
    gears = .rb_get_ices_gears(...),
    target = .rb_get_ices_target(...),
    metier5 = .rb_get_ices_metier5(...),
    metier6 = .rb_get_ices_metier6(...),
    metier5_benthis_lookup = .rb_get_ices_metier5_benthis_lookup(...)
  )
}
