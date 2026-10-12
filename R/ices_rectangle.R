#' Get ICES rectangles from coordinates
#'
#' Returns the ICES statistical rectangle for given longitude and latitude
#' coordinates. If a position is outside the ICES region, returns `NA`.
#'
#' @param lon Numeric vector of longitudes (decimal degrees, -44 < lon <= 69).
#' @param lat Numeric vector of latitudes (decimal degrees, 36 <= lat < 85).
#' @param method Character, default `"robust"`. `"robust"` uses consistent
#'   band boundaries, domain validation and optional midpoint snapping.
#'   `"vmstools"` reproduces the legacy `vmstools::icesRectangle()` algorithm
#'   for compatibility with vmstools-based workflows; its longitude bands have
#'   inconsistent boundaries (right-inclusive for bands A-D, right-exclusive
#'   for E-H, no upper bound on J), so the two methods diverge for positions
#'   exactly on 10-degree longitudes (e.g. `lon = -10`, `lon = -20`).
#' @param snap_to_midpoint Logical, default `TRUE`. If `TRUE`, coordinates are
#'   snapped to bin midpoints before rectangle calculation, reducing edge
#'   effects. Only used when `method = "robust"`.
#' @param use_i Logical, default `FALSE`. If `TRUE`, allows rectangles to use
#'   the letter 'I' in the longitude band (rarely used). Only used when
#'   `method = "robust"`.
#'
#' @return A character vector of ICES rectangles (or `NA` for positions
#'   outside the ICES region under `method = "robust"`), same length as input.
#'
#' @details
#' The ICES statistical rectangle system is a spatial reference grid for
#' marine data. Longitude and latitude must be decimal degrees.
#'
#' @references For `"vmstools"`: `vmstools::icesRectangle()` (Hintzen, N.T.,
#'   Bastardie, F., Beare, D., et al. (2012). "VMStools: Open-source software
#'   for the processing, analysis and visualisation of fisheries logbook and
#'   VMS data". Fisheries Research, 115–116, 31–43.
#'   \doi{10.1016/j.fishres.2011.11.007}).
#'
#' @examples
#' # Standard position inside ICES area
#' rb_encode_ices_rectangle(-10.5, 62.5)
#' # Vectorized input
#' rb_encode_ices_rectangle(c(-10.5, 15.2), c(62.5, 54.9))
#' # Position outside ICES area
#' rb_encode_ices_rectangle(100, 10)
#' # Legacy vmstools reproduction
#' rb_encode_ices_rectangle(-10.5, 62.5, method = "vmstools")
#'
#' @export
rb_encode_ices_rectangle <- function(
  lon,
  lat,
  method = c("robust", "vmstools"),
  snap_to_midpoint = TRUE,
  use_i = FALSE
) {
  if (!is.numeric(lon) || !is.numeric(lat)) {
    stop("Both 'lon' and 'lat' must be numeric vectors.")
  }
  if (length(lon) != length(lat)) {
    stop("'lon' and 'lat' must have the same length.")
  }
  method <- match.arg(method)
  if (method == "vmstools") {
    return(.rb_encode_ices_rectangle_vmstools(lon, lat))
  }
  if (!is.logical(snap_to_midpoint) || length(snap_to_midpoint) != 1) {
    stop("'snap_to_midpoint' must be a single logical value.")
  }
  if (!is.logical(use_i) || length(use_i) != 1) {
    stop("'use_i' must be a single logical value.")
  }

  # Letters used for rectangles (I is rarely used)
  if (use_i) {
    lettersUsed <- LETTERS[1:12] # A-L
  } else {
    lettersUsed <- LETTERS[c(1:8, 10:13)] # A-H, J-M (skipping I)
  }

  # Create tibble for processing and row tracking
  d <- tibble::tibble(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    .row_id = seq_along(lon),
    outside = lat < 36 | lat >= 85 | lon <= -44 | lon > 69
  )

  # Split inside and outside
  outside <- d[d$outside, ]
  inside <- d[!d$outside, ]

  # Apply midpoint rounding if snap_to_midpoint = TRUE
  if (snap_to_midpoint && nrow(inside) > 0) {
    dx <- 1
    dy <- dx / 2
    inside$lon <- inside$lon %/% dx * dx + dx / 2
    inside$lat <- inside$lat %/% dy * dy + dy / 2
  }

  # Calculate rectangle codes for inside points
  if (nrow(inside) > 0) {
    inside$y <- floor(inside$lat * 2) - 71
    inside$y <- ifelse(
      inside$y < 10,
      paste0("0", inside$y),
      as.character(inside$y)
    )
    # x1: rectangle letter, based on longitude band
    ix1 <- suppressWarnings((inside$lon + 60) %/% 10)
    inside$x1 <- ifelse(
      ix1 >= 1 & ix1 <= length(lettersUsed),
      lettersUsed[ix1],
      NA
    )
    inside$x2 <- ifelse(
      inside$x1 == "A",
      floor(inside$lon %% 4),
      floor(inside$lon %% 10)
    )
    inside$ir <- ifelse(
      is.na(inside$x1) | is.na(inside$x2),
      NA,
      paste0(inside$y, inside$x1, inside$x2)
    )
  } else {
    inside$ir <- character(0)
  }

  outside$ir <- NA_character_

  # Combine and restore input order
  res <- dplyr::bind_rows(inside, outside)
  res <- res[order(res$.row_id), , drop = FALSE]
  res$ir
}

# Faithful reproduction of vmstools::icesRectangle() on vectors, kept for
# compatibility with vmstools-based workflows. Known inconsistent longitude
# band boundaries; do not "fix".
.rb_encode_ices_rectangle_vmstools <- function(lon, lat) {
  rectChar1n2 <- sprintf("%02i", as.integer(2 * (lat - 35.5)))
  rectChar3 <- ifelse(
    lon > -50 & lon <= -40,
    "A",
    ifelse(
      lon > -40 & lon <= -30,
      "B",
      ifelse(
        lon > -30 & lon <= -20,
        "C",
        ifelse(
          lon > -20 & lon <= -10,
          "D",
          ifelse(
            lon > -10 & lon < 0,
            "E",
            # Note: at the 0 meridian the allocation of points switches
            ifelse(
              lon >= 0 & lon < 10,
              "F",
              ifelse(
                lon >= 10 & lon < 20,
                "G",
                ifelse(lon >= 20 & lon < 30, "H", "J")
              )
            )
          )
        )
      )
    )
  )
  rectChar4 <- rep(NA, length(lon))
  idxlowzero <- which(lon < 0)
  idxabozero <- which(lon >= 0)
  if (length(idxlowzero) > 0) {
    rectChar4[idxlowzero] <- ceiling(lon[idxlowzero] %% 10 - 1 + 10) %% 10
  }
  if (length(idxabozero) > 0) {
    rectChar4[idxabozero] <- floor(lon[idxabozero] %% 10)
  }
  paste(rectChar1n2, rectChar3, rectChar4, sep = "")
}
