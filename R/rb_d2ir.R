#' Get ICES rectangles from coordinates
#'
#' Returns the ICES statistical rectangle for given longitude and latitude coordinates.
#' If a position is outside the ICES region, returns \code{NA}.
#'
#' @param lon Numeric vector of longitudes (decimal degrees, -44 < lon <= 69).
#' @param lat Numeric vector of latitudes (decimal degrees, 36 <= lat < 85).
#' @param safe Logical, default TRUE. If TRUE, coordinates are rounded to bin midpoints before rectangle calculation, reducing edge effects.
#' @param useI Logical, default FALSE. If TRUE, allows rectangles to use the letter 'I' in x1 (rarely used).
#'
#' @return A character vector of ICES rectangles (or NA for positions outside the ICES region), same length as input.
#'
#' @details
#' The ICES statistical rectangle system is a spatial reference grid for marine data.
#' Positions outside the valid ICES region will return NA. Longitude and latitude must be decimal degrees.
#'
#' @examples
#' # Example 1: Standard position inside ICES area
#' rb_d2ir(-10.5, 62.5)
#' # Example 2: Vectorized input
#' rb_d2ir(c(-10.5, 15.2), c(62.5, 54.9))
#' # Example 3: Position outside ICES area
#' rb_d2ir(100, 10)
#' # Example 4: With safe = FALSE (no midpoint rounding)
#' rb_d2ir(-10.5, 62.5, safe = FALSE)
#'
#' @export
rb_d2ir <- function(lon, lat, safe = TRUE, useI = FALSE) {
  # --- Input checks ---
  if (!is.numeric(lon) || !is.numeric(lat)) stop("Both 'lon' and 'lat' must be numeric vectors.")
  if (length(lon) != length(lat)) stop("'lon' and 'lat' must have the same length.")
  if (!is.logical(safe) || length(safe) != 1) stop("'safe' must be a single logical value.")
  if (!is.logical(useI) || length(useI) != 1) stop("'useI' must be a single logical value.")
  
  # Letters used for rectangles (I is rarely used)
  if (useI) {
    lettersUsed <- LETTERS[1:12]      # A-L
  } else {
    lettersUsed <- LETTERS[c(1:8,10:13)] # A-H, J-M (skipping I)
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
  inside  <- d[!d$outside, ]
  
  # Apply midpoint rounding if safe = TRUE
  if (safe && nrow(inside) > 0) {
    dx <- 1
    dy <- dx / 2
    inside$lon <- inside$lon %/% dx * dx + dx/2
    inside$lat <- inside$lat %/% dy * dy + dy/2
  }
  
  # Calculate rectangle codes for inside points
  if (nrow(inside) > 0) {
    inside$y <- floor(inside$lat * 2) - 71
    inside$y <- ifelse(inside$y < 10, paste0("0", inside$y), as.character(inside$y))
    # x1: rectangle letter, based on longitude band
    ix1 <- suppressWarnings((inside$lon + 60) %/% 10)
    inside$x1 <- ifelse(ix1 >= 1 & ix1 <= length(lettersUsed), lettersUsed[ix1], NA)
    inside$x2 <- ifelse(inside$x1 == "A",
                        floor(inside$lon %% 4),
                        floor(inside$lon %% 10))
    inside$ir <- ifelse(is.na(inside$x1) | is.na(inside$x2), NA, paste0(inside$y, inside$x1, inside$x2))
  } else {
    inside$ir <- character(0)
  }
  
  outside$ir <- NA_character_
  
  # Combine and restore input order
  res <- dplyr::bind_rows(inside, outside)
  res <- res[order(res$.row_id), , drop = FALSE]
  return(res$ir)
}

# --- Minimal built-in test cases (run manually for verification) ---
if (interactive() || identical(Sys.getenv("RB_D2IR_TEST"), "yes")) {
  # 1. Standard in-area
  stopifnot(rb_d2ir(-10.5, 62.5) == "43E1")
  # 2. Outside area
  stopifnot(is.na(rb_d2ir(100, 10)))
  # 3. Vectorized, mixed
  test_res <- rb_d2ir(c(-10.5, 15.2, 100), c(62.5, 54.9, 10))
  stopifnot(test_res[1] == "43E1", test_res[2] == "37K5", is.na(test_res[3]))
  # 4. Check length mismatch errors
  expect_error <- function(expr) tryCatch(expr, error = function(e) TRUE)
  stopifnot(expect_error(rb_d2ir(1:3, 1:2)))
  # 5. Check safe = FALSE
  stopifnot(rb_d2ir(-10.5, 62.5, safe = FALSE) == "43E1")
}