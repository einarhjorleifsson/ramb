#' Calculate the upper IQR multiplier value
#' 
#' @param x A numeric vector
#' @param multiplier Default is 1.5
#'
#' @return The upper limit value
#' 
rb_iqr <- function(x, multiplier = 1.5) {
  y <- log(x)
  upper <- min(max(y, na.rm = TRUE), stats::quantile(y, 0.75, na.rm = TRUE) + (IQR(y, na.rm = TRUE) * multiplier))
  upper <- exp(upper)
  return(upper)
}

#' Cap value to the upper multiplier of the IQR range
#' 
#' Replace extreme values by less extreme using interquartile range multiplier 
#'
#' @param x A numeric vector
#' @param multiplier Default is 1.5
#'
#' @return A vector of same length as x
#' @export
#' 
rb_cap_iqr <- function(x, multiplier = 1.5) {
  upper <- rb_iqr(x, multiplier)
  x[x > upper] <- upper
  return(x)
}

#' Calculate the upper Winsor value
#' 
#' @param x A numeric vector
#' @param probs Default is 0.99
#'
#' @return The value corresponding to the percentile
#'  
rb_winsor <- function(x, probs = 0.99) {
  stats::quantile(x = x, probs = probs, na.rm = FALSE)
}

#' Winsorize
#' 
#' Replace extreme values by less extreme ones. Default is just to cap the higher
#' values, but if arguement p is of length 2, lower values can also be substituted.
#'
#' @param x A numeric vector
#' @param probs A single value of probabilities with values in [0,1], default 0.99.
#'
#' @return A vector of same length as x
#' @export
#'
rb_cap_winsorize <- function(x, probs = 0.99) {
  
  if(length(probs) > 1) {
    message("Cap vector can only be of length 1 (upper value)")
  }
  if(probs < 0 | probs > 1) {
    message("Value should be between 0 and 1")
  }

  limit <- rb_winsor(x, probs)
  x[x > limit] <- limit
  
  return(x)
  
}

#' Calculate the Miller limit
#'
#' @param x A numeric vector
#' @param step_limit A single value (default 1.5), log10 step values between consecutive
#' ordered values of x will be capped by the maximum step value.
#'
#' @return A value to be used as cap
#'
rb_miller <- function(x, step_limit = 1.5) {
  w <- x |> unique() |> sort()
  w <- w[w > 0]
  
  # A: If non-zero values
  if (length(w) > 0) {
    log_w <- log10(w)
    difw <- diff(log_w)
    # If any step difference greater than set by user, find the maximum value
    if (any(difw > step_limit)) {
      valids <- which(difw <= step_limit)
      if (length(valids) > 0) {
        # Add one to counter because diff returns a vector of length one less
        #  than the original data
        limit <- w[max(valids) + 1]
      } else {
        limit <- max(w, na.rm = TRUE)
      }
    }
  }
  
  # B: If only zero values
  if(length(w) == 0) {
    lmit <- 0
  }
  
  # Hail Mary - should really deal with this above
  if(!exists("limit")) limit <- max(w, na.rm = TRUE)
  
  return(limit)
}

#' Cap value to highest sequential 1.5 log step
#' 
#' Replaces extreme values by less extreme ones.
#'
#' @param x A numeric vector
#' @param step_limit A single value (default 1.5), log10 step values between consecutive
#' ordered values of x will be capped by the maximum step value.
#'
#' @return A vector of same length as x
#' @export
#'
rb_cap_miller <- function(x, step_limit = 1.5) {

  limit <- rb_miller(x, step_limit)
  
  x[x > limit]  <- limit
  return(x)
}
