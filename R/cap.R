#' Cap value to IQR range
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
  
  x <- log10(x)
  upper <- min(max(x), quantile(x, 0.75, na.rm = TRUE) + (IQR(x, na.rm = TRUE) * multiplier))
  upper <- 10^upper
  return(upper)
}

#' Winsorize
#' 
#' Replace extreme values by less extreme ones. Default is just to cap the higher
#' values, but if arguement p is of length 2, lower values can also be substituted.
#'
#' @param x A numeric vector
#' @param limit Normally a single value (default 0.99), values higher than this will be replaced by this value.
#' Default is the 98th percentile of the distribution of x.
#' 
#'
#' @return A vector of same length as x
#' @export
#'
rb_cap_winsorize <- function(x, limit = stats::quantile(x, probs = c(0.99), na.rm = FALSE)) {
  
  if(length(limit) > 2) {
    message("Cap vector can only be of length 2 (upper and lower values)")
  }
  if(length(limit) == 2) {
    if(limit[1L] >= limit[2L]) message("Lower bound must be less than upper bound")
  }
  
  if(length(limit) == 1) {
    x[x > limit] <- limit
  } else {
    x[x < limit[1L]] <- limit[1L]
    x[x > limit[2L]] <- limit[2L]
  }
  
  return(x)
  
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
        cap <- w[max(valids) + 1]
      } else {
        cap <- max(w, na.rm = TRUE)
      }
    }
  }
    
  # B: If only zero values
  if(length(w) == 0) {
    cap <- 0
  }
  
  # Hail Mary - should really deal with this above
  if(!exists("cap")) cap <- max(w, na.rm = TRUE)

  x[x >= cap]  <- cap
  return(x)
}
