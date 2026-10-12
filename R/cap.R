# Upper IQR-multiplier limit of x (computed on the log scale, back-transformed)
.rb_iqr <- function(x, multiplier = 1.5) {
  y <- log(x)
  upper <- min(
    max(y, na.rm = TRUE),
    stats::quantile(y, 0.75, na.rm = TRUE) + (IQR(y, na.rm = TRUE) * multiplier)
  )
  upper <- exp(upper)
  return(upper)
}

# Cap x at the upper IQR-multiplier limit (log scale; see .rb_iqr above)
.rb_cap_iqr <- function(x, multiplier = 1.5) {
  upper <- .rb_iqr(x, multiplier)
  x[x > upper] <- upper
  return(x)
}

# The probs percentile of x, used as the winsorizing limit (NAs ignored,
# as in .rb_iqr above — with na.rm = FALSE one NA made the limit NA and
# capped the whole vector to NA)
.rb_winsor <- function(x, probs = 0.99) {
  stats::quantile(x = x, probs = probs, na.rm = TRUE)
}

# Cap x at the probs percentile (upper tail only)
.rb_cap_winsorize <- function(x, probs = 0.99) {
  if (length(probs) > 1) {
    message("Cap vector can only be of length 1 (upper value)")
  }
  if (probs < 0 | probs > 1) {
    message("Value should be between 0 and 1")
  }

  limit <- .rb_winsor(x, probs)
  x[x > limit] <- limit

  return(x)
}

# Miller limit: highest value of x before a log10 step larger than step_limit
.rb_miller <- function(x, step_limit = 1.5) {
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
  if (length(w) == 0) {
    limit <- 0
  }

  # Hail Mary - should really deal with this above
  if (!exists("limit")) {
    limit <- max(w, na.rm = TRUE)
  }

  return(limit)
}

# Cap x at the highest value before a log10 step larger than step_limit (see .rb_miller above)
.rb_cap_miller <- function(x, step_limit = 1.5) {
  limit <- .rb_miller(x, step_limit)

  x[x > limit] <- limit
  return(x)
}

#' Cap extreme values
#'
#' @param x A numeric vector.
#' @param method `"iqr"` (upper quartile + `multiplier` x IQR), `"miller"` (highest value before a
#'   step larger than `step_limit` on the log scale) or `"winsorize"` (the `probs` percentile).
#' @param ... Passed on: `multiplier` (iqr), `step_limit` (miller), `probs` (winsorize).
#'
#' @return A vector of the same length as `x`.
#' @export
rb_cap_outliers <- function(x, method = c("iqr", "miller", "winsorize"), ...) {
  method <- match.arg(method)
  switch(
    method,
    iqr = .rb_cap_iqr(x, ...),
    miller = .rb_cap_miller(x, ...),
    winsorize = .rb_cap_winsorize(x, ...)
  )
}
