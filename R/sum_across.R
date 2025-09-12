#' Sum columns across by prefix in Data Frame
#'
#' Summarizes (row-wise) all numeric columns in a data frame whose names start with a specified prefix,
#' returning their sum as a new column. Optionally removes the original columns used in the sum.
#'
#' Useful for eflalo data where multiple columns represent catches of different species,
#' and you want to produce a total catch column and at the same time remove the original columns.
#'
#' @param d A data frame (typically an eflalo data frame).
#' @param start_name A string (default "LE_KG"). Prefix of column names to sum across. All columns starting with this string will be included in the sum.
#' @param remove Logical (default TRUE). If TRUE, removes all original columns whose names start with \code{start_name} after summing.
#'
#' @return
#' The input data frame with a new column named \code{start_name} containing the row sums. If \code{remove = TRUE}, the original columns are removed.
#'
#' @details
#' The function will error if no columns match \code{start_name}, or if any matching columns are not numeric.
#'
#' @examples
#' # Example: Summing all columns starting with 'catch_'
#' df <- data.frame(
#'   id = 1:3,
#'   LE_KG_COD = c(1, 2, 3),
#'   LE_KG_HAD = c(0, 1, 0),
#'   LE_KG_HER = c(2, 1, 2)
#' )
#' rb_sum_across(df, start_name = "LE_KG")
#'
#' # Keep original columns
#' rb_sum_across(df, start_name = "LE_KG", remove = FALSE)
#'
#' @export
rb_sum_across <- function(d, start_name = "LE_KG", remove = TRUE) {
  
  # Input checks
  if (is.null(start_name) || !is.character(start_name) || length(start_name) != 1) {
    stop("start_name must be a single character string.")
  }
  cols_to_sum <- grep(paste0("^", start_name), names(d), value = TRUE)
  if (length(cols_to_sum) == 0) {
    stop("No columns found starting with '", start_name, "'.")
  }
  if (!all(sapply(d[cols_to_sum], is.numeric))) {
    stop("All columns to sum must be numeric.")
  }
  
  d <- dplyr::mutate(d, !!start_name := rowSums(dplyr::across(dplyr::starts_with(start_name))))
  
  if (remove) {
    # Remove only the original columns, not the new sum column
    cols_to_remove <- setdiff(cols_to_sum, start_name)
    d <- dplyr::select(d, -dplyr::all_of(cols_to_remove))
  }
  return(d)
}