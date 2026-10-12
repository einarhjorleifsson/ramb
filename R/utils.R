# TRUE if x contains digits only; used by the MMSI functions
.rb_numbers_only <- function(x) !grepl("\\D", x)


#' Show rows around those exceeding a threshold
#'
#' For inspecting flagged rows in context: the rows where `what` exceeds
#' `threshold`, and the two rows on each side of them. Experimental.
#'
#' @param d A track tibble with a `.rid` row-number column.
#' @param what The column to test.
#' @param threshold The threshold.
#'
#' @return The matching rows and their neighbours, with a `whacky` column
#'   marking the matching ones.
#' @export
#'
rb_peek <- function(d, what, threshold) {
  d |>
    dplyr::filter({{ what }} > threshold) |>
    dplyr::pull(.rid) -> rid
  d <-
    d |>
    dplyr::mutate(whacky = ifelse(.rid %in% rid, "whacky", ""))
  # need to vectorize:
  rids <- NULL
  for (i in 1:length(rid)) {
    rids <- c(rids, rid[i] + c(-2, -1, 0, 1, 2))
  }
  d |> dplyr::filter(.rid %in% rids)
}
