# TRUE if x contains digits only; used by the MMSI functions
.rb_numbers_only <- function(x) !grepl("\\D", x)


#' rb_peek
#'
#' Experimental function, returns rows adjacent to the one fulfilling a critera
#'
#' @param d ais/vms track tibble
#' @param what the variable name to check
#' @param criteria the critera.
#'
#' @return a tibble
#' @export
#'
rb_peek <- function(d, what, criteria) {
  d |>
    dplyr::filter({{ what }} > criteria) |>
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
