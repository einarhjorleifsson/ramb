#' Sum logbook species catches
#'
#' @param d a eflalo dataframe
#' @param start_name a string, start name of variable to sum across
#' @param remove Boolean (default TRUE) removes catch by species
#'
#' @return Input with additional summed variable, name is 'start_name'
#' @export
rb_sum_across <- function(d, start_name = NULL, remove = TRUE) {

  # TODO: Input checks:
  #        Do variables exist
  #        Are variables to sum across integer/numeric

  d <-
    d |>
    dplyr::mutate(sum =  rowSums(dplyr::across(dplyr::starts_with( {{ start_name }} ))))
  if(remove) {
    d <-
      d |>
      dplyr::select(-c(dplyr::starts_with( {{ start_name }} )))
  }
  d <- d |> dplyr::rename( {{ start_name }} := sum)
  return(d)
}

