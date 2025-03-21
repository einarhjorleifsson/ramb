#' Create trip
#'
#' @param x A vector
#'
#' @return An integer vector of the same length as input, providing unique trip number
#' @export
#'
rb_trip <- function(x) {
  tibble::tibble(x = x) %>% 
    dplyr::mutate(tid = dplyr::if_else(x != dplyr::lag(x), 1L, 0L, 1L),
                  tid = ifelse(x, -tid, tid)) %>% 
    dplyr::group_by(x) %>% 
    dplyr::mutate(tid = cumsum(tid)) %>% 
    dplyr::ungroup(x) %>% 
    dplyr::pull(tid)
}
