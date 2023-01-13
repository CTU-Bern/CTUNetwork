#' Discard dataframes without any data (all NA)
#'
#' @param .x dataframe
#'
#' @return
#' @export
#'
#' @examples
#' data.frame(a = 1, b = NA, c = 9) %>% 
#'   discard_all_NA()
#' 
discard_all_NA <- function(.x) {
  purrr::discard(.x, ~all(is.na(.)))
}