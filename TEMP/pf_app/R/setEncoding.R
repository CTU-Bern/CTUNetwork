#' Set encoding of character variables to \code{latin-1}
#'
#' @param tabs e.g. all_tabs
#'
#' @return
#' @export
#'
#' @examples
#' all_tabs <- getPFData() %>% setEncoding()
setEncoding <- function(tabs){
  purrr::map(tabs, function(x){
    x %>% 
      dplyr::mutate(
        dplyr::across(tidyselect:::where(is.character), 
                      stringr::str_conv, 
                      encoding = "latin-1"))
  })
}



