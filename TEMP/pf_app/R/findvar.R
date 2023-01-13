#' Find which dataframes contain a given variable
#'
#' @param all_tabs e.g. all_tabs <- getPFData()
#' @param x variable name to look for (case is ignored)
#'
#' @return
#' @export
#'
#' @examples
#' all_tabs <- getPFData()
#' findvar(all_tabs, "parent")
findvar <- function(all_tabs, x){
  names(which(sapply(all_tabs, function(y) any(grepl(x, names(y), ignore.case = T)))))
}