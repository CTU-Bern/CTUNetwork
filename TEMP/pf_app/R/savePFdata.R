#' save projectfacts data to R drive
#'
#' @param all_tabs data from e.g. ODBC
#' @param dir directory in which to save the data
#' @param dated logical - whether to include the date in the filename
#'
#' @return
#' @export
#'
#' @examples
#' # load the data from R
#' # all_tabs <- getPFData()
#' ## download data via ODBC
#' # all_tabs <- getPFData(NULL)
#' # all_tabs$customer <- decodeCustomFields(all_tabs$customer, all_tabs$customfields)
#' # all_tabs$project <- decodeCustomFields(all_tabs$project, all_tabs$customfields)
#' # all_tabs$ticket <- decodeCustomFields(all_tabs$ticket, all_tabs$customfields)
#' # all_tabs$worker <- decodeCustomFields(all_tabs$worker, all_tabs$customfields)
#' ## save the data to R
#' # savePFdata(all_tabs)
savePFdata <- function(all_tabs, dir = "R:/Projectfacts/ODBC", dated = FALSE){
  saveRDS(all_tabs, file.path(dir, "pf_tabs.rds"))
  if(dated) saveRDS(all_tabs, file.path(dir, paste0("pf_tabs_", Sys.Date(),".rds")))
}