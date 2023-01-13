#' Update the installed version of the pf package
#'
#' @param ... additional arguments to \code{link[remotes]{install_local}}
#'
#' @return
#' @export
#'
#' @examples
#' pf::update_pf()
update_pf <- function(...){
  try(detach(package:pf, unload = TRUE))
  remotes::install_local("R:/Projectfacts/ODBC/pf_app/", ...)
}