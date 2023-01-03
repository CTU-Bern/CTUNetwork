#' CTU Network app
#'
#' The CTU Network shiny app provides a dynamic network visualization of Projectfacts data
#' specific to CTU Bern. Through filtering options, the user can fine-tune the
#' input data which will influence the network representation as well as the bar graphs and table.
#'
#' The app is also available at \href{https://shiny.ctu.unibe.ch/ctunetwork/}{https://shiny.ctu.unibe.ch/ctunetwork/}.
#'
# \if{html}{\figure{app.png}{options: width="100\%"}}
#'
#' @usage
#' runCTUNetworkApp()
#' @importFrom shiny runApp
#' @export
#' @examples
#' # launch the app
#' \dontrun{
#' runCTUNetworkApp()
#' }
#'

runCTUNetworkApp <- function(){
  appDir <- system.file("shinyApp", package = "CTUNetwork")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `CTUNetwork`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
