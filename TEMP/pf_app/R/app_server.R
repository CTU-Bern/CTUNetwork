#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  message("reading data")
  # rv <- reactiveValues()
  # rv$setupComplete <- FALSE
  pf_data <- getPFData(output, rv)
  message("data read")
  
  callModule(mod_help_server, "mod_help")
  callModule(mod_workerlist_server, "mod_workerlist", pf_data)
  
  
  
}
