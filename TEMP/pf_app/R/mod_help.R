#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_help_ui <- function(id){
  ns <- NS(id)
  # tagList(
  # )
  shinydashboard::tabItem(tabName = "mod_help",
                          fluidRow(
                            h2(id = ns("title"), "Help")
                          ), 
                          fluidRow(
                            "Welcome!"
                          ))
}
    
#' help Server Functions
#'
#' @noRd 
mod_help_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_help_ui("help_ui_1")
    
## To be copied in the server
# mod_help_server("help_ui_1")
