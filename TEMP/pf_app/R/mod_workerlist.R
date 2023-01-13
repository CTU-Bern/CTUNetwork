#' workerlist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_workerlist_ui <- function(id){
  ns <- NS(id)
  # tagList(
  # 
  # )
  message("mod_workerlist_ui")
  shinydashboard::tabItem(tabName = "mod_workerlist",
                          fluidRow(
                            h2(id = ns("title"), "List of workers")
                          ), 
                          fluidRow(
                            textOutput("n_tables")
                          ))
}
    
#' workerlist Server Functions
#'
#' @noRd 
mod_workerlist_server <- function(id){
  moduleServer( id, function(input, output, session, pf_data){
    ns <- session$ns
    message("workerlist_server")
    output$n_tables <- renderText(length(pf_data))
    message(length(pf_data))
  })
}
    
## To be copied in the UI
# mod_workerlist_ui("workerlist_ui_1")
    
## To be copied in the server
# mod_workerlist_server("workerlist_ui_1")
