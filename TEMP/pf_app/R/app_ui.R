#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Projectfacts reporting"),
      
      shinydashboard::dashboardSidebar(
        # conditionalPanel(condition = "!output.setupComplete",
        #                  box( title = "loading")), 
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Help", tabname = "mod_help"),
          shinydashboard::menuItem("List of workers", tabname = "mod_workerlist")
          
        )
      ), 
      
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          # fill dashboard body contents with module UI functions
          mod_help_ui("mod_help"),
          mod_workerlist_ui("mod_workerlist")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'pf_app'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

