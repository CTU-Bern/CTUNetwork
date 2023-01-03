library(shiny)
library(shinyjs)
library(shinydashboard)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Demo"),
  dashboardSidebar(
    actionButton("open", "Open Box"),
    actionButton("close", "Close Box")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    box(id = 'x',
        collapsible = T,
        collapsed = T,
        solidHeader = TRUE,
        title = 'Box',
        p("Hello"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$open, {
    shinyjs::runjs("openBox('x')")
  }, ignoreNULL = TRUE)
  
  observeEvent(input$close, {
    shinyjs::runjs("closeBox('x')")
  }, ignoreNULL = TRUE)
}

# Run the application
shinyApp(ui = ui, server = server)