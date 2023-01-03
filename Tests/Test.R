library(shiny)
library(visNetwork)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(title = "Test visEvents Javascript"),
  dashboardSidebar(width = 220,
                   actionButton("open", "Open Box"),
                   actionButton("close", "Close Box")),
  dashboardBody(useShinyjs(),
                tags$head(
                  tags$script(src = "custom.js")), # loading custom javascript functions
  fluidRow(box(id = "network",
               title = "Network",
               status = "primary",
               solidHeader = TRUE, 
               collapsible = TRUE,
               visNetworkOutput('network'))),
  
  # This box should collapse/uncollapse when node is selected/deselected
   fluidRow(box(id = "collapse",
                title = "Collapse",
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                collapsed = TRUE)),
))

server <- function(input, output, session) {
  getDiagramPlot <- function(nodes, edges){
    v <- visNetwork(
      nodes, 
      edges
    ) %>%
      visPhysics(stabilization = TRUE, enabled = TRUE) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = F), autoResize = TRUE, collapse = FALSE) %>%
      visEdges(color = list(highlight = "red")) %>% # The colour of the edge linking nodes
      visLayout(improvedLayout = TRUE) %>%
      visEdges(arrows = edges$arrows) %>%
      visInteraction(multiselect = F) 
    # %>%
    #   visEvents(selectNode = "openBox('collapse')",
    #             deselectNode = "closeBox('collapse')")
      # visEvents(selectNode = "function() {shinyjs::runjs('openBox('collapse')');;}",
      #         deselectNode = "function() {shinyjs::runjs('closeBox('collapse')');;}")
    return(v)
  }
  
  nodes <- data.frame(id = 1:3, label = 1:3)
  edges <- data.frame(from = c(1,2), to = c(1,3))
  
  output$network <- renderVisNetwork(
    getDiagramPlot(nodes, edges)
  )
  
  observeEvent(input$open, {
    shinyjs::runjs("openBox('collapse')")
  }, ignoreNULL = TRUE)
  
  observeEvent(input$close, {
    shinyjs::runjs("closeBox('collapse')")
  }, ignoreNULL = TRUE)
}

shinyApp(ui, server)