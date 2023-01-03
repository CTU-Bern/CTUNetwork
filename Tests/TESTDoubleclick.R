library(shiny)
library(visNetwork)
library(shinydashboard)
library(ggplot2)
library(cowplot)
library(scales)
library(grid)
library(gridExtra) 
# ui <- fluidPage(
#   visNetworkOutput('network')
# )

# User interface
ui <- dashboardPage(
  dashboardHeader(title = "CTU Network", titleWidth = 220),
  
  ## Sidebar content
  dashboardSidebar(width = 220,
                   sidebarUserPanel(name = "CTU Bern",image = "unibe_logo_mh.png"), # Logo
                   sidebarMenu(id = "tab",
                               # Look up available fonts: fontawesome:::fa_tbl
                               menuItem('CTU Division',
                                        menuSubItem("Data Management", tabName = "datamanagement", icon = icon("database")),
                                        menuSubItem("Statistics", tabName = "statistics", icon = icon("chart-area")),
                                        menuSubItem("Clinical Study Management", tabName = "studymanagement", icon = icon("laptop-medical")),
                                        menuSubItem("Monitoring", tabName = "monitoring", icon = icon("check")), # Would like to use the "magnifying-glass"
                                        menuItem("Quality Management", tabName = "qualitymanagement", icon = icon("broom"))),
                               radioButtons("projectlab", label = "Project labels", choices = c("IDs", "Names"), inline=T),
                               selectInput("servicetype", label = "Service", choices = c("\a", "Basic", "Full", "Light")),
                               checkboxGroupInput('projecttype', "Project types", c("External", "Consulting","Internal","FTE"), selected = "External"), 
                               selectInput("dlfsupport", label = "DLF support", choices = c("\a", "Yes", "No")),
                               selectInput("cdms", label = "CDMS", choices = c("\a","REDCap", "secuTrial", "Webspirit")),
                               checkboxGroupInput('tables', "Export tables", c("Time Bookings","Workers","Projects"), selected = c("Time Bookings","Workers","Projects")), 
                               downloadButton("DownloadReport", "Download Report", style = "margin: 5px 5px 35px 35px; "))), 
  
  ## Body content
  dashboardBody(tags$head(tags$style(HTML(".main-sidebar { font-size: 15px; }"))), # Changing sidebar font sizes
                # Boxes need to be put in a row (or column)
                fluidRow(
                  visNetworkOutput("network", height = "1200") # Unique name for an output
                ),
                fluidRow(
                  plotOutput("mylegend")
                  )
                )
)

server <- function(input, output, session) {
  getDiagramPlot <- function(nodes, edges){
    v <- visNetwork(
      nodes, 
      edges
    ) %>%
      visPhysics(stabilization = TRUE, enabled = F) %>%
      # visOptions(highlightNearest = list(enabled = T, degree = 1, hover = F), autoResize = TRUE, collapse = FALSE) %>%
      visOptions(highlightNearest = T, nodesIdSelection = T, selectedBy= list(variable="group",multiple=T)) %>%
      visEdges(color = list(highlight = "red")) %>% # The colour of the edge linking nodes
      visLayout(improvedLayout = TRUE) %>%
      visEdges(arrows = edges$arrows) %>%
      visInteraction(multiselect = F) %>%
      visEvents(doubleClick = "function(nodes) {
            Shiny.onInputChange('current_node_id', nodes.nodes);
            ;}")
    return(v)
  }
  
  testFunction <- function(node_id){
    print(paste("The selected node ID is:", node_id))
  }
  
  # NOT WOKING WHEN I AM USING MY DATASET!!! WHY ??
  load("R:/Projectfacts/Reports/1001_DLF_money/Corentin/TESTS/NodesEdges.RData")
  # nodes <- data.frame(id = 1:3, label = 1:3, group = c("group1","group1","group2"), value = c(10,10,11), color=c("#E41A1C","#48A462","#4A72A6"))
  # edges <- data.frame(from = c(1,2), to = c(1,3), width = c(0.4,0.8))
  
  output$mylegend <- renderPlot({
    
    # Color palette
    # Emulate ggplot2 color palette
    # See: https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    Colors <- gg_color_hue(length(unique(nodes$group))+1)
    
    # Draw a temporary ggplot 
    my_hist <- ggplot(nodes, aes(y = value, fill = group)) + 
      geom_bar() + 
      theme(legend.position='bottom',
            text = element_text(size = 20)) +
      scale_fill_manual(values = Colors) +
      guides(fill = guide_legend(title = "State", title.position = "top", title.hjust = 0.5))
    
    # Retrieve the legend using the cowplot package
    legend <- cowplot::get_legend(my_hist)
    
    # return plot
    grid.newpage(); grid.draw(legend)

    
  },height=80)
  
  output$network <- renderVisNetwork(
    getDiagramPlot(nodes, edges)
  )
  
  observeEvent(input$current_node_id,{
    testFunction(input$current_node_id)
  })
}

shinyApp(ui, server)