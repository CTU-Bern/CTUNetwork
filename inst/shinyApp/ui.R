# CTU Network Shiny app UI
#' @importFrom shinyjs useShinyjs
#' @importFrom DT dataTableOutput
#' @importFrom visNetwork visNetworkOutput
#' @import shiny
#' @import shinydashboard
#' @export

# User interface
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "CTU Network", titleWidth = 220),

  ## Sidebar content
  shinydashboard::dashboardSidebar(width = 220,
      shinydashboard::sidebarUserPanel(name = "CTU Bern",image = "unibe_logo_mh.png"), # Logo
      shinydashboard::sidebarMenu(id = "tab",
      shinyjs::useShinyjs(), # this package enables to reset the parameters to default
      shinydashboard::menuItem('CTU Divisions', startExpanded = TRUE,
                               shinydashboard::menuSubItem("Data Management", tabName = "datamanagement", icon = icon("database"), selected = T),
                               shinydashboard::menuSubItem("Statistics", tabName = "statistics", icon = icon("chart-area")),
                               shinydashboard::menuSubItem("Clinical Study Management", tabName = "studymanagement", icon = icon("laptop-medical")),
                               shinydashboard::menuSubItem("Monitoring", tabName = "monitoring", icon = icon("search")),
                               shinydashboard::menuItem("Quality Management", tabName = "qualitymanagement", icon = icon("broom"))),
      shiny::radioButtons("projectlab", label = "Project labels", choices = c("IDs", "Names"), inline=T),
      shiny::selectInput("servicetype", label = "Service types", choices = c("\a", "Basic", "Full", "Light")),
      shiny::checkboxGroupInput('projecttype', "Project types", c("External", "Consulting","Internal","FTE"), selected = "External"),
      shiny::selectInput("dlfsupport", label = "DLF support", choices = c("\a", "Yes", "No")),
      shiny::selectInput("cdms", label = "CDMS", choices = c("\a","REDCap", "secuTrial", "Webspirit")),
      shiny::dateRangeInput("prodfilter", label = "Productive date",
                    min = min(Data$ProdDate, na.rm = T), max = max(Data$ProdDate, na.rm = T)),
      shiny::dateRangeInput("timebookfilter", label = "Time bookings",
                     start = min(Data$BookedDate, na.rm = T), end = max(Data$BookedDate, na.rm = T),
                     min = min(Data$BookedDate, na.rm = T), max = max(Data$BookedDate, na.rm = T)),
      shiny::actionButton("reset", "Reset", icon = icon("undo-alt"), style="border-color: red; margin: 31px 31px 71px 71px;"))),

  ## Body content
  shinydashboard::dashboardBody(shinyjs::useShinyjs(),
                tags$head(
                  tags$style(HTML(".main-sidebar { font-size: 15px; }")), # Changing sidebar font sizes
                  tags$script(src = "custom.js")), # loading custom javascript functions
                  # See: https://stackoverflow.com/questions/55201517/r-shinyjs-shinydashboard-box-uncollapse-on-radionbuttons-input

  # 1. LEGEND & NETWORK
  # HELP ON BOXES: https://rstudio.github.io/shinydashboard/structure.html
  shiny::fluidRow(
    shinydashboard::box(title = "Network",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               column(10, visNetwork::visNetworkOutput("mynetworkid", height = "1200")),
               column(2, style = "height:150px; ", shiny::plotOutput("mylegend", height = "400"))
  )),

  # 2. Bar chart grouped (for selected node)
  shiny::fluidRow(
    shinydashboard::box(id = "groupedbars",
               title = "Grouped bars",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = T,
               shiny::plotOutput("GroupedNodes"))),

  # 3. Bar chart over time separated by worker (for selected node)
  shiny::fluidRow(
    shinydashboard::box(id = "indivbars",
               title = "Individual bars",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = T,
               # shiny::plotOutput("IndivNodes", height = 1000)
               shiny::uiOutput("IndivNodes.ui"))),

   # 4. Data table
  shiny::fluidRow(tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
    shinydashboard::box(id = "table",
                title = textOutput("TableTitle"),
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = F,
                shiny::checkboxGroupInput("tablevars",
                                   h3("Variables to include"),
                                   choices = AllRows,
                                   selected = SelectRows,
                                   inline = TRUE),
                DT::dataTableOutput("DataTable", width = "99%")))
  )
)
