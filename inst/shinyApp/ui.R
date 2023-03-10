# CTU Network Shiny app UI
#' @importFrom shinyjs useShinyjs
#' @importFrom DT dataTableOutput
#' @importFrom visNetwork visNetworkOutput
#' @importFrom shinyBS bsTooltip
#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
#' @import shinydashboardPlus
#' @export

# User interface
ui <- shinydashboard::dashboardPage(

  # Header
  shinydashboardPlus::dashboardHeader(title = "CTU Network",
                                      titleWidth = 220,
                                      leftUi = tagList(
                                        shinydashboardPlus::dropdownBlock(
                                          id = "graphparams",
                                          title = "Graph parameters",
                                          icon = shiny::icon("gears"),
                                          shinyWidgets::prettyRadioButtons(
                                            inputId = "physics",
                                            label = "Physics",
                                            choices = c("Yes","No"),
                                            selected = Defaults$physics,
                                            inline = TRUE,
                                            fill = TRUE,
                                            animation = "smooth",
                                            plain = TRUE),
                                          shiny::selectInput(
                                            inputId = "layout",
                                            label = "Layout",
                                            # See list of layouts: https://rstudio-pubs-static.s3.amazonaws.com/337696_c6b008e0766e46bebf1401bea67f7b10.html#network-layouts
                                            choices = c("Layout on sphere", "Layout on grid", "Layout nicely",
                                                        "Layout Reingold Tilford", "Layout components", "Layout as star",
                                                        "Layout in circle", "Layout with fr", "Layout with kk",
                                                        "Layout with mds", "Layout with lgl", "Layout with dh",
                                                        "Layout with gem", "Layout with graphopt", "Layout with sugiyama",
                                                        "Layout randomly"),
                                            selected = Defaults$layout),
                                          shinyWidgets::prettyRadioButtons(
                                            inputId = "solver",
                                            label = "Solver",
                                            choices = c("barnesHut", "forceAtlas2Based", "repulsion", "hierarchicalRepulsion"),
                                            selected = Defaults$solver,
                                            inline = FALSE,
                                            fill = TRUE,
                                            animation = "smooth",
                                            plain = TRUE),

                                          ## Physics parameters
                                          # See visPhysics: https://rdrr.io/cran/visNetwork/man/visPhysics.html
                                          shiny::conditionalPanel('input.solver == "barnesHut" || input.solver == "forceAtlas2Based"',
                                          shiny::uiOutput("theta.ui"),
                                          shiny::uiOutput("gravitationalConstant.ui")),
                                          shiny::conditionalPanel('input.solver == "repulsion" || input.solver == "hierarchicalRepulsion"',
                                          shiny::uiOutput("nodeDistance.ui")),
                                          shiny::uiOutput("centralGravity.ui"),
                                          shiny::uiOutput("springLength.ui"),
                                          shiny::uiOutput("springConstant.ui"),
                                          shiny::uiOutput("damping.ui"),
                                          shiny::conditionalPanel('input.solver != "repulsion""',
                                          shiny::uiOutput("avoidOverlap.ui")),
                                          shinyWidgets::sliderTextInput(
                                            inputId = "timestep",
                                            label = "timestep",
                                            choices = seq(0.01,1,0.01),
                                            selected = Defaults$timestep),
                                          shinyBS::bsTooltip("timestep", "The physics simulation is discrete. This means we take a step in time, calculate the forces, move the nodes and take another step. If you increase this number the steps will be too large and the network can get unstable. If you see a lot of jittery movement in the network, you may want to reduce this value a little.",
                                                            "left", options = list(container = "body")),
                                          shinyWidgets::sliderTextInput(
                                            inputId = "windX",
                                            label = "windX",
                                            choices = seq(-10,10,0.1),
                                            selected = Defaults$wind$X),
                                          shinyBS::bsTooltip("windX", "A force that pushes all non-fixed nodes in the given direction. Requires all nodes are connected to nodes which are fixed, otherwise non-attached nodes will keep moving indefinitely. The amount of force to be applied pushing non-fixed nodes to the right (positive value) or to the left (negative value)",
                                                            "left", options = list(container = "body")),
                                          shinyWidgets::sliderTextInput(
                                            inputId = "windY",
                                            label = "windY",
                                            choices = seq(-10,10,0.1),
                                            selected = Defaults$wind$Y),
                                          shinyBS::bsTooltip("windY", "A force that pushes all non-fixed nodes in the given direction. Requires all nodes are connected to nodes which are fixed, otherwise non-attached nodes will keep moving indefinitely. The amount of force to be applied pushing non-fixed nodes downwards (positive value) or upwards (negative value)",
                                                            "left", options = list(container = "body")),
                                          shiny::actionButton("defaults", "Save as defaults", icon = shiny::icon("floppy-disk"))
                                          ))),
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
      shiny::radioButtons("projectlab", label = "Project labels", choices = c("IDs", "Names"),  inline=T),
      shiny::selectInput("servicetype", label = "Service types", choices = c("\a", "Basic", "Full", "Light")),
      shiny::checkboxGroupInput('projecttype', "Project types", c("External", "Consulting","Internal","FTE"), selected = "External"),
      shiny::selectInput("dlfsupport", label = "DLF support", choices = c("\a", "Yes", "No")),
      shiny::selectInput("dlfreached", label = "DLF reached", choices = c("\a", "Yes", "No")),
      shiny::selectInput("cdms", label = "CDMS", choices = c("\a","REDCap", "secuTrial", "Webspirit")),
      shiny::dateRangeInput("prodfilter", label = "Productive date",
                    min = min(Data$ProdDate, na.rm = T), max = max(Data$ProdDate, na.rm = T)),
      shiny::dateRangeInput("timebookfilter", label = "Time bookings",
                     start = min(Data$BookedDate, na.rm = T), end = max(Data$BookedDate, na.rm = T),
                     min = min(Data$BookedDate, na.rm = T), max = max(Data$BookedDate, na.rm = T)),
      shiny::actionButton("reset", "Reset", icon = shiny::icon("undo-alt"), style="border-color: red; margin: 31px 31px 71px 71px;"))),

  ## Body content
  shinydashboard::dashboardBody(shinyjs::useShinyjs(),
                tags$head(
                  tags$style(HTML(".main-sidebar { font-size: 15px; }")), # Changing sidebar font sizes
                  tags$script(src = "custom.js")), # loading custom javascript functions
                  # See: https://stackoverflow.com/questions/55201517/r-shinyjs-shinydashboard-box-uncollapse-on-radionbuttons-input

                # Hide the gear ico from dashboardHeader (top right)
                # See: https://stackoverflow.com/questions/71058072/remove-toggle-gear-icon-controlbar
                tags$script(HTML('var e = document.querySelector("body > div.wrapper > header > nav > div:nth-child(4) > ul > li:last-child > a");
                      e.setAttribute("style", "display: none;");')),

  # 1. LEGEND & NETWORK
  # HELP ON BOXES: https://rstudio.github.io/shinydashboard/structure.html
  shiny::fluidRow(
    shinydashboard::box(title = "Network",
               width = 12,
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               column(10, visNetwork::visNetworkOutput("mynetworkid", height = "1200")),
               column(2, style = "height:150px; ",
                      shiny::plotOutput("mylegend", height = "400")))),

  # Grouping all in a tabBox instead of individual boxes
  shiny::fluidRow(
    shinydashboard::box(id = "Bargraphs",
                        title = "Bar graphs",
                         width = 12,
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = TRUE,
                         collapsed = T,
                         shinydashboard::tabBox(width = 12,
                                               # 2. Bar chart grouped (for selected node)
                                               shiny::tabPanel(id = "groupedbars",
                                                              title = "Grouped bars",
                                                              shiny::plotOutput("GroupedNodes")),
                                               # 3. Bar chart over time separated by worker (for selected node)
                                               shiny::tabPanel(id = "indivbars",
                                                              title = "Individual bars",
                                                              shiny::uiOutput("IndivNodes.ui"))))),

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
                shiny::selectInput("tabledata", label = h3("Show individual time bookings"),
                                   choices = c("Yes", "No"), selected = "Yes", width = "20%"),
                DT::dataTableOutput("DataTable", width = "99%")))
  )
)
