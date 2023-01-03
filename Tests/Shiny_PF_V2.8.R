#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# TEMPORARY !!!
#Using the Renv function for increase compatibility (temporary)
# https://rstudio.github.io/renv/articles/renv.html
# Every now and then run renv::snapshot() to log the packages (and versions) you're using.
# If code doesn't work, run renv::restore to get my packages and versions

# Check if dependencies are installed
new.packages <- c("pacman", "remotes")[!(c("pacman", "remotes") %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load/install the necessary packages
invisible(lapply(c("pacman","remotes"), require, character.only = TRUE))
if (!"pf" %in% installed.packages()) {remotes::install_local("R:/Projectfacts/ODBC/pf_app/")}
Pkgs = c("rmarkdown", "shiny", "shinydashboard", "visNetwork", "scales", "igraph", 
         "rmarkdown","foreach", "pf","stringr","reshape2","dplyr","lubridate",
         "tidyr","ggplot2","cowplot", "scales", "grid", "gridExtra", "shinyjs", 
         "DT", "data.table")
pacman::p_load(char = Pkgs, install = T, update = F, character.only = T)

# Load internal functions
invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source))

# Retrieve data from ProjectFacts
All_Tabs <- getPFData()

# Only keeping useful information from All_Tabs
# $activitycategory = activity types - e.g. billable, non-billable
# $customfields = list of the custom fields
# $financearticle = types of work packages (Basic, Light, Full services)
# $projectstatedefinition = Explanations related to projects state code
All_Tabs = All_Tabs[c("activitydata","customer","crmkontakt","financeposition",
                      "project","projectstatedefinition","worker")]

# Retrieve data
Data <- extractData(All_Tabs)

# Filter data
Data <- filterData(Data, All_Tabs)

# Filtering the lines corresponding to the project level (the aim is to separate for each division)
Data$Filt <- grepl("\\.|C-", Data$ProjectID)

# Free-up memory space
remove(All_Tabs)

# Changing data type
Data[,c("ProjectIDs","ProjectNames","State")] <- lapply(Data[,c("ProjectIDs","ProjectNames","State")], as.character)

# Dataframe with type of projects and corresponding patterns
ProjTypes.df <- data.frame(Name = c("External", "Consulting","Internal","Internal","FTE"),
                           Pattern = c("P-","C-","I-","IB-","FTE-"))
ProjStr <- c("P-","C-","I-","IB-","FTE-")

# values to show, or not show, these will be the 'choices' and 'selected' values
# for the checkboxGroupInput()
AllRows <- 1:dim(Data)[2]
names(AllRows) <-colnames(Data)
SelectRows <- AllRows[c(1,3,4,8,18,20,21,22,23,24)]

#####

# User interface
ui <- dashboardPage(
  dashboardHeader(title = "CTU Network", titleWidth = 220),
  
  ## Sidebar content
  dashboardSidebar(width = 220,
    sidebarUserPanel(name = "CTU Bern",image = "unibe_logo_mh.png"), # Logo
    sidebarMenu(id = "tab",
      shinyjs::useShinyjs(), # this package enables to reset the parameters to default
      menuItem('CTU Divisions', startExpanded = TRUE,
        menuSubItem("Data Management", tabName = "datamanagement", icon = icon("database"), selected = T),
        menuSubItem("Statistics", tabName = "statistics", icon = icon("chart-area")),
        menuSubItem("Clinical Study Management", tabName = "studymanagement", icon = icon("laptop-medical")),
        menuSubItem("Monitoring", tabName = "monitoring", icon = icon("search")), 
        menuItem("Quality Management", tabName = "qualitymanagement", icon = icon("broom"))),
      radioButtons("projectlab", label = "Project labels", choices = c("IDs", "Names"), inline=T),
      selectInput("servicetype", label = "Service types", choices = c("\a", "Basic", "Full", "Light")),
      checkboxGroupInput('projecttype', "Project types", c("External", "Consulting","Internal","FTE"), selected = "External"), 
      selectInput("dlfsupport", label = "DLF support", choices = c("\a", "Yes", "No")),
      selectInput("cdms", label = "CDMS", choices = c("\a","REDCap", "secuTrial", "Webspirit")),
      dateRangeInput("prodfilter", label = "Productive date", 
                    min = min(Data$ProdDate, na.rm = T), max = max(Data$ProdDate, na.rm = T)),
      dateRangeInput("timebookfilter", label = "Time bookings", 
                     start = min(Data$BookedDate, na.rm = T), end = max(Data$BookedDate, na.rm = T), 
                     min = min(Data$BookedDate, na.rm = T), max = max(Data$BookedDate, na.rm = T)), 
      actionButton("reset", "Reset", icon = icon("undo-alt"), style="border-color: red; margin: 31px 31px 71px 71px;"))),
       
  ## Body content
  dashboardBody(useShinyjs(),
                tags$head(
                  tags$style(HTML(".main-sidebar { font-size: 15px; }")), # Changing sidebar font sizes
                  tags$script(src = "custom.js")), # loading custom javascript functions
                  # See: https://stackoverflow.com/questions/55201517/r-shinyjs-shinydashboard-box-uncollapse-on-radionbuttons-input

  # 1. LEGEND & NETWORK
  # HELP ON BOXES: https://rstudio.github.io/shinydashboard/structure.html
  fluidRow(box(title = "Network",
               width = 12,
               status = "primary", 
               solidHeader = TRUE, 
               collapsible = TRUE,
               column(10, visNetworkOutput("mynetworkid", height = "1200")),
               column(2, style = "height:150px; ",plotOutput("mylegend", height = "400"))
  )),
  
  # 2. Bar chart grouped (for selected node)
  fluidRow(box(id = "groupedbars",
               title = "Grouped bars",
               width = 12,
               status = "primary", 
               solidHeader = TRUE, 
               collapsible = TRUE, 
               collapsed = T,
               plotOutput("GroupedNodes"))),

  # 3. Bar chart over time separated by worker (for selected node)
  fluidRow(box(id = "indivbars",
               title = "Individual bars",
               width = 12,
               status = "primary", 
               solidHeader = TRUE, 
               collapsible = TRUE, 
               collapsed = T,
               plotOutput("IndivNodes", height = 1000))),
  
   # 4. Data table
   fluidRow(tags$head(tags$style(".checkbox-inline {margin: 0 !important;}")),
            box(id = "table",
                title = textOutput("TableTitle"),
                width = 12,
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE, 
                collapsed = F,
                checkboxGroupInput("tablevars", 
                                   h3("Variables to include"), 
                                   choices = AllRows,
                                   selected = SelectRows,
                                   inline = TRUE),
                DT::dataTableOutput("DataTable"))) 
  )
)

# Server
server <- function(input, output) { # Assemble inputs into outputs
  
  # Preallocate the FiltIdx variable
  FiltIdx <- vector(mode = "logical", length = dim(Data)[1]) 
  
  # Plot data ----
  # Update the database
  AllData <- reactive({
    
    # Filter for project types (External, internal, FTE projects or consultings)
    if (length(input$projecttype)>0) {
      ProjLabs.df <- data.frame(Name = input$projecttype)
      ProjLabs.df <- left_join(ProjLabs.df, ProjTypes.df, by = "Name")
      ProjStr <- paste(ProjLabs.df$Pattern,collapse="|")
      DataUp <- Data[grepl(ProjStr,Data$ProjectIDs),]
    } else {DataUp <- Data}
    
    # Selecting a division only makes sense for External projects
    if (any(input$projecttype == "External")){
      
      # Data Management division
      if (input$tab == 'datamanagement'){
        Patterns = c("Data Management","Cloud","Study Website")
        FiltIdx <- grepl(paste(Patterns,collapse="|"), DataUp$ProjectName, ignore.case=T) | 
          grepl("DM", DataUp$ProjectName, fixed = T, ignore.case=F) & 
          !grepl("Others", DataUp$ProjectName, fixed = T) # Special case to exclude

      # Statistics division  
      } else if (input$tab == 'statistics'){
        Patterns = c("Statistics","Statistical Support")
        FiltIdx <- grepl(paste(Patterns,collapse="|"), DataUp$ProjectName, ignore.case=T) | 
          grepl("DM", DataUp$ProjectName, fixed = T, ignore.case=F) | 
          grepl("Programming grant", DataUp$ProjectName, fixed = T)
      
      # Clinical Study Management division   
      } else if (input$tab == 'studymanagement'){
        Patterns = c("Clinical Study Management","Project Coordination", "Project Management", "Projectmanagement")
        FiltIdx <- grepl(paste(Patterns,collapse="|"), DataUp$ProjectName, ignore.case=T)
      
      # Monitoring division     
      } else if (input$tab == 'monitoring'){
        Patterns = c("Monitoring", "Document Development", "Safety")
        FiltIdx <- grepl(paste(Patterns,collapse="|"), DataUp$ProjectName, ignore.case=T) |
          grepl("MON", DataUp$ProjectName, fixed = T)
      
      # Quality Management division     
      } else if (input$tab == 'qualitymanagement'){
        Patterns = c("Quality Management","QM Support")
        FiltIdx <- grepl(paste(Patterns,collapse="|"), DataUp$ProjectName, ignore.case=T) |
          grepl("Graphic Design/Layout", DataUp$ProjectName, fixed = T)
      }
      
      # Filter dataset
      FiltIdx <- DataUp$PackageLvl %in% DataUp$UniqueCode[as.logical(FiltIdx)] |
        DataUp$UniqueCode %in% DataUp$ProjectLvl[as.logical(FiltIdx)]
      
      # Filter for service type
      if (input$servicetype!="\a") {FiltIdx <- FiltIdx & as.logical(mapply(grepl,input$servicetype,DataUp$ServiceType, ignore.case=T))} 
      
      # Filter for clinical data management system (CDMS)
      if (input$cdms!="\a") {FiltIdx <- FiltIdx & as.logical(mapply(grepl,input$cdms,DataUp$CDMS, ignore.case=T))}
      
      # Filter for DLF support
      if (input$dlfsupport!="\a") {
        DataUp <- DataUp[which(DataUp$DLFSupport==ifelse(input$dlfsupport=="Yes", T, F)),]
      }
      
      # Apply filtering
      DataUp <- DataUp[FiltIdx,]
      
    } 

    # Filter for productive dates
    if (all(!is.na(input$prodfilter))) {
      ProjIdx <- which(grepl(ProjStr, DataUp$ProjectID))
      FiltProd <- ProjIdx[DataUp$ProdDate[ProjIdx] %in% input$prodfilter[1]:input$prodfilter[2]]
      DataUp <- DataUp[DataUp$ProjectIDs %in% DataUp$ProjectIDs[FiltProd],]
    }
    
    # Filter for time bookings
    if (all(!is.na(input$timebookfilter))) {
      DataUp <- DataUp[!DataUp$Filt | DataUp$BookedDate %in% input$timebookfilter[1]:input$timebookfilter[2],]
      # !DataUp$Filt is to avoid deleting project and package levels
    }
    
    # Computing different calculations based on filtering parameters
    DataUp <- Calculations(DataUp)
    
    ## Preparing data for network
    # Creating a separate database for the plot (not to mess with the Markdown report data)
    # Removing the lines corresponding to the project level (the aim is to separate for each division)
    DataPlot <- DataUp[DataUp$Filt,]
    
    # lock in factor level order
    DataPlot$State <- factor(DataPlot$State, levels = unique(DataPlot$State))
    
    # Color palette
    Colors <- ggColorHue(length(unique(DataPlot$State))+1)
    
    # Colors dataframe
    Colors.df <- data.frame(id = seq(1,length(levels(DataPlot$State))+1),
                            label = c("Workers", levels(DataPlot$State)), 
                            color = Colors) 
    
    # Project type
    Projtype <- paste0("Project",input$projectlab) # Determined from radiobutton
    UniqueProj <- unique((DataPlot[,Projtype])) # List of unique projects
    
    # Edges
    Edges <- data.frame(from = DataPlot$Workers,
                        to = DataPlot[,Projtype],
                        width = rescale(DataPlot$TimeSpent, to=c(0,10)))
    
    # Nodes
    Nodes <- data.frame(id = c(unique(DataPlot$Workers),unique(DataPlot[,Projtype])),
                        label = c(unique(DataPlot$Workers),unique(DataPlot[,Projtype])), 
                        group = c(rep("Workers",length(unique(DataPlot$Workers))),(as.character(DataPlot$State[match(unique(DataPlot[,Projtype]), DataPlot[,Projtype])]))),
                        value = c(rep(10,length(unique(DataPlot$Workers))),rescale(foreach(k=1:length(UniqueProj), .combine='c') %do% 
                                                                                     sum(DataPlot$TimeSpent[which(DataPlot[,Projtype] == UniqueProj[k])], na.rm = T), to=c(0,1000))),
                        shape = c(rep("square",length(unique(DataPlot$Workers))),rep("dot",length(unique((DataPlot[,Projtype]))))))
    
    # Remove lines containing NA (under ID column)
    Nodes <- Nodes[!is.na(Nodes$id),]
    Nodes$color <- lapply(Nodes$group, function(x) Colors.df$color[match(x, Colors.df$label)]) # Custom colors
    
    # lock in factor level order
    Nodes$group <- factor(Nodes$group, levels = unique(Nodes$group))
    
    ## Preparing data for table
    # Flag to determine which lines correspond to the top level
    DataUp$Levels <- grepl("-", DataUp$ProjectID)==T
    
    # Adding the fraction column
    # Indicate the percentage of TimeSpent on a project compared to all projects
    ProjIdx <- which(DataUp$Levels==T)
    SumTimeSpent <- sum(DataUp$TimeSpent[ProjIdx],na.rm=T)
    DataUp$Fraction <- rep(NA,dim(DataUp)[1]) # Populate the new column with NAs
    DataUp$Fraction[ProjIdx] <- DataUp$TimeSpent[ProjIdx]/SumTimeSpent*100
    
    # Convert time to HH:MM 
    DataUp[,c("TimeSpent","TimeBudget")] <- lapply(DataUp[,c("TimeSpent","TimeBudget")], ConvertTime)
    
    # Outputs
    return(list(DataPlot = DataPlot, DataUp = DataUp, Nodes = Nodes, Edges = Edges, Colors = Colors.df))
  })
  # ----
  ### RENDERING PLOTS ###
  
  # 1. Network plot
  output$mynetworkid <- renderVisNetwork(
    NetworkPlot(AllData()$Nodes, AllData()$Edges) # , AllData()$Colors
  )
  
  # 2. Legend
  output$mylegend <- renderPlot({

    # Draw a temporary ggplot 
    my_hist <- ggplot(AllData()$Nodes, aes(y = value, fill = group)) + 
      geom_bar() + 
      theme(legend.position='right',
            text = element_text(size = 15),
            legend.spacing.y = unit(1.0, 'cm')) +
      scale_fill_manual(values = as.character(unique(AllData()$Nodes$color))) +
      guides(fill = guide_legend(title = "State", title.position = "top", title.hjust = 0.5, byrow = TRUE))
    
    # Retrieve the legend using the cowplot package
    legend <- cowplot::get_legend(my_hist)
    
    # return plot
    grid.newpage(); grid.draw(legend)
    
  })

  # 3. Nodes plot
  # https://stackoverflow.com/questions/41018899/get-selected-node-data-from-visnetwork-graph-without-actionbutton
  # https://github.com/datastorm-open/visNetwork/issues/176
  
  # Filter data
  Plot.df <- reactive(
    if (grepl("P-",input$node_id, ignore.case = T)) {
      Plot.df <- AllData()$DataPlot[AllData()$DataPlot$ProjectIDs==input$node_id,]
    } else {
      Plot.df <- AllData()$DataPlot[AllData()$DataPlot$Workers==input$node_id,]
    })

  # 3.1 Grouped nodes
  output$GroupedNodes <- renderPlot({
    if (!is.null(input$node_id)) {
      if (grepl("P-",input$node_id, ignore.case = T)) {

        # Plot
        ggplot(Plot.df(), aes(x = Workers, y = TimeSpent, fill = Workers)) +
          labs(title = paste(input$node_id, Plot.df()$ProjectNames, sep=" - "), y = "Time Booked [Min]") +
          geom_bar(stat = "identity", alpha = 0.8) +
          stat_summary(aes(label = after_stat(y)), fun = sum, geom = 'text', vjust = -0.4) +
          themeShiny(titleSize = 17)
      }
    }
  })
  
  # 3.2 Individual nodes
  output$IndivNodes <- renderPlot({
    if (!is.null(input$node_id)) {
      
      Plots <- list(); TEMP.df <- data.frame()
      FilterTag <- ifelse(grepl("P-",input$node_id),"Workers","ProjectIDs")
      FilterItem <- sort(unique(Plot.df()[,FilterTag]), index.return = T)
      ProjNames <- unique(Plot.df()$ProjectNames)[FilterItem$ix]
      Colors <- ggColorHue(length(FilterItem$x))
      for(i in 1:length(FilterItem$x)){
        TEMP.df <- Plot.df()[Plot.df()[,FilterTag]==FilterItem$x[i],]
        Plots[[i]] <- ggplot(TEMP.df, aes(x = BookedDate, y = TimeSpent, fill = .data[[FilterTag]])) +
          scale_fill_manual(values = Colors[i]) + 
          scale_x_date(date_labels = "%d-%m-%Y") +
          labs(title = FilterItem$x[i], x = "Booked Date", y = "Time Spent [Min]") +
          themeShiny() + # homemade theme 
          theme(legend.position = "none") 
        
        # Either density or bar chart depending on number of time points
        if (length(unique(TEMP.df$BookedDate))>1) { 
          Plots[[i]] <- Plots[[i]] + 
            geom_area(aes(fill = .data[[FilterTag]]), alpha = 0.6) + # , position = position_dodge(0.8)
            stat_summary(fun = sum, geom = 'point', color = "#999999", size = 1)
        } else {
          Plots[[i]] <- Plots[[i]] + geom_col(alpha = 0.6)
        }
        # Adding project name to ID only if displaying grid of projects
        if (!grepl("P-",input$node_id)){
          Plots[[i]] <- Plots[[i]] + labs(title = paste(FilterItem$x[i], ProjNames[i], sep=" - "))}
       }
      do.call("grid.arrange", c(Plots,ncol=3))
    }
  })

  # 4.0 Data table
  output$DataTable = DT::renderDataTable({
    
    # Columns are adjusted based on checkboxes
    Cols <- c(colnames(AllData()$DataUp[as.integer(input$tablevars)]),"Fraction","Levels")
    
    # By default return the whole table and filter if node is selected
    if (is.null(input$node_id)) {
        BuildTable(AllData()$DataUp[,Cols])
      
      } else {
        # If one node is selected
        if(grepl("P-",input$node_id)) {
          # IdxProj <- AllData()$DataUp$ProjectIDs=="P-0628"
          IdxProj <- AllData()$DataUp$ProjectIDs==input$node_id
          BuildTable(AllData()$DataUp[IdxProj,Cols])
         
        # If one worker is selected
        } else {
          # IdxWorker <- grepl("André Moser",AllData()$DataUp$Workers,ignore.case = T)
          IdxWorker <- grepl(input$node_id,AllData()$DataUp$Workers,ignore.case = T)
          BuildTable(AllData()$DataUp[IdxWorker,Cols])
        }
      }
   })

  #### OBSERVE EVENTS
  # Enables to reset the parameters from SideBar
  # See: https://stackoverflow.com/questions/54347972/reset-action-button-output-in-shiny
  observeEvent(input$reset, {
    shinyjs::reset()
  })
  
  # Datatable box title
  output$TableTitle <- renderText({
    paste("Data table:",ifelse(is.null(input$node_id),"All",input$node_id))
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
