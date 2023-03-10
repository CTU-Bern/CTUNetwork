# CTU Network Shiny app server
#' @importFrom shinyjs reset
#' @importFrom stringr str_replace_all
#' @importFrom cowplot get_legend
#' @importFrom scales rescale
#' @importFrom foreach foreach
#' @importFrom gridExtra grid.arrange
#' @importFrom shinyWidgets updateSliderTextInput
#' @importFrom shinyBS tipify
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs disable
#' @importFrom ipify get_ip
#' @importFrom foreach foreach
#' @importFrom foreach "%do%"
#' @import visNetwork
#' @import dplyr
#' @import grid
#' @import ggplot2
#' @import shiny
#' @export

require(CTUNetwork)
require(ggplot2)

# Server
server <- function(input, output, session) { # Assemble inputs into outputs

  # Preallocate the FiltIdx variable
  FiltIdx <- vector(mode = "logical", length = dim(Data)[1])

  # Plot data ----
  # Update the database
  AllData <- shiny::reactive({

    # Filter for project types (External, internal, FTE projects or consultings)
    if (length(input$projecttype)>0) {
      ProjLabs.df <- data.frame(Name = input$projecttype)
      ProjLabs.df <- dplyr::left_join(ProjLabs.df, ProjTypes.df, by = "Name")
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
      if (input$servicetype!="\a") {FiltIdx <- FiltIdx & grepl(input$servicetype, DataUp$Service, ignore.case=T)}

      # Filter for clinical data management system (CDMS)
      # if (input$cdms!="\a") {FiltIdx <- FiltIdx & as.logical(mapply(grepl,input$cdms,DataUp$CDMS, ignore.case=T))}
      if (input$cdms!="\a") {FiltIdx <- FiltIdx & grepl(input$cdms,DataUp$CDMS, ignore.case=T)}

      # Filter for DLF support included
      if (input$dlfsupport!="\a") {
        FiltIdx <- FiltIdx & grepl(ifelse(input$dlfsupport=="Yes", T, F),DataUp$DLFSupport)
      }

      # Filter for DLF support reached (at within specific date range, if specified by time bookings)
      if (input$dlfreached!="\a") {
        # DataUp$DLFReached <- as.Date(DataUp$DLFReached)
        # DLFReachedDF <- data.frame(Projects = unique(DataUp$ProjectLvl), DLFReachedYN = NA)
        # DLFReachedDF$DLFReachedYN <- foreach::foreach(k=1:length(DLFReachedDF$Projects), .combine = 'c') %do%
        #   (DataUp$DLFReached[which(DataUp$UniqueCode == DLFReachedDF$Projects[k])[1]] %in% input$timebookfilter[1]:input$timebookfilter[2])
        #
        # # Save to filtering index
        # FiltIdx <- FiltIdx &
        #   DataUp$ProjectLvl %in% DLFReachedDF$Projects[DLFReachedDF$DLFReachedYN==ifelse(input$dlfreached=="Yes",T,F)]
      }

      # Temporarily disable the "DLF reached" selectinput component
      # See: https://stackoverflow.com/questions/60975324/shiny-disable-selectinput-but-the-users-can-see-the-available-options-but-i-can
      shinyjs::disable("dlfreached")


      # Apply filtering
      DataUp <- DataUp[FiltIdx,]
      DataUp <- droplevels(DataUp) # remove unused factor levels
    }

    # Filter for productive dates
    if (all(!is.na(input$prodfilter))) {
      ProjIdx <- which(grepl(ProjStr, DataUp$ProjectID))
      FiltProd <- ProjIdx[DataUp$ProdDate[ProjIdx] %in% input$prodfilter[1]:input$prodfilter[2]]
      DataUp <- DataUp[DataUp$ProjectIDs %in% DataUp$ProjectIDs[FiltProd],]
    }

    # Filter for time bookings
    if (all(!is.na(input$timebookfilter))) {
      DataUp <- DataUp[is.na(DataUp$BookedDate) | DataUp$BookedDate %in% input$timebookfilter[1]:input$timebookfilter[2],]
    }

    # Computing different calculations based on filtering parameters
    DataUp <- Calculations(DataUp)

    ## Preparing data for network
    # Removing the lines corresponding to the project level (the aim is to separate for each division)
    DataPlot <- DataUp[DataUp$Filt,]

    # lock in factor level order
    DataPlot$State <- factor(DataPlot$State, levels = unique(DataPlot$State))

    # Color palette
    Colors <- ggColorHue(length(levels(DataPlot$State))+1)

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
                        width = scales::rescale(DataPlot$TimeSpent, to=c(0,10)))

    # Nodes
    Nodes <- data.frame(id = c(unique(DataPlot$Workers),unique(DataPlot[,Projtype])),
                        label = c(unique(DataPlot$Workers),unique(DataPlot[,Projtype])),
                        group = c(rep("Workers",length(unique(DataPlot$Workers))),
                                  (as.character(DataPlot$State[match(unique(DataPlot[,Projtype]), DataPlot[,Projtype])]))),
                        value = c(rep(10,length(unique(DataPlot$Workers))),
                                  scales::rescale(foreach::foreach(k=1:length(UniqueProj), .combine='c') %do%
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
    return(list(DataPlot = DataPlot, DataUp = DataUp, Nodes = Nodes, Edges = Edges))
  })
  # ----
  ### RENDERING PLOTS ###

  # Retrieve graph parameters from UI
  GraphParams <- shiny::reactive(
    list(layout = input$layout,
         physics = input$physics,
         solver = input$solver,
         timestep = input$timestep,
         barnesHut = list(theta = input$theta,
                          gravitationalConstant = input$gravitationalConstant,
                          centralGravity = input$centralGravity,
                          springLength = input$springLength,
                          springConstant = input$springConstant,
                          damping = input$damping,
                          avoidOverlap = input$avoidOverlap),
         forceAtlas2Based = list(theta = input$theta,
                                 gravitationalConstant = input$gravitationalConstant,
                                 centralGravity = input$centralGravity,
                                 springLength = input$springLength,
                                 springConstant = input$springConstant,
                                 damping = input$damping,
                                 avoidOverlap = input$avoidOverlap),
         repulsion = list(nodeDistance = input$nodeDistance,
                          centralGravity = input$centralGravity,
                          springLength = input$springLength,
                          springConstant = input$springConstant,
                          damping = input$damping),
         hierarchicalRepulsion = list(nodeDistance = input$nodeDistance,
                                      centralGravity = input$centralGravity,
                                      springLength = input$springLength,
                                      springConstant = input$springConstant,
                                      damping = input$damping,
                                      avoidOverlap = input$avoidOverlap),
         wind = list(X = input$windX, Y = input$windY))
  )

  # 1. Network plot
  output$mynetworkid <- visNetwork::renderVisNetwork( {
    Layout <- tolower(ifelse(input$layout == "Layout Reingold Tilford",
                             stringr::str_replace_all(input$layout, " ","."),
                             stringr::str_replace_all(input$layout, " ","_")))
    NetworkPlot(AllData()$Nodes, AllData()$Edges, layout = Layout, physics = ifelse(input$physics == "Yes", T, F))
  })

  # 2. Legend
  output$mylegend <- shiny::renderPlot({

    # Draw a temporary ggplot
    my_hist <- ggplot(AllData()$Nodes, aes(y = value, fill = group)) +
      geom_bar() +
      theme(legend.position='right',
            text = element_text(size = 15),
            legend.spacing.y = unit(0.7, 'cm')) +
      scale_fill_manual(values = as.character(unique(AllData()$Nodes$color))) +
      guides(fill = guide_legend(title = "State", title.position = "top", title.hjust = 0.5, byrow = TRUE))

    # Retrieve the legend using the cowplot package
    legend <- cowplot::get_legend(my_hist)

    # return plot
    grid::grid.newpage()
    grid::grid.draw(legend)

  })

  # 3. Nodes plot
  # https://stackoverflow.com/questions/41018899/get-selected-node-data-from-visnetwork-graph-without-actionbutton
  # https://github.com/datastorm-open/visNetwork/issues/176

  # Filter data
  Plot.df <- shiny::reactive(
    if (any(grepl(input$node_id,unique(AllData()$DataPlot$Workers)))) {
      Plot.df <- AllData()$DataPlot[AllData()$DataPlot$Workers %in% input$node_id,]
    } else {
      Plot.df <- AllData()$DataPlot[AllData()$DataPlot[paste0("Project",input$projectlab)]==input$node_id,]
      Plot.df <- Plot.df[!is.na(Plot.df$Workers),]
    })

  # 3.1 Grouped nodes
  output$GroupedNodes <- shiny::renderPlot({
    if (!is.null(input$node_id)) {
      if (!any(grepl(input$node_id,unique(Plot.df()$Workers)))) {

        # Plot
        ggplot(Plot.df(), aes(x = Workers, y = TimeSpent, fill = Workers)) +
          # labs(title = paste(input$node_id, Plot.df()$ProjectNames, sep=" - "), y = "Time Booked [Min]") +
          labs(title = paste(Plot.df()$ProjectIDs, Plot.df()$ProjectNames, sep=" - "), y = "Time Spent [Min]") +
          geom_bar(stat = "identity", alpha = 0.8) +
          stat_summary(aes(label = after_stat(y)), fun = sum, geom = 'text', vjust = -0.4) +
          themeShiny(titleSize = 17)
      }
    }
  })

  # 3.2 Individual nodes
  plotCount <- reactive({
    req(input$node_id)
    # FilterTag <- ifelse(grepl("P-",input$node_id),"Workers","ProjectIDs")
    FilterTag <- ifelse(any(grepl(input$node_id,unique(Plot.df()$Workers))),"ProjectIDs","Workers")
    FilterItem <- sort(unique(Plot.df()[,FilterTag]), index.return = T)
    length(FilterItem$x)
  })

  # Dynamically adjusting plot height
  plotHeight <- reactive(300 * ceiling(plotCount()/3))

  output$IndivNodes <- shiny::renderPlot({
    if (!is.null(input$node_id)) {

      Plots <- list(); TEMP.df <- data.frame()
      # FilterTag <- ifelse(grepl("P-",input$node_id),"Workers","ProjectIDs")
      FilterTag <- ifelse(any(grepl(input$node_id,unique(Plot.df()$Workers))),"ProjectIDs","Workers")
      FilterItem <- sort(unique(Plot.df()[,FilterTag]), index.return = T)
      ProjNames <- unique(Plot.df()$ProjectNames)[FilterItem$ix]
      Colors <- ggColorHue(length(FilterItem$x))
      for(i in 1:length(FilterItem$x)){
        TEMP.df <- Plot.df()[Plot.df()[,FilterTag] %in% FilterItem$x[i],]
        Plots[[i]] <-
          ggplot(TEMP.df, aes(x = BookedDate, y = TimeSpent, fill = .data[[FilterTag]])) +
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
          Plots[[i]] <- Plots[[i]] +
            geom_col(alpha = 0.6)
        }
        # Adding project name to ID only if displaying grid of projects
        if (any(grepl(input$node_id,unique(Plot.df()$Workers)))){
          Plots[[i]] <- Plots[[i]] +
          labs(title = stringr::str_wrap(paste(FilterItem$x[i], ProjNames[i], sep=" - "),50))}
       }
      do.call(gridExtra::grid.arrange, c(Plots,ncol=3))
    }
  })

  # Render the plot
  output$IndivNodes.ui <- shiny::renderUI({
    shiny::plotOutput("IndivNodes", height = plotHeight())
  })

  # 4.0 Data table
  output$DataTable = DT::renderDataTable({

    # Removing the list of all workers at the project level (useless)
    DataUpTab <- AllData()$DataUp
    Idx = grepl("\\,", DataUpTab$Workers)
    DataUpTab$Workers[Idx] <- NA

    # No need to repeat the DLF support & service values
    DataUpTab$DLFSupport[DataUpTab$Levels == F] <- NA
    DataUpTab$DLFReached <- as.factor(DataUpTab$DLFReached) # looks different if not factor
    DataUpTab$Service[DataUpTab$Levels == F] <- NA

    # Round MoneySpent to 2 decimals
    DataUpTab$MoneySpent <- round(DataUpTab$MoneySpent,2)

    # Columns are adjusted based on checkboxes
    Cols <- c(colnames(DataUpTab[as.integer(input$tablevars)]),"Fraction","Levels")

    # Rows are adjusted based on input
    ifelse(input$tabledata == "Yes", Rows <- 1:dim(DataUpTab)[1], Rows <- which(!DataUpTab$Filt))

    # By default return the whole table and filter if node is selected
    if (is.null(input$node_id)) {
        BuildTable(DataUpTab[Rows,Cols])

      } else {
        # If one node is selected
        if(grepl("P-",input$node_id)) {
          Idx <- which(DataUpTab$ProjectIDs==input$node_id)

        # If one worker is selected
        } else {
          Idx <- which(grepl(input$node_id,DataUpTab$Workers,ignore.case = T))
        }
        BuildTable(DataUpTab[intersect(Idx,Rows), Cols])
      }
   })

  #### OBSERVE EVENTS
  # Enables to reset the parameters from SideBar
  # See: https://stackoverflow.com/questions/54347972/reset-action-button-output-in-shiny
  shiny::observeEvent(input$reset, {
    shinyjs::reset()
  })

  # Physics default parameters
  # See visPhysics: https://rdrr.io/cran/visNetwork/man/visPhysics.html
  PhysicsDef <- data.frame(
    theta = c(0.5, 0.5, NA, NA),
    gravitationalConstant = c(-2000, -50, NA, NA),
    nodeDistance = c(NA, NA, 100, 120),
    centralGravity = c(0.3, 0.1, 0.2, 0.0),
    springLength = c(95, 100, 200, 100),
    springConstant = c(0.04, 0.08, 0.05, 0.01),
    damping = c(0.09, 0.04, 0.09, 0.09),
    avoidOverlap = c(0, 0, NA, 0)
  )
  # VisPhysics solver list
  SolverList <- c("barnesHut", "forceAtlas2Based", "repulsion", "hierarchicalRepulsion")

  # Update defaults based on selected solver
  observeEvent(input$solver,{
    Pos <- which(SolverList == input$solver)
    shinyWidgets::updateSliderTextInput(session, inputId = "theta", selected = PhysicsDef$theta[Pos])
    shinyWidgets::updateSliderTextInput(session, inputId = "gravitationalConstant", selected = PhysicsDef$gravitationalConstant[Pos])
    shinyWidgets::updateSliderTextInput(session, inputId = "nodeDistance", selected = PhysicsDef$nodeDistance[Pos])
    shinyWidgets::updateSliderTextInput(session, inputId = "centralGravity", selected = PhysicsDef$centralGravity[Pos])
    shinyWidgets::updateSliderTextInput(session, inputId = "springLength", selected = PhysicsDef$springLength[Pos])
    shinyWidgets::updateSliderTextInput(session, inputId = "springConstant", selected = PhysicsDef$springConstant[Pos])
    shinyWidgets::updateSliderTextInput(session, inputId = "damping", selected = PhysicsDef$damping[Pos])
    shinyWidgets::updateSliderTextInput(session, inputId = "avoidOverlap", selected = PhysicsDef$avoidOverlap[Pos])
  })

  # FUNCTIONS TO UPDATE VISNETWORK GRAPH BASED ON UI (similar to VisConfigure())
  # A) Graph Data
  observeEvent(input$tab, {
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visUpdateNodes(AllData()$Nodes) %>%
      visNetwork::visUpdateEdges(AllData()$Edges)
  })

  # B) Graph solver
  observeEvent(input$solver, {
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visPhysics(solver = GraphParams()$solver)
  })

  # 1) theta
  observeEvent(input$theta, {
    Options = list(physics = NULL)
    Options$physics[[input$solver]] = list(theta = GraphParams()[[input$solver]]$theta)
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = Options)
  })

  # 2) gravitationalConstant
  observeEvent(input$gravitationalConstant, {
    Options = list(physics = NULL)
    Options$physics[[input$solver]] = list(gravitationalConstant = GraphParams()[[input$solver]]$gravitationalConstant)
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = Options)
  })

  # 3) nodeDistance
  observeEvent(input$nodeDistance, {
    Options = list(physics = NULL)
    Options$physics[[input$solver]] = list(nodeDistance = GraphParams()[[input$solver]]$nodeDistance)
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = Options)
  })

  # 4) centralGravity
  observeEvent(input$centralGravity, {
    Options = list(physics = NULL)
    Options$physics[[input$solver]] = list(centralGravity = GraphParams()[[input$solver]]$centralGravity)
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = Options)
  })

  # 5) springLength
  observeEvent(input$springLength, {
    Options = list(physics = NULL)
    Options$physics[[input$solver]] = list(springLength = GraphParams()[[input$solver]]$springLength)
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = Options)
  })

  # 6) springConstant
  observeEvent(input$springConstant, {
    Options = list(physics = NULL)
    Options$physics[[input$solver]] = list(springConstant = GraphParams()[[input$solver]]$springConstant)
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = Options)
  })

  # 7) damping
  observeEvent(input$damping, {
    Options = list(physics = NULL)
    Options$physics[[input$solver]] = list(damping = GraphParams()[[input$solver]]$damping)
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = Options)
  })

  # 8) avoidOverlap
  observeEvent(input$avoidOverlap, {
    Options = list(physics = NULL)
    Options$physics[[input$solver]] = list(avoidOverlap = GraphParams()[[input$solver]]$avoidOverlap)
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = Options)
  })

  # 9) timestep
  observeEvent(input$timestep, {
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = list(timestep = list(windX = GraphParams()$timestep)))
  })

  # 10) windX
  observeEvent(input$windX, {
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = list(physics = list(windX = GraphParams()$windX)))
  })

  # 11) windX
  observeEvent(input$windY, {
    visNetwork::visNetworkProxy("mynetworkid") %>%
      visNetwork::visSetOptions(options = list(windY = list(windX = GraphParams()$windY)))
  })

  # Saving parameters as defaults
  shiny::observeEvent(input$defaults, {
    saveRDS(GraphParams(), SettingsPath)

    # Shiny alert to confirm defaults are saved
    shinyalert::shinyalert(
      title = "Defaults saved",
      text = "Defaults were successfully saved",
      size = "xs",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "success",
      showConfirmButton = FALSE,
      # showCancelButton = FALSE,
      # confirmButtonText = "OK",
      # confirmButtonCol = "#AEDEF4",
      timer = 2000,
      imageUrl = "",
      animation = TRUE
    )
  })

  # Datatable box title
  output$TableTitle <- shiny::renderText({
    paste("Data table:",ifelse(is.null(input$node_id),"All",input$node_id))
  })

  ## RENDER UI elements

  # 1) theta
  output$theta.ui <- shiny::renderUI({
    if (input$solver == "barnesHut" | input$solver == "forceAtlas2Based") {
      shinyBS::tipify(
        shinyWidgets::sliderTextInput(
        inputId = "theta",
        label = "theta",
        choices = SolverRanges[[input$solver]]$theta,
        selected = Defaults[[input$solver]]$theta),
      title = "This parameter determines the boundary between consolidated long range forces and individual short range forces. To oversimplify higher values are faster but generate more errors, lower values are slower but with less errors.",
      placement = "left")
    }
  })

  # 2) gravitationalConstant
  output$gravitationalConstant.ui <- shiny::renderUI({
    if (input$solver == "barnesHut" | input$solver == "forceAtlas2Based") {
      shinyBS::tipify(
      shinyWidgets::sliderTextInput(
        inputId = "gravitationalConstant",
        label = "gravitationalConstant",
        choices = SolverRanges[[input$solver]]$gravitationalConstant,
        selected = Defaults[[input$solver]]$gravitationalConstant),
      title = "Gravity attracts. We like repulsion. So the value is negative. If you want the repulsion to be stronger, decrease the value.",
      placement = "left")
    }
  })

  # 3) nodeDistance
  output$nodeDistance.ui <- shiny::renderUI({
    if (input$solver == "repulsion" | input$solver == "hierarchicalRepulsion") {
      shinyBS::tipify(
      shinyWidgets::sliderTextInput(
        inputId = "nodeDistance",
        label = "nodeDistance",
        choices = SolverRanges[[input$solver]]$nodeDistance,
        selected = Defaults[[input$solver]]$nodeDistance),
      title = "This is the range of influence for the repulsion.",
      placement = "left")
    }
  })

  # 4) centralGravity
  output$centralGravity.ui <- shiny::renderUI({
    shinyBS::tipify(
    shinyWidgets::sliderTextInput(
      inputId = "centralGravity",
      label = "centralGravity",
      choices = SolverRanges[[input$solver]]$centralGravity,
      selected = Defaults[[input$solver]]$centralGravity),
    title = "There is a central gravity attractor to pull the entire network back to the center.",
    placement = "left")
  })

  # 5) springLength
  output$springLength.ui <- shiny::renderUI({
    shinyBS::tipify(
    shinyWidgets::sliderTextInput(
      inputId = "springLength",
      label = "springLength",
      choices = SolverRanges[[input$solver]]$springLength,
      selected = Defaults[[input$solver]]$springLength),
    title = "The edges are modelled as springs. This springLength here is the the rest length of the spring.",
    placement = "left")
  })

  # 6) springConstant
  output$springConstant.ui <- shiny::renderUI({
    shinyBS::tipify(
    shinyWidgets::sliderTextInput(
      inputId = "springConstant",
      label = "springConstant",
      choices = SolverRanges[[input$solver]]$springConstant,
      selected = Defaults[[input$solver]]$springConstant),
    title = "This is how sturdy the springs are. Higher values mean stronger springs.",
    placement = "left")
  })

  # 7) damping
  output$damping.ui <- shiny::renderUI({
    shinyBS::tipify(
    shinyWidgets::sliderTextInput(
      inputId = "damping",
      label = "damping",
      choices = SolverRanges[[input$solver]]$damping,
      selected = Defaults[[input$solver]]$damping),
    title = "The damping factor is how much of the velocity from the previous physics simulation iteration carries over to the next iteration.",
    placement = "left")
  })

  # 8) avoidOverlap
  output$avoidOverlap.ui <- shiny::renderUI({
    if (input$solver != "repulsion") {
      shinyBS::tipify(
      shinyWidgets::sliderTextInput(
        inputId = "avoidOverlap",
        label = "avoidOverlap",
        choices = SolverRanges[[input$solver]]$avoidOverlap,
        selected = Defaults[[input$solver]]$avoidOverlap),
      title = "When larger than 0, the size of the node is taken into account. The distance will be calculated from the radius of the encompassing circle of the node for both the gravity model. Value 1 is maximum overlap avoidance.",
      placement = "left")
    }
  })

  # End session when App is stopped
  session$onSessionEnded(stopApp)
}
