# CTU Network Shiny app server
#' @importFrom shinyjs reset
#' @importFrom cowplot get_legend
#' @importFrom scales rescale
#' @importFrom foreach foreach
#' @importFrom gridExtra grid.arrange
#' @import visNetwork
#' @import dplyr
#' @import grid
#' @import ggplot2
#' @import shiny
#' @export

require(CTUNetwork)
require(ggplot2)

# Server
server <- function(input, output) { # Assemble inputs into outputs

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
      DataUp <- DataUp[is.na(DataUp$BookedDate) | DataUp$BookedDate %in% input$timebookfilter[1]:input$timebookfilter[2],]
      # !DataUp$Filt is to avoid deleting project and package levels
    }

    # Computing different calculations based on filtering parameters
    DataUp <- Calculations(DataUp)

    ## Preparing data for network
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

  # 1. Network plot
  output$mynetworkid <- visNetwork::renderVisNetwork(
    NetworkPlot(AllData()$Nodes, AllData()$Edges)
  )

  # 2. Legend
  output$mylegend <- shiny::renderPlot({

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
    grid::grid.newpage()
    grid::grid.draw(legend)

  })

  # 3. Nodes plot
  # https://stackoverflow.com/questions/41018899/get-selected-node-data-from-visnetwork-graph-without-actionbutton
  # https://github.com/datastorm-open/visNetwork/issues/176

  # Filter data
  Plot.df <- shiny::reactive(
    if (grepl("P-",input$node_id, ignore.case = T)) {
      Plot.df <- AllData()$DataPlot[AllData()$DataPlot$ProjectIDs==input$node_id,]
    } else {
      Plot.df <- AllData()$DataPlot[AllData()$DataPlot$Workers==input$node_id,]
    })

  # 3.1 Grouped nodes
  output$GroupedNodes <- shiny::renderPlot({
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
  plotCount <- reactive({
    req(input$node_id)
    FilterTag <- ifelse(grepl("P-",input$node_id),"Workers","ProjectIDs")
    FilterItem <- sort(unique(Plot.df()[,FilterTag]), index.return = T)
    length(FilterItem$x)
  })

  # Dynamically adjusting plot height
  plotHeight <- reactive(300 * ceiling(plotCount()/3))


  output$IndivNodes <- shiny::renderPlot({
    if (!is.null(input$node_id)) {

      Plots <- list(); TEMP.df <- data.frame()
      FilterTag <- ifelse(grepl("P-",input$node_id),"Workers","ProjectIDs")
      FilterItem <- sort(unique(Plot.df()[,FilterTag]), index.return = T)
      ProjNames <- unique(Plot.df()$ProjectNames)[FilterItem$ix]
      Colors <- ggColorHue(length(FilterItem$x))
      for(i in 1:length(FilterItem$x)){
        TEMP.df <- Plot.df()[Plot.df()[,FilterTag]==FilterItem$x[i],]
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
        if (!grepl("P-",input$node_id)){
          Plots[[i]] <- Plots[[i]] +
          labs(title = paste(FilterItem$x[i], ProjNames[i], sep=" - "))}
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
          # IdxWorker <- grepl("AndrC) Moser",AllData()$DataUp$Workers,ignore.case = T)
          IdxWorker <- grepl(input$node_id,AllData()$DataUp$Workers,ignore.case = T)
          BuildTable(AllData()$DataUp[IdxWorker,Cols])
        }
      }
   })

  #### OBSERVE EVENTS
  # Enables to reset the parameters from SideBar
  # See: https://stackoverflow.com/questions/54347972/reset-action-button-output-in-shiny
  shiny::observeEvent(input$reset, {
    shinyjs::reset()
  })

  # Datatable box title
  output$TableTitle <- shiny::renderText({
    paste("Data table:",ifelse(is.null(input$node_id),"All",input$node_id))
  })

}
