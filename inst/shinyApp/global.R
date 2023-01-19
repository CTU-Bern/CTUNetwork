# CTU Network Shiny global variables
#' @importFrom remotes install_local
#' @importFrom pf getPFData
#' @export

# Load the package functions
library("CTUNetwork")

# Retrieve data from ProjectFacts

if (grepl("windows", Sys.info()[1], ignore.case = TRUE)){

  # Projectfacts data loaded through ODBC connection (if available)
  # Tryclass <- try({
    # All_Tabs <- pf::getPFData(NULL)
    # add custom fields
    # All_Tabs$customer <- pf::decodeCustomFields(All_Tabs$customer, All_Tabs$customfields)
    # All_Tabs$project <- pf::decodeCustomFields(All_Tabs$project, All_Tabs$customfields)
    # All_Tabs$ticket <- pf::decodeCustomFields(All_Tabs$ticket, All_Tabs$customfields)
    # All_Tabs$worker <- pf::decodeCustomFields(All_Tabs$worker, All_Tabs$customfields)
  # })

  # If ODBC connection fails, then loads from local R: drive
  # if (grepl("try-error", class(Tryclass), ignore.case = T)) {
    All_Tabs <- pf::getPFData()
  # }
} else {
  # print(paste0("the current directory is: ",getwd()))
  All_Tabs <- pf::getPFData(file = "pf_tabs.rds")
}
# Only keeping useful information from All_Tabs
# $activitycategory = activity types - e.g. billable, non-billable
# $customfields = list of the custom fields
# $financearticle = types of work packages (Basic, Light, Full services)
# $projectstatedefinition = Explanations related to projects state code
# $financeposition = Invoices at package level
# $financerecord = Invoices total at project level
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
AllRows <- 1:(dim(Data)[2]-1)
names(AllRows) <- colnames(Data)[-dim(Data)[2]]
SelectRows <- AllRows[c(1,3,4,8,18,20,21,22,23,24)]

# Load default parameters
LibPath <- .libPaths()[1] # There may be more than one
if (file.exists(paste0(LibPath,"/CTUNetwork/shinyApp/www/Defaults.rds"))) {
  Defaults <- readRDS(paste0(LibPath,"/CTUNetwork/shinyApp/www/Defaults.rds"))
} else {
  Defaults <- list(physics = "Yes",
                   layout = "Layout on sphere",
                   solver = "hierarchicalRepulsion",
                   timestep = 0.5,
                   wind = list(X = 0,
                               Y = 0),
                   hierarchicalRepulsion = list(nodeDistance = 90,
                                                centralGravity = 0.0,
                                                springLength = 850,
                                                springConstant = 0.01,
                                                damping = 0.09,
                                                avoidOverlap = 0))
}

# visPhysics ranges are different based on solver type
SolverRanges <- list(
  barnesHut = list(
    theta = seq(0.1, 1, 0.05),
    gravitationalConstant = seq(-3000, 0, 50),
    centralGravity = seq(0, 10, 0.05),
    springLength = seq(0, 500, 5),
    springConstant = seq(0, 1.2, 0.005),
    damping = seq(0, 1, 0.01),
    avoidOverlap = seq(0, 1, 0.01)),
  forceAtlas2Based = list(
    theta = seq(0.1, 1, 0.05),
    gravitationalConstant = seq(-500, 0, 1),
    centralGravity = seq(0, 1, 0.005),
    springLength = seq(0, 500, 5),
    springConstant = seq(0, 1.2, 0.005),
    damping = seq(0, 1, 0.01),
    avoidOverlap = seq(0, 1, 0.01)),
  repulsion = list(
    nodeDistance = seq(0, 500, 5),
    centralGravity = seq(0, 10, 0.05),
    springLength = seq(0, 500, 5),
    springConstant = seq(0, 1.2, 0.005),
    damping = seq(0, 1, 0.01)),
  hierarchicalRepulsion = list(
    nodeDistance = seq(0, 500, 5),
    centralGravity = seq(0, 10, 0.05),
    springLength = seq(0, 1020, 5),
    springConstant = seq(0, 1.2, 0.005),
    damping = seq(0, 1, 0.01),
    avoidOverlap = seq(0, 1, 0.01))
)

