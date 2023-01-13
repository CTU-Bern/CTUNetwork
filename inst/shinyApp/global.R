# CTU Network Shiny global variables
#' @importFrom remotes install_local
#' @importFrom pf getPFData
#' @export

# Load/install the local PF package
# if (!"pf" %in% installed.packages()) {remotes::install_github("CTU-Bern/pf")}
# library("pf")
library("CTUNetwork")

# Retrieve data from ProjectFacts
# THIS SHOULD ULTIMATELY BE CHANGED FOR "NULL" (To load from ODBC)
# All_Tabs <- getPFData(NULL)
if (grepl("windows", Sys.info()[1], ignore.case = TRUE)){
  All_Tabs <- pf::getPFData()
} else {
  print(paste0("the current directory is: ",getwd()))
  All_Tabs <- pf::getPFData(file = "/media/sf_SharedFolderVM/pf_tabs.rds")
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
if (file.exists("www/Defaults.rds")) {
  Defaults <- readRDS("www/Defaults.rds")
} else {
  Defaults <- list(physics = "Yes",
                   layout = "Layout on sphere",
                   solver = "hierarchicalRepulsion")}
