# CTU Network Shiny global variables
#' @importFrom remotes install_local
#' @export

# Load/install the local PF package
if (!"pf" %in% installed.packages()) {remotes::install_local("R:/Projectfacts/ODBC/pf_app/")}
library("pf")
library("CTUNetwork")

# Retrieve data from ProjectFacts
All_Tabs <- getPFData()

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
AllRows <- 1:dim(Data)[2]
names(AllRows) <-colnames(Data)
SelectRows <- AllRows[c(1,3,4,8,18,20,21,22,23,24)]
