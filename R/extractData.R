#' Extract and format ProjectFacts data from the getPFData function
#'
#' @param All_Tabs result from e.g. getPFData
#'
#' @return dataframe
#' @importFrom foreach foreach
#' @importFrom foreach "%do%"
#' @importFrom tidyr unite
#' @importFrom stringr str_replace
#' @import dplyr
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()
#' Data <- extractData(All_Tabs)
#'
extractData <- function(All_Tabs){

  # 1) FILTER INPUT DATASET ####
  # -------------------------------------------------------------------------- #
  # All_Tabs$...
  #   customer # List of customers/organisations
  #   crmkontakt # List of all contacts
  #   financeposition # Amounts in invoices at work package level
  #   financerecord # Invoice total amounts and contract total amounts
  #   All_Tabs$projectstatedefinition # Explanations related to projects state code
  #   worker # List of workers
  #   project # List of projects, work packages
  Projects.df <- All_Tabs$project[,colSums(is.na(All_Tabs$project))<nrow(All_Tabs$project)] # Removing columns containing only NAs

  # 2) BUILD MAIN DATASET ####
  # -------------------------------------------------------------------------- #
  Output.df <- data.frame(ProjectIDs = Projects.df$CaseId,
                         ProjectNames = Projects.df$Name,
                         ProjectID = Projects.df$CaseId,
                         ProjectName = Projects.df$Name,
                         ProjectLvl = Projects.df$FK_MAINPROJECT, # ID referring to the project level
                         PackageLvl = Projects.df$FK_PARENTPROJECT, # ID referring to the package level
                         UniqueCode = Projects.df$PK_Project, # ID referring to the unique entry code
                         Service = rep(NA,dim(Projects.df)[1]),
                         CDMS = rep(NA,dim(Projects.df)[1]),
                         State = Projects.df$FK_PROJECTSTATEDEFINITION_RELEVANT, # Relevant contains all infos !
                         CustomerID = Projects.df$FK_CUSTOMER,
                         Customer = Projects.df$FK_CUSTOMER,
                         Contact = Projects.df$FK_CUSTOMERCONTACT,
                         Sponsor = Projects.df$cf_sponsor, # Sponsor notes
                         DMNotes = Projects.df$CUSTOMFIELDVALUES, # Data Management notes
                         DLFSupport = Projects.df$cf_dlf_data_management_support,
                         DLFReached = rep(NA,dim(Projects.df)[1]),
                         ProdDate = Projects.df$STATECOMMENT,
                         Manager = Projects.df$FK_PERSONINCHARGE, # THIS DOES NOT CORRESPOND TO THE RESPONSIBLE/SUBSTITUTE COLUMNS !
                         TimeBudget = dplyr::coalesce(Projects.df$BillableTimeBudget,Projects.df$BillableTimeBudgetCumulated), # merge two columns
                         TimePercent = rep(NA,dim(Projects.df)[1]),
                         WorkerRate = Projects.df$WorkerRate,
                         HourlyCosts = rep(NA,dim(Projects.df)[1]),
                         FixedCosts =  rep(NA,dim(Projects.df)[1]),
                         MoneySpent = rep(NA,dim(Projects.df)[1]),
                         MoneyBudget = dplyr::coalesce(Projects.df$MoneyInBudget,Projects.df$MoneyInBudgetCumulated))

  # 3) RESHAPE THE MAIN DATASET ####
  # -------------------------------------------------------------------------- #
  # Fixed parameters (this may change)
  ServType <- c("Basic","Light","Full")
  CDMSType <- c("REDCap", "secutrial", "Webspirit")

  # Improving the "PackageLvl" column
  PkgIdx <-  which(Output.df$PackageLvl == Output.df$ProjectLvl)
  Output.df$PackageLvl[PkgIdx] <- Output.df$UniqueCode[PkgIdx]
  Idx <- which(is.na(Output.df$PackageLvl))
  Output.df$PackageLvl[Idx] <- Output.df$UniqueCode[Idx]

  # Changing data format (factors)
  Output.df[,c("ProjectName","ProjectLvl","PackageLvl","DLFSupport")] <- lapply(Output.df[,c("ProjectName","ProjectLvl","PackageLvl","DLFSupport")], factor)
  levels(Output.df$DLFSupport) <- c(FALSE,TRUE)

  # Filtering out information in the DM Notes field
  Output.df$DMNotes <- FilterNotes(Output.df$DMNotes,"<92\\.259156585>","</92\\.259156585>")

  # Standardizing the state date (in comment) to convert as Date
  # see: https://github.com/tidyverse/stringr/issues/103
  IDXNoNA <- !is.na(Output.df$ProdDate)
  Output.df$ProdDate[IDXNoNA] <- as.character(sapply(Output.df$ProdDate[IDXNoNA], StandardDates))
  Output.df$ProdDate <- as.Date(Output.df$ProdDate,format = "%d-%m-%Y") # Converting as Date format

  # Replicate ProjectID, ProjectName and DLFSupport inside each project (to facilitate filtering in table output)
  MatchIdx <- match(Output.df$ProjectLvl,Output.df$UniqueCode)
  Output.df$ProjectIDs <- Output.df$ProjectID[MatchIdx]
  Output.df$ProjectNames <- Output.df$ProjectName[MatchIdx]
  Output.df$DLFSupport <- Output.df$DLFSupport[MatchIdx]

  # Removing unused data
  # Some internal projects simply don't have a project number (NA)
  # P-1444 'ISPM Teaching office' & P-1627 'SCAD-Registry Bern' = EMPTY
  Output.df <- Output.df[!grepl("P-1444|P-1627",Output.df$ProjectIDs) & !is.na(Output.df$ProjectIDs),]

  # List of indexes corresponding to each project
  Output.df$ProjectLvl <- droplevels(Output.df$ProjectLvl)
  IdxProject <- split(seq_along(Output.df$ProjectLvl), Output.df$ProjectLvl)

  # Only keeping the dates in the headword of the Project level AND
  # packages "Database Set-up" packages (as being the Productive Dates)
  IndexDBSetup <- grepl("Database Set-up",Output.df$ProjectName, fixed=T) | grepl("Database Set-up DM",Output.df$ProjectName, fixed=T)
  Output.df$ProdDate[!IndexDBSetup] <- NA

  # Detecting the type of service from the package name
  # - paste0 to counteract the problem induced by character(0)
  # - tail(x, n=1) to always take the last returned value (sometimes a project changes service type, hence only taking the latest into account)
  Services <- foreach::foreach(k=1:length(IdxProject), .combine='c') %do%
    {tail(paste0(ServType[mapply(grepl,ServType,paste(Output.df$ProjectName[IdxProject[[k]]],collapse =" "), ignore.case=T)],""),n=1)}
  Services[Services==""] <- NA
  Output.df$Service[unlist(IdxProject)] <- rep.int(Services,times=as.integer(lengths(IdxProject)))

  # Detecting the clinical data management system used from the package name
  CDMS <- foreach::foreach(k=1:length(IdxProject), .combine='c') %do%
    {tail(paste0(CDMSType[mapply(grepl,CDMSType,paste(Output.df$ProjectName[IdxProject[[k]]],collapse =" "), ignore.case=T)],""),n=1)}
  CDMS[CDMS==""] <- NA
  Output.df$CDMS[unlist(IdxProject)] <- rep.int(CDMS,times=as.integer(lengths(IdxProject)))

  # Completing missing CDMS information
  # Light/Basic services are only provided with REDCap
  Output.df$CDMS[is.na(Output.df$CDMS) & (Output.df$Service=="Light" | Output.df$Service=="Basic")] <- "REDCap"

  # Sort dataframe by project and package
  Output.df <- Output.df[order(Output.df$ProjectLvl, Output.df$PackageLvl),]

  # Replacing the customer ID by name
  Output.df[!is.na(Output.df$Customer),"Customer"] <- All_Tabs$customer[as.numeric(na.omit(match(Output.df$CustomerID,All_Tabs$customer$PK_CUSTOMER))),"Name"]

  # Replacing the contact ID by name
  TEMP <- All_Tabs$crmkontakt[as.numeric(na.omit(match(Output.df$Contact,All_Tabs$crmkontakt$PK_CRMKONTAKT))),c("Vorname","Nachname")] %>%
    tidyr::unite("x", Vorname:Nachname, remove = T, sep = " ")
  Output.df[!is.na(Output.df$Contact),"Contact"] <- TEMP$x

  # Replacing the project state ID number by its name (taken from "projectstatedefinition")
  Output.df <- dplyr::left_join(Output.df, All_Tabs$projectstatedefinition[,c("PK_PROJECTSTATEDEFINITION","NAME")], by = c("State" = "PK_PROJECTSTATEDEFINITION"))
  Output.df$State <- Output.df$NAME
  Output.df <- Output.df[,-dim(Output.df)[2]]

  # Replacing the project "mandant" ID number by its name (taken from "worker")
  Output.df[!is.na(Output.df$Manager),"Manager"] <- All_Tabs$worker[as.numeric(na.omit(match(Output.df$Manager,All_Tabs$worker$PK_Worker))),"ShowName"]

  # Reformatting the ProjectID and ProjectIDs column (removing spaces)
  Output.df[,c("ProjectIDs","ProjectID")] <- Output.df %>% dplyr::select(starts_with("ProjectID")) %>% dplyr::mutate_all(list(~stringr::str_replace(., " ", "")))

  # Changing variable type to factor
  Output.df[,c("ProjectIDs","Customer","Contact")] <- lapply(Output.df[,c("ProjectIDs","Customer","Contact")], factor)

  # Output
  return(Output.df)
}
