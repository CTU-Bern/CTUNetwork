#' Filter ProjectFacts data from the getPFData function
#'
#' @param Data result from e.g. extractData
#' @param All_Tabs result from e.g. getPFData
#' @param DLF Optional: flag to filter project with "TRUE" or without "FALSE" DLF support
#' @param Divisions Optional: filter data for specific division, e.g. "Data Management", "Statistics", ...
#' @param DateFrom Optional: filter data from a specified date formatted as dd-mm-yyyy up until now.
#' @param ServiceType Optional: filter according to service types, e.g. "Light", "Full", ...
#'
#' @return dataframe
#' @importFrom reshape2 melt
#' @import dplyr
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()
#' Data <- extractData(All_Tabs)
#' Data <- extractData(DLF = T, Divisions = "Data Managemement")

filterData <- function(Data, All_Tabs, DLF = F, Divisions = NA, DateFrom = NA, ServiceType = NA){

  # Disable dplyr warning message
  options(dplyr.summarise.inform = FALSE)

  # Extracting useful data from All_Tabs and storing them into data-frames
  TimeBooking.df <- All_Tabs$activitydata[,c("FK_PROJECT","FK_WORKER","Timespent_billable","BookedDate", "INCOMEFROMINVOICE_BILLABLE")]

  # 1) FILTER INPUT DATASET BASED ON OPTIONAL ARGUMENTS ####
  # -------------------------------------------------------------------------- #
  # 1.1) Divisions
  if (!is.na(Divisions)) {
    FiltIdx <- mapply(grepl,Divisions, Data$ProjectName, ignore.case=T)
    FiltIdx <- Data$PackageLvl %in% Data$UniqueCode[FiltIdx] |
      Data$UniqueCode %in% Data$ProjectLvl[FiltIdx]
    Data <- Data[FiltIdx,]
  }

  # 1.2) DLF funding
  if (DLF == T){
    FiltIdx <- Data$DLFSupport==T # | is.na(Data$DLFSupport)
    FiltIdx <- Data$ProjectLvl %in% Data$ProjectLvl[which(FiltIdx)]
    Data <- Data[FiltIdx,]
  }

  # 1.3) Time bookings
  if (!is.na(DateFrom)) {
    DateFrom <- as.Date(as.Date(DateFrom, format="%d-%m-%Y"), format="%Y-%m-%d")
    IdxDateFrom <- which(ifelse(TimeBooking.df$BookedDate-DateFrom>0,T,F)==T)
    TimeBooking.df <- TimeBooking.df[IdxDateFrom,]
  }

  # 1.4) Service Type
  if (!is.na(ServiceType)) {
    FiltIdx <- Data$ServiceType==ServiceType
    FiltIdx <- Data$ProjectLvl %in% Data$ProjectLvl[which(FiltIdx)]
    Data <- Data[FiltIdx,]
  }

  # 2) AGGREGATING INPUT DATASET WITH All_Tabs ####
  # -------------------------------------------------------------------------- #

  # aggregating the project dataframe with corresponding timebookings
  Data <- dplyr::left_join(Data, TimeBooking.df, by = c("UniqueCode" = "FK_PROJECT")) %>%
    dplyr::rename(Workers = "FK_WORKER", TimeSpent = "Timespent_billable") %>%  # renaming new columns
    dplyr::mutate(HourlyCosts = INCOMEFROMINVOICE_BILLABLE) %>%
    dplyr::select(-INCOMEFROMINVOICE_BILLABLE)
  Data <- Data[,c(1:19,28,20,21,29,27,22:26)]# Reordering columns

  # Replacing the Workers code by names
  Data <- dplyr::left_join(Data, All_Tabs$worker[,c("PK_Worker","ShowName")], by = c("Workers" = "PK_Worker"))
  Data$Workers <- Data$ShowName
  Data <- Data[,-dim(Data)[2]]

  # converting strings as dates
  Data$BookedDate <- as.Date(Data$BookedDate,format="%Y-%m-%d")

  # Moving the ProdDate to the package level for each project
  # Using pmin instead of min to avoid warnings related to inf values
  # https://stackoverflow.com/questions/46289577/summarise-returning-inf-when-using-na-rm-true
  TEMP <- Data %>% dplyr::group_by(ProjectLvl,PackageLvl) %>%
    dplyr::summarize(pmin(unique(ProdDate[!is.na(ProdDate)]), na.rm = T)) %>%
    dplyr::rename(ProdDateNew =`pmin(unique(ProdDate[!is.na(ProdDate)]), na.rm = T)`)
  TEMP[,-3] <- lapply(TEMP[,-3], function(x) as.integer(as.character(x)))
  TEMP <- reshape2::melt(TEMP, id.vars=c("ProdDateNew")) # From wide to long format
  Data <- dplyr::left_join(Data, TEMP[,-2], by = c("UniqueCode" = "value"))

  # Regrouping the columns
  Data <- Data %>% dplyr::mutate(ProdDate = dplyr::coalesce(ProdDate,ProdDateNew))
  Data <- Data[,-dim(Data)[2]]

  # 3) EXTRACTING FIXED COSTS AND DATASET CLEANING ####
  # -------------------------------------------------------------------------- #
  # Took the gross value (SUMBRUTTO) since to determine if DLF is reached, VTA should not be included
  # FYI: For an unknown reason, SUMBRUTTO contains money spent + money budgeted ONLY FOR hourly costs
  # In either $financeposition or $financearticle...
  # BILLINGTYPE --> 0 = Fixed costs
  #                 3 & 4 = Hourly costs

  # Fixed costs
  InvoicesPkgLvl <- All_Tabs$financeposition[All_Tabs$financeposition$BILLINGTYPE == 0,] %>%
    dplyr::group_by(FK_PROJECT) %>%
    dplyr::summarize(SUMBRUTTO=sum(SUMBRUTTO, na.rm=T))
  Data <- dplyr::left_join(Data, InvoicesPkgLvl, by = c("UniqueCode" = "FK_PROJECT")) %>%
    dplyr::mutate(FixedCosts = SUMBRUTTO) %>%
    dplyr::select(-SUMBRUTTO)
  Data$FixedCosts <- replace(Data$FixedCosts, duplicated(Data$UniqueCode), NA)
  Data$MoneyBudget <- replace(Data$MoneyBudget, duplicated(Data$UniqueCode), NA)

  # Changing data format
  Cols <- c("ProjectID","Service","CDMS","State","DLFReached","CustomerID","Manager")
  Data[,Cols] <- lapply(Data[,Cols],factor)

  # Replacing incoherent information (i.e.., duplicates)
  UniquePkgs <- split(seq_along(Data$UniqueCode), Data$UniqueCode)
  UniquePkgs <- UniquePkgs[lapply(UniquePkgs, length)>1] # Only keeping repeated codes
  # Removing repeated information (but not for first instance since it's the higher package level)
  UniquePkgs <- as.integer(unlist(lapply(UniquePkgs, function(x) x[-1])))
  Data[UniquePkgs,c("TimeBudget","WorkerRate")] = NA

  # Output
  return(Data)
}
