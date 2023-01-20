#' Filter ProjectFacts data from the getPFData function
#'
#' @param Data result from e.g. extractData
#' @param All_Tabs result from e.g. getPFData
#'
#' @return dataframe
#' @importFrom reshape2 melt
#' @import dplyr
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()
#' Data <- extractData(All_Tabs)
#' Data <- filterData(Data, All_Tabs)

filterData <- function(Data, All_Tabs){

  # Disable dplyr warning message
  options(dplyr.summarise.inform = FALSE)

  # Extracting useful data from All_Tabs and storing them into data-frames
  TimeBooking.df <- All_Tabs$activitydata[,c("FK_PROJECT","FK_WORKER","Timespent_billable","BookedDate", "INCOMEFROMINVOICE_BILLABLE")]

  # AGGREGATING INPUT DATASET WITH All_Tabs ####
  # -------------------------------------------------------------------------- #

  # aggregating the project dataframe with corresponding timebookings
  Data <- dplyr::left_join(Data, TimeBooking.df, by = c("UniqueCode" = "FK_PROJECT")) %>%
    dplyr::rename(Workers = "FK_WORKER", TimeSpent = "Timespent_billable") %>%  # renaming new columns
    dplyr::mutate(HourlyCosts = INCOMEFROMINVOICE_BILLABLE) %>%
    dplyr::select(-INCOMEFROMINVOICE_BILLABLE)
  Data <- Data[,c(1:19,29,20,21,30,28,22:26,27)]# Reordering columns

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

  # Determining the date DLF > 3000CHF was reached
  # THIS IS TOO SLOW....
  # UniqueProj <- unique(Data$ProjectIDs)
  # UniqueProj <- UniqueProj[grepl("P-",UniqueProj)]
  # for (k in 1:length(UniqueProj)) {
  #   DF <- Data[Data$ProjectIDs==UniqueProj[k] & grepl("001",Data$ProjectID),] # package names starting by 001 are DM-related
  #   MoneyDF <- data.frame(ID = c(rownames(DF[!is.na(DF$FixedCosts),]),rownames(DF)),
  #                         Sum = c(DF$FixedCosts[!is.na(DF$FixedCosts)], tidyr::replace_na(DF$HourlyCosts,0)))
  #   DLFReachedIdx <- MoneyDF$ID[match(1, cumsum(MoneyDF$Sum > 3000))]
  #   DLFReachedDT <- DF$BookedDate[which(rownames(DF)==DLFReachedIdx)]
  #
  #   # Store date when DLF was reached in main dataset
  #   Data$DLFReached[!Data$Filt & Data$ProjectIDs==UniqueProj[k]] <-
  #     as.Date(ifelse(length(DLFReachedDT)<1,NA,DLFReachedDT), origin = .Date(0))
  # }

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
