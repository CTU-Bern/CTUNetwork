#' Computing calculations for HourlyCosts, MoneySpent and TimePercent variables
#'
#' @param Data result from e.g. filterData
#'
#' @return dataframe
#' @importFrom stringr str_order
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()
#' Data <- extractData(All_Tabs)
#' Data <- filterData(Data, All_Tabs)
#' Data <- Calculations(Data)

Calculations <- function(Data){

  # Computing the hourly costs (time X worker rate)
  # Not needed anymore, now taken from INCOMEFROMINVOICE_BILLABLE (same result)
  # Data$HourlyCosts <- Data$TimeSpent/60 * Data$WorkerRate

  # Adding up FixedCosts (for packages)
  Data$MoneySpent[Data$Filt] <- rowSums(Data[Data$Filt, c("FixedCosts","HourlyCosts")], na.rm=T)

  # Cumulative sum over package and project levels
  Data <- AvgTimeBookings(Data)

  # Adding up FixedCosts (for projects) - This needs to be done after the averaging process
  Data$MoneySpent[!Data$Filt] <- rowSums(Data[!Data$Filt,c("FixedCosts","MoneySpent")], na.rm=T)
  Data$MoneySpent  <- ifelse(Data$MoneySpent==0,NA,Data$MoneySpent)

  # Computing the percentage of time worked/time budgeted
  Data$TimePercent[!Data$Filt] <- Data$TimeSpent[!Data$Filt]/Data$TimeBudget[!Data$Filt]*100

  # Reordering the database according to natural order
  Data <- Data[stringr::str_order(Data$ProjectIDs),]

  # Determine if DLF is reached (MoneySpent > 3000CHF)
  Idx = which(grepl("P-", Data$ProjectID))
  Data$DLFReached <- as.logical(Data$DLFReached)
  Data$DLFReached[Idx] <- ifelse(Data$MoneySpent[Idx] > 3000, TRUE, FALSE)

  # OUTPUT
  return(Data)
}
