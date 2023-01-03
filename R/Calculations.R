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

  # Cumulative sum over package and project levels
  Data <- CTUNetwork::AvgTimeBookings(Data)

  # Computing the hourly costs (time X worker rate)
  Data$HourlyCosts <- Data$TimeSpent/60 * Data$WorkerRate

  # Adding up FixedCosts and HourlyCosts
  Data$MoneySpent <- rowSums(Data[,c("FixedCosts","HourlyCosts")], na.rm=T)
  Data$MoneySpent  <- ifelse(Data$MoneySpent==0,NA,Data$MoneySpent)

  # Computing the percentage of time worked/time budgeted
  Data$TimePercent <- Data$TimeSpent/Data$TimeBudget*100

  # Reordering the database according to natural order
  Data <- Data[stringr::str_order(Data$ProjectIDs),]

  # OUTPUT
  return(Data)
}
