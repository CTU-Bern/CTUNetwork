#' Cumulative sum of time bookings over package/project levels
#' This function is applied to all project types expect consultings since their unique code is shared
#'
#' @param DF result from e.g. filterData
#'
#' @return dataframe
#' @importFrom data.table setnames
#' @import dplyr
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()
#' Data <- extractData(All_Tabs)
#' Data <- filterData(Data, All_Tabs)
#' Data <- AvgTimeBookings(Data)

AvgTimeBookings <- function(DF) {

  # Need to split the dataframe since Consultings needs to be dealt differently
  Idx <- grepl("C-", DF$ProjectID)
  ConsultDF <- DF[Idx,]
  ProjDF <- DF[!Idx,]

  # Should not be applied to Consultings
  if (dim(ProjDF)[1]>0) {

    # Focusing on package level
    Pkg <- ProjDF %>%
      dplyr::group_by(PackageLvl,ProjectLvl) %>%
      dplyr::summarise(TimeSpent=sum(TimeSpent, na.rm=T),
                Workers = paste(unique(Workers[!is.na(Workers)]),collapse=","))
    Pkg <- Pkg[!is.na(Pkg$TimeSpent),]

    # Focusing on project level
    Proj <- ProjDF %>%
      dplyr::group_by(ProjectLvl) %>%
      dplyr::summarise(TimeSpent=sum(TimeSpent, na.rm=T),
                Workers = paste(unique(Workers[!is.na(Workers)]),collapse=","))
    Proj$Workers <- ifelse(substr(Proj$Workers, 1, 1) == ",", sub("^,", "", Proj$Workers), Proj$Workers)

    # Removing lines with values = 0
    Pkg <- Pkg[Pkg$TimeSpent!=0,-2]

    # Renaming column
    names(Proj)[1] <- names(Pkg)[1]

    # Grouping all together
    PkgProj <- rbind(Pkg,Proj)

    # Changing data type
    ProjDF$UniqueCode <- as.factor(ProjDF$UniqueCode)

    # Joining the two dataframes
    ProjDF <- left_join(ProjDF, PkgProj, by = c("UniqueCode" = "PackageLvl")) %>%
      dplyr::mutate(TimeSpent.x = coalesce(TimeSpent.x,TimeSpent.y)) %>% # Regrouping the columns
      dplyr::mutate(Workers.x = coalesce(Workers.x,Workers.y)) %>%
      data.table::setnames(c("TimeSpent.x","Workers.x"), c("TimeSpent","Workers")) %>%
      dplyr::select(-TimeSpent.y, -Workers.y)
  }

  # Merge the two datasets
  DF <- rbind(ConsultDF, ProjDF)

  # Output
  return(DF)
}
