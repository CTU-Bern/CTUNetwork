#' Standardizing dates to European format, i.e. DD-MM-YYYY
#'
#' @param DateVec character containing a date
#'
#' @return character
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @export
#'
#' @examples
#' StandardDates("2022-12-01")
#' "01-12-2022"
#'
StandardDates = function(DateVec) {

  # Replace some characters
  DateVec <- stringr::str_replace_all(DateVec,c(" "),"-")
  DateVec <- stringr::str_replace_all(DateVec,c("[.]"),"-")

  # Output vector
  Out <- NA

  # Only taking comments containing at least one number
  if (grepl("\\d", DateVec)) {

    # if contains hyphen
    if (grepl("-",DateVec)) {

      # Split at hyphen
      SplitDate <- stringr::str_split(DateVec,"-")[[1]]

      # Cases with D:M:YYYY/DD:MM:YYYY
      if (nchar(SplitDate[1])<=2) {

        # Format days as DD
        if (nchar(SplitDate[1])<2) {
          Out <- paste0("0",SplitDate[1],"-",SplitDate[2],"-",SplitDate[3])
        }

        # Format months as MM
        if (nchar(SplitDate[2])<2) {
          SplitDate <- stringr::str_split(DateVec,"-")[[1]]
          Out <- paste0(SplitDate[1],"-0",SplitDate[2],"-",SplitDate[3])
        }

        # Format year as YYYY
        if (nchar(SplitDate[3])<4) {
          SplitDate <- stringr::str_split(DateVec,"-")[[1]]
          Out <- paste0(SplitDate[1],"-",SplitDate[2],"-20",SplitDate[3])
        }

        # If format is correct
        Out <- DateVec

        # Cases with YYYY:MM:DD
      } else if (nchar(SplitDate[1])==4) {
        Out <- paste0(SplitDate[3],"-",SplitDate[2],"-",SplitDate[1])
      }

      # Other cases
    } else if (!is.na(DateVec)) {

      # Format months as MM
      SplitDate <- substring(DateVec, seq(1, nchar(DateVec), 2), seq(2, nchar(DateVec), 2))
      Out <- paste0(SplitDate[1],"-",SplitDate[2],"-",SplitDate[3],SplitDate[4])
    }

    # In case does not contain number, replace with NA
  } else {Out <- NA}

  # Output
  return(Out)
}
