#' Convert time in minutes as integers to standard HH:MM
#'
#' @param DataTab vector of time in minutes formatted as integers
#'
#' @return vector
#' @export
#'
#' @examples
#' TimeasHHMM <- ConvertTime(200)
#' TimeasHHMM
#' "3:20"

ConvertTime = function(Vector) {

  # The function will fail if the vector only contains NAs
  if (any(!is.na(Vector))) {
    # Dividing input by 60 and separating integers from decimals
    # NOTE: using suppressWarnings to bypass sprintf warning message: "NAs introduced by coercion"
    Split <- suppressWarnings(do.call(rbind, strsplit(sprintf("%.2f",as.integer(Vector)/60),"\\.")))

    # Converting decimals to seconds and combining with minutes into a string
    Time <- suppressWarnings(paste0(Split[,1],":",as.character(sprintf("%02d", round(as.integer(Split[,2])*0.6)))))
    Time[grepl("NA",Time)] <- NA # replacing NA strings by reall NAs

  } else {
    Time <- Vector
  }

  # Output
  return(Time)
}
