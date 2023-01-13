#' Load projectfacts data either from file or ODBC
#'
#' @param file filename to load or \code{NULL}. If \code{NULL}, data is loaded via ODBC.
#'
#' @return
#' List containing the following data frames
#' \describe{
#'   \item{activitycategory}{activity types - e.g. billable, non-billable}
#'   \item{activitydata}{individual time bookings}
#'   \item{attendance_time}{unclear (unlikely to be useful)}
#'   \item{benutzergruppe}{user groups (unlikely to be useful)}
#'   \item{crmactivity}{unclear}
#'   \item{crmkontakt}{contacts - individuals}
#'   \item{customer}{customers/organisations}
#'   \item{customfields}{list of the custom fields}
#'   \item{customfieldvalue}{unclear how this links to customsfields... most values are in \code{project}}
#'   \item{entitycheck}{status of invoice checks (unlikely to be useful)}
#'   \item{entityrelation}{unclear (unlikely to be useful)}
#'   \item{expenseposition}{expenses}
#'   \item{expensepositiontype}{types of allowed expenses}
#'   \item{financearticle}{work package types}
#'   \item{financeposition}{amounts in invoices at work package level}
#'   \item{financepositiongroup}{unclear}
#'   \item{financerecord}{invoice total amounts and contract total amounts}
#'   \item{grouppermission}{individual permissions}
#'   \item{mandant}{list of mandants (5point customers on the system)}
#'   \item{notification}{list of notifications, probably not useful}
#'   \item{project}{projects, work packages}
#'   \item{projectactivity}{some sort of time bookings, unclear if its useful}
#'   \item{projectstatedefinition}{list of possible project states}
#'   \item{projecttype}{list of possible project types}
#'   \item{ticket}{tickets (includes customfields)}
#'   \item{ticketprocessitem}{ticket process items (e.g. notes)}
#'   \item{ticketstatedefinition}{list of possible ticket states}
#'   \item{ticketworkerrelation}{link between ticket and worker?}
#'   \item{workday}{time worked by worker on given day (probably not so useful)}
#'   \item{worker}{workers}
#'   
#' }
#' @export
#'
#' @examples
#' # Load from R
#' all_tabs <- getPFData()
#' # Load from ODBC (requires DNS to be configured)
#' # all_tabs <- getPFData(NULL)
#' # all_tabs <- getPFData(NULL, all = TRUE)
getPFData <- function(file = "R:/Projectfacts/ODBC/pf_tabs.rds", ...){
  
  if(is.null(file)){
    
    all_tabs <- downloadPFData(...) 
    
  } else {
    
    all_tabs <- readRDS(file)
    fi <- file.info(file)
    message("Data last modified ", fi$mtime)
  
  }
  
  # output$setupComplete <- reactive({
  #   return(rv$setupComplete)
  # })
  # outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
  
  return(all_tabs)
}