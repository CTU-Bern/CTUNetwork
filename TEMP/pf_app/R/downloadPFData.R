#' Download data from projectfacts via ODBC
#'
#' @param all logical - export all tables in the database (TRUE) or just the useful ones (FALSE)
#'
#' @return
#'
#' @examples
#' # downloadPFData()
downloadPFData <- function(all = FALSE, ...){
  con <- DBI::dbConnect(odbc::odbc(), "pf", catalog_name = "projectfacts")
  usefultabs <- c("activitycategory", 
                  "activitydata", 
                  "attendance_time", 
                  "benutzergruppe", 
                  "contactfield",
                  "crmactivity", 
                  "crmkontakt", 
                  "customer", 
                  "customfields", 
                  "customfieldvalue", 
                  "entitycheck", 
                  "entityrelation", 
                  "expenseposition", 
                  "expensepositiontype", 
                  "financearticle", 
                  "financeposition", 
                  "financepositiongroup", 
                  "financerecord", 
                  "grouppermission",
                  "mandant",
                  "notification", 
                  "priceposition",
                  "pricerecord",
                  "project",
                  "projectactivity", 
                  "projectstatedefinition", 
                  "projecttype", 
                  "ticket", 
                  "ticketprocessitem", 
                  "ticketstatedefinition", 
                  "ticketworkerrelation", 
                  "workday", 
                  "worker")
  
  if(all) usefultabs <- DBI::dbListTables(con, catalog_name = "projectfacts")
  
  all_tabs <- lapply(usefultabs, function(x){
    DBI::dbReadTable(con, x)
  })
  names(all_tabs) <- usefultabs
  
  # filter the mandants?
  ctu <- all_tabs$mandant %>%
    dplyr::filter(Name == "ctu")

  all_tabs2 <- purrr::map(all_tabs, function(x){
    if("FK_MANDANT" %in% names(x)){
      x <- x %>%
        dplyr::filter(FK_MANDANT %in% ctu$PK_Mandant)
    }
    x %>%
      dplyr::select(-ends_with("MANDANT"))
    x
  }) %>% 
    setEncoding()
  
  
  return(all_tabs2)
}