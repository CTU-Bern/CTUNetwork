#' prepare pf time entries
#' 
#' the output of this can be grouped by whatever variable (ctu_division, worker, ...) 
#' to create summaries of minutes worked at the designated level
#'
#' @param all_tabs result from e.g. getPFData
#'
#' @return dataframe
#' @export
#'
#' @examples
#' all_tabs <- getPFData()
#' x <- prepTime(all_tabs)
prepTime <- function(all_tabs){
  
  all_tabs$activitydata %>%
    pf::discard_all_NA() %>% 
    # filter(BookedDate >= start_date & BookedDate <= end_date) %>% 
    # # add whether it's billable time
    dplyr::left_join(all_tabs$activitycategory %>%
                       dplyr::select(PK_ActivityCategory, Name) %>%
                       dplyr::rename(billable = Name),
                     by = c(FK_ACTIVITYAREA = "PK_ActivityCategory")) %>%
    dplyr::mutate(
      year = lubridate::year(BookedDate),
      q_in_year = lubridate::quarter(BookedDate),
      billable_time = dplyr::case_when(billable == "Billable" ~ Timespent),
      nonbillable_time = dplyr::case_when(billable == "Non-billable" ~ Timespent),
    ) %>%
    # # group_by(FK_PROJECT, FK_FINANCEPOSITION, q_in_year) %>% 
    # # summarize(Timespent = sum(Timespent),
    # #           Timespent_billable = sum(Timespent_billable),
    # #           n = n()) %>%
    # # add project parents
    dplyr::left_join(constructProjectParents(all_tabs$project),
                     by = c("FK_PROJECT" = "PK_Project")) %>% 
    # filter(str_detect(top_ProjectName, "STREAM"))
    # add finance article (work package level bills)
    dplyr::left_join(all_tabs$financeposition %>%
                       discard_all_NA() %>% 
                       dplyr::select(PK_FINANCEPOSITION, FK_PROJECT, FK_FINANCEARTICLE, 
                                     UNITPRICE, COUNT, SUMNETTO),
                     by = c("FK_FINANCEPOSITION" = "PK_FINANCEPOSITION",
                            "FK_PROJECT")) %>%
    # add finance article parents 
    dplyr::left_join(all_tabs$financearticle %>% 
                       dplyr::left_join(constructFinanceArticleParents(all_tabs$financearticle)) %>%  
                       dplyr::select(PK_FINANCEARTICLE, starts_with("finart"), NUMBER),
                     by = c(FK_FINANCEARTICLE = "PK_FINANCEARTICLE")) %>% 
    dplyr::mutate(billed = !is.na(FK_FINANCEPOSITION)) %>%
    # add (internal) main project number
    dplyr::left_join(all_tabs$project %>% 
                       dplyr::select(FK_MAINPROJECT, PK_Project, FK_CUSTOMER),
                     by = c(FK_PROJECT = "PK_Project")) %>% 
    # get the 4 digit project number
    dplyr::left_join(all_tabs$project %>% 
                       dplyr::select(FK_PROJECTTYPE, PK_Project, CaseId) %>% 
                       dplyr::rename(proj = CaseId) %>% 
                       dplyr::mutate(projnum = stringr::str_replace(proj, "^.{1,}-", ""),
                                     projnum = trimws(projnum)),
                     by = c(FK_MAINPROJECT = "PK_Project")) %>% 
    # project type (e.g. project, consulting, internal, ...)
    dplyr::left_join(all_tabs$projecttype %>% 
                       dplyr::select(PK_ProjectType, Name) %>% 
                       dplyr::rename(projecttype = Name),
                     by = c(FK_PROJECTTYPE = "PK_ProjectType")) %>%
    # customer
    dplyr::left_join(constructCustomerParents(all_tabs$customer), 
                     by = c(FK_CUSTOMER = "PK_CUSTOMER"))
  
}
