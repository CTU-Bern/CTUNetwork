#' List of workers with their active status
#'
#' @param all_tabs e.g. the result from \code{getPFData()}
#'
#' @return
#' @export
#'
#' @examples
#' getPFData() %>% 
#' setEncoding() %>% 
#' workerActiveList()
workerActiveList <- function(all_tabs){
  dplyr::left_join(all_tabs$worker %>% 
                    dplyr::select(-c(Vorname, Nachname)), 
                  all_tabs$crmkontakt %>% 
                    discard_all_NA(), 
                  by = c("FK_CRMKONTAKT" = "PK_CRMKONTAKT")) %>% 
    dplyr::left_join(all_tabs$customer %>% 
                dplyr::filter(stringr::str_detect(Path, "CTU Bern")) %>% 
                dplyr::select(PK_CUSTOMER, Path),
              by = c("FK_CUSTOMER" = "PK_CUSTOMER")) %>% 
    dplyr::select(FK_CRMKONTAKT, FK_CUSTOMER, Vorname, 
                  Nachname, EMail, Path, workerAktiv) %>% 
    dplyr::rename(Customer = Path,
           'First name' = Vorname,
           'Last name' = Nachname,
           Email = EMail)
}



