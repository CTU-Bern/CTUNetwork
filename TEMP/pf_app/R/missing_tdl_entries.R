



rc_api_token <- "D2F71E35ACDAA83025D9EEA0F16D8BDF"

missing_tdl_entries <- function(all_tabs, rc_api_token){
  library(tidyverse)
  
  # REDCap data 
  req <- httr2::request("https://redcap.ctu.unibe.ch/api/") %>% 
    httr2::req_headers() %>% 
    httr2::req_body_form(list(token = rc_api_token,
                  content = "record",
                  format = "csv",
                  type = "flat"))
  
  req %>% httr2::req_dry_run()
  resp <- req %>% httr2::req_perform()
  dat <- read.csv(textConnection(resp %>% httr2::resp_body_string()))
  
  req <- httr2::request("https://redcap.ctu.unibe.ch/api/") %>% 
    httr2::req_headers() %>% 
    httr2::req_body_form(list(token = rc_api_token,
                       content = "metadata",
                       format = "csv"))
  resp <- req %>% httr2::req_perform()
  meta <- read.csv(textConnection(resp %>% httr2::resp_body_string()))
  
  
  projs <- dat %>% 
    dplyr::filter(stringr::str_detect(redcap_event_name, "details")) %>% 
    purrr::discard(~ all(is.na(.) | . == ""))
  
  staff <- dat %>% 
    dplyr::filter(stringr::str_detect(redcap_event_name, "entries")) %>% 
    purrr::discard(~ all(is.na(.) | . == ""))
  
  staff_code <- meta %>% 
    dplyr::filter(field_name == "staff") %>% 
    dplyr::pull(select_choices_or_calculations) %>% 
    strsplit("|", fixed = TRUE) %>% 
    unlist() %>% 
    as.data.frame() %>% 
    dplyr::rename(staffmember = 1) %>% 
    dplyr::mutate(split = stringr::str_split_fixed(staffmember, ", ", n = 2),
           code = split[, 1],
           name = split[, 2],
           code = as.numeric(code)) %>% 
    dplyr::select(code, name)
    
  
  rc_staff <- staff %>% 
    dplyr::left_join(staff_code, by = c("staff" = "code")) %>% 
    dplyr::select(record_id, name) %>% 
    dplyr::mutate(rc = 1)
  
  # PF data
  rc_pf <- all_tabs %>% 
    prepTime() %>% 
    dplyr::group_by(proj, projnum, FK_WORKER) %>% 
    dplyr::summarize(across(matches("time"), sum)) %>% 
    dplyr::left_join(all_tabs$worker %>% 
                dplyr::select(PK_Worker, FK_CRMKONTAKT, Aktiv), 
              by = c("FK_WORKER" = "PK_Worker")) %>% 
    dplyr::left_join(all_tabs$crmkontakt %>% 
                dplyr::select(PK_CRMKONTAKT, Vorname, Nachname), 
              by = c("FK_CRMKONTAKT" = "PK_CRMKONTAKT")) %>% 
    dplyr::mutate(name = stringr::str_c(Vorname, Nachname, sep = " "),
           projnum = stringr::str_remove(projnum, "^0{1,}"),
           pf = 1,
           projnum = as.numeric(projnum)) %>% 
    dplyr::full_join(rc_staff, by = c("projnum" = "record_id", "name"))
  
  missing <- rc_pf %>% 
    dplyr::filter(pf == 1 & is.na(rc)) %>% 
    dplyr::filter(!stringr::str_detect(proj, "^C")) %>% 
    dplyr::filter(!stringr::str_detect(Nachname, "Superuser")) %>% 
    dplyr::filter(projnum %in% projs$record_id) %>% 
    dplyr::filter(Aktiv == 1)
  
  missing2 <- rc_pf %>% 
    dplyr::filter(pf == 1 & is.na(rc)) %>% 
    dplyr::filter(!stringr::str_detect(proj, "^C")) %>% 
    dplyr::filter(!stringr::str_detect(Nachname, "Superuser")) %>% 
    dplyr::filter(projnum %in% projs$record_id) %>% 
    dplyr::filter(Aktiv == 0)
  
  missing
    dplyr::ungroup() %>% 
    dplyr::count(name.x, sort = TRUE)
  
    
  missing %>% 
    dplyr::select(proj, name.x) %>% 
    View()
}