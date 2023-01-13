#' function to get the projects in a specific year
#' 
#' This code has been adapted for the purpose of producing the graphs for the CTU Yearly report
#' 
#' @param all_tabs result from e.g. getPFData
#' @param selected_year year for which projects should be summarized
#' 
#' @return projects_year
#' @export
#' 
#' @examples 
#' all_tabs <-  getPFData()
#' x <- projects_ctu_report(all_tabs,selected_year)



projects_ctu_report <- function(all_tabs,selected_year){


# TABLES ----
### Needed tables with information for the CTU Report: activitydata, project, customer 
tmp1 <- pf::constructCustomerParents(all_tabs$customer) %>% 
  dplyr::left_join(all_tabs$customer %>% 
                     dplyr::select(PK_CUSTOMER, FK_DEFAULTPRICELIST) %>%
                     dplyr::left_join(all_tabs$pricerecord %>%
                                        dplyr::select(PK_PRICERECORD, NAME), 
                                      by=c("FK_DEFAULTPRICELIST"="PK_PRICERECORD")),
                   by="PK_CUSTOMER") %>% 
  rename(Organization = NAME) %>%
  dplyr::left_join(all_tabs$project %>% 
              dplyr::select(PK_Project, FK_CUSTOMER, Path, CaseId, cf_applicable_ordinance_2, cf_study_category,
                     cf_snf, cf_eu, cf_charity_non_profit, cf_industry, cf_other_funding, cf_swiss_ctu, 
                     cf_other_ctu_cro, cf_location, cf_setup_number_of_sites), 
            by=c("PK_CUSTOMER"="FK_CUSTOMER")) %>%
  dplyr::filter(grepl("P-|FTE", CaseId)) %>%
  dplyr::select(top_CustomerName, CustomerName, Organization, Path, CaseId, cf_applicable_ordinance_2, cf_study_category,
                cf_snf, cf_eu, cf_charity_non_profit, cf_industry, cf_other_funding, cf_swiss_ctu, 
                cf_other_ctu_cro, cf_location, cf_setup_number_of_sites) %>%
  dplyr::mutate(projnum = readr::parse_number(gsub('[-]', ' ', CaseId))) %>%
  dplyr::arrange(projnum) %>%
  dplyr::distinct(projnum, Path, .keep_all=TRUE) %>%
  dplyr::mutate(across(projnum, as.character)) %>%
  dplyr::mutate(projnum = stringr::str_pad(projnum, 4, pad = "0")) %>%
  dplyr::rename(Project.name = Path) %>%
  dplyr::relocate(projnum, Project.name, .before = top_CustomerName)

### Select projects and FTE necessary. 
tmp2 <- pf::prepTime(all_tabs) %>%
  filter(lubridate::year(BookedDate) == selected_year) %>%
  dplyr::filter(grepl("P-|FTE", proj)) %>%
  select(BookedDate, Timespent, ctu_projectName, ctu_division, proj, projnum, projecttype) %>%
  group_by(projnum, ctu_division) %>% 
  summarise(total_time = sum(Timespent, na.rm = TRUE),
  ProjectName = first(ctu_projectName)) %>%
  group_by(projnum) %>%
  summarise( total_time = sum(total_time, na.rm = TRUE),
             divisions = paste0(ctu_division, collapse = ",")) %>%
  filter(total_time >= 8*60) %>%
  dplyr::mutate(Data.management = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("DM", ignore_case = TRUE))  ~ "yes", 
    TRUE ~ "no") ) %>%
  dplyr::mutate(Project.management = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("CSM|QM", ignore_case = TRUE))  ~ "yes", 
    TRUE ~ "no") ) %>%
  dplyr::mutate(Monitoring = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("MON", ignore_case = TRUE))  ~ "yes",
    TRUE ~ "no") ) %>%
  dplyr::mutate(Statistics = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("STA", ignore_case = TRUE))  ~ "yes",
    TRUE ~ "no") ) %>%
  dplyr::mutate(Clinical.investigation = dplyr::case_when(
    stringr::str_detect(divisions, stringr::regex("CI", ignore_case = TRUE))  ~ "yes",
    TRUE ~ "no") )

### merge the two dataframes 
projects_year <- merge(tmp1, tmp2, by="projnum", all.x = FALSE) %>%
  dplyr::mutate(Centers = dplyr::case_when(
    stringr::str_detect(cf_location, stringr::regex("International", ignore_case = TRUE)) ~ "multicenter (international)",
    stringr::str_detect(cf_setup_number_of_sites, stringr::regex("Single center", ignore_case = TRUE)) ~ "monocenter",
    cf_location=="Swiss" & cf_setup_number_of_sites=="Multicenter" ~ "multicenter (national only)" ) ) %>%
  dplyr::mutate(Ordinance_scto = dplyr::case_when(
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("IMP", ignore_case = TRUE)) ~ "ClinO_IMP",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Medical Device", ignore_case = TRUE)) ~ "ClinO_MD",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Other health", ignore_case = TRUE)) ~ "ClinO_Other",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Further", ignore_case = TRUE)) ~ "HRO_FurtherUse",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("Observational", ignore_case = TRUE)) ~ "HRO_sampleDataCollection",
    stringr::str_detect(cf_applicable_ordinance_2, stringr::regex("HRA", ignore_case = TRUE)) ~ "Non-HRA",
  )) %>%
  dplyr::mutate(dplyr::across(c(cf_snf,cf_eu,cf_charity_non_profit, cf_industry, cf_other_funding, cf_swiss_ctu, cf_other_ctu_cro), 
                tolower) ) %>%
  dplyr::mutate(Institution = dplyr::case_when(
    stringr::str_detect(top_CustomerName, stringr::regex("Insel|Universität Bern", ignore_case = TRUE)) ~ CustomerName,
    stringr::str_detect(Organization, stringr::regex("External", ignore_case = TRUE)) ~ Organization,
    stringr::str_detect(CustomerName, stringr::regex("Campus SLB|Winterthur Institute|Basel Institute for Clinical|GWT-TUD GmbH", ignore_case = TRUE)) ~ "External non-profit",
    stringr::str_detect(CustomerName, stringr::regex("Universitäre Psychiatrische Dienste|Department for BioMedical ", ignore_case = TRUE)) ~ CustomerName,
    stringr::str_detect(CustomerName, stringr::regex("External For-Profit|InnoMedica", ignore_case = TRUE)) ~ "External for-profit",
  )) %>%
  dplyr::filter(projnum != 9999) %>%
  dplyr::mutate(dplyr::across(c(Centers, Institution, Ordinance_scto), 
                as.factor)) %>%
  dplyr::mutate(Centers = factor(Centers, 
                          levels(Centers)[c(1,3,2)])) %>%
  dplyr::mutate(Inseluni = dplyr::case_when(
    stringr::str_detect(Institution, stringr::regex("Universitätsklinik für|Insel|Palliative", ignore_case = TRUE)) ~ 1, 
    stringr::str_detect(Institution, stringr::regex("for-profit", ignore_case = TRUE)) ~ 4,
    stringr::str_detect(Institution, stringr::regex("non-profit", ignore_case = TRUE)) ~ 3,
    TRUE ~ 2) ) %>%
  dplyr::mutate(Institution = stringr::str_remove(Institution,"Universitätsklinik für ")) %>%
  dplyr::mutate(Institution = fct_reorder(Institution, Inseluni)) %>%
  dplyr::mutate(Uni.med.oth = dplyr::case_when(
    stringr::str_detect(Institution, stringr::regex("Institut für Psychologie", ignore_case = TRUE)) ~ 'other',
    stringr::str_detect(Inseluni, stringr::regex("2", ignore_case = TRUE)) ~ 'medizin',
    
  ))
  
return(projects_year)
}

