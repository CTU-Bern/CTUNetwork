#' Construct a dataframe with the parents 
#'
#' @param x a dataframe with Parents variable
#' @param keyvar the primary key
#' @param stub Stub of variable name
#'
#' @return
#'
#' @examples
#' constructParents(all_tabs$customer, "PK_CUSTOMER", "Customer")
#' constructParents(all_tabs$project, "PK_Project", "Project") 
constructParents <- function(x, keyvar, stub){
  
  textvar_stub <- paste0(stub, "Name")
  key_stub <- paste0(stub, "Parent")
  
  tmp <- x %>% 
    discard_all_NA() %>% 
    dplyr::select({{keyvar}}, Parents, Name) %>% 
    dplyr::mutate(Parents = stringr::str_remove(Parents, "^,"),
                  Parents = stringr::str_remove(Parents, ",$"),
                  n_parents = stringr::str_count(Parents, ",") + 1, 
                  max_parents = max(n_parents)) 
  tmp <- tmp %>% 
    tidyr::separate(Parents, 
                    sep = ",", 
                    into = stringr::str_c("parent", 1:max(tmp$max_parents))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("parent", ignore.case = FALSE), as.numeric))
  
  name1 <- paste0(textvar_stub, 1)
  top_name <- paste0("top_", textvar_stub)
  
  tmp <- tmp %>% 
    dplyr::bind_cols(purrr::map_dfc(1:max(tmp$max_parents), function(x){
      by <- keyvar
      names(by) <- paste0("parent", x)
      newname <- paste0("name", x)
      tmp <- tmp %>% 
        dplyr::select(paste0("parent", x)) %>% 
        dplyr::left_join(tmp %>% 
                           dplyr::select({{keyvar}}, Name),
                         by = by
        ) %>% 
        dplyr::select(Name) # %>% 
      # rename( {{newname}} = Name)
      names(tmp) <- newname
      tmp
    })) %>% 
    dplyr::mutate(!!top_name := dplyr::case_when(!is.na(name1) ~ name1,
                                              TRUE ~ Name)) %>% 
    dplyr::select(-c(n_parents, max_parents)) %>% 
    dplyr::rename_with(.fn = function(x){
      stringr::str_replace(x, "^name", textvar_stub)
    }, dplyr::starts_with("name")) %>% 
    dplyr::rename_with(.fn = function(x){
      stringr::str_replace(x, "^parent", key_stub)
    }, dplyr::starts_with("parent")) %>% 
    dplyr::rename(!!textvar_stub := Name)

  return(tmp)
}


#' Customer hierarchy
#'
#' @param customer e.g. \code{all_tabs$customer}
#'
#' @return data dataframe of customer names and parents
#' @export
#'
#' @examples
#' all_tabs <- getPFData()
#' customerParents <- constructCustomerParents(all_tabs$customer)
constructCustomerParents <- function(customer){
  
  constructParents(customer, "PK_CUSTOMER", "Customer")
  
}


#' Project hierarchy
#'
#' @param project e.g. \code{all_tabs$project}
#'
#' @return data dataframe of project names and parents
#' @export
#'
#' @examples
#' all_tabs <- getPFData()
#' projectParents <- constructProjectParents(all_tabs$project)
#' 
#' 
#' # diagnostics
#' table(projectParents$ProjectName2, projectParents$ctu_division, useNA = "ifany")
#' x <- projectParents %>% 
#'   dplyr::group_by(ctu_projectName, ProjectName1) %>% 
#'   dplyr::summarize(n_divs = length(unique(ctu_division)))
#' 
#' 
 constructProjectParents <- function(project){
  
  constructParents(project, "PK_Project", "Project") %>% 
    dplyr::rename(ctu_projectName = top_ProjectName) %>% 
    dplyr::mutate(ctu_division = dplyr::case_when(
      stringr::str_detect(ProjectName2, stringr::regex("Data Management|DataManagment|secuTrial|REDCap|Webspirit|Study Website|cloud|dm support|sharefile|sharepoint", ignore_case = TRUE)) ~ "DM",
      stringr::str_detect(ProjectName2, stringr::regex("Statistic|Programming grant", ignore_case = TRUE)) ~ "STA",
      stringr::str_detect(ProjectName2, stringr::regex("Clinical Investigation|Study Nurse", ignore_case = TRUE)) ~ "CI",
      stringr::str_detect(ProjectName1, stringr::regex("Support Neuromuskuläres Zentrum|Hämatologie Support", ignore_case = TRUE)) & !is.na(ProjectName2) ~ "CI",
      stringr::str_detect(ProjectName2, stringr::regex("Clinical Study Management|Project (Coord|manage)|Project(Coord|manage)", ignore_case = TRUE)) ~ "CSM",
      stringr::str_detect(ProjectName2, stringr::regex("monitoring|Document Development|Regulatory|Safety", ignore_case = TRUE)) ~ "MON",
      stringr::str_detect(ProjectName2, stringr::regex("Regulatory", ignore_case = TRUE)) ~ "RA",
      stringr::str_detect(ProjectName2, stringr::regex("Quality Management|QM Support|Graphic Design/Layout|Auditing", ignore_case = TRUE)) ~ "QM",
      stringr::str_detect(ProjectName2, stringr::regex("Material Expenses|IT support", ignore_case = TRUE)) ~ "IT",
      # SwissTavi is different - no Divs in the project
      stringr::str_detect(ProjectName1, stringr::regex("Swiss Tavi Registry", ignore_case = TRUE)) & stringr::str_detect(ProjectName, stringr::regex("monitoring", ignore_case = TRUE)) ~ "MON",
      stringr::str_detect(ProjectName1, stringr::regex("Swiss Tavi Registry|Small Projects", ignore_case = TRUE)) & stringr::str_detect(ProjectName, stringr::regex("statistics", ignore_case = TRUE)) ~ "STA",
      stringr::str_detect(ProjectName1, stringr::regex("Swiss Tavi Registry", ignore_case = TRUE)) & stringr::str_detect(ProjectName, stringr::regex("Project (Coord|manage)", ignore_case = TRUE)) ~ "CSM",
      stringr::str_detect(ProjectName1, stringr::regex("Swiss Tavi Registry", ignore_case = TRUE)) & stringr::str_detect(ProjectName, stringr::regex("Data Management", ignore_case = TRUE)) ~ "DM",
      stringr::str_detect(ProjectName1, stringr::regex("PreferenceSensitiveCare", ignore_case = TRUE))  ~ "STA",
      # stringr::str_detect(ProjectName1, stringr::regex("PreferenceSensitiveCare", ignore_case = TRUE))  ~ "STA",
      
      ),
      CaseId = project$CaseId,
      top_project = dplyr::case_when(!is.na(ProjectParent1) ~ ProjectParent1,
                                     TRUE ~ as.double(PK_Project))
      )
  
}


#' Finance article hierarchy
#'
#' @param financearticle e.g. \code{all_tabs$financearticle}
#'
#' @return data dataframe of project names and parents
#' @export
#'
#' @examples
#' all_tabs <- getPFData()
#' projectParents <- constructProjectParents(all_tabs$financearticle)
constructFinanceArticleParents <- function(financearticle){
  
  financearticle %>% 
    dplyr::rename(Parents = PARENTS,
           Name = NAME) %>% 
  constructParents("PK_FINANCEARTICLE", "finart_")
  
}

