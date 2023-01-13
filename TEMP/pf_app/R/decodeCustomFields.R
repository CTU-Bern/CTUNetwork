#' Add custom fields to projects data
#'
#' @param project e.g. \code{all_tabs$project}
#' @param customfields e.g. \code{all_tabs$customfields}
#'
#' @return
#' @export
#'
#' @examples
#' project <- all_tabs$project
#' customfields <- all_tabs$customfields
#' project2 <- decodeCustomFields(project, customfields)
#' @importFrom magrittr %>%
decodeCustomFields <- function(project, customfields){
  
  tags <- customfields %>% 
    dplyr::mutate(
      storekey = as.character(STOREKEY),
      storekey = stringr::str_replace(storekey, "\\.", "\\\\."),
      regex = stringr::str_c("(?<=<", storekey, ">).+(?=</", storekey, ">)"),
      # regex = stringr::str_c("(?<=<", storekey, ">).{1,}(?=</", storekey, ">)"),
      name = janitor::make_clean_names(NAME),
      name = stringr::str_c("cf_", name)
    ) %>% 
    dplyr::select(PK_CUSTOMFIELD, NAME, AKTIV, STOREKEY, AREA, regex, name) 
    
  tags <- tags %>% 
    dplyr::filter(.data$STOREKEY %in% names(which(sapply(tags$STOREKEY, function(x) any(grepl(x, project$CUSTOMFIELDVALUES))))))
  
  out <- project
  
  if(nrow(tags) > 0){
    res <- purrr::map_dfc(1:nrow(tags), function(x){
      name <- tags$name[x]
      project %>% dplyr::mutate(
        {{name}} := stringr::str_extract(CUSTOMFIELDVALUES, tags$regex[x])
        ) %>% 
          dplyr::select({{name}})
      }) 
    
    out <- out %>% 
      dplyr::bind_cols(res)
  }
  
  return(out)
  
}



