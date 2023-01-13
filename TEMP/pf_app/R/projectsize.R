#' Estimate project size based on planned number of hours
#' 
#' Small projects are < 200 hours, medium are 200 - 500 hours, large are > 500 hours
#'
#' @param all_tabs list of tables (i.e. result of \code{getPFData()})
#'
#' @return dataframe with total time planned for a project and the size of the project. 
#' 
#' @export
#'
#' @examples
projectSize <- function(all_tabs) {
  all_tabs$projectactivity  %>%
    dplyr::group_by(FK_PROJECT) %>%
    dplyr::summarise(timeplanned = dplyr::first(TimePlanned[!is.na(TimePlanned)])) %>%
    dplyr::left_join(constructProjectParents(all_tabs$project),
              by = c("FK_PROJECT" = "PK_Project")) %>%
    dplyr::group_by(ctu_projectName) %>%
    dplyr::summarise(timeplanned = sum(timeplanned, na.rm = TRUE)) %>%
    dplyr::mutate(
      timeplanned = timeplanned / 60,
      size = dplyr::case_when(
        timeplanned < 200 ~ "small",
        timeplanned >= 200 &
          timeplanned <= 500 ~ "medium",
        timeplanned > 500 ~ "large"
      )
    )
}