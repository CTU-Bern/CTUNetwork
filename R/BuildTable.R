#' Print a datatable object
#'
#' @param DataTab result from e.g. Calculations
#' @param OldCols Optional: vector of strings including DataTab columns names to replace
#' @param NewCols Optional: vector of strings for the replacement columns which has to be the same length as OldCols
#'
#' @importFrom data.table setnames
#' @import DT
#' @export
#'
#' @examples
#' All_Tabs <- getPFData()
#' Data <- extractData(All_Tabs)
#' Data <- filterData(Data, All_Tabs)
#' Data <- Calculations(Data)
#' Data <- BuildTable(Data)

BuildTable <- function(DataTab,
                       OldCols = c("ProdDate", "TimeSpent", "TimeBudget", "TimePercent", "Fraction"),
                       NewCols = c("Productive Date", "Time Spent (HH:MM)", "Time Budget (HH:MM)",
                                   "Time Spent/Budget [%]", "Fraction Spent/All [%]")) {

  # Isolating columns useful for this table
  DataTab[,"ProjectID"] <- droplevels(DataTab[,"ProjectID"]) # Removing unused factor levels
  DataTab[DataTab$Levels == 1,"ProdDate"] <- NA # No need to duplicate the productive date
  DataTab$TimePercent <- ifelse(DataTab$TimePercent>100,100,DataTab$TimePercent) # Maximum is set to 100%

  # Renaming columns
  if (length(OldCols) == length(NewCols)) {
    data.table::setnames(DataTab,OldCols,NewCols,skip_absent=T)
    } else {stop("The length of the OldCols and NewCols arguments should be identical.")}

  # rowGroup columns
  rowGroupColL1 <- which(colnames(DataTab)=="ProjectIDs")-1
  rowGroupColL2 <- which(colnames(DataTab)=="ProjectName")-1

  # Building the table
  DT::datatable(DataTab,
            class = 'cell-border stripe',
            rownames = F,
            filter = 'top',
            extensions = c('Buttons','ColReorder','FixedHeader','RowGroup'),
            options = list(autoWidth = T,
                           fixedHeader= T,
                           pageLength = 200,
                           colReorder = T, # Enable to dynamically change the position of columns
                           rowGroup = list(dataSrc = c(rowGroupColL1,rowGroupColL2)),
                           columnDefs = list(list(visible = F, targets = c(0,dim(DataTab)[2]-1))), # Hide specific columns
                           dom = 'Bfrtip', # Buttons position
                           buttons = list('copy', 'print',
                                          # Custom button to expand all lines
                                          # See: https://stackoverflow.com/questions/74800879/shiny-datatable-rowgroup-actionbutton-collapse-expand/
                                          list(extend = "",
                                               text = "Expand rows",
                                               action = DT::JS("function (e, dt, node, config) {
                                                           dt.rowGroup().dataSrc(0).draw();
                                                           dt.rowGroup().dataSrc(1).draw();
                                                           }")
                                               ),
                                          # Group of buttons to enable downloading the table in different formats
                                          list(extend = 'collection',
                                               buttons = list('csv', 'excel', 'pdf',
                                                              list(extend = 'pdfHtml5',
                                                                   orientation = 'landscape',
                                                                   pageSize ='LEGAL',
                                                                   text = "pdfLand")),
                                               text = 'Download'))),
            # Javascript function that collapses all rows at initialisation
            # See: https://stackoverflow.com/questions/66761304/dt-collapse-all-row-groups-by-default/
            callback = DT::JS(
              "table.on('click', 'tr.dtrg-group', function () {",
              "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
              "  $(rowsCollapse).toggleClass('hidden');",
              "});",
              "table.one('init', () => $('#DataTable .dtrg-group').trigger('click'))")) %>%

    # Project level headers
    DT::formatStyle('Levels',target = 'row', backgroundColor = DT::styleEqual(c(0, 1), c('#F5F5F5', "#999999"))) %>%

    # Progress bar (to highlight lines with TimePercent = 100%)
    DT::formatStyle(
      "Time Spent/Budget [%]",
      backgroundColor = DT::styleEqual(100, "#B24745FF"),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center') %>%

    # round specific columns data to two digits
    DT::formatRound(c("Time Spent/Budget [%]","Fraction Spent/All [%]"), 2)
}
