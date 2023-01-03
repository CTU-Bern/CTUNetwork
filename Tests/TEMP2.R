library(shiny)
library(DT)
ui <- fluidPage(# Application title
  titlePanel("Collapse/Expand table"),
  mainPanel(
    tabsetPanel(
      tabPanel("table1",
               # actionButton("expandButton", "Expand/Collapse"),
               dataTableOutput("my_table"))
    )
  ))

# Column corresponding to the rowGroup argument
ColNum <- 3

js <- "
var ColNum = 3;
$(document).ready(function() {
  Shiny.addCustomMessageHandler('column-integer', function(x) {
    ColNum = x;
  });
});
"

tags$head(tags$script(HTML(js)))

server <- function(input, output, session) {
#
#   # Send ColNum to the browser
#   observe({
#     session$sendCustomMessage("column-integer", jsonlite::toJSON(ColNum))
#   })

  # Generate the table
  output$my_table <- DT::renderDataTable({
    datatable(
      mtcars[1:15, 1:5],
      extensions = c('RowGroup',"Buttons"),
      options = list(rowGroup = list(dataSrc = ColNum),
                     pageLength = 20,
                     dom = 'tB',
                     # buttons = list(
                     #   list(extend = "",
                     #      text = "Expand rows",
                     #      action = JS("function (e, dt, node, config) {dt.rowGroup().dataSrc(3).draw();}")))),
                     buttons = list(
                       list(extend = "",
                            text = "Expand rows",
                            action = JS("function (e, dt, node, config) {dt.rowGroup().dataSrc(ColNum).draw();}")))
                     ),
      callback = JS(
        "table.on('click', 'tr.dtrg-group', function () {",
        "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
        "  $(rowsCollapse).toggleClass('hidden');",
        "});",
        "table.one('init', () => $('#my_table .dtrg-group').trigger('click'))"
      ),
      selection = 'none'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
