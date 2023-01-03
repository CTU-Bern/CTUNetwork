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

server <- function(input, output, session) {

  # Send ColNum to the browser
  observe({
    session$sendCustomMessage("column-integer", jsonlite::toJSON(ColNum))
  })

  # Generate the table
  output$my_table <- DT::renderDataTable({
    datatable(
      mtcars[1:15, 1:5],
      extensions = c('RowGroup',"Buttons"),
      options = list(rowGroup = list(dataSrc = 3),
                     pageLength = 20,
                     dom = 'tB',
                   #   buttons = list(
                   #     list(extend = "",
                   #          text = "UnGroup rowGroup disp",
                   #          action = JS("function (e, dt, node, config) {dt.rowGroup().dataSrc('').draw();}")
                   #     ),
                   #     list(extend = "",
                   #          text = "Group rowGroup disp",
                   #          action = JS("function (e, dt, node, config) {dt.rowGroup().dataSrc(3).draw();}")
                   #     )
                   #   )
                   # ),
                     # buttons = list(
                     #   list(extend = "",
                     #      text = "Expand rows",
                     #      action = JS("function (e, dt, node, config) {dt.rowGroup().dataSrc(3).draw();}")))),
                     buttons = list(
                       list(extend = "",
                            text = "Expand rows",
                            action = JS("Shiny.addCustomMessageHandler('column-integer', function (e, dt, node, config, ColNum) {dt.rowGroup().dataSrc(ColNum).draw();})")))),
      # callback = JS(
      #   "table.on('click', 'tr.dtrg-group', function () {",
      #   "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
      #   "  $(rowsCollapse).toggleClass('hidden');",
      #   "});",
      #   "table.one('init', () => $('#my_table .dtrg-group').trigger('click'))",
      #   "$('#expandButton').on('click', function(){",
      #   "  $('#my_table').toggleClass('hidden')",
      #   "});"
      # ),
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
