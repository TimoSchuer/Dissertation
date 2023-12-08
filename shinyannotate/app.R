library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    DTOutput('x1')
  ),
  server = function(input, output, session) {
    x = iris
    x$Date = Sys.time() + seq_len(nrow(x))
    output$x1 = renderDT(x, selection = 'none', editable = TRUE)

    proxy = dataTableProxy('x1')

    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      x[i, j] <<- DT::coerceValue(v, x[i, j])
      replaceData(proxy, x, resetPaging = FALSE)  # important
    })
  }
)
