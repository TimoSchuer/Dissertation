
  result <- kwic
  selectInputIDs <- intersect(names(kwic), names(AnnValues)) %>%
    sapply(paste0,"_", seq(1:nrow(kwic)), sep="")
  dataInit <-  kwic %>%
    dplyr::mutate(across(where(is.character), ~as.factor(.x))) %>%
    addDropdown(AnnValues)
  ui <- fluidPage(
    DT::dataTableOutput("data")
  )

  server <- function(input, output, session) {
    dataDisplay <- reactive({
      dataInit %>%
        addDropdownInputValues( selectInputIDs, input=input)
    })
    dataResult <- reactive({
      for (var in names(AnnValues)){
        dataInit[[var]] <- input[[paste0(var, "_", seq(1:nrow(kwic)))]]
      }
      return(dataInit)
    })

    output$data <- DT::renderDataTable({
      DT::datatable(
        dataInit, escape = FALSE, selection = 'none', rownames = FALSE,
        options = list(paging = FALSE, ordering = FALSE, scrollx = TRUE, dom = "t",
                       preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                       drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')
        )
      )
    }, server = TRUE)
    proxy <- DT::dataTableProxy(outputId = 'data')
    shiny::observeEvent({sapply(selectInputIDs, function(x){input[[x]]})}, {
      DT::replaceData(proxy = proxy, data = dataDisplay(), rownames = FALSE) # must repeat rownames = FALSE see ?replaceData and ?dataTableAjax
    }, ignoreInit = TRUE)
  }
shinyApp(ui, server)

