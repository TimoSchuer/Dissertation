annotateUiDash <- function(id, width = 10) {
  shinydashboard::box(
    width = width,
    fluidRow(
      column(width = 4,
             offset = 6,
             actionButton(NS(id,"prevItem"),
                          label = "",
                          icon = shiny::icon("arrow-up")
             )
      )
    ),
    fluidRow(
      column(width = 2,
             textInput("ItemNumber",
                       label = "Direkt zu Item Nummer",
                       placeholder = 1)
      ),
      column(width = 4,
             offset = 2,
             shiny::selectizeInput(NS(id,"annCat"),
                                   "Annotation",
                                   label="",
                                   choices = c("a","b","c"),
                                   width = "100%"
             ),
      )
    ),
    fluidRow(
      column(width = 2,
             offset = 2,
             actionButton(NS(id,"prevAnnCat"),
                          label = "",
                          icon = shiny::icon("arrow-left")
             )
      ),
      column(width=4,
             offset = 0,
             textInput(NS(id,"annValue"),
                       "Annotation Wert",
                       value = "",
                       width = "100%")
      ),
      column(width = 2,
             offset = 2,
             actionButton(NS(id,"nextAnnCat"),
                          label = "",
                          icon = shiny::icon("arrow-right")
             ))
    ),
    fluidRow(
      column(width = 4,
             offset = 6,
             actionButton(NS(id,"nextItem"),
                          label = "",
                          icon = shiny::icon("arrow-down")
             ))
    )
  )
}



annotateServer <- function(id, AnnData){
  #stopifnot(is.reactive(Task))
  stopifnot(is.data.frame(AnnData))
  moduleServer(id, function(input,output, session){
    item <- reactiveVal(1)
    annCat <- reactiveVal(6)
    data <- reactiveValues(data = AnnData)
    observeEvent(input$prevItem,{
      item(item()-1)
    })
    observeEvent(input$nextItem,{
      item(item()+1)
    })
    observeEvent(input$prevAnnCat,{
      annCat(annCat()-1)
    })
    observeEvent(input$nextAnnCat,{
      annCat(annCat()+1)
    })
    observeEvent(input$ItemNumber,{
      n <- input$ItemNumber
      item(as.numeric(n))
    })

    observeEvent(item() | annCat(),{
      val <- reactive({
        data$data[item(),annCat()]
      })
      updateTextInput(session,
                      "annValue",
                      value = val())

      updateSelectizeInput(session,
                           "annCat",
                           label = names(data$data)[annCat()],
                           choices= unique(data$data[,annCat()]),
                           selected = val(),
                           server = TRUE)
    })

    observeEvent(input$annCat,{
      req(input$annCat)
      if(is.numeric(data$data[,annCat()])){
        data$data[item(), annCat()] <- as.numeric(input$annCat)
        data$data[,annCat()] <- as.numeric(data$data[,annCat()])
      }else{
        data$data[item(), annCat()] <- input$annCat
      }
    })

    observeEvent(input$annValue,{
      req(input$annValue)
      if(is.numeric(data$data[,annCat()])){
        data$data[item(), annCat()] <- as.numeric(input$annValue)
        data$data[,annCat()] <- as.numeric(data$data[,annCat()])
      }else{
        data$data[item(), annCat()] <- input$annValue
      }
    })

    list(item=reactive(item()),
         annCat=reactive(annCat()),
         data=reactive(data$data %>%
                         bind_rows() %>%
                         as.data.frame()
         )
    )

  })

}

