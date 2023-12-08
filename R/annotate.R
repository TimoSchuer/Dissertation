#' Title
#'
#' @param kwic concordanz
#' @param corpus corpus by ip
#' @param objectName name of object tp save to
#' @param pathPraat path where praat and sendpraat are saved does not end with /
#'
#' @return
#' @export
#'
#' @examples
#'
annotateKWIC <- function(kwic=data.frame(IPId=paste("a",1:10, sep=""),a=1:10,b=10:1),
                     corpus = data.frame(IPId=paste("a",1:30, sep=""),a=1:30,b=30:1),
                     objectName=paste(deparse(substitute(kwic)),"_ann",sep=""),
                     pathPraat= "../praat"){

  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(2,
      shiny::actionButton("quit","Quit"),
      offset = 10
      )
    ),
 #   mainPanel(
      shiny::fluidRow(
        shiny::column(width=11,
          DT::dataTableOutput("data")
          ),
        shiny::column(width = 1,
               shiny::br(),
               shiny::br(),
               shiny::br(),
               shiny::br(),
               shiny::br(),
               shiny::br(),
               shiny::br(),
          shiny::verbatimTextOutput("ann"))
        ),
      #shiny::actionButton("start","Start Praat"),
      shiny::fluidRow(shiny::selectInput("tiers", "Select Tiers to be shown",
                                         choices = c("Transkript", "Standard","beides"), selected = "beides")  ,
                      shiny::actionButton("show","Show Transcript"),
                      shiny::actionButton("send","Send to Praat"),
                      shiny::actionButton("clear","Clear Praat")),
      shiny::fluidRow(
        DT::dataTableOutput("transcript"))
 #   )
  )
  server <- function(input,output,session){
    #data <- kwic %>% data.frame(stringsAsFactors = TRUE)
    hiddenCols <-which(names(kwic) %in% c("IPNumber" ,"EventID" ,"File","Speaker" ,"TierID", "TierCategory" ,"Start" ,"End" ,"Start_time",   "End_time", "pathAudio", "pathFile"))-1
    data <- kwic %>% dplyr::mutate(across(where(is.character), ~as.factor(.x)))
    output$data <- DT::renderDataTable(data ,
                                   editable=TRUE,
                                   filter="top",
                                   rownames=FALSE,
                                   extensions="Buttons",
                                   selection= list(mode="single",target="row+column"),
                                   width= "100%",
                                   options = list(
                                     paging = TRUE,
                                     pageLength=10,
                                     searching = TRUE,
                                     fixedColumns = FALSE,
                                     autoWidth = FALSE,
                                     #ordering = TRUE,
                                     dom = 'Bfrtip',
                                     buttons = list(
                                       list(extend='colvisGroup', text="Show", show=hiddenCols),
                                       list(extend='colvisGroup', text="Hide", hide=hiddenCols),
                                       "colvis"
                                     ),
                                     autoWidth = TRUE,
                                     columnDefs = list(list(width = '100px', targets = "_all"),
                                                       list(targets = hiddenCols, visible = FALSE))),
                                   class = 'cell-border stripe'
                                   )
    proxy <-  DT::dataTableProxy("data")
    shiny::observeEvent(input$data_cell_edit,{
      # info = input$data_cell_edit
      # #str(info)
      # i = info$row
      # j = info$col
      # v = info$value
      # data[i, j] <<- DT::coerceValue(v, data[i, j])
      # DT::replaceData(proxy, data, resetPaging = FALSE)  # important

      data <<- DT::editData(data, input$data_cell_edit,proxy,resetPaging = FALSE, rownames = FALSE)
       assign(objectName, data, envir = .GlobalEnv)
      # renderDataTable(data)
    })

    # observeEvent(input$start,{
    #   system("start C:/Users/Admin/sciebo/Diss/Praat/praat.exe", intern = TRUE)
    # })

    shiny::observeEvent(input$send,{

      s <- input$data_rows_selected
      if(length(s)){
        sendPraat(data=data, rowNumber = s, pathPraat = pathPraat)
      }

    })
     shiny::observeEvent(input$show,{
      s <- input$data_rows_selected
      if(length(s)){
        transcript <- showTranscript(corpus=corpus, rowNumber = s,tier = input$tiers)
      }
    output$transcript <- DT::renderDataTable(transcript
    ,selection= list(selected=11),
    options=list(
      paging=TRUE,
      searching=FALSE,
      pageLength=25))
    })
    shiny::observeEvent(input$data_columns_selected,{
      output$ann <- shiny::renderText({
        if(length(input$data_columns_selected)){
          data[,input$data_columns_selected+1] %>% unique() %>% na.omit() %>% paste0(collapse = "\n")
        }
      }, sep="")
    })
    shiny::observeEvent(input$quit,{
      shiny::stopApp()
    })
    shiny::observeEvent(input$clear,{
      paste0(pathPraat,'/sendpraat-win.exe praat ','"select all"',' "Remove"') %>% system()
    })

  }
  shiny::shinyApp(ui=ui,server=server)
}
annotateKWIC(kwic = kwic, corpus = CorpusIP)

