#' Title
#'
#' @param kwic concordanz
#' @param corpus corpus by ip
#' @param objectName name of object tp save to
#'
#' @return
#' @export
#'
#' @examples
#'
annotate <- function(kwic=data.frame(IPId=paste("a",1:10, sep=""),a=1:10,b=10:1),
                     corpus = data.frame(IPId=paste("a",1:30, sep=""),a=1:30,b=30:1),
                     objectName=paste(deparse(substitute(kwic)),"_ann",sep="") ){
  data <- kwic %>% dplyr::mutate(across(where(is.character), ~as.factor(.x)))
  ui <- fluidPage(
    mainPanel(
      dataTableOutput("data"),
      actionButton("send","SendPraat"),
      dataTableOutput("transcript")
    )

  )
  server <- function(input,output,session){
    #data <- kwic %>% data.frame(stringsAsFactors = TRUE)
    output$data <- renderDataTable(data ,
                                   editable=TRUE,
                                   filter="top",
                                   rownames=FALSE,
                                   extensions="Buttons",
                                   selection= "single",
                                   options = list(
                                     paging = TRUE,
                                     pageLength=10,
                                     searching = TRUE,
                                     fixedColumns = TRUE,
                                     autoWidth = TRUE,
                                     #ordering = TRUE,
                                     dom = 'Bfrtip',
                                     buttons = c('csv', 'excel','colvis'),
                                     autoWidth=TRUE,
                                     columnDefs = list(list(width = '2px', targets = c(0,2)))
                                   ))
    #proxy= dataTableProxy("data")
    observeEvent(input$data_cell_edit,{
     data <<- editData(data, input$data_cell_edit, rownames = FALSE)
     assign(objectName, data, envir = .GlobalEnv)
    })

    observeEvent(input$send,{
      s <- input$data_rows_selected
      paste("C:/Users/Admin/sciebo/Diss/Praat/sendpraat-win.exe praat ",
            paste0("\"Read from file... ",data[s, "pathAudio"],"\"",sep=""), sep="") %>%
        system()



      paste0('C:/Users/Admin/sciebo/Diss/Praat/sendpraat-win.exe praat ',
             '"selectObject: ', '\\"Sound ',
             data[s,] %>%
               dplyr::pull(pathAudio) %>%
               stringr::str_extract("/[^/]*\\.") %>%
               stringr::str_remove_all("\\.") %>%
               stringr::str_remove_all("/") ,'\\""',
             " \"View & Edit\"",
             paste(" \"editor: ", '\\"Sound ',
                   data[s,] %>%
                     dplyr::pull(pathAudio) %>%
                     stringr::str_extract("/[^/]*\\.") %>%
                     stringr::str_remove_all("\\.") %>%
                     stringr::str_remove_all("/") ,
                   '\\""' ,

                   sep=""),
             paste(" \"Zoom: ",data[s, "Start_time"]-3,", ",data[s, "End_time"]+3,"\"",sep=""),
             " endeditor",
             sep="") %>% system()


      })
    output$transcript <- renderDataTable({
      s <- input$data_rows_selected
      if(length(s)){
        selected <- which(corpus$IPId==data[s,"IPId"])
      corpus %>% select(IPId,Name,Text) %>% slice(seq(selected-10,selected+10))
      }
    }, options=list(
      paging=TRUE,
      pageLength=25
    ))
  }
  shinyApp(ui=ui,server=server)
}
#annotate(kwicDasIst, corpus =  CorpusIP)
