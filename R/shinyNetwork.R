shinyNetwork <- function(exb){
  shiny::shinyApp(
    ui= shiny::fluidPage(
      shiny::fluidRow(DT::dataTableOutput("ExbData")),
      shiny::fluidRow(shiny::selectInput("vars", "Variablen auswählen", choices = names(exb), multiple = TRUE)),
      shiny::fluidRow(shiny::actionButton("plotData", "Auswahl plotten")),
      shiny::fluidRow(shiny::plotOutput("Netzwerk")),
      #fluidRow(textOutput("Variablen", "Ausgewählte Variablen")),
      shiny::fluidRow(DT::dataTableOutput("clusterData")),
      shiny::fluidRow(shiny::textInput("filename", "Enter filename"),
                      shiny::downloadButton("Dcluster", "Download")),
      shiny::fluidRow(shiny::plotOutput("barplot")),
      shiny::fluidRow(DT::dataTableOutput("countVars")),
      shiny::fluidRow(shiny::actionButton("saveToVar", "Ergebnis als Variable speichern"))
      #shiny::fluidRow(shiny::actionButton("Done","Ich habe fertig"))

    ),
    server = function(input, output){
      #output$ExbData <- DT::renderDataTable(DT::datatable(exb))
      output$ExbData <-  DT::renderDataTable({DT::datatable(exb, filter= "top",selection = list(target = 'row', selected= c(seq(1:nrow(exb)))),options = list(lengthChange = TRUE, autoWidth= TRUE, scrollX= TRUE))})
      NetworkPlot <- shiny::eventReactive(input$plotData,{
        data <- exb[input$ExbData_rows_selected,]
        Dissertation::com_clust(data, variables = input$vars)})
      output$Netzwerk <- shiny::renderPlot({plot(NetworkPlot()[[2]],NetworkPlot()[[3]],vertex.size=10, vertex.label.font=20, family="serif", edge.width=igraph::E(NetworkPlot()[[3]])$weight, sub= stringr::str_c("Modularity=",modularity(NetworkPlot()[[2]])))})
      output$clusterData <- DT::renderDataTable({NetworkPlot()[[1]] %>% dplyr::select(c("IPId","File", "Name", "Text", input$vars, "group")) %>%  DT::datatable(.,extensions = "Buttons",options = list(dom = 'Bfrtip',buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')), autoWidth= TRUE, scrollX= TRUE))})
      output$Dcluster <- shiny::downloadHandler(filename = function(){paste(input$filename,".csv", sep="")}, content = function(file){write.csv(NetworkPlot()[[1]], file, row.names = FALSE)})
      output$barplot <- shiny::renderPlot({NetworkPlot()[[1]] %>% dplyr::select(input$vars, "group") %>% reshape2::melt(id.vars= "group") %>% dplyr::group_by(group, variable, value) %>% dplyr::count() %>% dplyr::mutate(pos = cumsum(n) - (0.5 * n)) %>% ggplot2::ggplot(ggplot2::aes(y=n, x= variable, fill = value)) + ggplot2::geom_bar( stat= "identity", position = "stack", color = "black") + geom_text(aes(label = value),position = position_stack(vjust= 0.5), size= 3, angle= -90)+ ggplot2::facet_grid(~ group) + theme_bw()+ scale_colour_brewer(palette = "Paired")})
      output$countVars <- DT::renderDataTable({NetworkPlot()[[1]] %>% dplyr::select(input$vars, "group") %>% reshape2::melt(id.vars= "group") %>% dplyr::group_by(group, variable, value) %>% dplyr::count()})
    # ShinyAnalyse <- shiny::eventReactive(input$saveToVar,{DT::renderDataTable({NetworkPlot()[[1]] %>% dplyr::select(c("IPId","File", "Name", "Text", input$vars, "group"))%>%  assign("ShinyAnalyse",., envir = .GlobalEnv) %>% as.data.frame()})})
      #Test <<-shiny::eventReactive(input$saveToVar,{NetworkPlot()[[1]] %>% dplyr::select(c("IPId","File", "Name", "Text", input$vars, "group")) %>% as.data.frame()}) %>% as.data.frame()
              # shiny::observeEvent(input$Done,{
      #    as.data.frame(NetworkPlot()[[1]])
      #    shiny::stopApp(as.data.frame(NetworkPlot()[[1]]))
      #  })
    }
  )
}
#
# shinyNetwork <- function(exb){
#   shinyApp(
#     ui = dashboardPage(
#       dashboardHeader(title =  "ExmaraldaR"),
#       dashboardSidebar(
#         sidebarMenu(
#           menuItem("Daten importieren und Analyse", tabName = "daten"),
#         )
#       ),
#       dashboardBody(
#         tabItems(
#           tabItem(tabName = "daten",
#                   fluidRow(dataTableOutput("ExbData")),
#                   fluidRow(selectInput("vars", "Variablen auswählen", choices = names(exb), multiple = TRUE)),
#                   fluidRow(actionButton("plotData", "Auswahl plotten")),
#                   fluidRow(plotOutput("Netzwerk")),
#                   #fluidRow(textOutput("Variablen", "Ausgewählte Variablen")),
#                   fluidRow(dataTableOutput("clusterData")),
#                   fluidRow(textInput("filename", "Enter filename"),
#                            downloadButton("Dcluster", "Download"))
#
#           )
#         )
#       )
#     ),
#
#     server <- function(input, output){
#       output$ExbData <-  renderDataTable({datatable(exb, filter= "top",selection = list(target = 'row', selected= c(seq(1:nrow(exb)))),options = list(lengthChange = TRUE, autoWidth= TRUE, scrollX= TRUE))})
#       output$download <- downloadHandler(filename = function(){paste(sub('\\.exb$', '', input$file$name),".csv", sep="")}, content = function(file){write.csv(exb, file, row.names = FALSE)})
#       output$test <- renderPrint(str(input$file))
#       NetworkPlot <- eventReactive(input$plotData,{
#         data <- exb[input$ExbData_rows_selected,]
#         com_clust(data, variables = input$vars)})
#       output$Netzwerk <- renderPlot({plot(NetworkPlot()[[2]],NetworkPlot()[[3]],vertex.size=10, vertex.label.font=20, family="serif", edge.width=E(NetworkPlot()[[3]])$weight, sub= stringr::str_c("Modularity=",modularity(NetworkPlot()[[2]])))})
#       output$clusterData <- renderDataTable({datatable(NetworkPlot()[[1]],options = list(lengthChange = TRUE, autoWidth= TRUE, scrollX= TRUE))})
#       output$Dcluster <- downloadHandler(filename = function(){paste(input$filename,".csv", sep="")}, content = function(file){write.csv(NetworkPlot()[[1]], file, row.names = FALSE)})
#
#     }
#
#   )
#
# }
