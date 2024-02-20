#' Title
#'
#' @param kwic concordanz
#' @param corpus corpus by ip
#' @param objectName name of object tp save to
#' @param pathPraat path where praat is saved
#' @param pathSave path to save rds
#' @param seed seed for random things to be repeatable
#' @param AnnValue data.frame with values to annotate in dropdown menu
#'
#' @return
#' @export
#'
#' @examples
#'
#'
shinyAnalyse <- function(kwic=data.frame(IPId=paste("a",1:10, sep=""),a=1:10,b=10:1),
                         corpus = data.frame(IPId=paste("a",1:30, sep=""),a=1:30,b=30:1),
                         AnnValues = NULL,
                         objectName=paste(deparse(substitute(kwic)),"_ann",sep=""),
                         pathPraat="../Praat",
                         pathSave=getwd(),
                         seed=7){
if(!is.na(seed)){
  set.seed(seed)
}
  dataInit <-  kwic %>%
    dplyr::mutate(across(where(is.character), ~as.factor(.x))) %>%
    addDropdown(AnnValues)
# ui ----------------------------------------------------------------------
  ui <- shiny::fluidPage(
    shiny::fluidRow(
      tags$script(
        HTML("$(document).on(
      'shiny:inputchanged',
      function(event) {
       console.log(event);
    });"
        )),
      shiny::column(width = 2, offset = 10,
                    shiny::actionButton("quit","Quit")
      )
    ),
    shiny::fluidRow(
      shiny::tabsetPanel(

        shiny::tabPanel("Annotate Data",
                        # shiny::fluidRow(
                        #   shiny::column(2,
                        #                 shiny::actionButton("quit","Quit"),
                        #                 offset = 10
                        #   )
                        # ),
                        #   mainPanel(
                        shinyBS::bsCollapse(multiple = TRUE, open="Annotate",
                                            shinyBS::bsCollapsePanel("Annotate",
                                            shiny::fluidRow(
                                              shiny::column(width=11,offset = 0,
                                                            DT::dataTableOutput("data")
                                              ),
                                              shiny::column(width = 1,
                                                            shiny::verbatimTextOutput("ann"))
                                            ),
                                            #shiny::actionButton("start","Start Praat"),
                                            shiny::fluidRow(shiny::selectInput("tiers", "Select Tiers to be shown",
                                                                               choices = c("Transkript", "Standard","beides"), selected = "beides")  ,
                                                            shiny::numericInput("lengthTranscript", "Length of Transcript",min = 10,max = 50, value = 10
                                                                               ),
                                                            shiny::actionButton("send","Send to Praat"),
                                                            shiny::actionButton("clear","Clear Praat")),
                                            shiny::fluidRow(
                                              DT::dataTableOutput("transcript")),style = "primary"),
                                            shinyBS::bsCollapsePanel("Explore",
                                                                     shiny::fluidRow(
                                                                       shiny::selectInput("VarsExploreData",
                                                                                          label="Variables",
                                                                                          choices = names(kwic),selected=setdiff(names(kwic),
                                                                                                                                 c("IPId","IPNumber","EventID","File"  , "Speaker","TierID","TierCategory" ,"Start" ,"End","Start_time","End_time","Name", "Text","pathAudio", "pathFile","BelegID","pos","BelegNummer" )),
                                                                                          multiple=TRUE
                                                                                          ),
                                                                       shiny::selectInput("perc",
                                                                                          label="Percent or absolute",
                                                                                          choices = c("absolute", "perc"),
                                                                                          selected = "absolute")),
                                                                     shiny::fluidRow(
                                                                       shiny::plotOutput("ExploreData")

                                                                     ),style = "primary")
                                            )),


        shiny::tabPanel("Analyse",
                        shinyBS::bsCollapse(multiple = TRUE, open = "Plot",
                                            shinyBS::bsCollapsePanel("Plot",
                                                                     shiny::fluidRow(
                                                                       shiny::column(width = 3,
                                                                                     shiny::selectInput("vars",
                                                                                                        label = "Variables to consider",
                                                                                                        choices=names(kwic),
                                                                                                        selected=setdiff(names(kwic),
                                                                                                                         c("IPId","IPNumber","EventID","File"  , "Speaker","TierID","TierCategory" ,"Start" ,"End","Start_time","End_time","Name", "Text","pathAudio", "pathFile","BelegID","pos","BelegNummer" )),
                                                                                                        multiple=TRUE)),
                                                                       shiny::column(width = 3,
                                                                                     shiny::selectInput("metric",
                                                                                                        label="metric for DistMat",
                                                                                                        choices = c("euclidean", "manhattan", "gower"),
                                                                                                        selected = "gower"),
                                                                                     shiny::selectizeInput("Vis",
                                                                                                           label="Visualisation",
                                                                                                           choices = c("umap","Network"),
                                                                                                           selected = "umap")),
                                                                       shiny::column(width = 3,
                                                                                     shiny::h4("Controls for umap Visualisation"),
                                                                                     shiny::sliderInput("n_neighbors",
                                                                                                        label = "n_neighbors",
                                                                                                        min = 2,
                                                                                                        max = nrow(kwic),
                                                                                                        value=10,
                                                                                                        step=1),
                                                                                     shiny::sliderInput("minDist",
                                                                                                        label = "minDist",
                                                                                                        min=0.01,
                                                                                                        max=0.99,
                                                                                                        step = 0.05,
                                                                                                        value = 0.1)),
                                                                       shiny::column(width = 3,
                                                                                     shiny::sliderInput("minPts",
                                                                                                        label="minPts for clustering",
                                                                                                        min = 0,
                                                                                                        max = nrow(kwic),
                                                                                                        value=10,
                                                                                                        step=1),
                                                                                     shiny::selectInput("clusterBase",
                                                                                                        label = "base clustering on Vis oder DistMat",
                                                                                                        choices = c("visualisation","distMat"),
                                                                                                        selected = "Visualisation"
                                                                                     ),
                                                                                     shiny::actionButton("Export",
                                                                                                         label="Export to enviroment"),
                                                                                     # shiny::actionButton("SaveExcel",
                                                                                     #                     label = "Save Excel"
                                                                                     # ),
                                                                                     shiny::actionButton("SaveRDS",
                                                                                                         label="Save RDS")
                                                                                     )
                                                                     ),
                                                                     shiny::fluidRow(
                                                                       plotly::plotlyOutput("plot", width = "auto", height = "500px")),
                                                                     style = "primary"),
                                            shinyBS::bsCollapsePanel("Selected Points",
                                                                     DT::dataTableOutput("selected"),
                                                                     DT::dataTableOutput("transcriptSelected"),
                                                                     style = "primary"),
                                            shinyBS::bsCollapsePanel("Treeplot Selected Points",
                                                                     shiny::plotOutput("VarPlotSelected"),
                                                                     style = "primary"),
                                            shinyBS::bsCollapsePanel("Explore Cluster",
                                                                     shiny::selectizeInput("ClusterExpl",
                                                                                           label="Choose Cluster to explore",
                                                                                           choices=1:10,
                                                                                           selected=1),
                                                                     shiny::selectInput("VisCluster",
                                                                                        label="Choose Visualisation",
                                                                                        choices = c("treemap","barplots"),
                                                                                        selected = "barplots"),
                                                                     shiny::plotOutput("plotCluster"),
                                                                     DT::dataTableOutput("tableCluster"),
                                                                     DT::dataTableOutput("transcriptCluster"),
                                                                     style = "primary"),
                                            shinyBS::bsCollapsePanel("Explore Variables",
                                                                     shiny::selectizeInput("ChooseVarExpl",
                                                                                           label="Choose Variable to explore",
                                                                                           choices=names(kwic),
                                                                                           selected=names(kwic)[1]),
                                                                     shiny::plotOutput("plotVars"),
                                                                     DT::dataTableOutput("tableVars"),
                                                                     style = "primary")

                        )
        ),

      )))
  server <- function(input, output,session){
    result <- kwic
  selectInputIDs <- intersect(names(kwic), names(AnnValues)) %>%
    sapply(paste0,"_", seq(1:nrow(kwic)), sep="")
# server annotate ---------------------------------------------------------
    if(!is.null(AnnValues)){
      hiddenCols <-which(names(kwic) %in% c("IPNumber" ,"EventID" ,"File","Speaker" ,"TierID", "TierCategory" ,"Start" ,"End" ,"Start_time",   "End_time", "pathAudio", "pathFile"))-1

      dataDisplay <- shiny::reactive({
        kwic %>%
          dplyr::mutate(across(where(is.character), ~as.factor(.x))) %>%
          addDropdown(AnnValues)
      })
      dataResult <- reactive({
        for(Var in names(AnnValues)){
          values <- sapply(selectInputIDs[which(stringr::str_starts(selectInputIDs,Var))], function(x){input[[x]]})
          result[[Var]] <- values
        }
        return(result)
      })
      output$data <- DT::renderDataTable(dataInit,editable=TRUE,
                                         filter="top",
                                         rownames=FALSE,
                                         extensions="Buttons",
                                         plugins="input",
                                         selection= list(mode="single"),
                                         width= "100%",
                                         escape = FALSE,
                                         options = list(
                                           paging = TRUE,
                                           pagingType="input",
                                           dom = 'Blfrtip',
                                           lengthMenu = list(c(1,5, 15, -1), c('1','5', '15', 'All')),
                                           pageLength=1,
                                           searching = TRUE,
                                           fixedColumns = FALSE,
                                           autoFill=TRUE,
                                           # autoWidth = FALSE,
                                           ordering = FALSE,
                                           scrollX = TRUE,
                                           buttons = list(
                                             list(extend='colvisGroup', text="Show", show=hiddenCols),
                                             list(extend='colvisGroup', text="Hide", hide=hiddenCols),
                                             "colvis"
                                           ),
                                           autoWidth = TRUE,
                                           columnDefs = list(list(width = '100px', targets = "_all"),
                                                             list(targets = hiddenCols, visible = FALSE))),
                                         class = 'cell-border stripe',
                                         preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                         drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
      proxy <-  DT::dataTableProxy(outputId = "data", session = session)
      shiny::observeEvent(input$data_cell_edit,{
        data <<- DT::editData(data,proxy = proxy,input$data_cell_edit,resetPaging = FALSE, rownames = FALSE)
        assign(objectName, data, envir = .GlobalEnv)

        # renderDataTable(data)
      })
      observeEvent({sapply(selectInputIDs, function(x){input[[x]]})},
                   {
                     DT::replaceData(proxy=proxy, data = dataDisplay(), rownames=FALSE)
                   }, ignoreInit = TRUE)

    } else{
    hiddenCols <-which(names(kwic) %in% c("IPNumber" ,"EventID" ,"File","Speaker" ,"TierID", "TierCategory" ,"Start" ,"End" ,"Start_time",   "End_time", "pathAudio", "pathFile"))-1
    data <- kwic %>% dplyr::mutate(across(where(is.character), ~as.factor(.x)))

    output$data <- DT::renderDataTable(data ,
                                       editable=TRUE,
                                       filter="top",
                                       rownames=FALSE,
                                       extensions="Buttons",
                                       plugins="input",
                                       selection= list(mode="single",target="row+column"),
                                       width= "100%",
                                       options = list(
                                         paging = TRUE,
                                         pagingType="input",
                                         dom = 'Blfrtip',
                                         lengthMenu = list(c(1,5, 15, -1), c('1','5', '15', 'All')),
                                         pageLength=1,
                                         searching = TRUE,
                                         fixedColumns = FALSE,
                                         autoFill=TRUE,
                                         # autoWidth = FALSE,
                                         ordering = FALSE,
                                         scrollX = TRUE,
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
      data <<- DT::editData(data,proxy = proxy,input$data_cell_edit,resetPaging = FALSE, rownames = FALSE)
      assign(objectName, data, envir = .GlobalEnv)

      # renderDataTable(data)
    })
    }

    # observeEvent(input$start,{
    #   system("start C:/Users/Admin/sciebo/Diss/Praat/praat.exe", intern = TRUE)
    # })

    shiny::observeEvent(input$send,{

      s <- input$data_rows_current
      if(length(s)==1){
        sendPraat(data=data, rowNumber = s, pathPraat = pathPraat)
      }else if(length(s)>1){
        r <- input$data_rows_selected
        if(length(r)==1){
          sendPraat(data=data, rowNumber = r, pathPraat = pathPraat)
        }
      }

    })
    shiny::observeEvent(input$data_rows_current,{
      s <- input$data_rows_current
      if(length(s)==1){
        transcript <- Dissertation::showTranscript(data=data,corpus=corpus, rowNumber = s,tier = input$tiers)

      output$transcript <- DT::renderDataTable(transcript
                                               ,selection= list(selected=11),
                                               options=list(
                                                 paging=TRUE,
                                                 searching=FALSE,
                                                 pageLength=25))
      }
    })
    shiny::observeEvent(input$data_rows_selected,{
      r <- input$data_rows_selected
      if(length(r)==1){
        transcript <- Dissertation::showTranscript(data=data,corpus=corpus, rowNumber = r,tier = input$tiers)
        output$transcript <- DT::renderDataTable(transcript
                                               ,selection= list(selected=11),
                                               options=list(
                                                 paging=TRUE,
                                                 searching=FALSE,
                                                 pageLength=25))
      }
    })
    shiny::observeEvent(input$tiers,{
      s <- input$data_rows_current
      if(length(s)==1){
        transcript <- Dissertation::showTranscript(data=data,corpus=corpus, rowNumber = s,tier = input$tiers)
      }else if(length(s)>1){
        r <- input$data_rows_selected
        if(length(r)==1){
          transcript <- Dissertation::showTranscript(data=data,corpus=corpus, rowNumber = r,tier = input$tiers)
        }
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
      paste0('C:/Users/Admin/sciebo/Diss/Praat/sendpraat-win.exe praat ','"select all"',' "Remove"') %>% system()
    })
    output$ExploreData <- shiny::renderPlot({
      if(input$perc=="absolute"){
      data %>%
        select(dplyr::any_of(input$VarsExploreData)) %>%
        tidyr::pivot_longer(cols=dplyr::everything(), names_to = "vars") %>%
        dplyr::summarise(n=n(),.by = c(vars,value)) %>%
        dplyr::mutate(perc=n/sum(n)) %>%
        ggplot2::ggplot(aes(y=value,x=n))+
        ggplot2::geom_col()+
        ggplot2::facet_wrap(facets = ggplot2::vars(vars), scales = "free_y")
      }else{
        data %>%
          select(dplyr::any_of(input$VarsExploreData)) %>%
          tidyr::pivot_longer(cols=dplyr::everything(), names_to = "vars") %>%
          dplyr::summarise(n=n(),.by = c(vars,value)) %>%
          dplyr::mutate(perc=n/sum(n)) %>%
          ggplot2::ggplot(aes(y=value,x=perc))+
          ggplot2::geom_col()+
          ggplot2::facet_wrap(facets = ggplot2::vars(vars), scales = "free_y")
      }
    })

# server analyse ----------------------------------------------------------
    shiny:::observeEvent(cluster(),{
      if("membership_prob" %in% names(cluster())){
        choices <- cluster()$cluster %>% unique() %>% sort()
      }else{
        choices <- cluster()$membership %>% unique()  %>% sort(decreasing = FALSE)
      }
      shiny::updateSelectizeInput(inputId = "ClusterExpl",choices = choices)
    })
    shiny::observeEvent(input$vars,{
      shiny::updateSelectizeInput(inputId =  "ChooseVarExpl", choices = input$vars)
    })
    distMat <- shiny::reactive({
      data %>%
        dplyr::select(dplyr::any_of(input$vars)) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.character),~as.factor(.x))) %>%
        cluster::daisy(metric=input$metric) %>%
        as.matrix()
    })

    umapData <- shiny::reactive({
      #if(input$Vis=="umap"){
      Config <- umap::umap.defaults
      Config$n_neighbors <- input$n_neighbors # je größer desto besser wird globale Struktur erhalten
      Config$min_dist <- input$minDist # Wert zwischen 0 und 1; je kleiner desto enger sind CLuster zusammen
      Config$input <- "dist"
      umap::umap(distMat(), Config)
     # }
    })
    cluster <- shiny::reactive({
      if(input$Vis=="umap"){
        shiny::req(input$clusterBase)

        if(input$clusterBase=="visualisation"){
          dbscan::hdbscan(umapData()$layout, minPts = input$minPts)
        }else{
          dbscan::hdbscan(distMat(), minPts = input$minPts)
        }



      }else if(input$Vis=="Network"){
        igraph:::graph_from_adjacency_matrix(distMat(),
                                                        mode = c("lower"),
                                                        weighted = TRUE,
                                                        diag = FALSE,
                                                        add.colnames = NULL,
                                                        add.rownames = NA) %>%
          igraph::cluster_louvain( weights = NULL)


      }
    })

# output plot -------------------------------------------------------------

    output$plot <- plotly::renderPlotly({
      shiny::req(input$Vis)
      if(input$Vis=="umap"){

        dataVis <- umapData()$layout %>%
          as.data.frame() %>%
          dplyr::mutate(cluster=cluster()$cluster %>% as.factor()) %>%
          dplyr::mutate(membershipProb= cluster()$membership_prob %>% as.numeric()) %>%
          dplyr::mutate(TokenID=data$TokenID)
       # dataVis$membershipProb <- cluster()$membershipProp
          g <- dataVis %>%
            ggplot2::ggplot(ggplot2::aes(x=V1,
                                         y=V2,
                                         color= cluster,
                                         alpha= membershipProb,
                                         key=TokenID))+
            ggplot2::geom_jitter()#ggplot2::aes(label=kwic[,input$label])
          plotly::ggplotly(g, source = "plot") %>%
            plotly::event_register("plotly_click") %>%
            plotly::event_register("plotly_doubleclick") %>%
            plotly::event_register("plotly_selected")
      }else if(input$Vis=="Network"){
        g1 <- igraph:::graph_from_adjacency_matrix(distMat(), mode = c("lower"), weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NA) #erstellt Graphfile aus unterer H??lfte der Aehnlichkeitsmatrix
        modularity <-g1 %>%
          igraph::cluster_louvain( weights = NULL) %>%
          igraph::modularity()
        p <- ggnetwork::ggnetwork(g1) %>%
          dplyr::left_join(data.frame(name=cluster()$name, group=as.factor(cluster()$membership), TokenID= as.factor(data$TokenID)), by="name") %>%
          ggplot2::ggplot(aes(x=x,y=y,xend=xend,yend=yend,key=TokenID))+
          ggnetwork::geom_edges(ggplot2::aes(linewidth= weight/30, color=group),alpha=0.3)+
          ggnetwork::geom_nodes(ggplot2::aes(fill=group),color="black",size=5) +
          ggnetwork::geom_nodetext(ggplot2::aes(label=name),size=2, fontface="bold")+
          ggplot2::guides(linewidth="none", fill="none")+
          ggnetwork::theme_blank()+
          ggplot2::annotate(geom="text",y=0,x=0, label=paste("modularity = ", modularity))
        plotly::ggplotly(p, tooltip = "key",source="plot") %>%
          plotly::event_register("plotly_click") %>%
          plotly::event_register("plotly_doubleclick") %>%
          plotly::event_register("plotly_selected")
      }
    })
    points <- shiny::reactiveVal()
    points <- shiny::reactive({
      plotly::event_data("plotly_selected",source = "plot",priority = "event")$key
    })
    # points <- shiny::reactive({
    #   plotly::event_data("plotly_click", source = "plot", priority = "event")$key %>%
    #     c(shiny::isolate(points())) %>%
    #     unique()
    # })
    shiny::observeEvent(plotly::event_data("plotly_doubleclick", source = "plot", priority = "event"),{
      points(NULL)
    })

# output$selected ---------------------------------------------------------


    output$selected <- DT::renderDataTable({
      shiny::req(points)
      if("membership_prob" %in% names(cluster())){
        data$membershipProb <- cluster()$membership_prob
        data %>%
          dplyr::mutate(cluster=cluster()$cluster) %>%
          dplyr::filter(TokenID %in% points()) %>%
          dplyr::select(dplyr::any_of(c("BelegNummer","Name","Text","cluster","membershipProb", input$vars)))
      }else{
        data %>%
          dplyr::mutate(cluster= cluster()$membership) %>%
          dplyr::filter(TokenID %in% points()) %>%
          dplyr::select(dplyr::any_of(c("BelegNummer", "Name","Text","cluster", input$vars)))
      }


    }, options= list(scrollX = TRUE,
                     dom = 'Blfrtip'))

# output VarPlotSelected --------------------------------------------------


    output$VarPlotSelected <- shiny::renderPlot({
      shiny::req(points)
      data %>%
        dplyr::filter(TokenID %in% points()) %>%
        dplyr::select(dplyr::any_of(input$vars)) %>%
        tidyr::pivot_longer(cols= dplyr::everything(), names_to = "var") %>%
        dplyr::summarise(n=n(), .by = c(var,value)) %>%
        treemap::treemap(index = c("var","value"),
                         vSize = "n",
                         type = "index")
    })

# output tableCluster -----------------------------------------------------


    output$tableCluster <- DT::renderDataTable({
      if("membership_prob" %in% names(cluster())){
        data %>%
          dplyr::mutate(cluster=cluster()$cluster) %>%
        dplyr::filter(cluster==input$ClusterExpl) %>%
          dplyr::select(dplyr::any_of(unique( c("BelegNummer","File","Name","Text","TokenID", input$vars))))
      }else{
        data %>%
          dplyr::mutate(cluster=cluster()$membership) %>%
          dplyr::filter(cluster==input$ClusterExpl) %>%
          dplyr::select(dplyr::any_of(unique( c("BelegNummer","File","Name","Text","TokenID", input$vars))))
      }

    }, options= list(scrollX = TRUE,
                     dom = 'Blfrtip'))

# output plotCluster ------------------------------------------------------

  output$plotCluster <- shiny::renderPlot({
    if(input$VisCluster=="barplots"){
      if("membership_prob" %in% names(cluster())){
        data %>%
          dplyr::mutate(cluster=cluster()$cluster) %>%
          dplyr::filter(cluster==input$ClusterExpl) %>%
          dplyr::select(dplyr::any_of( input$vars))%>%
          tidyr::pivot_longer(cols=dplyr::everything(), names_to = "var") %>%
          dplyr::summarise(n=n(), .by = c(var,value)) %>%
          ggplot(aes(y=value,x=n))+
          ggplot2::geom_col()+
          ggplot2::facet_wrap(facets = ggplot2::vars(var), scales = "free_y")

      }else{
        data %>%
          dplyr::mutate(cluster=cluster()$membership) %>%
          dplyr::filter(cluster==input$ClusterExpl) %>%
          dplyr::select(dplyr::any_of( input$vars))%>%
          tidyr::pivot_longer(cols=dplyr::everything(), names_to = "var") %>%
          dplyr::summarise(n=n(), .by = c(var,value)) %>%
          ggplot(aes(y=value,x=n))+
          ggplot2::geom_col()+
          ggplot2::facet_wrap(facets = ggplot2::vars(var), scales = "free_y")      }

    }else if(input$VisCluster=="treemap"){
      if("membership_prob" %in% names(cluster())){
        data %>%
          dplyr::mutate(cluster=cluster()$cluster) %>%
          dplyr::filter(cluster==input$ClusterExpl) %>%
          dplyr::select(dplyr::any_of( input$vars))%>%
          tidyr::pivot_longer(cols=dplyr::everything(), names_to = "var") %>%
          dplyr::summarise(n=n(), .by = c(var,value))  %>%
          treemap::treemap(index = c("var","value"),
                         vSize = "n",
                         type = "index")
      }else{
        data %>%
          dplyr::mutate(cluster=cluster()$membership) %>%
          dplyr::filter(cluster==input$ClusterExpl) %>%
          dplyr::select(dplyr::any_of( input$vars))%>%
          tidyr::pivot_longer(cols=dplyr::everything(), names_to = "var") %>%
          dplyr::summarise(n=n(), .by = c(var,value))  %>%
          treemap::treemap(index = c("var","value"),
                           vSize = "n",
                           type = "index")
      }
    }
  })

# output transcript cluster -----------------------------------------------

  output$transcriptCluster <- shiny::reactive({
    row <- input$tableCluster_selected_rows
    if(length(row)){
      transcript <- Dissertation::showTranscript(data = data,
                                                 corpus = corpus,
                                                 rowNumber = row,
                                                 tier = "beides"
                                                 )

      DT::renderDataTable(transcript,
                        selection= list(selected=11),
                        options=list(
                          paging=TRUE,
                          searching=FALSE,
                          pageLength=25))
      }
    }
  )

# output transcriptSelected ------------------------------------------------
    output$transcriptSelected <- shiny::reactive({
      row <- input$selected_selected_rows
      if(length(row)){
        transcript <- Dissertation::showTranscript(data = data,
                                                   corpus = corpus,
                                                   rowNumber = row,
                                                   tier = "beides"
        )
      }
      DT::renderDataTable(transcript,
                          selection= list(selected=11),
                          options=list(
                            paging=TRUE,
                            searching=FALSE,
                            pageLength=25))
    }
    )


# output plotVars ---------------------------------------------------------
    output$plotVars <- shiny::renderPlot({
      if("membership_prob" %in% names(cluster())){
        data %>%
          dplyr::mutate(cluster=cluster()$cluster) %>%
        ggplot2::ggplot(ggplot2::aes(x=.data[[input$ChooseVarExpl]])) +
          ggplot2::geom_bar()+
          ggplot2::facet_wrap(facets = ggplot2::vars(cluster), scales = "free_x")
      }else{
        data %>%
          dplyr::mutate(cluster=cluster()$membership) %>%
          ggplot2::ggplot(ggplot2::aes(x=.data[[input$ChooseVarExpl]])) +
          ggplot2::geom_bar()+
          ggplot2::facet_wrap(facets = ggplot2::vars(cluster), scales = "free_x")
      }
    })

# output tableVars --------------------------------------------------------

    output$tableVars <- DT::renderDataTable({

      if("membership_prob" %in% names(cluster())){
        data %>%
          dplyr::mutate(cluster=cluster()$cluster) %>%
          dplyr::group_by(cluster, .data[[input$ChooseVarExpl]]) %>%
          dplyr::count()
      }else{
        data %>%
          dplyr::mutate(cluster=cluster()$membership) %>%
          dplyr::group_by(cluster, .data[[input$ChooseVarExpl]]) %>%
          dplyr::count()
      }
    }, options= list(scrollX = TRUE,
                     dom = 'Blfrtip'))

# save --------------------------------------------------------------------


  result <- reactive({
    if(input$Vis=="umap"){
      list(data=data %>% as.data.frame(),
           umap=umapData(),
           cluster= cluster()
           ,vars= input$vars,
           metric= input$metric)
    }else{
      list(data=data %>% as.data.frame(),
           cluster= cluster(),
           vars=input$vars,
           metric=input$metric)
    }

  })
  shiny::observeEvent(input$Export,{
    assign("result",result(),envir = .GlobalEnv )
  })
  # shiny::observeEvent(input$saveExcel,{
  #   if(input$Vis=="umap"){
  #
  #   }else{
  #
  #   }
  #
  # })
  shiny::observeEvent(input$SaveRDS,{
    readr::write_rds(result(), file = paste(pathSave,"\\",Sys.Date(),"_result.rds",sep=""))
  })

  }
  shiny::shinyApp(ui=ui,server = server)
}
