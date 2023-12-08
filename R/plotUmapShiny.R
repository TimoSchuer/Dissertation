plotUmapShiny <- function(kwic=data.frame(a=1:10, b=letters[1:10], c=letters[10:1])){
  ui <- shiny::fluidPage(
    shiny::titlePanel("Interactive Exploration"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
      shiny::selectInput("vars",
                         label = "Variables to consider",
                         choices=names(kwic),
                         selected=setdiff(names(kwic),
                                             c("IPId","IPNumber","EventID","File"  , "Speaker","TierID","TierCategory" ,"Start" ,"End","Start_time","End_time","Name", "Text","pathAudio", "pathFile","BelegID","pos","BelegNummer" )),
                         multiple=TRUE),
      shiny::selectInput("metric",
                         label="metric for DistMat",
                         choices = c("euclidean", "manhattan", "gower"),
                         selected = "gower"),
      shiny::sliderInput("n_neighbors",
                         label = "n_neighbors",
                         min = 0,
                         max = nrow(kwic),
                         value=10,
                         step=1),
      shiny::sliderInput("minDist",
                         label = "minDist",
                         min=0,
                         max=1,
                         step = 0.05,
                         value = 0.1),
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
                         )
    ),
    shiny::mainPanel(
      shinyBS::bsCollapse(multiple = TRUE, open = "Plot",
                          shinyBS::bsCollapsePanel("Plot",
                            plotly::plotlyOutput("plot"),
                            style = "primary"),
                          shinyBS::bsCollapsePanel("Selected Points",
                            DT::dataTableOutput("selected"),
                            style = "primary"),
                          shinyBS::bsCollapsePanel("Treeplot Selected Points",
                                                   shiny::plotOutput("VarPlotSelected"),
                                                   style = "primary")

    )
    )
    )
  )

  server <- function(input,output,session){
    distMat <- reactive({
       kwic %>%
        dplyr::select(dplyr::any_of(input$vars)) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.character),~as.factor(.x))) %>%
        cluster::daisy(metric=input$metric) %>%
        as.matrix()
    })
    Config <- umap::umap.defaults
    umapData <- reactive({
      Config$n_neighbors <- input$n_neighbors # je größer desto besser wird globale Struktur erhalten
      Config$min_dist <- input$minDist # Wert zwischen 0 und 1; je kleiner desto enger sind CLuster zusammen
      Config$input <- "dist"
      umap::umap(distMat(), Config)
    })
    cluster <- reactive({
      shiny::req(input$clusterBase)
      if(input$clusterBase=="visualisation"){
        dbscan::hdbscan(umapData()$layout, minPts = input$minPts)
      }else{
        dbscan::hdbscan(distMat(), minPts = input$minPts)
      }

    })


    output$plot <- plotly::renderPlotly({
      #key <- kwic$IPId
      dataVis <- umapData()$layout %>%
        as.data.frame() %>%
        dplyr::mutate(cluster=cluster()$cluster %>% as.factor()) %>%
        dplyr::mutate(TokenID=kwic$TokenID)
      dataVis$membershipProb <- cluster()$membership_prob
      g <- dataVis %>%
        ggplot2::ggplot(ggplot2::aes(x=V1, y=V2, color= cluster, alpha= membershipProb, key=TokenID))+
        ggplot2::geom_jitter()#ggplot2::aes(label=kwic[,input$label])
      plotly::ggplotly(g) %>%
        plotly::event_register("plotly_click") %>%
        plotly::event_register("plotly_doubleclick") %>%
        plotly::event_register("plotly_selected")
    })
    points <- reactiveVal()

    # On hover, the key field of the event data contains the car name
    # Add that name to the set of all "selected" cars
    observeEvent(plotly::event_data("plotly_click"), {
      points <- plotly::event_data("plotly_click")$key
      points_old_new <- c(points(), points)
      points(unique(points_old_new))
    })
    observeEvent(plotly::event_data("plotly_selected"), {
      points <- plotly::event_data("plotly_selected")$key
      points_old_new <- c(points(), points)
      points(unique(points_old_new))
    })

    # clear the set of cars when a double-click occurs
    observeEvent(plotly::event_data("plotly_doubleclick"), {
      points(NULL)
    })

    output$selected <- DT::renderDataTable({

      shiny::req(points)
      kwic %>%
        dplyr::mutate(cluster=cluster()$cluster, membershipProp=cluster()$membership_prob) %>%
        dplyr::filter(TokenID %in% points()) %>%
        dplyr::select(dplyr::any_of(c("Name","Text","cluster","membershipProp", input$vars)))
    })
    output$VarPlotSelected <- shiny::renderPlot({
      shiny::req(points)
      kwic %>%
        dplyr::filter(TokenID %in% points()) %>%
        dplyr::select(dplyr::any_of(input$vars)) %>%
        tidyr::pivot_longer(cols= dplyr::everything(), names_to = "var") %>%
        dplyr::summarise(n=n(), .by = c(var,value)) %>%
        treemap::treemap(index = c("var","value"),
                         vSize = "n",
                         type = "index")
    })

  }
  shiny::shinyApp(ui=ui,server = server)
}

