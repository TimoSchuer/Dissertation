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
  header <-      shinydashboard::dashboardHeader(title="shinyAnalyse")
  sidebar <-         shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Annotieren", tabName = "annotate", icon = shiny::icon("pencil")),
      shinydashboard::menuItem("Annotation erkunden", tabName = "explore", icon = shiny::icon("search")),
      shinydashboard::menuItem("Analysieren",  icon = shiny::icon("chart-simple"),
                               shinydashboard::menuSubItem("umap", tabName = "umap"),
                               shinydashboard::menuSubItem("Network", tabName = "network", icon= shiny::icon("circle-nodes"))
      )
    )
  )
  body <-     shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName="annotate",
                              shiny::h2("Annotieren"),
                              fluidRow(
                                shinydashboard::box(
                                  width=12,
                                  title="Beleg",
                                  DT::dataTableOutput("kwic")
                                )
                              ),
                              fluidRow(
                                annotateUiDash("annotate", width=8),
                                shinydashboard::box(width = 2,
                                                    title= "Beleg anhören",
                                                    actionButton("sendPraat", "In Praat anhören", icon=shiny::icon("play")),
                                                    actionButton("clearPraat", "Praat aufräumen", icon=shiny::icon("trash"))
                                ),
                                shinydashboard::valueBoxOutput("item")
                              ),
                              fluidRow(
                                shinydashboard::box(
                                  width=12,
                                  title="Transkript",
                                  DT::dataTableOutput("transcript")
                                )
                              )

      ),
      shinydashboard::tabItem(tabName="explore",
                              shiny::h2("Annotation erkunden")
      ),
      shinydashboard::tabItem(tabName="umap",
                              shiny::h2("Analysieren umap"),
                              fluidRow(
                                shinydashboard::box(
                                  width=4,
                                  title="Select Items and Variables",
                                  shiny::selectInput("umapItems",
                                                     "Exclude Items",
                                                     choices = 1:10,
                                                     selected = FALSE,
                                                     multiple = TRUE),
                                  shiny::selectInput("umapVars",
                                                     "Select Variables to consider",
                                                     choices = c("a","b"),
                                                     selected = c("a","b"),
                                                     multiple =  TRUE)
                                ),
                                shinydashboard::box(
                                  width=4,
                                  title="umap parameter",
                                  shiny::selectInput("metric",
                                                     label="metric for DistMat",
                                                     choices = c("euclidean", "manhattan", "gower"),
                                                     selected = "gower"),
                                  shiny::sliderInput("n_neighbors",
                                                     label = "n_neighbors",
                                                     min = 2,
                                                     max = 10,#nrow(kwic)
                                                     value=10,
                                                     step=1),
                                  shiny::sliderInput("minDist",
                                                     label = "minDist",
                                                     min=0.01,
                                                     max=0.99,
                                                     step = 0.05,
                                                     value = 0.1)
                                ),
                                column(4,
                                       shinydashboard::box(
                                         width=NULL,
                                         title="Controls Cluster",
                                         shiny::sliderInput("minPts",
                                                            label="minPts for clustering",
                                                            min = 0,
                                                            max = 15,#nrow(kwic),
                                                            value=10,
                                                            step=1),
                                         shiny::selectInput("clusterBase",
                                                            label = "base clustering on Vis oder DistMat",
                                                            choices = c("visualisation","distMat"),
                                                            selected = "Visualisation")
                                       ),
                                       shinydashboard::box(
                                         width=NULL,
                                         title="umap",
                                         shiny::actionButton("saveUmapRDS",
                                                             "Save umap to RDS",
                                                             icon=shiny::icon("save")),
                                         shiny::actionButton("saveUmapEnviroment",
                                                             "Save umap to Enviroment",
                                                             icon=shiny::icon("save")),
                                       )
                                )
                              ),
                              fluidRow(
                                shinydashboard::box(
                                  width=12,
                                  title="umap",
                                  shiny::plotOutput("umap")
                                )
                              )


      ),
      shinydashboard::tabItem(tabName="network",
                              shiny::h2("Analysieren Network")
      )
    )
  )
    ui <- shinydashboard::dashboardPage( header, sidebar, body)




  server <- function(input, output, session){
    kwic <- kwic %>% dplyr::mutate(across(where(is.factor), as.character))
    Annotation <- annotateServer("annotate", kwic)
    output$item <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = paste(Annotation$item(), "von", nrow(kwic)),
        subtitle = "Fortschritt",
        icon = shiny::icon("list")
      )
    })
    observeEvent(Annotation$item(),{
      output$kwic <- DT::renderDataTable({
        kwic[Annotation$item(),]
      }, options = list(scrollX = TRUE))
      output$transcript <- DT::renderDataTable({
        Dissertation::showTranscript(kwic,
                                     corpus = CorpusIP,
                                     rowNumber = Annotation$item(),
                                     tier= "beides")
      }, options = list(scrollX = TRUE,
                        pageLength = 50))
    })
    observeEvent(Annotation$data(),{
      kwic <- kwic %>% dplyr::rows_update(Annotation$data(),
                                          by = "TokenID")
      output$kwic <- DT::renderDataTable({
        kwic[Annotation$item(),]
      }, options = list(scrollX = TRUE))
    })
  }
    shiny::shinyApp(ui=ui, server=server)

}

