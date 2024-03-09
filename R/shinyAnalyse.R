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
      shinydashboard::menuItem("Analysieren",  icon = shiny::icon("bar-chart-o"),
                               shinydashboard::menuSubItem("umap", tabName = "umap"),
                               shinydashboard::menuSubItem("Network", tabName = "Network")
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
                                  shiny::dataTableOutput("kwic")
                                )
                              ),
                              fluidRow(
                                shinydashboard::box(
                                  width=10,
                                  div(style= "text-align: center;",
                                      actionButton("prevItem",
                                                   label="",
                                                   icon=shiny::icon("arrow-up"))
                                  ),

                                  br(),
                                  div(style="display: flex;",
                                      actionButton("prevAnnCat",
                                                   label="",
                                                   icon=shiny::icon("arrow-left")),
                                     div(style="width: 20px"),
                                      shiny::selectInput("annCat",
                                                         "Annotation",
                                                         choices = c("a","b"),
                                                         width = "80%"),
                                     div(style="width: 20px"),
                                      actionButton("nextAnnCat",
                                                   label="",
                                                   icon=shiny::icon("arrow-right"))
                                  ),
                                  br(),
                                  textInput("annValue", "Annotation Wert", value = ""),
                                  div(style= "text-align: center;",
                                    actionButton("nextItem",
                                                 label="",
                                                 icon=shiny::icon("arrow-down"))
                                  )
                                ),
                                shinydashboard::box(width = 2,
                                                    title= "Beleg anhören",
                                                    actionButton("sendPraat", "In Praat anhören", icon=shiny::icon("play")),
                                                    actionButton("clearPraat", "Praat aufräumen", icon=shiny::icon("trash"))
                                )
                              ),
                              fluidRow(
                                shinydashboard::box(
                                  width=12,
                                  title="Transkript",
                                  shiny::dataTableOutput("transcript")
                                )
                              )

      ),
      shinydashboard::tabItem(tabName="explore",
                              shiny::h2("Annotation erkunden")
      ),
      shinydashboard::tabItem(tabName="analyse",
                              shiny::h2("Analysieren")
      )
    )
  )
    ui <- shinydashboard::dashboardPage( header, sidebar, body)




  server <- function(input, output, session){}
    shiny::shinyApp(ui=ui, server=server)

}

