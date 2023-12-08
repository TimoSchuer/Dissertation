#' plotUmap
#'
#'nutzt hdbscan um dichtebereiche zu finden und plottet distanzmatrix mit umap
#'
#' @param kwic daten, muuss keine kwic sein, kann auch andere daten
#' @param vars spaltennamen mit variablen die zur ähnlichkeitsberechnung herangezogen werden sollen
#' @param metric metric für die distanzmatrix, standard ist gower
#' @param n_neighbors Anzahl, der direkten Nachbarn eines Punkts, die in niedrieger Dimension erhalten bleiben sollen;
#' je größer desto besser wird globale Struktur erhalten, je kleiner desto besser lokale Struktur
#' @param min_dist je kleiner desto enger die cluster
#' @param minPts minPts pro Cluster
#' @param interactive soll der plot interaktiv rauskommen
#' @param clusterBase soll auf der Visualisierung mit Umap oder auf der Distanzmatix geclustert werden
#' @param label label das angezeigt wird wenn man auf einen Punkt geht
#'
#' @return plot
#' @export
#'

plotUMAP <- function(kwic=data.frame(a=1:10, b=letters[1:10], c=letters[10:1]),
                     vars=setdiff(names(kwic), c("IPId","IPNumber","EventID","File"  , "Speaker","TierID","TierCategory" ,"Start" ,"End","Start_time","End_time","Name", "Text","pathAudio", "pathFile","BelegID","pos","BelegNummer" )),
                     metric="gower",
                     n_neighbors=50,
                     min_dist=0.01,
                     minPts=50,
                     interactive=TRUE,
                     clusterBase=c("visualization","distMat"),
                     label="IPId"){
  Config <- umap::umap.defaults
  Config$n_neighbors <- n_neighbors # je größer desto besser wird globale Struktur erhalten
  Config$min_dist <- min_dist # Wert zwischen 0 und 1; je kleiner desto enger sind CLuster zusammen
  Config$input <- "dist"

  distMat <- kwic %>%
    dplyr::select(dplyr::any_of(vars)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character),~as.factor(.x))) %>%
    cluster::daisy(metric=metric) %>%
    as.matrix()
  umapData <- umap::umap(distMat, config = Config)
  if(clusterBase %>% length()==2){
    cluster <- dbscan::hdbscan(umapData$layout, minPts = minPts)
  }else if(clusterBase=="visualisation"){
    cluster <- dbscan::hdbscan(umapData$layout, minPts = minPts)
  }else if(clusterBase=="distMat"){
    cluster <- cluster::hdbscan(distMat, minPts = minPts)
  }else{
    return(cat("Unzulässiger Wert für clusterBase"))
  }
  dataVis <- umapData$layout %>% as.data.frame() %>% dplyr::mutate(cluster=cluster$cluster %>% as.factor())
  dataVis$membershipProb <- cluster$membership_prob
  g <- dataVis %>% ggplot2::ggplot(ggplot2::aes(x=V1, y=V2, color= cluster, alpha= membershipProb))+ ggplot2::geom_jitter(ggplot2::aes(label=kwic[,label]))
  if(interactive==TRUE){
  g <-   plotly::ggplotly(g) # interaktive Version
  }
return(g)

    }
