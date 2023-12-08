com_clust <- function(data,variables = c(1:ncol(data)),weight= 1){
  data[,variables] <- dplyr::mutate(data[,variables],dplyr::across(where(purrr::negate(is.factor)), as.factor))
  adjmatrix <- 1- cluster::daisy(data[,variables], metric = c("gower"), weights = 1) # ungewichteter Vergleich, ausgenommen e0, e1, da nur ein auftreten überhaupt

  x <- Matrix::as.matrix(adjmatrix) #schreibt Aehnlichkeitsmatrix in ein R Objekt

  g1 <- igraph:::graph_from_adjacency_matrix(x, mode = c("lower"), weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NA) #erstellt Graphfile aus unterer H??lfte der Aehnlichkeitsmatrix

  cfg <- igraph::cluster_louvain(g1, weights = NULL) #sucht im Graphen nach Gruppen
  p <- ggnetwork::ggnetwork(g1) %>%
    dplyr::left_join(data.frame(name=cfg$name, group=as.factor(cfg$membership)), by="name") %>%
    ggplot(aes(x=x,y=y,xend=xend,yend=yend))+
    geom_edges(aes(linewidth= weight/30, color=group),alpha=0.3)+
    geom_nodes(aes(fill=group),color="black",size=5) +
    ggnetwork::geom_nodetext(aes(label=name),size=2, fontface="bold")+
    guides(linewidth="none", fill="none")+
    theme_blank()+
    ggplot2::labs(caption=paste("modularity = ", igraph::modularity(cfg)))
    ggplotly(p, tooltip = "label")

   k <- plot(cfg, g1, vertex.size=10, vertex.label.font=20, family="serif", edge.width=E(g1)$weight, sub= stringr::str_c("Modularity=",modularity(cfg))) #Zeichnet Graphen samt Gruppierungen

  group <- as.factor(igraph:::membership(cfg))
  data <- cbind(data,group)
  return(list(data,cfg, g1,
              plot(cfg, g1, vertex.size=10, vertex.label.font=20, family="serif", edge.width=igraph::E(g1)$weight, sub= stringr::str_c("Modularity=",igraph::modularity(cfg)))))
}
