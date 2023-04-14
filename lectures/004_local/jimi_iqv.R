iqv <- function(graph, attribute) {
  N <- length(V(graph))
  cats <- unique(get.vertex.attribute(graph,attribute,V(graph)))
  nlev <- length(cats)
  cat_list <- rep(0,N)
  p <- rep(0, N) 
  p2_list <- as.list(0)
  for (j in 1:nlev) {
    for(i in 1:length(V(graph))){
      i_att <- get.vertex.attribute(graph, attribute, V(graph)[neighborhood(graph,1)[[i]]]) 
      att <- length(which(i_att==cats[j]))
      num <- length(V(graph)[neighborhood(graph, 1)[[i]]])
      p[i]<-att/num
      p2<-p*p
    }
    p2_list[[j]] <- p2
    cat_list <- cat_list + p2
  }
  IQV <- (nlev/(nlev-1))*(1-cat_list)
  IQV1 <- as.list(0)
  IQV1[[2]] <- IQV
  IQV1[[1]] <- mean(IQV)
  names(IQV1) <- c("full_graph", "egonet")
  return (IQV1)
} 
