library(igraph)

ceo.bip <- read.graph("CEOS.net", format="pajek")

bipartite.mapping(ceo.bip)

plot(ceo.bip)

V(ceo.bip)$color <- ifelse(V(ceo.bip)$type, "tomato", "gold")
V(ceo.bip)$shape <- ifelse(V(ceo.bip)$type, "circle", "square")
E(ceo.bip)$color <- "lightgray"

name <- V(ceo.bip)$name

name2 <- gsub(".*-","",name)

V(ceo.bip)$name <- name2

coords <- layout_with_fr(ceo.bip)

plot(ceo.bip, layout=coords, vertex.label.color = "black")
legend(x=-1.5, y=-1.1, c("Clubs","CEOs"), pch=21,
       col="#777777", pt.bg=c("tomato", "gold"), pt.cex=2, cex=.8, bty="n", ncol=1)


coords <- layout_as_bipartite(ceo.bip)

plot(ceo.bip, layout=coords, vertex.label.color = "black")
legend(x=-1.5, y=-1.1, c("Clubs","CEOs"), pch=c(21,22),
       col="#777777", pt.bg=c("tomato", "gold"), pt.cex=2, cex=.8, bty="n", ncol=1)


bi.com <- cluster_louvain(ceo.bip)

V(bi.c om)$lv_comms <- bi.com$membership


ceo.mat <- as_incidence_matrix(ceo.bip)

clubs <-  t(ceo.mat) %*% ceo.mat

clubs.g <- as.undirected(simplify(graph_from_adjacency_matrix(clubs)))

plot(clubs.g, vertex.color="tomato")

ceos <-  ceo.mat %*% t(ceo.mat)

ceos.g <- as.undirected(simplify(graph_from_adjacency_matrix(ceos)))

plot(ceos.g, vertex.shape="square", vertex.color="gold")
