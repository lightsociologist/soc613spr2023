library(igraph)

star.wars <- read.csv("data/star_wars.csv" , header=TRUE)

sw.g <- graph_from_data_frame(star.wars)

V(sw.g)$type <- bipartite_mapping(sw.g)$type

bimat <- as_incidence_matrix(sw.g)

pmat <- bimat %*% t(bimat)

diag(pmat) <- 0

write.csv(pmat, "sw_actnet.csv", row.names=TRUE)

nsw.g <- graph.adjacency(pmat, weighted=TRUE)

sw.edge <- get.data.frame(nsw.g)

write.csv(pmat, "sw_edgelist.csv", row.names=TRUE)
