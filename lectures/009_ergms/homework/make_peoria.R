library(igraph)

whole <- graph_from_adjacency_matrix(ckm_mat)

V(whole)$time <- CKM$community

V(whole)$city <- CKM$city

peoria.g <- induced.subgraph(whole, which(V(whole)$city==1))

peoria.g <- simplify(peoria.g)

library(intergraph)

peoria.n <- asNetwork(peoria.g)

summary(peoria.n)

save(peoria.n, file="data/peoria.RDA")