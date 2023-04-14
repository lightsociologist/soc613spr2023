library(igraph)

g <- erdos.renyi.game(10, 20, type = "gnm")
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 10, xlab = "Random Network: G(10,20) model")

g <- erdos.renyi.game(30, 60, type = "gnm")
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 10, xlab = "Random Network: G(30,60) model")

g <- erdos.renyi.game(90, 180, type = "gnm")
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 10, xlab = "Random Network: G(90,180) model")

g <- erdos.renyi.game(500,100, type = "gnm")
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 10, xlab = "Random Network: G(500,1000) model")

edge_density(simplify(g))


