library(blockmodeling)

mat <- as.matrix(get.adjacency(flomarriage)) # Extract the matrix in igraph (if you haven't already)

# Try a two block partition.
class2 <- opt.random.par(M=mat, k=2, rep=10, approach="ss", blocks="com")
# Tru a four block partition
class4 <- optRandomParC(M=mat, k=4, rep=10, approach="ss", blocks="com")


g_rege <- REGE.for(mat)$E   

heatmap(g_rege)