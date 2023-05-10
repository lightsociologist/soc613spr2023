library(igraph)

org.df <- read.table("data/cross_parker_consulting.txt", header=TRUE)

orgnet <- graph_from_data_frame(ord.df)

orgnet <- delete.edges(orgnet, E(orgnet)[E(orgnet)$weight < 2]) 

#get rid of edges that are 1 for never wtf.

V(orgnet)$region <- as.numeric(cross_parker_attributes$region)

V(orgnet)$gender <- as.numeric(cross_parker_attributes$gender)

orgnet <- delete.vertices(orgnet, which(degree(orgnet)==0))

orgnet <- simplify(orgnet)

plot(orgnet, vertex.color=V(orgnet)$region)

saveRDS(orgnet, file = "data/orgnet.rds")

# Restore the object
#readRDS(file = "my_data.rds")