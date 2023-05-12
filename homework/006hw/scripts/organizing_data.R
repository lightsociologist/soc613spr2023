library(igraph)
library(readr)

cross_parker_attributes <- read_csv("data/cross_parker_attributes.csv", 
                                    col_types = cols(region = col_number(), 
                                                    gender = col_number()))

org.df <- read.table("data/cross_parker_consulting.txt", header=TRUE)

orgnet <- graph_from_data_frame(org.df)

#get rid of edges that are 1 for never wtf.

orgnet <- delete.edges(orgnet, E(orgnet)[E(orgnet)$weight < 2]) 

V(orgnet)$region <- cross_parker_attributes$region

V(orgnet)$gender <- as.numeric(cross_parker_attributes$gender)

orgnet <- delete.vertices(orgnet, which(degree(orgnet)==0))

orgnet <- simplify(orgnet)

orgnet <- as.undirected(orgnet, mode="collapse")

plot(orgnet, vertex.color=V(orgnet)$region)

saveRDS(orgnet, file = "data/orgnet.rds")

# Restore the object
#readRDS(file = "my_data.rds")