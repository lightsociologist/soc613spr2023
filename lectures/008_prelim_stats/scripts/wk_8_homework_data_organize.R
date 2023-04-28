library(dplyr)
library(statnet)

class <- read.table(file="data/vickers_multiplex.txt", header = FALSE, sep = "", dec = ".")

class <- rename(class, type=V1, node1=V2, node2=V3)

class <- class %>% select(-V4)

get_on_edge <- class %>% filter(type==1) %>% select(-type)

besties_edge <- class %>% filter(type==2) %>% select(-type)

work_edge <- class %>% filter(type==3) %>% select(-type)

geton.n <- network(symmetrize(network(get_on_edge,matrix.type="edgelist"), rule="weak"))

work.n <- network(symmetrize(network(get_on_edge,matrix.type="edgelist"), rule="weak"))

best.n <- network(symmetrize(network(get_on_edge,matrix.type="edgelist"), rule="weak"))

gender <- data.frame(id=seq(1:29))

gender$gender <- ifelse(gender$id>12, "girl", "boy")

#easier to do this in igraph

library(igraph)
library(intergraph)

gender.g <- graph.data.frame(gender)
V(gender.g)$type <- bipartite_mapping(gender.g)$type
proj <- bipartite.projection(gender.g)
gender.g <- proj$proj1

gender.n <- asNetwork(gender.g)

work.g <- asIgraph(work.n)

geton.g <- asIgraph(geton.n)

best.g <- asIgraph(best.n)

work.g <- as.undirected(work.g)
best.g <- as.undirected(best.g)
geton.g <- as.undirected(geton.g)

save(gender.n, geton.n, work.n, best.n, gender.g, geton.g, work.g, best.g, file="data/vickers_class.Rdata")

gs <- vector('list', 1000)


for(i in 1:1000){
  gs[[i]] <- sample_gnm(n=gorder(best.g), m=gsize(best.g))
}


library(ggplot2)

gs.dist <- data.frame(mdist = unlist(lapply(gs, mean_distance)))

ggplot(gs.dist, aes(x=mdist))+
  geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
  geom_density(alpha=0.2, fill="tomato") +
  geom_vline(aes(xintercept=mean_distance(best.g)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Average Path Length: Vickers Best Network",
       x ="Average Path Length", y = "Density") +
  theme_bw()


gs.transitiv <- data.frame(transitiv = unlist(lapply(gs, transitivity)))

ggplot(gs.transitiv, aes(x=transitiv))+
  geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
  geom_density(alpha=0.2, fill="tomato") +
  geom_vline(aes(xintercept=transitivity(best.g)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Transitivity: Vickers Best Network",
       x ="Transitivity", y = "Density") +
  theme_bw()

gs <- vector('list', 1000)


for(i in 1:1000){
  gs[[i]] <- sample_gnm(n=gorder(work.g), m=gsize(work.g))
}


library(ggplot2)

gs.dist <- data.frame(mdist = unlist(lapply(gs, mean_distance)))

ggplot(gs.dist, aes(x=mdist))+
  geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
  geom_density(alpha=0.2, fill="tomato") +
  geom_vline(aes(xintercept=mean_distance(work.g)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Average Path Length: Vickers Work Network",
       x ="Average Path Length", y = "Density") +
  theme_bw()


gs.transitiv <- data.frame(transitiv = unlist(lapply(gs, transitivity)))

ggplot(gs.transitiv, aes(x=transitiv))+
  geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
  geom_density(alpha=0.2, fill="tomato") +
  geom_vline(aes(xintercept=transitivity(work.g)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Transitivity: Vickers Work Network",
       x ="Transitivity", y = "Density") +
  theme_bw()

gcor(best.n, work.n)

netcor <- qaptest(list(best.n, work.n), gcor, g1=1, g2=2, reps=1000)

summary(netcor)

plot(netcor)


nlog <-netlogit(work.n, list(best.n, gender.n),reps=1000)

summary(nlog)





