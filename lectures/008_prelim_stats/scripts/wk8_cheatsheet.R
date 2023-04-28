#This sets up the Vickers data (https://manliodedomenico.com/data.php) for homework 8 
#and then provides a cheatsheet.

#Feel free to skip ahead.


library(dplyr)
library(statnet)

#The data is downloaded as a .edges file, but it is just an edgelist in a text file
#that has layers (e.g. three different networks marked by an additional column).

class <- read.table(file="data/vickers_multiplex.txt", header = FALSE, sep = "", dec = ".")

class <- rename(class, type=V1, node1=V2, node2=V3)

#This column is just the 1s so let's remove.

class <- class %>% select(-V4)

#We can select and the remove type to have a simple edgelist

get_on_edge <- class %>% filter(type==1) %>% select(-type)

besties_edge <- class %>% filter(type==2) %>% select(-type)

work_edge <- class %>% filter(type==3) %>% select(-type)

#Let's symmetrize so that direction of nomination doesn't matter...a little convoluted 
#but now we have a series of network variables.

geton.n <- network(symmetrize(network(get_on_edge,matrix.type="edgelist"), rule="weak"))

work.n <- network(symmetrize(network(get_on_edge,matrix.type="edgelist"), rule="weak"))

best.n <- network(symmetrize(network(get_on_edge,matrix.type="edgelist"), rule="weak"))

#We know that 1:12 are boys, so let's make a co-gender network.
#I make this in igraph more easily and then bring back into statnet using intergraph. 

gender <- data.frame(id=seq(1:29))

gender$gender <- ifelse(gender$id>12, "girl", "boy")

library(igraph)
library(intergraph)

gender.g <- graph.data.frame(gender)
V(gender.g)$type <- bipartite_mapping(gender.g)$type
proj <- bipartite.projection(gender.g)
gender.g <- proj$proj1


gender.n <- asNetwork(gender.g)

#To make things easier I'm setting these up in both igraph and statnet

work.g <- asIgraph(work.n)

geton.g <- asIgraph(geton.n)

best.g <- asIgraph(best.n)

work.g <- as.undirected(work.g)
best.g <- as.undirected(best.g)
geton.g <- as.undirected(geton.g)

#and I save all of that here.

save(gender.n, geton.n, work.n, best.n, gender.g, geton.g, work.g, best.g, file="data/vickers_class.Rdata")

#ASSIGNMENT CHEATSHEET

#Breezing through this but here are some ideas:

#I can imagine plotting and considering edges and maybe some other variables as descriptive
#statistics. I jump into the random pieces.

#First, shortest paths for the friends graph

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

#Then the clustering coefficient for the Best friends graph.

gs.transitiv <- data.frame(transitiv = unlist(lapply(gs, transitivity)))

ggplot(gs.transitiv, aes(x=transitiv))+
  geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
  geom_density(alpha=0.2, fill="tomato") +
  geom_vline(aes(xintercept=transitivity(best.g)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Transitivity: Vickers Best Network",
       x ="Transitivity", y = "Density") +
  theme_bw()

#Paths for work

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

#CLustering coefficient for work

gs.transitiv <- data.frame(transitiv = unlist(lapply(gs, transitivity)))

ggplot(gs.transitiv, aes(x=transitiv))+
  geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
  geom_density(alpha=0.2, fill="tomato") +
  geom_vline(aes(xintercept=transitivity(work.g)),
             color="blue", linetype="dashed", size=1) +
  labs(title="Transitivity: Vickers Work Network",
       x ="Transitivity", y = "Density") +
  theme_bw()

#QAP Correlation

netcor <- qaptest(list(best.n, work.n), gcor, g1=1, g2=2, reps=1000)

summary(netcor)

#QAP Regression

nlog <-netlogit(work.n, list(best.n, gender.n),reps=1000)

summary(nlog)





