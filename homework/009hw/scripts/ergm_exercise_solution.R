library(statnet)
library(igraph)
library(intergraph)

ckm_data <- load(file="data/CKM_data.Rdata")

ckm_net <- as.network(ckm_mat, directed=T, loops=F, matrix.type="adjacency")

network.vertex.names(ckm_net) <- c(1:246)

set.vertex.attribute(ckm_net, "community", CKM$community)

set.vertex.attribute(ckm_net, "city", CKM$city)

summary.network(ckm_net, print.adj=FALSE)

plot.network(ckm1, vertex.col=get.vertex.attribute(ckm_net, "city"))

ckm_peoria<-get.inducedSubgraph(ckm_net,v=which(ckm_net%v%'city'==1))

plot.network(ckm_peoria)

summary(ckm_peoria)

library(ergm)

model1 <- ergm(ckm_peoria ~ edges)

summary(model1)

#The Bernoulli model suggests that Peoria network is less dense than we'd expect.


model2 <- ergm(ckm_peoria ~ edges + nodematch("community"))

summary(model2)

#log-odds of different types of ties: heterogenous=-3.33/homogenous by number of years in community=.667
#But note that we aren't really tapping into network effects and the MCMC isn't being deployed

model3 <- ergm(ckm_peoria ~ edges + nodematch("community") + triangle)

#Let's check triangles...degeneracy!

model3b <- ergm(ckm_peoria ~ edges + nodematch("community") + gwesp(0, fixed = TRUE))

summary(model3b)

#Let's used the geometrically weighted edgewise shared partner to evaluate local clustering
# This shows that if a pair of nodes "have any positive number of friends in common and each of them
#is in at least one other traingle with each of those friends" then log-odds of a tie between them increases to
#around -3.

model4 <- ergm(ckm_peoria ~ edges + nodematch("community") + gwesp(0, fixed = TRUE) + mutual)

summary(model4)

#And reciprocity has a very large positive effect on tie formation improving the log-odds to nearly -2.
