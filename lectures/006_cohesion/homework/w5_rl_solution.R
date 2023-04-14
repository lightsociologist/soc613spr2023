library(igraph)

#Load the two networks and the adopt attribute for both networks

korea1 <- read.graph("homework/Korea1.net", format="pajek")

korea2 <- read.graph("homework/Korea2.net", format="pajek")

adopter1 <- as.matrix(read.table("homework/Korea1_adopters.clu", skip=1))	
V(korea1)$adopter <- as.vector(adopter1)					

adopter2 <- as.matrix(read.table("homework/Korea2_adopters.clu", skip=1))	
V(korea2)$adopter <- as.vector(adopter2)					

#plot to check it out

plot(korea1, vertex.color=V(korea1)$adopter+1, main="Original Korea 1")

plot(korea2, vertex.color=V(korea2)$adopter+1, main="Original Korea 2")

#centrality scores

k1.deg <- degree(korea1)

k1.close <- closeness(korea1)

k1.between <- betweenness(korea1)

k1.eigen <- eigen_centrality(korea1)

k2.deg <- degree(korea2)

k2.close <- closeness(korea2)

k2.between <- betweenness(korea2)

k2.eigen <- eigen_centrality(korea2)


#store centralization scores as a vectors

k1.central <-c(centralization.degree(korea1)$centralization, centralization.closeness(korea1)$centralization, centralization.betweenness(korea1)$centralization, centr_eigen(korea1)$centralization)

k2.central <-c(centralization.degree(korea2)$centralization, centralization.closeness(korea2)$centralization, centralization.betweenness(korea2)$centralization, centr_eigen(korea2)$centralization)


#summarize individual centrality with means

k1.means <- c(mean(k1.deg), mean(k1.close), mean(k1.between), mean(k1.eigen$vector))

k2.means <- c(mean(k2.deg), mean(k2.close), mean(k2.between), mean(k2.eigen$vector))


#Build a table/data.frame

scores <- c("degree", "closeness", "betweenness", "eigenvector")

central.df <- data.frame(measure=scores, k1.centrality=k1.means, k2.centrality=k2.means, k1.centralization=k1.central, k2.centralization=k2.central)

#let's run some quick correlations between centrality scores and adopter status

cor.test(k1.deg, adopter1)
cor.test(k1.close, adopter1)
cor.test(k1.between, adopter1)
cor.test(k1.eigen$vector, adopter1)


cor.test(k2.deg, adopter2)
cor.test(k2.close, adopter2)
cor.test(k2.between, adopter2)
cor.test(k2.eigen$vector, adopter2)

#One-way Anovas

ad1 <- aov(k1.deg ~ adopter1)
summary(ad1)
ac1 <- aov(k1.close ~ adopter1)
summary(ac1)
ab1 <- aov(k1.between ~ adopter1)
summary(ab1)
ae1 <- aov(k1.eigen$vector ~ adopter1)
summary(ae1)


ad2 <- aov(k2.deg ~ adopter2)
summary(ad2)
ac2 <- aov(k2.close ~ adopter2)
summary(ac2)
ab2 <- aov(k2.between ~ adopter2)
summary(ab2)
ae2 <- aov(k2.eigen$vector ~ adopter2)
summary(ae2)

#Isolates

sum(degree(korea1)==0)

sum(degree(korea2)==0)

#Size of largest component

components(korea1)$csize

components(korea2)$csize

#Bicomponents

bi.1 <- biconnected.components(korea1)

bi.2 <- biconnected.components(korea2)

max(sapply(bi.1$components, length))

max(sapply(bi.2$components, length))

#Cores
table(coreness(korea1))

table(coreness(korea2))


#Plot bicomponents

red1 <- induced.subgraph(graph=korea1, vids=bi.1$components[[12]])

plot(red1, vertex.color=V(red1)$adopter+1, main="Korea 1 bicomponent")

red2 <- induced.subgraph(graph=korea2, vids=bi.2$components[[7]])

plot(red2, vertex.color=V(red2)$adopter+1, , main="Korea 2 bicomponent")


#Communities over largest component (delete )

#make the largest component. The components are stored in $membership by size so the biggest is 1.

comp1 <- delete_vertices(korea1, components(korea1)$membership!=1)

comp2 <- delete_vertices(korea2, components(korea2)$membership!=1)

plot(comp1)

plot(comp2)

#Communities

lv1 <- cluster_louvain(comp1)
rw1 <- cluster_walktrap(comp1)
fg1 <- cluster_fast_greedy(comp1)


lv2 <- cluster_louvain(comp2)
rw2 <- cluster_walktrap(comp2)
fg2 <- cluster_fast_greedy(comp2)

#Correlations

cor.test(lv1$membership, V(comp1)$adopter)
cor.test(rw1$membership, V(comp1)$adopter)
cor.test(fg1$membership, V(comp1)$adopter)

cor.test(lv2$membership, V(comp2)$adopter)
cor.test(rw2$membership, V(comp2)$adopter)
cor.test(fg2$membership, V(comp2)$adopter)

