## ----setup, include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ----message=FALSE--------------------------------------------------------------

library(igraph)
load("karate.rda")

plot.igraph(karate)

kdens <- make_ego_graph(karate, 1) %>%
vapply(graph.density, numeric(1))

df.dens <- data.frame(V(karate)$id, kdens)



## ---- echo=TRUE-----------------------------------------------------------------

V(karate)$label <- c(1:34)

comps <- components(karate)

print(comps$csize)



## ----echo=TRUE------------------------------------------------------------------

V(karate)$name <- V(karate)$label

biconnected <- biconnected.components(karate)

b1 <- biconnected$components[[1]]

g2 <- induced.subgraph(graph=karate,vids=b1)

plot.igraph(g2)




## ---- echo=TRUE-----------------------------------------------------------------
kcore <- coreness(karate)    # Find the cores
V(karate)$core <- kcore      # Add the cores as a vertex attribute

library(RColorBrewer)
coul = brewer.pal(4, "Set1") 
 
# Create a vector of color
my_color=coul[as.numeric(as.factor(V(karate)$core))]

plot.igraph(karate, vertex.color=my_color)
legend("bottomleft", legend=levels(as.factor(V(karate)$core)) , 
col = coul , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, 
horiz = FALSE, inset = c(0.1, 0.1))
 


## ----echo=TRUE------------------------------------------------------------------

cliques <- largest_cliques(karate)

c1 <- cliques[[1]]

clique2 <- induced.subgraph(graph=karate,vids=c1)

plot(clique2)



## ----echo=TRUE------------------------------------------------------------------

art_points <- articulation.points(karate)

rem <- delete.vertices(karate, art_points)

plot(rem)

art.points2 <- articulation.points(rem)

rem2 <- delete.vertices(rem, art.points2)

plot(rem2)



## ---- echo=TRUE-----------------------------------------------------------------
fg_comms <- cluster_fast_greedy(karate)

V(karate)$fg_comms <- fg_comms$membership

plot.igraph(karate, vertex.color=V(karate)$fg_comms)



## ---- echo=TRUE-----------------------------------------------------------------

lv_comms <- cluster_louvain(karate)

V(karate)$lv_comms <- lv_comms$membership

plot.igraph(karate, vertex.color=V(karate)$lv_comms)



## ---- echo=TRUE-----------------------------------------------------------------
wt_comms <- cluster_walktrap(karate)

V(karate)$wt_comms <- wt_comms$membership

plot.igraph(karate, vertex.color=V(karate)$wt_comms)



## ---- echo=T, message=F---------------------------------------------------------
library(network)
library(sna)
library(intergraph)

knet <- asNetwork(karate)


## ---- echo=T--------------------------------------------------------------------
plot.network(knet, label=get.vertex.attribute(knet, "id"))	

kclus <- equiv.clust(knet, mode="digraph")

plot(kclus)


## ---- echo=T--------------------------------------------------------------------
kbm <- blockmodel(knet, kclus, k=4)		#looking at it made the 2-solution clear
kbm
plot.blockmodel(kbm)	# take a look

kbm.mem <- kbm$block.membership



## ---- echo=TRUE-----------------------------------------------------------------

modularity(karate, lv_comms$membership)

modularity(karate, fg_comms$membership)

modularity(karate, wt_comms$membership)

modularity(karate, kbm.mem)




## ---- echo=F--------------------------------------------------------------------


#library(knitr)
#purl("week5_centrality_workshop.Rmd")


