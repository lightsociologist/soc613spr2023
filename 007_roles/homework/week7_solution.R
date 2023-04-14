#Here is a brief sketch of possible solutions to the week 7 homework.

#Note: network object is affiliated with sna package/igraph object is connected with igraph

#I first build a blockmodel in sna (but by peeling off the adjacency matrix from an igraph object). I plot 
#the blockmodel in the requested ways.

#Next I offer two quick (and somewhat incomplete ways) to look at roles and positions. There are many ways to
#do this. I start by leveraging the relationship between betweenness and constriant as suggested by Burt.

#Last, I identify cutpoints using the preferred way in sna. I then can view how the block align with the
#cutpoints.

#Block model section

library(igraph)

#Here I make music.igraph

int.trade <- as.matrix(read.csv("homework/musicinstrument_trade_1980_1998.csv", row.names=1))

int.trade <- round(int.trade, 2)

log.trade <- log(int.trade)

int.g <- graph_from_adjacency_matrix(log.trade)

comp.g <- induced.subgraph(int.g, components(int.g)$membership==1)

plot.igraph(comp.g, edge.arrow.width=.05)

music.igraph <- comp.g

#Here I peel off the matrix

mat <- as.matrix(get.adjacency(as.directed(music.net)))

#I also make a network object for doing things in sna if necessary

music.network=network(mat,matrix.type="adjacency",directed=TRUE) 

library(sna)

#I could do this with the network object, but I start with the matrix instead 

sc <- equiv.clust(mat)

#Here I plot the dendogram

plot(sc)

#Here I make the blockmodel

blocks <- blockmodel(mat, sc, k=6) 

plot.blockmodel(blocks)	

#Take a look at the block image

bimage <- blocks$block.model

#Plot the block image network

gplot(bimage, diag=TRUE, 								# let's plot the image network
      edge.lwd=bimage,									# edge-thickness as image-matrix density
      vertex.cex=sqrt(table(blocks$block.membership))/2,		# vertex-size as block size
      vertex.sides=50, label.pos=5, vertex.col="gray",
      label=names(table(blocks$block.membership)))	

#Plot the image matrix

plot.sociomatrix(bimage)			# the image matrix

#Plot the network, but not super helpful

plot.igraph(music.net, vertex.color=blocks$block.membership, edge.arrow.width=.05)

#Data.frame with the blocks

country_blocks <- data.frame(country=blocks$plabels, blocks=blocks$block.membership)

#Detach sna because it doesn't play nice with igraph in this part

detach("package:sna")

cons <- constraint(music.igraph)

btween <- betweenness(music.igraph)

#log_between

lbtween <- log(btween)

#normal_between

nbetween <- btween/((vcount(music.igraph)-1)*(vcount(music.igraph)-2))

becon <- data.frame(name=V(music.igraph)$name, constraint=cons, between=btween, logbtween = lbtween, blocks=blocks$block.membership,
                    nbetween=nbetween)

library(ggplot2)

ggplot(becon, aes(x=btween, y=constraint)) + 
  geom_point() + geom_smooth(method='lm',formula=y~x)


ggplot(becon, aes(x=logbtween, y=constraint)) + 
  geom_point() + geom_smooth(method='lm',formula=y~x)


ggplot(becon, aes(x=nbetween, y=constraint)) + 
  geom_point() + stat_smooth(method='lm',formula=y~x)

#let's look at cutpoints as well and evaluate alongside blocks.

library(sna)

music.network %v% "cutpoints" <- cutpoints(music.network, return.indicator=T, mode="digraph") + 1

plot(music.network, vertex.col="cutpoints", displaylabels=T)	

cutpoint.df <- data.frame(cpoints = music.network %v% "cutpoints", names = music.network %v% "vertex.names", blocks=blocks$block.membership)

