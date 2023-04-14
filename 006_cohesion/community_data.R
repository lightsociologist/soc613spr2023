##########################################################################
#	R & igraph Tutorial for Socy 496/696 - 2014/03/24
#	author: jimi adams
#	last updated: 2014-03-23
#
#	NOTE:	This is code to produce the data file for the Cohesion & Clustering tutotial/HW.
##########################################################################
setwd("/Users/jimiadams/Documents/My Documents/_current/classes/496_696/Problem Sets/20140324 - Cohesion/")
require(igraph)
require(intergraph)
require(statnet)

ex1a <- read.csv("simple_undirected.csv", header=T)		# reading in the undirected mixing example
ties<- subset(ex1a,select=c(alt1,alt2,alt3))			# stripping off just the adjacency list
nodes<- subset(ex1a,select=c(id,attr1))					# stripping off just the attributes
df <- data.frame(snd = row(ties)[!is.na(ties)], rcv = ties[!is.na(ties)])	# converting to an edgelist; igraph can natively handle edgelists and adjacency matrices
ex1a_g <- graph.data.frame(df, directed=F, vertices=nodes)	# converting to an igraph “graph” object
ex1a_g <- simplify(ex1a_g)

korea1 <- read.graph("korea1.net", format="pajek")
V(korea1)$adopt <- as.vector(as.matrix(read.table("Korea1_adopters.clu", skip=1)))
V(korea1)$club <- as.vector(as.matrix(read.table("Korea1_members.clu", skip=1)))

korea2 <- read.graph("korea2.net", format="pajek")
V(korea2)$adopt <- as.vector(as.matrix(read.table("Korea2_adopters.clu", skip=1)))
V(korea2)$club <- as.vector(as.matrix(read.table("Korea2_members.clu", skip=1)))

karate <- read.graph("karate.gml", format="gml")
V(karate)$split <- c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1)

rm(df, ex1a, nodes, ties)
save(list=ls(),file="Tutorial.Rdata")