

library(dplyr)
library(tidyr)

ents <- data.frame(tweets$id, tweets$entities[3])

hashs <- ents %>% unnest(hashtags) %>% group_by(tweets.id)

twt <- data.frame(tweets.id=tweets$id, auid = tweets$author_id)

hash_author <- left_join(hashs, twt)

hash_edge <- data.frame(auid=hash_author$auid, tag=hash_author$tag)

hash_edge$tag <- trimws(tolower(hash_edge$tag))

library(igraph)

tw.g <- graph.data.frame(hash_edge)

V(tw.g)$type <- bipartite.mapping(tw.g)$type

plot(tw.g)

V(tw.g)$color <- ifelse(V(tw.g)$type, "lightblue", "salmon")

V(tw.g)$shape <- ifelse(V(tw.g)$type, "circle", "square")

E(tw.g)$color <- "lightgray"

plot(tw.g, vertex.size=4, edge.arrow.size=.01, layout=layout.fruchterman.reingold)

tw.g2 <- delete.vertices(tw.g, V(tw.g)$name=="socaf")

tw.g2 <- delete.vertices(tw.g2, V(tw.g2)$name=="soctwitter")

lb.df <- data.frame(type=V(tw.g2)$type, name=V(tw.g2)$name)

lb.df$nms <- ifelse(lb.df$type=="TRUE", lb.df$name, "") 

plot(tw.g2, vertex.size=4, vertex.label=lb.df$nms, edge.arrow.size=.01, layout=layout.fruchterman.reingold)

isos <- which(degree(tw.g2)==0)

comp.g <- delete.vertices(tw.g2, isos)

lb.df <- data.frame(type=V(comp.g)$type, name=V(comp.g)$name)

lb.df$nms <- ifelse(lb.df$type=="TRUE", lb.df$name, " ")

plot(comp.g, vertex.size=4, vertex.label=lb.df$nms, vertex.label.cex=.5, vertex.label.color="black", edge.arrow.size=.01, layout=layout.fruchterman.reingold)

bip.g <- bipartite.projection(comp.g)

hash.g <- bip.g$proj2

plot(hash.g)

peep.g <- bip.g$proj1

isos <- which(degree(peep.g)==0)

peep2.g <- delete.vertices(peep.g, isos)

nhash.g <- delete.edges(hash.g, which(E(hash.g)$weight<2))

nhash.g <- delete.edges(hash.g, which(E(hash.g)$weight<2))

isos <- which(degree(nhash.g)==0)

nhash2.g <- delete.vertices(nhash.g, isos)

plot.igraph(nhash2.g, layout=layout.fruchterman.reingold, vertex.label.cex=.5, vertex.label.color="black", vertex.size=4, edge.arrow.size=.01)


plot(peep2.g)
