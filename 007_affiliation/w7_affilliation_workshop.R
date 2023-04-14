## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)


## ---- echo=T, eval=FALSE-----------------------------------------------------------------------------------------------------------------
## #load the package
## 
## library(academictwitteR)
## 
## #Use your token here
## 
## bearer_token <- "YOUR TOKEN" # Insert bearer token
## 
## 


## ---- echo=T, eval=FALSE-----------------------------------------------------------------------------------------------------------------
## 
## #List hashtags
## 
## htagquery <- c("#socAF", "soctwitter")
## 
## #List query elements. Here getting rids of retweets
## 
## no_retweets <- build_query(htagquery, is_retweet=FALSE)
## 


## ---- echo=T, eval=FALSE-----------------------------------------------------------------------------------------------------------------
## 
## #This gets the tweets
## 
## soc_json <-
##   get_all_tweets(
##     no_retweets,
##     "2021-05-01T00:00:00Z",
##     "2021-05-10T00:00:00Z",
##     bearer_token,
##     data_path = "data/",
##     bind_tweets = FALSE
##   )
## 


## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

library(academictwitteR)

#Here we push the json files together

tweets <- bind_tweet_jsons(data_path = "data/")



## ---- echo=T, eval=FALSE-----------------------------------------------------------------------------------------------------------------
## 
## #Here we grab the list of users
## 
## users <- tweets$author_id
## 
## users <- get_user_profile(users, bearer_token)
## 
## save(users, tweets, file="twitter_data.rda")
## 


## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

#Load Twitter user and tweets data using RStudio.

load("~/Documents/GitHub/sna_course/week_8_affiliation/twitter_data.rda")


library(dplyr)
library(tidyr)

#This jumbles on each download so you have to inspect data frrame

ents <- data.frame(tweets$id, tweets$entities[3])


hashs <- ents %>% unnest(hashtags) %>% 
  group_by(tweets.id)




## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

twt <- data.frame(tweets.id = tweets$id, auid = tweets$author_id)

hash_author <- left_join(hashs, twt)

hash_edge <- data.frame(auid=hash_author$auid, tag=hash_author$tag)

hash_edge$tag <- trimws(tolower(hash_edge$tag))

head(hash_edge)



## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

library(igraph)

tw.g <- graph.data.frame(hash_edge)


V(tw.g)$type <- bipartite.mapping(tw.g)$type



## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------
V(tw.g)$color <- ifelse(V(tw.g)$type, "lightblue", "salmon")
V(tw.g)$shape <- ifelse(V(tw.g)$type, "circle", "square")
E(tw.g)$color <- "lightgray"

plot(tw.g, vertex.size=4, edge.arrow.size=.01, layout=layout.fruchterman.reingold)



## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

plot(tw.g, vertex.size=4, edge.arrow.size=.01, vertex.label="", layout=layout.fruchterman.reingold)



## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

V(tw.g)$name <- trimws(tolower(V(tw.g)$name))

tw.g2 <- delete.vertices(tw.g, V(tw.g)$name=="socaf")

tw.g2 <- delete.vertices(tw.g2, V(tw.g2)$name=="soctwitter")


lb.df <- data.frame(type=V(tw.g2)$type, name=V(tw.g2)$name)

lb.df$nms <- ifelse(lb.df$type=="TRUE", lb.df$name, " ")

plot.igraph(tw.g2, layout=layout.fruchterman.reingold, vertex.label.color="black", vertex.label=lb.df$nms, vertex.size=4, edge.arrow.size=.01)



## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

comp.g <- induced_subgraph(tw.g2, components(tw.g2)$membership==1)

isos <- which(degree(comp.g)==0)

comp.g <- delete.vertices(comp.g, isos)

lb.df <- data.frame(type=V(comp.g)$type, name=V(comp.g)$name)

lb.df$nms <- ifelse(lb.df$type=="TRUE", lb.df$name, " ")



plot.igraph(comp.g, layout=layout.fruchterman.reingold, vertex.label.cex=.5, vertex.label.color="black", vertex.label=lb.df$nms, vertex.size=4, edge.arrow.size=.01)


## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

bip.g <- bipartite.projection(comp.g)

hash.g <- bip.g$proj2

V(hash.g)$deg <- degree(hash.g, normalized=TRUE)

hashlab <- data.frame(name=V(hash.g)$name, degree=V(hash.g)$deg)

hashlab$lab <- ifelse(hashlab$degree>.13, hashlab$name, " ")

plot.igraph(hash.g, layout=layout.fruchterman.reingold, vertex.label.cex=.5, vertex.label.color="black", vertex.label=hashlab$lab, vertex.size=4, edge.arrow.size=.01)



## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

nhash.g <- delete.edges(hash.g, which(E(hash.g)$weight<2))

isos <- which(degree(nhash.g)==0)

nhash2.g <- delete.vertices(nhash.g, isos)

plot.igraph(nhash2.g, layout=layout.fruchterman.reingold, vertex.label.cex=.5, vertex.label.color="black", vertex.size=4, edge.arrow.size=.01)




## ---- echo=T-----------------------------------------------------------------------------------------------------------------------------

peep.g <- bip.g$proj1

plot.igraph(peep.g, layout=layout.fruchterman.reingold, vertex.label.cex=.5, vertex.label="", vertex.label.color="black", vertex.size=4, edge.arrow.size=.01)



## ---- echo=F-----------------------------------------------------------------------------------------------------------------------------


library(knitr)
purl("week7_affiliation_workshop.Rmd")


