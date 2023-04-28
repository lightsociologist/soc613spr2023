## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo=T, message=FALSE---------------------------------------------------------------------
#load the package

library(igraph)

#We want to build random graphs that have the same characteristics of the 

load("data/flonets.Rdata")			



## ---- echo=T------------------------------------------------------------------------------------
plot.igraph(flomarriage, vertex.color=as.factor(V(flomarriage)$party))


## ---- echo=T------------------------------------------------------------------------------------
plot.igraph(flobusiness, vertex.color=as.factor(V(flomarriage)$party))



## ---- echo=T------------------------------------------------------------------------------------


gs <- vector('list', 1000)

for(i in 1:1000){
  gs[[i]] <- sample_gnm(n=gorder(flomarriage), m=gsize(flomarriage))
}



## ---- echo=T------------------------------------------------------------------------------------

library(ggplot2)

gs.dist <- data.frame(mdist = unlist(lapply(gs, mean_distance)))

ggplot(gs.dist, aes(x=mdist))+
geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
     geom_density(alpha=0.2, fill="tomato") +
geom_vline(aes(xintercept=mean_distance(flomarriage)),
            color="blue", linetype="dashed", size=1) +
  labs(title="Average Path Length: Florentine Marriage Network",
        x ="Average Path Length", y = "Density") +
  theme_bw()


gs.transitiv <- data.frame(transitiv = unlist(lapply(gs, transitivity)))

ggplot(gs.transitiv, aes(x=transitiv))+
geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
     geom_density(alpha=0.2, fill="tomato") +
geom_vline(aes(xintercept=transitivity(flomarriage)),
            color="blue", linetype="dashed", size=1) +
   labs(title="Transitivity: Florentine Marriage Network",
        x ="Transitivity", y = "Density") +
  theme_bw()




## ---- echo=T------------------------------------------------------------------------------------

gs <- vector('list', 1000)

for(i in 1:1000){
  gs[[i]] <- sample_gnm(n=gorder(flobusiness), gsize(flobusiness))
}

gs.dist <- data.frame(mdist = unlist(lapply(gs, mean_distance)))



ggplot(gs.dist, aes(x=mdist))+
geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
     geom_density(alpha=0.2, fill="tomato") +
geom_vline(aes(xintercept=mean_distance(flobusiness)),
            color="blue", linetype="dashed", size=1)+
   labs(title="Average Path Length: Florentine Business Network",
        x ="Average Path Length", y = "Density") +
  theme_bw()

gs.transitiv <- data.frame(transitiv = unlist(lapply(gs, transitivity)))

ggplot(gs.transitiv, aes(x=transitiv))+
geom_histogram(bins=20, aes(y = ..density..), color="black", fill="gray") +
     geom_density(alpha=0.2, fill="tomato") +
geom_vline(aes(xintercept=transitivity(flobusiness)),
            color="blue", linetype="dashed", size=1) +
 labs(title="Transitivity: Florentine Business Network",
        x ="Transitivity", y = "Density") +
  theme_bw()



## ---- echo=T, message=FALSE---------------------------------------------------------------------


detach("package:igraph", unload = TRUE)

library(statnet)

library(intergraph)

fl.mar <- asNetwork(flomarriage)

fl.bus <- asNetwork(flobusiness)


fl.par <- asNetwork(floparty)


## ---- echo=T------------------------------------------------------------------------------------

cug.mar <- cug.test(fl.mar, gtrans, cmode="edges", reps=1000)

cug.bus <- cug.test(fl.bus, gtrans, cmode="edges", reps=1000)



## ---- echo=T------------------------------------------------------------------------------------

G.Transitivity <- c(cug.mar$obs.stat, cug.bus$obs.stat) 

Pct.Greater <- c(cug.mar$pgteobs, cug.bus$pgteobs)

Pct.Lower <- c(cug.mar$plteobs, cug.bus$plteobs)

comp.transitivity <- cbind(G.Transitivity, Pct.Greater, Pct.Lower)

rownames(comp.transitivity) <- c("Marital Network", "Business Network")
  
round(comp.transitivity, 2)


## ---- echo=T------------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(cug.mar, main="Marital Network \nTransitivity (Edges)" )
plot(cug.bus, main="Business Network \nTransitivity (Edges" )
par(mfrow=c(1,1))



## ---- echo=T------------------------------------------------------------------------------------

gcor(fl.bus, fl.mar)



## ---- echo=T------------------------------------------------------------------------------------
netcor <- qaptest(list(fl.bus, fl.mar), gcor, g1=1, g2=2, reps=1000)

summary(netcor)



## ---- echo=T------------------------------------------------------------------------------------
plot(netcor)



## ---- echo=T------------------------------------------------------------------------------------

nlog <-netlogit(fl.bus, list(fl.mar, fl.par),reps=1000)



summary(nlog)



## ---- echo=F------------------------------------------------------------------------------------

#Random leftovers

#gs.con <- lapply(gs, mean_distance)


#gs.con<- data.frame(con = unlist(lapply(gs.con, mean)))



#library(knitr)
#purl("w8_intro_to_network_stats_workshop.Rmd")


