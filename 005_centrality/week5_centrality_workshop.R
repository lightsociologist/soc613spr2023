## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE--------------------------------------------------------------------------------
library(igraph)

flomarriage <- as.undirected(read.graph("flomarriage.net", format="pajek"))

wealth <- as.matrix(read.table("flowealth.clu", skip=1))	
V(flomarriage)$wealth <- as.vector(wealth)					

party <- as.matrix(read.table("floparty.clu"))	
V(flomarriage)$party <- as.vector(party)				

flomarriage <- delete_vertices(simplify(flomarriage), degree(flomarriage)==0)

plot.igraph(flomarriage, vertex.color=as.factor(V(flomarriage)$party))


## ----message=FALSE, echo=TRUE---------------------------------------------------------------------

degree_centrality <- degree(flomarriage)



## -------------------------------------------------------------------------------------------------
plot.igraph(flomarriage, vertex.size=degree_centrality*5,
            edge.arrow.size=.5, vertex.color=as.factor(V(flomarriage)$party))



## ----message=FALSE, echo=TRUE---------------------------------------------------------------------

centralization.degree(flomarriage, normalized=T)

#print(cd$centralization)
#print(vcd$centralization)


## ----message=FALSE, echo=TRUE---------------------------------------------------------------------

close_centrality <- closeness(flomarriage, normalized=TRUE, mode="ALL")

cc <- centralization.closeness(flomarriage)

cc$centralization



## -------------------------------------------------------------------------------------------------


plot.igraph(flomarriage, vertex.size=close_centrality*50, 
            edge.arrow.size=.5,vertex.color=as.factor(V(flomarriage)$party))



## ----message=FALSE, echo=TRUE---------------------------------------------------------------------

between_centrality <- betweenness(flomarriage)



## -------------------------------------------------------------------------------------------------

plot.igraph(flomarriage, vertex.size=between_centrality/2, 
            edge.arrow.size=.5, vertex.color=as.factor(V(flomarriage)$party))



## ----message=FALSE, echo=TRUE---------------------------------------------------------------------

bonacich_centrality <- power_centrality(flomarriage)

eig_centrality <- eigen_centrality(flomarriage)

ce <- centr_eigen(flomarriage)

print(ce$centralization)



## ----message=FALSE, echo=FALSE--------------------------------------------------------------------

plot.igraph(flomarriage, vertex.size=eig_centrality$vector*10,
            edge.arrow.size=.5, vertex.color=as.factor(V(flomarriage)$party))


## ---- echo=T--------------------------------------------------------------------------------------

coreness(flomarriage)

median(coreness(flomarriage))



## ---- echo=T--------------------------------------------------------------------------------------

ecent <- eig_centrality$vector

flo.df <- data.frame(V(flomarriage)$name, V(flomarriage)$party, V(flomarriage)$wealth, 
                     degree(flomarriage), closeness(flomarriage), betweenness(flomarriage),   eig_centrality$vector)

library(dplyr)

flo.df <- rename(flo.df, c(family = 1, party=2, wealth=3, degree=4, close=5, between=6, eig=7))

flo.df <- flo.df %>% mutate(medici=recode(party, 'Medici'='1', "Split"='0', 'Oligarch'='0'))

library("Hmisc")

nflo.df <- flo.df[,3:8]

cmat <- rcorr(as.matrix(nflo.df))

cmat



## ---- echo=T--------------------------------------------------------------------------------------

wlth.reg <- lm(wealth~medici + between, data=flo.df)

summary(wlth.reg)



## -------------------------------------------------------------------------------------------------

library(ggplot2)
library(ggrepel)

constraint <- constraint(flomarriage) 

prty <- as.numeric(as.factor(V(flomarriage)$party))

between_centrality <- betweenness(flomarriage)

degree <- degree(flomarriage)



becon <- data.frame(between=between_centrality, cons=constraint, name=V(flomarriage)$name, party=prty, degree=degree)


flo.df$cons <- constraint(flomarriage)

flo.df$prty <- as.numeric(as.factor(flo.df$party))

ggplot(flo.df, aes(x=between, y=cons, label=family)) + 
  geom_point() + geom_label_repel(aes(label=family, fill = factor(prty+4)),  max.overlaps=Inf)+
  geom_smooth(method='lm') +
    scale_fill_manual(values=c("tomato", "darkgoldenrod", "deepskyblue"), 
                       name="Party",
                       breaks=c("5", "6", "7"),
                       labels=c("Medici", "Oligarchs", "Split Loyalists"))+
                         theme_bw() 
  




## ---- echo=T--------------------------------------------------------------------------------------



ggplot(flo.df, aes(x=degree, y=cons, label=family)) + 
  geom_point() + geom_label_repel(aes(label=family, fill = factor(prty+4)),  max.overlaps=Inf)+
  geom_smooth(method='lm') +
  scale_fill_manual(values=c("tomato", "darkgoldenrod", "deepskyblue"), 
                       name="Party",
                       breaks=c("5", "6", "7"),
                       labels=c("Medici", "Oligarchs", "Split Loyalists"))+
                         theme_bw() 
   





## ---- echo=T--------------------------------------------------------------------------------------

ggplot(flo.df, aes(x=wealth, y=eig, label=family)) + 
  geom_point() + 
  geom_label_repel(aes(label=family, fill = factor(prty+4)),  max.overlaps=Inf) +
   geom_smooth(method='lm') +
  scale_fill_manual(values=c("tomato", "darkgoldenrod", "deepskyblue"), 
                       name="Party",
                       breaks=c("5", "6", "7"),
                       labels=c("Medici", "Oligarchs", "Split Loyalists"))+
                         theme_bw() 




## ---- echo=T--------------------------------------------------------------------------------------

library(CINNA)

proper_centralities(flomarriage)



