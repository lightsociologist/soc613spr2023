## ----setup, include=FALSE---------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)



## ----results='hide', echo=TRUE, message=FALSE-------------------------------------------------------------------------------
library(igraph)
valente <- read.graph("valente.net", format="pajek")
gender <- as.matrix(read.table("valente.clu", skip=1))	
V(valente)$gender <- as.vector(gender)					
v_deg <- degree(valente)								


## ----message=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------
v_indeg <- degree(valente, mode="in")					
v_outdeg <- degree(valente, mode="out")				
#Sum to check
v_deg==v_indeg + v_outdeg								


## ----message=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------
library(ggplot2)

valente.degree <- data.frame(v_deg, v_indeg, v_outdeg)

ggplot(data=valente.degree, aes(v_deg)) + 
  
  geom_histogram(breaks=seq(2,14, by=1),
                col="black", 
                fill="black", 
                alpha = .2)+
 labs(title="Total Degree Distribution") +
  labs(x="Degree", y="Count")


## ---------------------------------------------------------------------------------------------------------------------------
ggplot(data=valente.degree, aes(v_indeg))  +
  geom_histogram(breaks=seq(-1,14, by=1), col="black", fill="black", alpha = .2)+
 labs(title="In-Degree Distribution") +
  labs(x="In-Degree", y="Count")


## ----message=FALSE, echo=FALSE----------------------------------------------------------------------------------------------

ggplot(data=valente.degree, aes(v_outdeg)) + geom_histogram(breaks=seq(-1,14, by=1),
                                                          col="black", 
                                                          fill="black", 
                                                          alpha = .2)+
 labs(title="Out-Degree Distribution") +
  labs(x="Out-Degree", y="Count")


## ----message=FALSE, echo=TRUE, fig.show='hide'------------------------------------------------------------------------------
ggplot(data=valente.degree,aes(v_indeg,v_outdeg))+
  geom_point(aes(colour=factor(V(valente)$gender+1)))+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())


## ---- echo=TRUE, results="hide"---------------------------------------------------------------------------------------------

adjmat <- as_adjacency_matrix(valente)


girl <- ifelse(gender==1,1,0)
boy <- ifelse(gender==0, 1, 0)

gendermat <- cbind(girl, boy)

multiplication <- adjmat %*% gendermat

percent.cat <- data.frame(toboy=multiplication[,2], togirl=multiplication[,1], gender=gender, outgree=v_outdeg)

percent.cat$prop.boy <- percent.cat$toboy/percent.cat$outgree

percent.cat$prop.girl <- percent.cat$togirl/percent.cat$outgree

mean(percent.cat$prop.boy, na.rm=TRUE)

mean(percent.cat$prop.girl, na.rm=TRUE)




## ---- echo=TRUE-------------------------------------------------------------------------------------------------------------
#devtools::install_github("mbojan/isnar")

library(isnar)

mixingm(valente, "gender")

ei(valente, "gender")

assort(valente, "gender")

gamix(valente, "gender")



## ---- echo=TRUE-------------------------------------------------------------------------------------------------------------
source("jimi_iqv.r") 
#stanford function...see https://sna.stanford.edu/lab.php?l=2
v_iqv_gender <- as.data.frame(iqv(valente, "gender"))
quantile(v_iqv_gender, na.rm=TRUE)
mean(v_iqv_gender$egonet)


## ---- echo=TRUE-------------------------------------------------------------------------------------------------------------

graph.density(valente)



## ---- echo=TRUE-------------------------------------------------------------------------------------------------------------

valente_ego_dens <- make_ego_graph(valente, 1) %>%
vapply(graph.density, numeric(1))

head(valente_ego_dens)



## ---- echo=TRUE-------------------------------------------------------------------------------------------------------------

transitivity(valente)

transitive.valente <- transitivity(valente, type="local")
ego.size <- ego_size(valente)

tr.df <- data.frame(transitivity=transitive.valente, size=ego.size)



## ----message=FALSE, echo=FALSE----------------------------------------------------------------------------------------------
ggplot(data=tr.df,aes(transitivity,size))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())


## ----message=FALSE, echo=FALSE----------------------------------------------------------------------------------------------
plot.igraph(valente, vertex.size=transitive.valente*20, edge.arrow.size=.5)


## ---- echo=TRUE-------------------------------------------------------------------------------------------------------------

constraint.valente <- constraint(valente)
ego.size <- ego_size(valente)

con.df <- data.frame(constraint=constraint.valente, size=ego.size)



## ----message=FALSE, echo=FALSE----------------------------------------------------------------------------------------------
ggplot(data=con.df,aes(constraint,size))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())


## ----message=FALSE, echo=FALSE----------------------------------------------------------------------------------------------
plot.igraph(valente, vertex.size=constraint.valente*20, edge.arrow.size=.5)

