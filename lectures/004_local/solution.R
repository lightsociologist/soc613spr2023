#Sketch of a solution for HW4

library(igraph)
library(ggplot2)

#Bring in the Data

sch23 <- read.graph("sch23.net", format="pajek")	# getting the adjacency matrix
att <- read.table("sch23_attr.txt", header=T, sep=" ")

att$female <- ifelse(att$sex==1, 1, 0)

V(sch23)$female <- att$female
V(sch23)$race <- att$race
V(sch23)$grade <- att$grade
V(sch23)$school <- att$school

#1A

#Degree
v_deg<-degree(sch23)
v_indeg <- degree(sch23, mode="in")					
v_outdeg <- degree(sch23, mode="out")	

#Density

graph.density(sch23)

sch23_ego_dens <- make_ego_graph(sch23, 1) %>%
  vapply(graph.density, numeric(1))

head(sch23_ego_dens)

ego.size <- ego_size(sch23)

den.df<-data.frame(density=sch23_ego_dens, size=ego.size)

#Transitivity

transitivity(sch23)

transitive.sch23 <- transitivity(sch23, type="local")

tr.df <- data.frame(transitivity=transitive.sch23, size=ego.size)

#Constraint

constraint.sch23 <- constraint(sch23)
ego.size <- ego_size(sch23)

con.df <- data.frame(constraint=constraint.sch23, size=ego.size)

#1B

mean(v_deg)

#2A

adjmat <- as_adjacency_matrix(sch23)

girl <- ifelse(att$sex==1,1,0)
boy <- ifelse(att$sex==2, 1, 0)

gendermat <- cbind(girl, boy)

multiplication <- adjmat %*% gendermat

percent.cat <- data.frame(toboy=multiplication[,2], togirl=multiplication[,1], gender=gendermat, outgree=v_outdeg)

percent.cat$prop.boy <- percent.cat$toboy/percent.cat$outgree

percent.cat$prop.girl <- percent.cat$togirl/percent.cat$outgree

mean(percent.cat$prop.boy, na.rm=TRUE)

mean(percent.cat$prop.girl, na.rm=TRUE)

mixmat <- t(gendermat) %*% multiplication

print(mixmat)

#Construct IQV..note that categories need to start with 0.
#Hence recoding above (e.g. att$female <- ifelse(att$sex==1, 1, 0))

source("stanford_iqv.R") #stanford's SNA lab function (https://sna.stanford.edu/lab.php?l=2). Note the extensive
#explanation in the staford_iwv.r file

v_iqvs_gender <- get_iqvs(sch23, "female")

iqvs_gender.df <- as.data.frame(v_iqvs_gender)

print(v_iqvs_gender)

quantile(v_iqvs_gender, na.rm=TRUE)

#Plot Degree Distribution

sch23.degree <- data.frame(v_deg, v_indeg, v_outdeg)

ggplot(data=sch23.degree, aes(v_deg)) + 
  
  geom_histogram(breaks=seq(2,14, by=1),
                 col="black", 
                 fill="black", 
                 alpha = .2)+
  labs(title="Total Degree Distribution") +
  labs(x="Degree", y="Count")

#Plot Transitivity by Size

ggplot(data=tr.df,aes(transitivity,size))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())

#Plot Density by Size

ggplot(data=den.df, aes(density,size))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())

#Plot Constraint by Size

ggplot(data=con.df, aes(constraint,size))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())

#Plot Transitivity

ggplot(data=tr.df,aes(transitivity,size))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())


##Plot Degree of graph

V(sch23)$degree <- v_deg

is.connected(sch23)

plot.igraph(sch23, vertex.size=V(sch23)$degree/2, edge.arrow.size=.5,
            vertex.label=NA)


giant.component <- function(graph, ...) {
  cl <- clusters(graph, ...)
  induced_subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

sch23.sub <- giant.component(sch23)

is.connected(sch23.sub)

plot.igraph(sch23.sub, vertex.size=V(sch23.sub)$degree/5, edge.arrow.size=.1,
            vertex.label=NA, vertex.color=V(sch23.sub)$female+1)



