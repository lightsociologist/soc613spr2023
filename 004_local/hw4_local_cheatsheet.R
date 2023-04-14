#This is a "cheetsheet" for homework #4

#You will need the following packages

library(igraph)
library(ggplot2)

#Here we bring in the data using read.graph

sch23 <- read.graph("sch23.net", format="pajek")	# getting the adjacency matrix

#We can load the attributes file using read.table
att <- read.table("sch23_attr.txt", header=T, sep=" ")

#Make a variable for students gender
att$girl <- ifelse(att$sex==1, 1, 0)

#We can identify other attributes
V(sch23)$girl <- att$girl
V(sch23)$race <- att$race
V(sch23)$grade <- att$grade
V(sch23)$school <- att$school

#1 Here we go through the cosntruction of the local network measures

#Here we look at degree
v_deg<-degree(sch23)
v_indeg <- degree(sch23, mode="in")					
v_outdeg <- degree(sch23, mode="out")	

#Here we look at density

#Graph denisty is density for the entire graph
graph.density(sch23)

#Here we get for each ego: Note that density returns NaN for isolates 
sch23_ego_dens <- make_ego_graph(sch23, 1) %>%
  vapply(graph.density, numeric(1))

head(sch23_ego_dens)

ego.size <- ego_size(sch23)

#Here I make a data frame with ego size for plotting later, but could do this more efficiently by combining dfs at the end

den.df<-data.frame(density=sch23_ego_dens, size=ego.size)

#Here we calculate transitivity which is also called the clustering coefficient

transitivity(sch23)

transitive.sch23 <- transitivity(sch23, type="local", isolates="zero") #We have to set isolates to "zero" otherwise NaN

tr.df <- data.frame(transitivity=transitive.sch23, size=ego.size)

#Here we calculate constraint (Note that constraint is undefined for isolates)

constraint.sch23 <- constraint(sch23)

ego.size <- ego_size(sch23)

con.df <- data.frame(constraint=constraint.sch23, size=ego.size)


#2 Let's use our two ways of evaluating how attributes are distributed in local graphs

#First mixing matrices

source("mixmat.R") #Here we use a function to do the matrix algebra for us

mixmat(sch23, "girl", use.density=F)

#Construct IQV..note that categories need to start with 0.
#Hence recoded above (e.g. att$girl <- ifelse(att$sex==1, 1, 0))

source("jimi_iqv.R") #Here we use jimi's version of the Argesti and Argesti (1978:208) approach. 
#Note that this will take a couple of minutes

v_iqv_gender <- as.data.frame(iqv(sch23, "girl"))

quantile(v_iqv_gender, na.rm=TRUE)

mean(v_iqv_gender$egonet)

#Bonus: Build random grpah for quick comparison 

#Here we use erdos.renyi.game to build a graph with the same properties, but with edges randomly drawn

rg <- erdos.renyi.game(length(V(sch23)), length(E(sch23)), type="gnm", directed=T)  # a random graph with the same number of nodes & edges as the Valente graph above


table(V(sch23)$girl)


V(rg)$girl <- sample(c(0,1), length(V(rg)), replace=T, prob=c(310/679, 369/37))  #randomly assigning gender, matching the probabilities from the Valente graph

rg_iqv_gender <- as.data.frame(iqv(rg, "girl"))

quantile(rg_iqv_gender, na.rm=TRUE)

mean(rg_iqv_gender$egonet)

mixmat(sch23, "girl", use.density=F)

mixmat(rg, "girl", use.density=F)

#Graph and Interpret Results

#Plot Degree Distribution

sch23.degree <- data.frame(v_deg, v_indeg, v_outdeg)

ggplot(data=sch23.degree, aes(v_deg)) + 
  
  geom_histogram(breaks=seq(2,14, by=1),
                 col="black", 
                 fill="black", 
                 alpha = .2)+
  labs(title="Total Degree Distribution") +
  labs(x="Degree", y="Count")


##Plot Degree of Graph and Gender

V(sch23)$degree <- v_deg

is.connected(sch23)

plot.igraph(sch23, vertex.size=V(sch23)$degree/5, vertex.color=V(sch23)$girl+2, edge.arrow.size=.01,
            vertex.label=NA)



#Example Rough Bonus Plots

#Plot Transitivity by Size

ggplot(data=tr.df,aes(size, transitivity))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())

#Plot Density by Size (Note error related to isolates)

ggplot(data=den.df, aes(size, density))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())

#Plot Constraint by Size (Note error related to isolates)

ggplot(data=con.df, aes(size, constraint))+
  geom_point()+
  geom_smooth(method='lm', level=.95, formula=y~x)+
  theme(panel.background = element_blank())

