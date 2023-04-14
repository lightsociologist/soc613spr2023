

library(igraph)
library(RColorBrewer)

sch4 <- read.graph("data/sch4.net", format="pajek")	

sch4_attr <- read.table("data/sch4_attr.txt", header=TRUE)

sch4 <- set_vertex_attr(sch4, "grade", index = V(sch4), sch4_attr$grade)
sch4 <- set_vertex_attr(sch4, "race", index = V(sch4), sch4_attr$race) 



popularity <- runif(291, min = 1, max = 10)       

V(sch4)$popularity <- popularity

sch4 <- delete.vertices(sch4, V(sch4)$grade==0)


plot(sch4)

isolates <- which(degree(sch4)==0)

sch4.comp <- delete.vertices(sch4, isolates)

plot(sch4.comp, vertex.label=NA)


plot(sch4.comp, vertex.label=NA, edge.width=E(sch4.comp)$weight/2, vertex.size=4, edge.arrow.size = 0.1)

plot(sch4.comp, vertex.label=NA, vertex.color=V(sch4.comp)$grade+1, edge.width=E(sch4.comp)$weight/2, vertex.size=4, edge.arrow.size = 0.1)

plot(sch4.comp, vertex.label=NA, vertex.color=V(sch4.comp)$grade+1, edge.width=E(sch4.comp)$weight/2,  
     vertex.size=V(sch4.comp)$popularity*.75, edge.arrow.size = 0.1)

G_Grouped = sch4.comp
E(G_Grouped)$weight = 1

## Add edges with high weight between all nodes in the same group
for(i in unique(V(sch4.comp)$grade)) {
  GroupV = which(V(sch4.comp)$grade == i)
  G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=1.5))
} 

## Now create a layout based on G_Grouped
set.seed(567)
LO = layout_with_fr(G_Grouped)


pal <- brewer.pal(6, "Accent")

vertex.col <- (pal[as.numeric(as.factor(vertex_attr(sch4.comp, "grade")))])

plot(sch4.comp, vertex.label=NA, vertex.color=adjustcolor(vertex.col, alpha=.5), edge.width=E(sch4.comp)$weight/2,  
     vertex.size=V(sch4.comp)$popularity*.75, edge.arrow.size = 0.1, layout=LO)

legend("topleft",bty = "n",
       legend=levels(as.factor(V(sch4.comp)$grade)),
       fill=pal, border=NA)
