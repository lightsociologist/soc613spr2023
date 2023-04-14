library(readr)
library(tidyr)
library(igraph)

#first section: draw this graph

edges <- c(1,2, 1,3, 1,6, 2,3, 2,5, 3,4, 3,5, 6,2)

colors <- c("yellow", "red", "green", "yellow", "red", "green")

t.g <- graph(edges, directed=T)

plot(t.g, vertex.color=colors, edge.arrow.size=.5)




stormofswords <- read_csv("stormofswords.csv")

nams <- pivot_longer(stormofswords, Target:Source)

namecnt <- as.data.frame(table(nams$value))

namecnt$house<-0

namecnt$house <- if_else(namecnt$Var1=="Tyrion",1,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Robb",2,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Sansa",2,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Jon",2,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Tywin",1,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Catelyn",2,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Arya",2,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Jaime",1,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Cersei",1,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Daenerys",3,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Eddard",2,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Joffrey",1,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Bran",2,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Robert",4,namecnt$house)

namecnt$house <- if_else(namecnt$Var1=="Stannis",4,namecnt$house)

namecnt <- namecnt %>% rename("degree"="Freq")



thr.g <- graph_from_data_frame(stormofswords)

plot(thr.g)

onam <- as.data.frame(V(thr.g)$name)

colnames(onam)[1] <- "names"

onam <- left_join(onam, namecnt, by=c("names"="Var1"))

got_att <- onam

V(thr.g)$degree <- got_att$degree

V(thr.g)$house <- got_att$house

plot(thr.g, vertex.color=V(thr.g)$house+1, vertex.size=V(thr.g)$degree/2, edge.arrow.size=.1, edge.width=E(thr.g)$Weight/10,
     layout=layout_with_kk)

library(RColorBrewer)

pal <- brewer.pal(6, "Accent")

vertex.col <- (pal[as.numeric(as.factor(vertex_attr(thr.g, "house")))])

plot(thr.g, vertex.color=adjustcolor(vertex.col, alpha=.75), vertex.size=V(thr.g)$degree/2, edge.arrow.size=.1, 
     edge.width=E(thr.g)$Weight/10, layout=layout_with_kk)

legend("topleft", title="House", bty = "n",
       legend=c("Other","Lannister", "Stark", "Tagaryen", "Baratheon"),
       fill=pal, border=NA)

save(got_att, file="data/got_att.rda")