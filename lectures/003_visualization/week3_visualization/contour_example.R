#This script develops a contour sociogram similar to Moody and Light (2020). Contour sociograms within a network context
#provide a solution for visualizing densely connected networks by leveraged force-directed or spring-embedded layout
#algorithms that try to pull connected nodes together and push disconnected nodes apart.

library(ggplot2)
library(igraph)

#This is a network of overlapping papers that I read into a table as an edgelist
#due to some funky in read.graph(x, format="pajek").

pap.df <- read.table("data/papsim_edgelist.txt")

#make igraph object

pap.g <- graph_from_data_frame(pap.df)

pap.g

#first layout by fruchterman reingold

pap_fg <- layout.fruchterman.reingold(pap.g)

#plot is a hairball and takes a lot of time to render (see papsim_network.png)

png("fg_net.png", 600, 600)

plot.igraph(pap.g, layout=pap_fg, vertex.label=NA, vertex.size=2, edge.arrow.size = 0.1, edge.color=NA)

title("FG Layout Papers",cex.main=1,col.main="blue")

dev.off()


#We can pull out the coordinates now from the layout.

fg_coords <- as.data.frame(pap_fg)


#Now we can plot the density
fg_contour <- ggplot(fg_coords, aes(x=V1, y=V2)) +
  stat_density_2d(aes(fill=..level..), geom="polygon")


#Now we can test against alternative drl layout
pap_drl <- layout_with_drl(pap.g, dim=2)

png("drl_net.png", 600, 600)

plot.igraph(pap.g, layout=pap_drl, vertex.label=NA, vertex.size=2, edge.arrow.size = 0.1, edge.color=NA)

title("Drl Layout Papers",cex.main=1,col.main="blue")

dev.off()

drl_coords <- as.data.frame(pap_drl)

drl_contour <- ggplot(drl_coords, aes(x=V1, y=V2)) +
  stat_density_2d(aes(fill=..level..), geom="polygon") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )