#This is a "cheetsheet" for homework #3

#You will need the following packages

library(igraph)
library(readr)
library(RColorBrewer)

#first section: draw this graph

#Look at the edges and list them. Each pair is a directed tie.

edges <- c(1,2, 1,3, 1,6, 2,3, 2,5, 3,4, 3,5, 6,2)

#Make a vector of colors that correspond with each node in order

colors <- c("yellow", "red", "green", "yellow", "red", "green")

#Read the directed edge list using the graph function

t.g <- graph(edges, directed=T)

plot(t.g, vertex.color=colors, edge.arrow.size=.5)

#Plot the colors vertex. You could also assign V(t.g)$color and then you don't need to set that feature in the plot.

V(t.g)$color <- colors

plot(t.g, edge.arrow.size=.5)



#second section: game of thrones network

#Bring in the csv file 

stormofswords <- read_csv("stormofswords.csv") #make sure that this points to where your downloaded file is located

#Inspect the thing that you loaded

head(stormofswords)

#We can see that this is a weighted edgelist in a data frame. So, we can use graph_from_data_frame to make an igraph object.

thr.g <- graph_from_data_frame(stormofswords)

#Let's take a peek at the graph summary

thr.g

#Initial plot

plot(thr.g)

#Messy and a bit boring...let's add more information and clean up

load("~/Documents/GitHub/sna_course/week_3_visualization/week3_visualization/data/got_att.rda") #This point to where attribute file is located

#Inspect thing

head(got_att)

#We can see there are names, degree, and the house (0="Other", 1="Lannister", 2="Stark", 3="Tagaryen", 4="Baratheon")

#Let's store those in the vertices

V(thr.g)$degree <- got_att$degree

V(thr.g)$house <- got_att$house

#And plot with the setting vertex color by house and vertex size by degree, and edge width by weight

plot(thr.g, vertex.color=V(thr.g)$house+1, vertex.size=V(thr.g)$degree/2, edge.arrow.size=.1, edge.width=E(thr.g)$Weight/10,
     layout=layout_with_kk)

#It might be nice to include a legend. The Moody and Light 2020 tutorial describes how to accomplish that.

#Start with the RColorBrewer Package

library(RColorBrewer)

#Index the colors

pal <- brewer.pal(5, "Accent")

#Connect the colors to the house attribte as a factor

vertex.col <- (pal[as.numeric(as.factor(vertex_attr(thr.g, "house")))])

#plot...note I adjust the color by making more transparent (alpha=)

plot(thr.g, vertex.color=adjustcolor(vertex.col, alpha=.75), vertex.size=V(thr.g)$degree/2, edge.arrow.size=.1, 
     edge.width=E(thr.g)$Weight/10, layout=layout_with_kk)

#Add legend (see https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/legend)

legend("topleft", title="House", bty = "n", #set location, legend title, and box type
       legend=c("Other","Lannister", "Stark", "Tagaryen", "Baratheon"), #legend words
       fill=pal, border=NA) #fill of circles and we don't need a border

#Done
