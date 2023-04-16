#This program shows one way of taking affiliation data in the standard person-row form and building a bipartite 
#(two-mode) network and then projecting to one-mode networks.

library(igraph)
library(dplyr)

#Here is a dataframe with persons, colleges, and towns.

t.df <- data.frame (person  = c("A", "B", "C", "D", "E", "F", "G", "H"),
                  college = c("West", "West", "East", "North", "South", "West", "East", "South"),
                  town = c("Eugene", "Springfield", "Cottage Grove", "Eugene", "Springfield", "Eugene", "Cottage Grove", "Eugene")
)

#First let's focus on colleges. By selecting these two columns we already have a bipartite edge list of persons and their
#colleges.

per.col <- t.df %>% select(person, college)

#The edge list is in a data frame so we can use graph_from_data_frame().

pcol.g <- graph_from_data_frame(per.col)

pcol.g

#There is no "b" in the fourth column of the summary as we have to let igraph know that there are two different types of
#nodes.

#Fortunately, igraph has a function to help with this. bipartite_mapping identifies this difference.

bipartite_mapping(pcol.g)

#We store the outcome of this function as a vertice attribute called "type."

V(pcol.g)$type <- bipartite_mapping(pcol.g)$type

pcol.g

#And now we see the "b" in the fourth column indicating that the graph is a bipartite or two-mode network. We can plot it and see.

plot(pcol.g)

#Let's make this super clear by changing the vertex shapes and colors by type.

V(pcol.g)$color <- ifelse(V(pcol.g)$type, "lightblue", "salmon")
V(pcol.g)$shape <- ifelse(V(pcol.g)$type, "circle", "square")

plot(pcol.g)


#Ok, but most folks just want to know how people connect via their overlapping affiliations. bipartite_projection does
#the simple math for that.

one_mode <- bipartite_projection(pcol.g) 

#The result of this function is two graphs. One for one more (persons) and one for the other (colleges)

#The person one, here, is stored as the first type. The edges are people who went to the same college.

p.g <- one_mode[[1]]

#And here we can see that it worked!

plot(p.g)

#Here is the college one. The edges are overlapping people across two schools.

c.g <- one_mode[[2]]

plot(c.g)

#We can see that no one went to two schools!


#What about if we have more than one affiliation of interest?

#There might be a better way to do this, but my sense is to make two edge lists and append.

p.town <- t.df %>% select(person, town)

#I reanme as affiliation in the two edge lists (person-college, person-town) for binding.

p.town <- p.town %>% rename("affiliation"="town")

per.col <- per.col %>% rename("affiliation"="college")

#And make a total edge list.

tot.list <- bind_rows(per.col, p.town)

#And now we do the same steps. Make a graph from dataframe.

tot.g <- graph_from_data_frame(tot.list)

tot.g

#Bipartite map and store as type.

bipartite_mapping(tot.g)

V(tot.g)$type <- bipartite_mapping(tot.g)$type

tot.g

#Plot the two mode. 

plot(tot.g)

#Bipartite projection to locate the one mode networks

tone_mode <- bipartite_projection(tot.g) 

ptot.g <- tone_mode[[1]]

#Here is the one mode network where people are connected if they share either town or college or both.

plot(ptot.g)

#Here is the one mode network where afilliations (e.g. colleges and towns) are connected if they share people in common.

a.g <- tone_mode[[2]]

plot(a.g)
