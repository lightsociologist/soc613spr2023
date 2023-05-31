library(igraph)
library(dplyr)
library(readxl)

greys_network <- read_excel("data/greys_network.xlsx")

greys_nodes <- read_excel("data/greys_network.xlsx", 
                         sheet = "Sheet2")

greys.g <- graph_from_data_frame(greys_network)

plot(greys.g)

name_order <- data.frame(label=V(greys.g)$name)

name_order <- left_join(name_order, greys_nodes)

name_order$age <- 2022-as.numeric(name_order$birthyear)

V(greys.g)$gender <- name_order$sex

V(greys.g)$birthyear <- as.numeric(name_order$birthyear)

V(greys.g)$nrace <- name_order$nrace

V(greys.g)$race <- name_order$race

V(greys.g)$age <- name_order$age

name_order$female <- ifelse(name_order$sex=="F", 1, 0)

V(greys.g)$female <- name_order$female

greys.g <- as.undirected(greys.g)

saveRDS(greys.g, file=("data/greys_graph.RDS"))

