#This is the Homework 5 Cheatsheet

#Read the two graphs into R and load igraph

library(igraph)

k1 <- read.graph("Korea1.net", format="pajek")

k2 <- read.graph("Korea2.net", format="pajek")

#Turn the weird .clu files into useable vectors

adopt1 <- as.matrix(read.table("Korea1_adopters.clu", skip=1))

adopt2 <- as.matrix(read.table("Korea2_adopters.clu", skip=1))

#Store the adopt attribute (e.g. having adopted the family planning practice) as a node attribute

V(k1)$adopt <- adopt1

V(k2)$adopt <- adopt2

#Graph the two networks

plot(k1, vertex.color=as.factor(V(k1)$adopt))

plot(k2, vertex.color=as.factor(V(k2)$adopt))

#Make two data frames with the centrality scores of interest

k1.df <- data.frame(adopt=V(k1)$adopt, deg=degree(k1), close=closeness(k1), btwn=betweenness(k1, normalized=T), 
                    eig=eigen_centrality(k1)$vector)

k2.df <- data.frame(adopt=V(k2)$adopt, deg=degree(k2), close=closeness(k2), btwn=betweenness(k2, normalized=T), 
                    eig=eigen_centrality(k2)$vector)

#What's going on with closeness?

#Makes sense to use centralization to compare. Adoption of something like family planning
#strategy requires pretty 

centr_betw(k1)

mean(adopt1)

centr_betw(k2)

mean(adopt2)

#I choose betweenness because it is is a medial measure and seems to make sense for
#information diffusion. You can pick another one, but be sure to defend decision.

#library(Hmisc) #I think I deleted what I used this for

library(dplyr) #This is to manipulate data

library(ggplot2) #This is to plot pretty graphs


ggplot(k1.df, aes(x=as.factor(adopt), y=btwn)) + 
  geom_boxplot() +
  labs(title="Village 1",
        x ="Adopt Practice = 1", y = "Betweenness") +
  theme_bw()

ggplot(k2.df, aes(as.factor(adopt), btwn)) + 
  geom_boxplot() +
labs(title="Village 2",
     x ="Adopt Practice = 1", y = "Betweenness") +
theme_bw()


#Let's combine the data frames and use facet() to "panelize" the figures

k1.df$village <- "One"
k2.df$village <- "Two"

ktot.df <- rbind(k1.df, k2.df)

ggplot(ktot.df, aes(x=as.factor(adopt), y=btwn)) + 
  geom_boxplot() +
  labs(title="Comparing Villages",
       x ="Adopt Practice = 1", y = "Betweenness") +
  theme_bw()+
  facet_wrap(~ village)

#We could construct formal tests.

t.test(btwn ~ adopt, data = ktot.df,
       var.equal = TRUE, alternative = "less")

#We could run a logistic regression on the likelihood of adopting the practice

areg <- glm(adopt~btwn + as.factor(village), data=ktot.df, family="binomial")

summary(areg)