#Here is some code by jimi adams and 
#(http://badhessian.org/2012/09/lessons-on-exponential-random-graph-modeling-from-greys-anatomy-hook-ups/) 
#with modifications by Light for working through the basics of ERGMs in statnet.


library(statnet)

##########################################################################
#	1.1 	A simple example comparing a logistic & ergm approach to fitting the same model
#			Created by: David Schaefer
##########################################################################
#  enter some data coded by hand
tie <- c(1,1,0,0,0,1,1,1,0,1)
nm <- c(1,0,0,1,0,0,1,1,0,0)
ks <- c(3,2,5,4,5,4,3,3,4,3)
tr <- c(0,0,2,1,2,1,1,0,1,1)

# enter the same data as a network object
par(mfrow=c(1,1))
net1 <- network(matrix(c(0,1,1,0,0, 1,0,0,1,1, 1,0,0,1,0, 0,1,1,0,1, 0,1,0,1,0),5,5),directed=F) 
network.vertex.names(net1) <- c('i','j','K','L','m')
set.vertex.attribute(net1,"cap",c(0,0,1,1,0))
plot.network(net1,displaylabels=T,usearrows=F)

# run logistic regression models
ex1a <- glm(tie ~ nm, family=binomial("logit"))
ex1b <- glm(tie ~ ks, family=binomial("logit"))
ex1c <- glm(tie ~ tr, family=binomial("logit"))
ex1d <- glm(tie ~ nm + ks , family=binomial("logit"))

# run ERG models
ex2a <- ergm(net1 ~ edges + nodematch("cap"))
ex2b <- ergm(net1 ~ edges + kstar(2) )
ex2c <- ergm(net1 ~ edges + triangle )
ex2d <- ergm(net1 ~ edges + nodematch("cap") + kstar(2))

# compare the output from the two approaches
cbind(ex1a$coef,ex2a$coef)  # nodematch terms are identical
cbind(ex1b$coef,ex2b$coef)  # kstar(2) terms are quite close (probably a miscount)
cbind(ex1c$coef,ex2c$coef)  # triangle terms NOT close (degeneracy in ERGM)
cbind(ex1d$coef,ex2d$coef)  # nodematch and kstar(2) terms don't work well together 

# The next several steps follow along with Goodreau et al. (2008), but mostly with the Grey's anatomy
#"hook-up" data

#4. Network Exploration

load("ergm_data.Rdata")


# The greys data should be in your environment now. Check:

greys

#Let's make an age variable

set.vertex.attribute(greys, "age", 2013-get.vertex.attribute(greys, "birthyear"))

#Before statistically modeling we should plot to get a better sense

la <- plot(greys)		#fix plot coordinates, so they don't jump around if we replot later

#Component size distribution

table(component.dist(greys)$csize)

#Network summary

summary(greys)

#Plot with attributes

plot.network(greys, coord=la, vertex.col=get.vertex.attribute(greys, "nrace"), 		# Numeric converstion of race variable
     label=get.vertex.attribute(greys, "name"), label.cex=.75, 		# Names
     vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 	# circles=women, squares=men
     vertex.cex=.05*(get.vertex.attribute(greys, "age")))	# size proportional to age

#Degree distributions are often interesting (note divide two or select cmode as network is undirected)

degree(greys)

gry.degreedist <− table(degree(greys)/2)

gry.degreedist 

#Compare degree distribution by sex

summary(greys ~ degree(0:8, "sex"))

#Triangle count

summary(greys~triangle)

summary(greys~cycle(4))

#Mixing matrix by Sex

mixingmatrix(greys, "sex")

#Mixing matrix by Race

mixingmatrix(greys, "race")

#5. Fitting an ERG model

#  Bernoulli model: Equal probalbility for all edges in network

model1 <- ergm(greys ~ edges)

summary(model1)

exp(−2.974)/(1 + exp(−2.974))

summary(greys)

#"edges" configuration is equivalent of density

#To extract coefficients and likelihood

model1$coef

model1$mle.lik[1]

#But we know this network is nonrandom and other factors likely contribute to network structure

#Let's look first at assorative mixing

 
#Sex homophily - use the nodematch term

model2 <- ergm(greys ~ edges + nodematch("sex"))

summary(model2)

model2$mle.lik[1]

#This improves the model a good bit

model2a <- ergm(greys ~ edges + nodematch("race"))

summary(model2a)

model2a$mle.lik[1]

#This doesn't improve the model that much

model2c <- ergm(greys ~ edges + nodematch("sex") + nodematch("race"))

summary(model2c)

model2c$mle.lik[1]

#Could interpret as odds of particular types of ties: Log-odds of a tie that is completely heterogenous is -2.7, 
#log-odds is homogenous by sex alone is -3.2, log odds of homogeneity sex and race (-2.8 + -3.2 +.8 = -5.2)

#Note that when inclduign nodematch terms one also would tyically include nodefactor as well ("main effects" vs.
#"interaction effects")

#Degeneracy is common in these models as local clustering often messes up the estimation. This is problematic b/c
#many social netwokrs exhibit high local clustering (think: Small Worlds). 

data("faux.magnolia.high")

model3 <− ergm(faux.magnolia.high ~ edges + triangle, control=control.ergm(seed=99))

library(coda)

pdf("C:/Users/Ryan Light/Documents/GitHub/week9_ergms/model3diagnostics.pdf")

mcmc.diagnostics(model3)

dev.off()

#Scholars have developed an alternative to triangles that seems robust to degeneracy: 
#The GWESP (geometrically weighted edgewise shared partner).

model4.take1 <− ergm(faux.magnolia.high ~ edges + nodematch("Grade") + nodematch("Race") + nodematch("Sex") + 
                       gwesp(0, fixed = TRUE), MCMCsamplesize = 1e+5, maxit = 15, verbose = TRUE, 
                       control = control.ergm(seed = 123))

model4.take1$coef 

summary(model4.take1)

#If two people differ on grade, race, and sex and they have no friends in common then the log odds of becoming 
#friends is =9.85. If they have nay number of friends in common and each is at least one other triange with each
#of those friends then the log odds is 

-9.85 + 1.79

#Let's look at some additional Greys models

model5 <- ergm(greys~edges+degree(1)+ kstar(2) + nodematch("sex")+ absdiff("age") + nodematch("race"))

summary(model5)

model5a <- ergm(greys~edges+degree(1)+ kstar(2) + nodematch("sex")+ absdiff("age") + nodematch("race", diff=T))

summary(model5a)

model5b <- ergm(greys~edges+degree(1)+ cycles(4) + nodematch("sex")+ absdiff("age") + nodematch("race", diff=T, 
                                                                keep=c(1,3)))

summary(model5b)

model5$mle.lik[1]

model5b$mle.lik[1]

