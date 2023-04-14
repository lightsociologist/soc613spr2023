## ----setup, include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo=T, message=FALSE-----------------------------------------------------

library(statnet)

load("ergm_data.Rdata")



## ---- echo=T, message=FALSE-----------------------------------------------------

set.vertex.attribute(greys, "age", 2013-get.vertex.attribute(greys, "birthyear"))



## ---- echo=T, message=FALSE-----------------------------------------------------

la <- plot(greys)		#fix plot coordinates, so they don't jump around if we replot later

#Component size distribution

table(component.dist(greys)$csize)

#Plot with attributes

plot.network(greys,  vertex.col=get.vertex.attribute(greys, "nrace"), 		# Numeric conversion of race variable
     label=get.vertex.attribute(greys, "name"), label.cex=.75, 		# Names
     vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 	# circles=women, squares=men
     vertex.cex=.05*(get.vertex.attribute(greys, "age")))	# size proportional to age



## ---- echo=T, message=FALSE-----------------------------------------------------

degree(greys)

gry.degreedist <- table(degree(greys)/2)

gry.degreedist 

#Compare degree distribution by sex

summary(greys ~ degree(0:8, "sex"))



## ---- echo=T, message=FALSE-----------------------------------------------------


#Triangle count

summary(greys~triangle)

summary(greys~cycle(4))



## ---- echo=T, message=FALSE-----------------------------------------------------

#Mixing matrix by Sex

mixingmatrix(greys, "sex")

#Mixing matrix by Race

mixingmatrix(greys, "race")



## ---- echo=T, message=TRUE------------------------------------------------------

model1 <- ergm(greys ~ edges)

summary(model1)



## ---- echo=T, message=TRUE------------------------------------------------------

plogis(coef(model1))

network.density(greys)



## ---- echo=T, message=TRUE------------------------------------------------------

model2 <- ergm(greys ~ edges + nodefactor("sex"))

summary(model2)



## ---- echo=T, message=TRUE------------------------------------------------------

model1$mle.lik[1]
model2$mle.lik[1]

model1$glm$aic
model2$glm$aic


## ---- echo=T, message=TRUE------------------------------------------------------

model3 <- ergm(greys ~ edges + nodematch("sex"))

summary(model3)

exp(coef(model3))



## ---- echo=T, message=TRUE------------------------------------------------------

model1$mle.lik[1]
model3$mle.lik[1]

model1$glm$aic
model3$glm$aic


## ---- echo=T, message=FALSE-----------------------------------------------------

model3a <- ergm(greys ~ edges + nodematch("race"))

summary(model3a)

exp(coef(model3a))



## ---- echo=T, message=FALSE-----------------------------------------------------
model3c <- ergm(greys ~ edges + nodematch("sex") + nodematch("race"))

summary(model3c)

model3c$mle.lik[1]



## ---- echo=T, message=T, error=T------------------------------------------------

model4 <- ergm(greys ~ edges + degree(1))



## ---- echo=T--------------------------------------------------------------------

model4a <- ergm(greys~edges + degree(1) + nodematch("sex") + nodematch("race"))

summary(model4a)

exp(coef(model4a))



## ---- echo=T--------------------------------------------------------------------

model4b <- ergm(greys ~ edges + degree(1) + kstar(2) + nodematch("sex") + nodematch("race"), control=control.ergm(seed=50, SAN.maxit = 6))

summary(model4b)

model4a$mle.lik[1]
model4b$mle.lik[1]



## ---- echo=T--------------------------------------------------------------------

gof4b <- gof(model4b)

plot(gof4b)




## ---- echo=T--------------------------------------------------------------------


mcmc.diagnostics(model4b)



## ---- echo=T--------------------------------------------------------------------

exp(coef(model4b))

plogis(coef(model4b))



## ---- echo=F--------------------------------------------------------------------

library(knitr)
purl("w9_ergm_wksp.Rmd")


