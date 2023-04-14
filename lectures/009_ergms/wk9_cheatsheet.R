
library(statnet)


load("~/Documents/GitHub/sna_course/week_9_ergms/homework/peoria.RDA")

plot.network(peoria.n)

summary(peoria.n)

model1 <- ergm(peoria.n ~ edges)

summary(model1)

#The Bernoulli model suggests that Peoria network is less dense than we'd expect.


model2 <- ergm(peoria.n ~ edges + nodematch("time"))

summary(model2)

#log-odds of different types of ties: heterogenous=-3.33/homogenous by number of years in community=.667
#But note that we aren't really tapping into network effects and the MCMC isn't being deployed

model3 <- ergm(peoria.n ~ edges + nodematch("time") + triangle)

#Let's check triangles...degeneracy!

model3b <- ergm(peoria.n ~ edges + nodematch("time") + gwesp(0, fixed = TRUE))

summary(model3b)

#Let's used the geometrically weighted edgewise shared partner to evaluate local clustering
# This shows that if a pair of nodes "have any positive number of friends in common and each of them
#is in at least one other triangle with each of those friends" then log-odds of a tie between them increases to
#around -3.

model4 <- ergm(peoria.n ~ edges + nodematch("time") + gwesp(0, fixed = TRUE) + mutual,
               control=control.ergm(parallel=4, parallel.type="PSOCK"))

summary(model4)

#And reciprocity has a very large positive effect on tie formation improving the log-odds to nearly -2.

md4.gof <- gof(model4)

plot(md4.gof)

mcmc.diagnostics(model4)

#The gwesp statistics bounce around a little in terms of fit, but ultimately are probably as the gwesp.0 is fine. The evaluation of degeneracy indicates that this isn't a problem.
