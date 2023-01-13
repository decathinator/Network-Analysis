#### Labs 2: Fitting ERGMs

library(statnet)

#change working directory
setwd("C:/Users/Cathy/Desktop/QTM 385 Network Analysis")

### Import data
data <- as.matrix(read.csv("Manager_friends.csv", header = T))

### Create a network object
Fnet <- network(data, directed = T)

### Import and set attributes for nodes
att <- read.csv("Manager_att.csv", header = T, row.names = 1 )

# atrributes - covariates
set.vertex.attribute(Fnet, "age", att$age ) 
set.vertex.attribute( Fnet, "tenure", att$tenure )
set.vertex.attribute( Fnet, "level", att$level )
set.vertex.attribute( Fnet, "dept", att$dept )

### Model 1: Covariates
## Covariate Effects
# nodeicov: receiver effects for continuous variables
# nodeocov: sender effects for continuous variables
# nodecov: connection effects for continuous variables - don't distinguish between directions
# nodeifactor, nodeofactor, and nodefactor for categorical variables

## Homophily
# absdiff: difference for continuous variables
# nodematch: being the same for binary/categorical variables

model1 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") + 
                nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept") ) # nodematch is to test homophily, nodefactor is for popularity
summary(model1)
# est <- as.matrix(summary(model1)$coef)[,c(1,2,4)]
# write.csv(est, file = "est1.csv")

### Model 2: Covariates + Local Structures
# The GW terms help model convergence.
# Instead of using raw measures, 
# the GW terms weighted down additional similar structures.-- so if you have 1 common friend, it's 1, but the next friend is 0.9, and the next is 0.81, etc. 
# So you can reduce the variance so the model is easier to converge
# control the weight with 0.1
# gwidegree is why we have connection from i to j?
# gwesp is transitive triangle
# gwdsp is for two paths
# mcmc burnin is you need to learn how to cook, so you throw out the first couple sandwiches, throw out 10000 first
# mcmc interval is pick one, throw out a bunch, pick another, throw out a bunch, etc. Don't pick in a row b/c the fire burnt the sandwiches around. So wait 200
# set seed to replicate results

# mutual is reciprocity
# gw stands for geometrically weighted
# gwidegree is the term for describing preferential attachment - the tendency for a person to send a tie to a popular other (person who gets lot of ties)
# gwodegree is differential sociality - you prefer to send ties to a person who already sends out a lot of ties
# gwesp is transitivity
# gwdsp is a term to control two-path 

# fixed = false : algorithm chooses ideal value

model2 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") +
               nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept")+
               mutual + gwidegree(0.1, fixed = TRUE) + gwesp(0.1, fixed = TRUE) + gwdsp(0.1, fixed = TRUE),
               control = control.ergm(MCMLE.maxit = 300, MCMC.burnin=10000, MCMC.interval=200, seed = 178 ))
summary(model2)
# est <- as.matrix(summary(model2)$coef)[,c(1,2,4)]
# write.csv(est, file = "est2.csv")

### Compare models
AIC <- rbind(AIC(model1), AIC(model2))
AIC
# smaller is better
# second model is much better

## Check goodness-of-fit of the better model
# plots network features
## The solid lines represent observed statistics - features of observed networks
## The dashed lines represent simulated statistics- the features of simulated networks
## That solid lines are within the 95% confidence intervals of the simulated lines
## indicates model convergence.
gof2 <- gof(model2)
par(mfrow=c(1,5))
plot(gof2)

### Model 3: Covariates + Local Structures + Multiplex Network
# Use the advice-seeking network as a predictor
advice <- as.matrix(read.csv("Manager_advice.csv", header = T))
anet <- network(advice, directed = T)

# Whether advice-seeking relationships lead to friendships
model3 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") +
                 nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept")+
                 mutual + gwidegree(0.1, fixed = TRUE) + gwesp(0.1, fixed = TRUE) + gwdsp(0.1, fixed = TRUE) +
                 edgecov(anet), control = control.ergm(MCMLE.maxit = 300, MCMC.burnin=10000, MCMC.interval=200, seed = 178 ))
summary(model3)
# Not signficiant, so advice seeking ties don't lead to friendship ties

### Sometimes if the model does not converge, one can force using logistic regression by setting the "MPLE" option.
# naive logistic regression
### Model 4: MPLE
model4 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") +
                 nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept")+
                 mutual + gwidegree(0.1, fixed = TRUE) + gwesp(0.1, fixed = TRUE) + gwdsp(0.1, fixed = TRUE) +
                 edgecov(anet), estimate = "MPLE", control = control.ergm(MCMLE.maxit = 300, MCMC.burnin=10000, MCMC.interval=200, seed = 178 ))
summary(model4)

