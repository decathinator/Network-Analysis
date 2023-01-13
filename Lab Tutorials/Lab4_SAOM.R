##### Lab4: SAOM

library(RSiena)
library(statnet)

#change working directory
setwd("C:/Users/Cathy/Desktop/QTM 385 Network Analysis")

##### Importing data #######
# Make sure the data is in your working directory 
friend.data.w1 <- as.matrix(read.table("s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("s50-network3.dat"))
drink <- as.matrix(read.table("s50-alcohol.dat"))
smoke <- as.matrix(read.table("s50-smoke.dat")) # 3 means smoke a lot
age <- as.matrix(read.csv("s50-age.csv", header = T)) # time invariant covariate - doesn't change over time

##### set up the networks #############
net1 <- as.network(friend.data.w1)
net2 <- as.network(friend.data.w2)
net3 <- as.network(friend.data.w3)

par( mfrow = c( 1, 3 ) )
coordin <- plot( net1 ) 
plot( net2, coord = coordin ) # fix nodes to the coordinates of the first network
plot( net3, coord = coordin )

##### preparing data for RSiena ####

# Create network object
# The integers in the dim() here refer to the number of nodes (senders=50, 
# receivers=50) and the number of waves/timepoints (3 waves)
friendship <- sienaNet(array( c( friend.data.w1, friend.data.w2, friend.data.w3 ), dim = c( 50, 50, 3 ) ) )  

# Create behavior object
drinking <- sienaNet( drink, type = "behavior" ) # dependent variable

# Create a time varying covariate of smoking
smoke <- varCovar( smoke )

# Create a time constant covariate  
age <- coCovar( as.vector(age) )

# Create dataset
mydata <- sienaDataCreate( friendship, drinking, smoke, age ) 


#################################################
#######Coevolution of networks and behavior######
#################################################

# Model specification for coevolution . Will get some of the default effects
myeff <- getEffects(mydata)

### Network dynamics
# Covariate effects
# ego - sender, alt - receiver, sim - homophily

myeff <- includeEffects(myeff, egoX, altX, simX, interaction1="smoke")
myeff <- includeEffects(myeff, egoX, altX, simX, interaction1="age")

# Local network structure
# transtrip - transitivity, inpopsqrt - preferential attachment (indegree popularity. ppl that have a lot of ties already receive more ties later on) but the square root
# outpopsqrt - outdegree popularity sowhether ppl who send out a lot of ties receive ties later on
myeff <- includeEffects(myeff, transTrip, inPopSqrt, outPopSqrt)

### To distinguish effect on creation of new ties (creation) from effect of maintaining old ties (endowment).
# myeff <- includeEffects(myeff, egoX, altX, simX, type = "creation", interaction1="smoke")
# myeff <- includeEffects(myeff, egoX, altX, simX, type = "endow", interaction1="smoke")

# Effect of behavior on network (e.g., selection of friends based on homophilous behavior)
myeff <- includeEffects(myeff, egoX, altX, simX, interaction1="drinking")

# Interaction between behavior and network
# Drinkers are more likely to form mutual ties?
# myeff <- includeInteraction( myeff, egoX, recip, interaction1 = "drinking" )


### Behavior dynamics
# Peer influence and network effects
myeff<-includeEffects(myeff, name="drinking", avAlt, indeg, outdeg, interaction1="friendship")

# Covariate effects
myeff<-includeEffects(myeff, name="drinking", effFrom, interaction1="smoke") # smoke a lot and drink a lot?
myeff<-includeEffects(myeff, name="drinking", effFrom, interaction1="age") # older kids morer likely to drink?

### Show my effects
myeff


### Define and estimate the model
mymodel <- sienaModelCreate(useStdInits = TRUE, projname = 'drinking', n3 = 1000) # proj name is project name. useStdInits automatically gives the guess to start...
# ...simulating the network. and use 1000 sequences of networks

# estimate. answer1.  The silent is to see the cooking process. See the messages in batch. nbrNodes depends on the computing nodes in your computer. 
ans1 <- siena07(mymodel, data=mydata, effects=myeff, silent = F, batch=TRUE, nbrNodes=2, useCluster=TRUE, seed = 385)

# sqrt the thing to get std error
est <- cbind(ans1$theta, sqrt(diag(ans1$covtheta)), ans1$tstat)

rname <- myeff$effectName[myeff$include==T]

write.csv(est, "est1_RSiena.csv", row.names = rname )


##### Diagnostics and Re-estimate #####

# If the estimation algorithm has not produced good estimates
# (it 'has not converged well'),
# as will be indicated by many of the t-ratios for convergence
# being larger than 0.1
# the best thing to do is continuing the estimation,
# using the estimates produced here,
# This is done by the option prevAns ('previous ans') as in

ans2 <- siena07(mymodel, data=mydata, effects=myeff, prevAns = ans1, silent = F, batch=TRUE, nbrNodes=2, useCluster=TRUE)
ans2


