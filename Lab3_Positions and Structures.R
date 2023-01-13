##### Lab3: Position and Structure
library(statnet)

#change working directory
setwd("C:/Users/Cathy/Desktop/QTM 385 Network Analysis")

### Import data
data <- as.matrix(read.csv("Manager_friends.csv", header = T))

### Create a network object
Fnet <- network(data, directed = T)

### Import and set attributes for nodes
att <- read.csv("Manager_att.csv", header = T, row.names = 1 )

set.vertex.attribute(Fnet, "age", att$age )
set.vertex.attribute( Fnet, "age", att$age )
set.vertex.attribute( Fnet, "tenure", att$tenure )
set.vertex.attribute( Fnet, "level", att$level )
set.vertex.attribute( Fnet, "dept", att$dept )

#### Positional analysis
# structual brokerage
sb <- brokerage(Fnet, att$dept)
# Observed brokerage scores, by vertex. t column shows the total roles.
sb$raw.nli # e.g. x1 served t = 12 times as broker(4 times with w_I, 4 times with b_io, etc.), 2nd manager served 11 times as broker
?brokerage

# ?brokerage. First the number of actural brokerage roles. Then the expected number 
# of brokerage roles and its standard deviation. Z = (t - E(t)) / sd(t) in random networks.

?brokerage

# Structural equivalence
se <- sedist (Fnet, method = "euclidean") # use euclidean distance
se[upper.tri(se)] <- ""
write.table(se, file = "SE.csv", sep =",", col.names = F, row.names = F)

# Hierarchical clustering of network positions
eq <- equiv.clust (Fnet, method="euclidean") # use euclidean distance

png("Manager_HC.png", width = 900, height = 600 )
plot(eq, main="Hierarchical clustering of network positions", xlab = "", sub="")
dev.off()

## central vs. periphery by blockmodeling
# This approach maximizes structural equivalence within blocks.
# divide into blocks, let's do 2 blocks
block <- blockmodel(Fnet, eq, k = 2 )
block
# gives you density table, also tells you which manager belongs to which blocks


# Color the network by membership
cl <- block$block.membership[order(block$order.vector)]

plot.network(Fnet, displaylabels = T, boxed.labels = FALSE, vertex.cex = 0.6, label.cex = 1, 
             vertex.col = cl, label.col = "blue", edge.lwd = .5, edge.col = "gray50")

# Write out the block network
tab <- block[[5]]
write.csv(tab, file = "Manager_block.csv")

png("Manager_block.png", width = 900, height = 600 )
plot(block, displaylabels = F, xlab="", ylab="", main="Blockgrouping", sub="")
dev.off()







## Four blocks rather than 2
block <- blockmodel(Fnet, eq, k = 4)
block

png("Manager_block2.png", width = 900, height = 600 )
plot(block, displaylabels = F, xlab="", ylab="", main="Blockgrouping", sub="")
dev.off()




##### Randomization test for network structures
### Get a series of statistics for the observed network
mut <- grecip(Fnet, measure = "edgewise")
tran <- gtrans(as.matrix(Fnet))

### Randomize the network and get the statistics/estimates
## Fix the number of ties each actor sent 
## But randomize to whom the ties are directed to
## Vectors to store statistics from randomized networks
muts <- NULL
trans <- NULL

# Get the adjacency matrix
FMat <- as.matrix(Fnet)
K <- nrow(FMat)
# number of randomization
M <- 200

for (i in 1:M) {
  S <- sample(K)
  RMat <- FMat[, S]
  RNet <- network(RMat)

  muts <- append(muts, grecip(RNet, measure = "edgewise"))
  trans <- append(trans, gtrans(RNet))
}

boxplot(muts, ylim=c(0,0.9) )
abline(h=mut, col = "Red")
# we see higher mutuality than randomized network

boxplot(trans, ylim=c(0,0.5))
abline(h=tran, col = "Red")
# we see higher transitivity than randomized network
