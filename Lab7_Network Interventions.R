#### Lab 7: Network Interventions

library(sna)
library(keyplayer)
library(NbClust)

#change working directory
setwd("C:/Users/Cathy/Desktop/QTM 385 Network Analysis")

### Import data
data <- read.csv("Manager_friends.csv", header = T)
dat <- as.matrix(data)

# data2 <- read.csv("Manager_advice.csv", header = T)
# dat2 <- as.matrix(data2)

##### Selection of Key Players
### Calculate the outdegree

deg <- degree(dat, cmode = "outdegree") # use outdegree rather than indegree

names(deg) <- seq(1, 21, 1)

# Find the ids of the most central four individuals
ids <- as.numeric(names(sort(deg, decreasing=T)[1:4]))
ids

# Find their group centrality for the four people as a group
kpcent(dat,ids,type="degree", cmode="outdegree", method = "max")
  # they connect to 10 people

# now use key player method
# Find the ids of the most central group of size 4, which may not completely agree with the set chosen based on
# individual centrality scores.
kpset(dat,size=4,type="degree", cmode="outdegree", method = "max")
  # together this group connects to 13 others. So i would choose second group if I want to spread info more widely.


#### Methods for selecting groups
### k-means methods
set.seed(385)
# range from 2 to 8 groups
nc <- NbClust(dat, min.nc=2, max.nc=8, index = "kl", method="kmeans")
nc
# manager 1 is group 1, manager 2 is in group 6, etc.

#write.table(nc$Best.partition, file = "tab_cluster.csv", sep =",", na ="", col.names = TRUE )

# Another way to do k-means clustering
# km <- kmeans(dat, 3, nstart=10)
# km
# km$cluster

##### The Newman-Girvan Method for community detection. edge betweennes method
library(igraph)

Fnet <- graph.adjacency(dat)

# Calculates the edge betweenness of the graph, 
# removes the edge with the highest edge betweenness score and repeats. 
# caveat: treat as UNDIRECTED (ties are all mutual), so treats directed as undirected
# the command works for undirected networks only. directed networks will be treated as undirected
g <- edge.betweenness.community(Fnet)

### To see the successive cuts
plot(as.dendrogram(g))

# Show the best group division with the highest modularity
# Roughly speaking, modularity is the fraction of edges that fall within groups 
g$membership
# in this case, 7 is the best number of groups

#write.table(g$membership, file = "tab_cluster.csv", sep =",", na ="", col.names = TRUE )

### Plot the result
V(Fnet)$color=g$membership

plot(Fnet)
