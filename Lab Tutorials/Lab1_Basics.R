
#install.packages("statnet")

library(statnet)

#change working directory
setwd("C:/Users/Cathy/Desktop/QTM 385 Network Analysis")

### Import data
data <- as.matrix(read.csv("Manager_friends.csv", header = T))

## Import attributes.
att <- read.csv("Manager_att.csv", header = T)

### Create a network object
Fnet <- network(data, directed = T)

### Plot the network
plot.network(Fnet, displaylabels = T, boxed.labels = FALSE, vertex.cex = 0.6, label.cex = 1, 
             vertex.col = att$dept, label.col = "blue", edge.lwd = .5, edge.col = "gray50")

# Interactive plotting
plot.network(Fnet, displaylabels = T, boxed.labels = FALSE, vertex.cex = 0.6, label.cex = 1, 
             vertex.col = att$dept, label.col = "blue", edge.lwd = .5, edge.col = "gray50", interactive = T)

# Save the graph to your computer
png("Manager_friends.png", width = 1600, height = 1600, res=35)
plot.network(Fnet, displaylabels = T, boxed.labels = FALSE, vertex.cex = 0.6, label.cex = 5, 
             vertex.col = att$dept, label.col = "blue", edge.lwd = .5, edge.col = "gray50")
dev.off()

### Node level
degree <- degree(Fnet)
# degree is number of connections everyone has

ind <- degree(Fnet, cmode = "indegree") # number of incoming ties

out <- degree(Fnet, cmode = "outdegree") # number of outgoing ties

# How close an actor is to everyone else (sort of the average shortest paths)
# One ego has short paths to all other vertices in the graph
# sum inverse direct closeness - inverse of the total distance you have to everyone else
# big number is means you're closer to everyone
closeness <- closeness(Fnet, cmode="suminvdir")


# Number of shortest paths passing through the egos (bridges)
# number of times you serve as a bridge between other people on the network
# higher, the more "gatekeeping" power you have
betweenness <- betweenness(Fnet)

# The extent to which an ego is connected to other important nodes
# the extent to which you are connected to important others
ecent <- evcent(Fnet) # eigenvector centrality

tab1 <- cbind(degree, closeness, betweenness, ecent)

write.csv(tab1, file = "tab1.csv")

# You cuold check the correlations among the centrality measures
# library(Hmisc)
# cor(tab1)

### Dyad level
dyad.census(Fnet)

# reciprocity. It counts null ties as mutual ties, meaning people that don't think of each other as friend
reciprocity <- grecip(Fnet)
(24+174)/(24+174+12)

# Ignoring null ties in calculating reciprocity. Mutual ties count as twice.
grecip(Fnet, measure = "edgewise")
24*2/(24*2+12)

# Geodistance
# measures the shortest path between any two pairs of people
# how many steps one person has to take to reach the other person
# creates a matrix
geodist <- geodist(Fnet)

# number of shortest paths between two people (are there multiple shortest paths you can take?)
geodist$counts 

# diameter
max(geodist$gdist[geodist$gdist!=Inf])
# mean geodistance
mean(geodist$gdist[geodist$gdist!=Inf&geodist$gdist!=0])

### Group level
# The transitive triads
# transitive = how likely your friend's friend is also your friend
# three friends together (triangle/triad), but most interested in 16th configuration of the triangle
triad.census(Fnet)[16]

# Within a clique, everyone is a friend with one another.
clique <- clique.census(Fnet)

tab2 <- clique$clique.count[,1]
tab2

write.csv(tab2, file = "tab2.csv")

# Four components include one node, respectively; one component includes 17 nodes
# component is the largest subnetwork that's connected within the network
component.dist(Fnet)

### Network level
# density
# whats the average connection in your network? the propotion of ties out of all the possible ties
# eg. 14% of all possible ties are realized in the network
density <- gden(Fnet)

# centralization, between zero and one.
# e.g. 1 means that one guy who monopolizes all the connections
# 0 means totally equal, everybody has the same number of ties
# The higher the number is, the more inequality in centrality.
cent <- centralization(Fnet, degree)

# transitivity. Proportion of transitive triads out of possible triads.
# Divide the number of {AB, BC, AC} triads by the number of {AB, BC, anything} triads.
transitivity <- gtrans(Fnet)
# e.g. out of all the triangles, 45% of triangles are transitive triangles
# my friend's friend is very likely to be my friend

