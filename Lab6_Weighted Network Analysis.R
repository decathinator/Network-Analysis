# Lab 6: Special Networks

library(statnet)

#change working directory
setwd("C:/Users/Cathy/Desktop/QTM 385 Network Analysis")

### Import the data
dat <- read.csv("TwoMode.csv", header = T, row.names = 1, strip.white = T, stringsAsFactors = F )
data <- as.matrix(dat)

### Convert to a faculty network 
faculty <- t(data) %*% data

# Remove loops
diag(faculty)=0

### Make a network
net <- network(faculty, matrix.type="adjacency", directed=F, ignore.eval=FALSE, names.eval="value")

### Calculating centralities
degree(net, ignore.eval=FALSE) #For binary networks, set ignore.eval=TRUE.
closeness(net, ignore.eval=FALSE, cmode="suminvundir")
betweenness(net, ignore.eval=FALSE, cmode="undirected")
evcent(net, ignore.eval=FALSE)


### Import the attributes for nodes
att <- read.csv("TM_att.csv", header = T, row.names = 1 )
set.vertex.attribute(net, "Rank", att$Rank )
set.vertex.attribute(net, "Tenure", att$Tenure )
set.vertex.attribute(net, "Sex", att$Sex )
set.vertex.attribute(net, "Quant", att$Quant )
set.vertex.attribute(net, "Area", att$Area)

### Plot the network
plot(net, displaylabels = T, edge.lwd=net%e%'value', vertex.col=att$Area)
dev.off()

### Modeling weighted network
model2 <- ergm(net~sum +nodefactor("Rank")+nodecov("Tenure")+nodefactor("Sex") + nodefactor("Quant")+
              nodematch("Rank") + nodematch("Sex")+nodematch("Quant")+nodematch("Area") +
              transitiveweights, response="value", reference=~Poisson)
summary(model2)
est <- as.matrix(summary(model2)$coefs[,c(1,2,4)])
write.table(est, file = "est_weighted.csv", sep =",", na ="", col.names = TRUE )

