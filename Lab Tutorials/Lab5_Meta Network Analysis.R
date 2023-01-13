##### Lab5: Meta Network Analysis

library(statnet)
library(mvmeta)

#change working directory
setwd("C:/Users/Cathy/Desktop/QTM 385 Network Analysis")

### 1. Friendship network
data <- as.matrix(read.csv("Manager_friends.csv", header = T))
Fnet <- network(data, directed = T)

### Import and set attributes for nodes
att <- read.csv("Manager_att.csv", header = T, row.names = 1 )
set.vertex.attribute(Fnet, "age", att$age )
set.vertex.attribute( Fnet, "tenure", att$tenure )
set.vertex.attribute( Fnet, "level", att$level )
set.vertex.attribute( Fnet, "dept", att$dept )

### ERGM for the friendship network
model1 <- ergm(Fnet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") +
                 nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept"),
               control = control.ergm(MCMLE.maxit = 30, MCMC.burnin=10000, MCMC.interval=200, seed = 651 ))
summary(model1)
est1 <- as.matrix(summary(model1)$coefficients)[,c(1,2)]
cov1 <- as.matrix(model1$covar)

### 2. Advice-seeking network
data <- as.matrix(read.csv("Manager_advice.csv", header = T))
Anet <- network(data, directed = T)

### Import and set attributes for nodes
set.vertex.attribute(Anet, "age", att$age )
set.vertex.attribute( Anet, "tenure", att$tenure )
set.vertex.attribute( Anet, "level", att$level )
set.vertex.attribute( Anet, "dept", att$dept )

### ERGM for the advice network
model2 <- ergm(Anet ~ edges + nodeicov("age") + nodeocov("age") + nodecov("tenure") +
                 nodefactor("dept") + absdiff("age") + absdiff("tenure") + nodematch("dept"),
               control = control.ergm(MCMLE.maxit = 30, MCMC.burnin=10000, MCMC.interval=200, seed = 651 ))
summary(model2)
est2 <- as.matrix(summary(model2)$coefficients)[,c(1,2)]
cov2 <- as.matrix(model2$covar)

### Prepare output for meta analysis
est <- cbind(est1, est2)
theta <- est[,c(1,3)]
var <- est[,c(2,4)]^2

### Univarate meta-regression
## Fixed effects

# 10 regressions
umeta <- list()
K <- nrow(theta)
tab <- NULL
for (i in 1:K) {
  umeta[[i]] <- mvmeta(theta[i,]~1, S=var[i,], method="fixed")
  out <- summary(umeta[[i]])$coef[,c(1,2,4)]
  tab <- rbind(tab, out)
}
rownames(tab) <- rownames(est)
tab
write.csv(tab, "tab1.csv", na="", row.names = TRUE)

summary(umeta[[10]])


## Random effects
umeta <- list()
K <- nrow(theta)
tab2 <- NULL
for (i in 1:K) {
  umeta[[i]] <- mvmeta(theta[i,]~1, S=var[i,], method="reml") # random effects from maximum likelihood = reml
  out <- c(summary(umeta[[i]])$coef[,c(1,2,4)], summary(umeta[[i]])$qstat$pvalue)
  tab2 <- rbind(tab2, out)
}
rownames(tab2) <- rownames(est)
colnames(tab2)[4] <- "Q"
write.csv(tab2, "tab2.csv", na="", row.names = TRUE)

summary(umeta[[10]]) # do i  instead of 10 to look at all the regressions


### For Future Use. When you have more networks than variables. 
### Multivariate meta-regressions.
cov <- list(cov1, cov2)
### Fixed effects
mvm1 <- mvmeta(theta~1, S=cov, method="fixed")

### Random effects
mvm2 <- mvmeta(theta~1, S=cov, method="reml")


