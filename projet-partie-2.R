source("./KmeansAdaptative.R")
library(mclust)

## Données synthetiques

X <- read.csv("donnees/Synth3.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]

# valeurs réelles
plot(X, col = c(1:2)[z], pch = c(1, 2)[z])

# addaptative k-means
grp.synth <- KmeansAdaptative(X, 2, niter = 100, ness = 200)
plot(X, col = c(1:2)[grp.synth$partition], pch = c(1,2)[z])
points(grp.synth$centres, pch = 3, cex = 3)
grp.synth$crit
grp.synth$centres

# classic k-means
grp.synth.kmeans <- kmeans(X, 2, iter.max = 100, nstart = 200)
plot(X, col = c(1:2)[grp.synth.kmeans$cluster], pch = c(1,2)[z])


print(paste("k-means addaptatif :", adjustedRandIndex(z, grp.synth$partition)))
print(paste("k-means classique  :", adjustedRandIndex(z, grp.synth.kmeans$cluster)))



## Iris

acp.iris <- princomp(iris[1:4])

# valeurs réelles
plot(acp.iris$scores[,2]~acp.iris$scores[,1], col = c("red","green","blue")[iris[,5]])

# addaptative k-means
grp.iris <- KmeansAdaptative(as.matrix(iris[,1:4]), 3, niter = 100, ness = 200)
plot(acp.iris$scores[,2]~acp.iris$scores[,1], col = c(1:3)[grp.iris$partition], pch = c(2, 4, 8)[iris[,5]])
grp.iris$crit
grp.iris$centres

# classic k-means
grp.iris.kmeans <- kmeans(as.matrix(iris[,1:4]), 3, iter.max = 100, nstart = 200)
plot(acp.iris$scores[,2]~acp.iris$scores[,1], col = c(1:3)[grp.iris.kmeans$cluster], pch = c(2, 4, 8)[iris[,5]])


print(paste("k-means addaptatif :", adjustedRandIndex(iris[,5], grp.iris$partition)))
print(paste("k-means classique  :", adjustedRandIndex(iris[,5], grp.iris.kmeans$cluster)))



## Spam

Spam <- read.csv("donnees/spam.csv", header=T, row.names=1)
X <- Spam[,-58]
z <- Spam[,58]

table(z)

acp.spam <- princomp(X)

# valeurs réelles
plot(acp.spam$scores[,c(1,2)], col=c(1:2)[z], pch = c(1,2)[z])
plot(acp.spam$scores[,c(2,3)], col=c(1:2)[z], pch = c(1,2)[z])
plot(acp.spam$scores[,c(3,4)], col=c(1:2)[z], pch = c(1,2)[z])


# addaptative k-means
grp.spam <- KmeansAdaptative(X/acp.spam$scores[,1], 2, niter = 100, ness = 200) # , v = c(0.4, 0.6)
plot(acp.spam$scores[,c(2,3)], col = c(1,2)[grp.spam$partition], pch = c(1,2)[z])
grp.spam$crit

# classic k-means
grp.spam.kmeans <- kmeans(X, 2, iter.max = 100, nstart = 200)
plot(acp.spam$scores[,c(2,3)], col = c(1,2)[grp.spam.kmeans$cluster], pch = c(1,2)[z])


print(paste("k-means addaptatif :", adjustedRandIndex(z, grp.spam$partition)))
print(paste("k-means classique  :", adjustedRandIndex(z, grp.spam.kmeans$cluster)))
