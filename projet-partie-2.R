source("./KmeansAdaptative.R")
library(mclust)


## Données synthetiques

num = "1"

X <- read.csv(paste("donnees/Synth", num, ".csv", sep = ""), header=T, row.names=1)
z <- X[,3]
X <- X[,-3]

# valeurs réelles
plot(X, col = c(1:2)[z], pch = c(1, 2)[z],
     main = paste("Données synthétiques", num), xlab = "", ylab = "")

clusplot(X, clus = z, color=TRUE, shade=TRUE, lines=0, col.p="black",
         main = paste("Données synthétiques", num), sub = "")

# addaptative k-means
grp.synth <- KmeansAdaptative(X, 2, niter = 100, ness = 200)
plot(X, col = c(1:2)[grp.synth$partition], pch = c(1,2)[z],
     main = paste("Données synthétiques", num, "- K-means adaptatif"), xlab = "", ylab = "",
     sub = paste("Adjusted Rand index :", adjustedRandIndex(z, grp.synth$partition)))
points(grp.synth$centres, pch = 3, cex = 3)

clusplot(X, clus = grp.synth$partition, color=TRUE, shade=TRUE, lines=0, col.p="black",
         main = paste("Données synthétiques", num, "- K-means adaptatif"), sub = "")

print(grp.synth$critere)

# classic k-means
grp.synth.kmeans <- kmeans(X, 2, iter.max = 100, nstart = 200)
plot(X, col = c(1:2)[grp.synth.kmeans$cluster], pch = c(1,2)[z],
     main = paste("Données synthétiques", num, "- K-means"), xlab = "", ylab = "",
     sub = paste("Adjusted Rand index :", adjustedRandIndex(z, grp.synth.kmeans$cluster)))

clusplot(X, clus = c(2,1)[grp.synth.kmeans$cluster], color=TRUE, shade=TRUE, lines=0, col.p="black",
         main = paste("Données synthétiques", num, "- K-means"), sub = "")


print(paste("k-means addaptatif :", adjustedRandIndex(z, grp.synth$partition)))
print(paste("k-means classique  :", adjustedRandIndex(z, grp.synth.kmeans$cluster)))



## Iris

data(iris)
X <- iris[,1:4]
z <- iris[,5]


acp.iris <- princomp(X)

# valeurs réelles
plot(acp.iris$scores, col=c(1:3)[z], pch=c(1:3)[z],
     main = "Iris groupes d'espece")

par(mfrow=c(1,2))

for (k in 2:5) {
  
  print(paste("K =", k))
  
  # addaptative k-means
  grp.iris <- KmeansAdaptative(X, k, niter = 100, ness = 200)
  plot(acp.iris$scores, col = c(1:k)[grp.iris$partition], pch = c(1:3)[z],
       main = paste("Cluster K-means adaptatif K =", k),
       sub = paste("critere :", floor(grp.iris$critere)))
  
  # classic k-means
  grp.iris.kmeans <- kmeans(X, k, iter.max = 100, nstart = 200)
  plot(acp.iris$scores, col = c(1:k)[grp.iris.kmeans$cluster], pch = c(1:3)[z],
       main = paste("Cluster K-means K =", k),
       sub = paste("Withinss :", floor(grp.iris.kmeans$tot.withinss)))
  
  print(paste("critere :", floor(grp.iris$critere)))
  print(paste("Withinss :", floor(grp.iris.kmeans$tot.withinss)))
  
}

par(mfrow=c(1,1))

# K = 1
centre <- colMeans(X)
V <- det(cov(X))^(-1/ncol(X)) * cov(X) # covariance normalisée
distance.K1 <- sum(distXY(as.matrix(X), centre, solve(V)))
print(paste("Distance totale =", distance.K1, "pour K=1"))


inertie.K1 <- kmeans(X, centers = t(centre))$totss
print(paste("Inertie totale =", inertie.K1, "pour K=1"))

kmean.critere <- function(X, max.k = 10, iter.max=20, nstart=20) {
  critere = matrix(nrow = max.k, ncol = 2)
  
  critere[1,1] <- kmeans(X, centers = t(centre))$totss
  critere[1,2] <- sum(distXY(as.matrix(X), colMeans(X),
                             solve(det(cov(X))^(-1/ncol(X)) * cov(X))))
  
  for (k in 2:max.k) {
    critere[k,1] <- kmeans(X, k, iter.max = iter.max, nstart = nstart)$tot.withinss
    critere[k,2] <- KmeansAdaptative(X, k, niter = iter.max, ness = nstart)$critere
  } 
  return(critere)
}

crit <- kmean.critere(X, max.k = 6)



par(mfrow=c(1,2))

plot(crit[,1], col = 1, main = "Critere K-mean",
     ylab = "Inertie", xlab = "Nombre de groupes" )
lines(crit[,1], col = 1)
abline(v=2, lty=2, col = "blue")
# k=2 semble etre la valeur la plus interessante pour K-means

plot(crit[,2], col = 2, main = "Critere K-mean adaptatif",
     ylab = "Distance", xlab = "Nombre de groupes" )
lines(crit[,2], col = 2)
abline(v=3, lty=2, col = "blue")
# k=3 semble etre la valeur la plus interessante pour K-means adaptatif

par(mfrow=c(1,1))



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




Spam <- read.csv("donnees/spam.csv", header=T, row.names=1)
X <- Spam[,-58]
z <- Spam[,58]

table(z)

acp.spam <- princomp(X)

summary(acp.spam)

plot(acp.spam$scores[,c(1,2)], col=c(1:2)[z], pch = c(1,2)[z], main="Répartition mails spam ou non")
plot(acp.spam$scores[,c(2,4)], col=c(1:2)[z], pch = c(1,2)[z], main="Répartition mails spam ou non")

adaptative.kmeans.RandI <- function(tab, z, K, nb.axes = c(2:10), n = 20) {
  RandI <- vector()
  for(i in nb.axes) {
    print(i)
    RandI[i] <- adjustedRandIndex(z, KmeansAdaptative(tab[,1:i], centers.init = K, ness = n)$partition)
  }
  
  return(RandI)
}

acp.ari <- adaptative.kmeans.RandI(acp.spam$scores, z, K = 2, nb.axes = c(2:57), n = 10)
plot(acp.ari)

ari.02.48 <- acp.ari # 10 ess
plot(ari.02.48~c(01:57),
     ylab = "adjusted rand index", xlab = "nombre de composantes",
     main = "Ajusted rand index pour 2 à 57 composantes principales (10 essaies)")

ari.20.35 <- acp.ari # 30 ess
plot(ari.20.35~c(20:35),
     ylab = "adjusted rand index", xlab = "nombre de composantes",
     main = "Ajusted rand index pour 20 à 35 composantes principales (30 essaies)")

ari.24.32 <- acp.ari[24:32] # 100 ess
plot(ari.24.32~c(24:32),
     ylab = "adjusted rand index", xlab = "nombre de composantes",
     main = "Ajusted rand index pour 24 à 32 composantes principales (100 essaies)")

# valeurs réelles
plot(X[,56:57],col=c(1:2)[z])

# addaptative k-means
grp.spam <- KmeansAdaptative(acp.spam$scores[,0:29], 2, niter = 100, ness = 100)
plot(acp.spam$scores[,c(1,2)], col = c(2,1)[grp.spam$partition], pch = c(1,2)[z],
     main="k-mean adaptatif sur 29 composantes")
plot(acp.spam$scores[,c(2,4)], col = c(2,1)[grp.spam$partition], pch = c(1,2)[z],
     main="k-mean adaptatif sur 29 composantes")

# classic k-means
grp.spam.kmeans <- kmeans(acp.spam$scores[,0:29], 2, iter.max = 100, nstart = 100)
plot(acp.spam$scores[,c(2,3)], col = c(1,2)[grp.spam.kmeans$cluster], pch = c(1,2)[z])


print(paste("k-means addaptatif :", adjustedRandIndex(z, grp.spam$partition)))
print(paste("k-means classique  :", adjustedRandIndex(z, grp.spam.kmeans$cluster)))
