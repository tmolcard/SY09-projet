# projet SY09

# Question 2 ---------------------
source("fonctions/distXY.r")

KmeansDistAdaptative <- function(X, centers, v, niter=100, ness=1, eps=1e-5){
  # --- preparation ---
  if(!is.matrix(X)) X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  dtot_opt <- Inf
  res <- NA
  for(ess in 1:ness){
    
    # --- initialisation ---
    # si centers est un chiffre qui indique le nombre de groupes
    if(class(centers) == "numeric") {
      # centres des groupes
      K <- centers
      centers <- X[sample(n, K),]
      v <- rep(1,K)
    # si centers est les coordonnees
    } else if(ncol(centers) == p) {
      if(!is.matrix(centers)) { centers <- as.matrix(centers) }
      K <- nrow(centers)
      v <- rep(1,K)
    } else {
      cat("Error : Format of centers not correct !")
      return(res)
    }
    # V est la matrice cov corrigee, v est le volume souhaité pour la matrice Vk(-1)
    V <- sapply(v, function(x) x^(-1/p) * diag(p), simplify = "array")
    # distance Mahalanobis
    distance <- matrix(data = NA, nrow = n, ncol = K)
    for(k in 1:K){
      distance[,k] <- distXY(X, centers[k,], solve(V[,,k]))
    }
    # groupe
    P <- apply(distance, 1, which.min)
    
    for(i in 1:niter){
      
      # --- mise a jour des parametres ---
      
      # sauvegarder la centre precendante
      centersInit <- centers
      # pour chaque groupe
      for(k in 1:K)
      {
        grp_k <- P == k
        grp_nb <- nrow(X[grp_k,])
        # nouvelles centres
        centers[k,] <- colMeans(X[grp_k,])
        # nouvelle covariance au sein de groupe
        VCov <- cov(X[grp_k,]) * (grp_nb-1) / grp_nb
          # VCov <- tcrossprod(apply(X[P==k,], 1, function(x) x-centers[k,])) / nrow(X[P==k,])
          # OU crossprod(sweep(X[P==k,], 2, centers[k,])) / nb
        # nouvelle covariance corrigee
        # bug
        V[,,k] <- (v[k] * det(VCov))^(-1/p) * VCov
        # nouvelle distance
        distance[,k] <- distXY(X, centers[k,], solve(V[,,k]))
      }
      # nouvelle groupe
      P <- apply(distance, 1, which.min)
      # difference entre deux centres
      if(sum((centers - centersInit)^2) < eps) break()
    }
    # s'il converge, la valeur de critere
    dtot <- sum(sapply(1:K, function(k) sum(distance[P==k,k])))
    if(dtot < dtot_opt){
      dtot_opt <- dtot
      # sauvegarder les resultats
      res <- list(critere = dtot_opt, nb_iteration = i, partition = P, centres = centers, 
                    cov_normalisee = V)
    }
  }
  return(res)
}


# Applications
library(mclust)

# Synth1
X <- read.csv("donnees/Synth1.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]
plot(X, col=c("red", "blue")[z], pch=c(21,24)[z])
resKmeansClassique <- kmeans(X, 2, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 2, ness=5)
adjustedRandIndex(resKmeansClassique$cluster,z)
adjustedRandIndex(resKmeansDistAdaptative$partition,z)
plot(X, col=c("red", "blue")[resKmeansClassique$cluster], 
     pch=c(21,24)[resKmeansClassique$cluster])
adjustedRandIndex(resKmeansDistAdaptative$partition,z)
plot(X, col=c("red", "blue")[resKmeansDistAdaptative$partition], 
     pch=c(21,24)[resKmeansDistAdaptative$partition])

# Synth2
X <- read.csv("donnees/Synth2.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]
resKmeansClassique <- kmeans(X, 2, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 2, ness=5)
adjustedRandIndex(resKmeansClassique$cluster,z)
adjustedRandIndex(resKmeansDistAdaptative$partition,z)

# Synth3
X <- read.csv("donnees/Synth3.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]
resKmeansClassique <- kmeans(X, 2, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 2, ness=5)
adjustedRandIndex(resKmeansClassique$cluster,z)
adjustedRandIndex(resKmeansDistAdaptative$partition,z)

# iris
data(iris)
X <- iris[,1:4]
z <- iris[,5]

resKmeansClassique <- kmeans(X, 2, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 3, ness=5)
adjustedRandIndex(resKmeansClassique$cluster,z)
adjustedRandIndex(resKmeansDistAdaptative$partition,z)

resKmeansClassique <- kmeans(X, 3, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 3, ness=5)
adjustedRandIndex(resKmeansClassique$cluster,z)
adjustedRandIndex(resKmeansDistAdaptative$partition,z)

resKmeansClassique <- kmeans(X, 4, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 4, ness=5)

resKmeansClassique <- kmeans(X, 5, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 5, ness=5)

# Spam
Spam <- read.csv("donnees/spam.csv", header=T, row.names=1)
X <- Spam[,-c(55,56,57,58)]
z <- Spam[,58]
# pourcentage ne peut pas bien representer la distance
X[X != 0] <- 1
# raison possible, les mots appraissent sont fortement correles
# faire pca pour avoir des nouveaux axes
X.pca <- princomp(cor(X))
summary(X.pca) # 29 -> 90%
X2 <- as.matrix(X) %*% as.matrix(X.pca$loadings)
# X2 <- as.matrix(X) %*% as.matrix(X.pca$loadings)
X2 <- X2[,1:29]

resKmeansClassique <- kmeans(X2, 2, nstart=5)
adjustedRandIndex(resKmeansClassique$cluster,z)
resKmeansDistAdaptative <- KmeansDistAdaptative(X2, 2, ness=5)
adjustedRandIndex(resKmeansDistAdaptative$partition,z)

# Error in chol.default(M) : 
#  the leading minor of order 1 is not positive definite
# pre-traitement ?

