# projet SY09

# Question 1 -------------------------

# recettes
recettesP <- read.csv("donnees/recettes-pays.data", na.strings="NA", header=T, dec=",")
dim(recettesP)
summary(recettesP)
recettesP[,2:51] <- as.numeric(as.character(unlist(recettesP[,2:51])))
row.names(recettesP) <- recettesP[,1]
recettesP <- recettesP[,2:51]
boxplot(recettesP[,1:9])
corr <- cor(recettesP)
plot(recettesP[,c(3,11)])

recettesPM <- as.matrix(recettesP)
recettesPM <- scale(recettesPM, scale = FALSE)
covPM <- cov(recettesPM)
lambda <- eigen(covPM)$values
inertie_total <- sum(abs(lambda))
# en pourcentage
inertie_explique <- seq(1,39)
for (i in 1:39)
{
  pourc_inertie <- sum(lambda[1:i])/inertie_total * 100
  inertie_explique[i] <- pourc_inertie
}
# composants pricipals
U <- eigen(covPM)$vectors
# coord dans ACP
recettesP_ACP <- recettesPM %*% U
plot(recettesP_ACP[,c(1,2)])
text(recettesP_ACP[,c(1,2)], row.names(recettesP), pos = 3)

# Q3 Q4
# Manhattan = somme|xj − yj|
distM <- dist(recettesP, method = "manhattan")
recettesP_hc <- hclust(distM, method = "ward.D")
plot(recettesP_hc)
# K means
recettesPclusters <- kmeans(recettesP, centers = 3)
plot(recettesPclusters)


# recettes-echant
recettesE <- read.csv("donnees/recettes-echant.data", na.strings="", header=T, dec=",")
dim(recettesE)
summary(recettesE)
corrE <- cor(recettesE[,-1])

#table individus-variables
pays <- unique(recettesE[,1])
ingredients <- colnames(recettesE[,-1])

ingredients.pays <- data.frame(matrix(nrow = length(ingredients), ncol = length(pays)))
colnames(ingredients.pays) <- pays
row.names(ingredients.pays) <- ingredients

for (i in 1:length(ingredients)) {
  ingredient <- ingredients[i]
  
  for (j in 1:length(pays)) {
    p <- pays[j]
    ingredients.pays[i, j] <- mean(recettesE[recettesE[,1] == p,][,ingredient])
  }
}
  
distE <- dist(ingredients.pays, method = "manhattan")
recettesE_hc <- hclust(distE, method = "ward.D")
plot(recettesE_hc) # ingedients & aliments

# K-médoïdes
library(cluster)
plot(pam(ingredients.pays,2)) # 2




# Question 2 ---------------------
source("fonctions/distXY.r")

KmeansDistAdaptative <- function(X, centers, v=rep(1,K), niter=100, ness=1, eps=1e-5){
  # --- preparation ---
  if(!is.matrix(X)) X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  dtot_opt <- 999999
  res <- NA
  for(ess in 1:ness){
    
    # --- initialisation ---
    if(class(centers) == "numeric"){
      # centres des groupes
      K <- centers
      centers <- X[sample(n, K),]
    }
    else if(ncol(centers) == p){
      if(!is.matrix(centers)) centers <- as.matrix(centers)
      K <- nrow(centers)
    }
    else{
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
        # nouvelles centres
        centers[k,] <- colMeans(X[P==k,])
        # nouvelle covariance au sein de groupe
        VCov <- tcrossprod(apply(X[P==k,], 1, function(x) x-centers[k,])) / nrow(X[P==k,])
        # OU crossprod(sweep(X[P==k,], 2, centers[k,])) / nb
        # nouvelle covariance corrigee
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
resKmeansClassique <- kmeans(X, 2, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 2, ness=5)
adjustedRandIndex(resKmeansClassique$cluster,z)
adjustedRandIndex(resKmeansDistAdaptative$partition,z)

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

resKmeansClassique <- kmeans(X, 3, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 3, ness=5)
adjustedRandIndex(resKmeansClassique$cluster,z)
adjustedRandIndex(resKmeansDistAdaptative$partition,z)

resKmeansClassique <- kmeans(X, 4, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 4, ness=5)

resKmeansClassique <- kmeans(X, 5, nstart=5)
resKmeansDistAdaptative <- KmeansDistAdaptative(X, 5, ness=5)

# pas compris pour K=1

# Spam
Spam <- read.csv("donnees/spam.csv", header=T, row.names=1)
X <- Spam[,-58]
z <- Spam[,58]
# pre-traitement

