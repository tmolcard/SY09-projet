source("./fonctions/distXY.r")
kmeans.addaptative <- function(X, k, volk = rep(1, k), niter = 100, ness = 1, eps = 10^-5) {

  n <- nrow(X)
  p <- ncol(X)
  
  # initialisation
  
  U.k <- X[sample(n, k), ]                        # center of groups
  
  V.k <- array(diag(p), c(p, p, k)) * volk^(-1/p) # inv. mat. cov.
  
  grp <- array(NA, n)                             # groupes de chaque indiv. par indices
  dist.X <- matrix(NA, n, k)                      # matrice des distances
  
  # iterations
  for (i in 1:k) {
    dist.X[,i] <- distXY(X, U.k[i,], V.k[,,i])
  }
  
  grp <- apply(dist.X, 1, which.min)
  
#  U.k <- apply(t(1:k), 1, function(i) colMeans(X[grp==i,]))
#  V.k <- apply(t(1:k), 1, function(i) sum((X[grp==i,] - U.k[i,]) %*% t(X[grp==i,] - U.k[i,])))
  
  for (i in 1:k) {
    grp.i <- grp == i
    U.k[i,] <- colMeans(X[grp.i,])
    #V.k[,,i] <- print((X[grp.i,] - U.k[i,]) %*% t(X[grp.i,] - U.k[i,]))
  #print((X[grp.i,] - U.k[i,]) %*% t(X[grp.i,] - U.k[i,]))

  print(apply(X[grp.i,], 1, function(x) (x - U.k[i,]) %*% t(x - U.k[i,])))
  
  
        
  }
  #print(X[1,])
  #print(U.k[1,])
  #print((X[1,] - U.k[1,]) %*% t(X[1,] - U.k[1,]))
  
  #print((X[1,]) %*% t(X[1,]))
  
  
  return(grp)

}

toto = kmeans.addaptative(as.matrix(ingredients.pays), 3)
X= ingredients.pays
toto

