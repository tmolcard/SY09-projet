source("./fonctions/distXY.r")
kmeans.addaptative <- function(X, k, volk = rep(1, k), niter = 100, ness = 1, eps = 10^-5) {

  n <- nrow(X)
  p <- ncol(X)
  
  # initialisation
  
  U.k <- X[sample(n, k), ]                        # center of groups
  
  V.k <- array(diag(p), c(p, p, k)) * volk^(-1/p) # cov. mat.
  
  grp <- array(NA, n)                             # groups
  dist.X <- matrix(NA, n, k)                      # dist. mat.
  
  for (i in 1:k) {
    dist.X[,i] <- distXY(X, U.k[i,], solve(V.k[,,i]))
  }
  grp <- apply(dist.X, 1, which.min)
  
  # iterations
  
  
  for (iter in 1:niter) {
    U.kp <- U.k
    for (i in 1:k) {
      grp.i <- grp == i
      U.k[i,] <- colMeans(X[grp.i,])                        # update centers
      V.k[,,i] <- cov(X[grp.i,] - U.k[i,])                  # update cov. mat.
      V.k[,,i] <- V.k[,,i] * (volk[i]*det(V.k[,,i]))^(-1/p) # normalizing
      
      dist.X[,i] <- distXY(X, U.k[i,], solve(V.k[,,i])) # compute new Mahalanobis dist.
    }
    
    grp <- apply(dist.X, 1, which.min) 
    
    if (sum((U.k-U.kp)^2) < eps) {
      break
    }
  }
  
  result <- list(groups = grp, centers = U.k, nrml.cov = V.k, nb.iter = iter)
  
  return(result)

}

toto = kmeans.addaptative(as.matrix(ingredients.pays), 3)
X= ingredients.pays
toto

