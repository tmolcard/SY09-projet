source("./fonctions/distXY.r")
kmeans.addaptative <- function(X, k, volk = rep(1, k), niter = 100, ness = 1, eps = 10^-5) {
  
  result = NA
  
  n <- nrow(X)
  p <- ncol(X)
  
  for (ess in 1:ness){
    
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
        grp.nb <- nrow(X[grp.i,])
        
        if(qr(X[grp.i,])$rank <= p){
          break #If the rank of the matrix is < P
        }
        
        U.k[i,] <- colMeans(X[grp.i,])                        # update centers
        
        V.k[,,i] <- cov(X[grp.i,]) * (grp.nb-1) / grp.nb      # update cov. mat.
        V.k[,,i] <- V.k[,,i] * (volk[i]*det(V.k[,,i]))^(-1/p) # normalizing
        
        dist.X[,i] <- distXY(X, U.k[i,], solve(V.k[,,i])) # compute new Mahalanobis dist.
      }
      
      grp <- apply(dist.X, 1, which.min) 
      
      if (sum((U.k-U.kp)^2) < eps) {
        break
      }
    }
    
    crit <- sum(apply(t(1:n), 2, function(x) dist.X[x,grp[x]])) # sum(dist.X[,grp])
    
    # algorithm has converged
    if (ess == 1) {
      result <- list(groups = grp, centers = U.k, nrml.cov = V.k, nb.iter = iter, crit = crit)
    } else if (crit < result$crit) {
      result <- list(groups = grp, centers = U.k, nrml.cov = V.k, nb.iter = iter, crit = crit)
    }
  }
    
  return(result)
}

## application

acp.iris <- princomp(iris[1:4])
grp.iris <- kmeans.addaptative(as.matrix(iris[,1:4]), 3, ness = 200)

plot(acp.iris$scores[,2]~acp.iris$scores[,1], col = c("red","green","blue")[iris[,5]])
plot(acp.iris$scores[,2]~acp.iris$scores[,1], col = c("red","green","blue")[grp.iris$groups], pch = c(2, 4, 8)[iris[,5]])
grp.iris$crit
grp.iris$centers


##2.2

X <- read.csv("donnees/Synth1.csv", header=T, row.names=1)
z <- X[,3]
X <- X[,-3]

grp.X <- kmeans.addaptative(as.matrix(X), 2, ness = 5)
grp.km.X <- kmeans(X, X[sample(nrow(X), 2), ], iter.max = 100, nstart = 5)

plot(X[,2]~X[,1], col = c("red","blue")[z])
plot(X[,2]~X[,1], col = c("red","blue")[grp.X$groups], pch = c(2, 4)[z])
points(grp.X$centers, pch = 3, cex = 3)
plot(X[,2]~X[,1], col = c("red","blue")[grp.km.X$cluster], pch = c(2, 4)[z])

