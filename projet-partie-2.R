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
        grp.count <- table(grp)
        
        if(is.na(match(i, grp))){
          break
        }
        
        if(grp.count[names(grp.count)==i] <= p) {
          break
        }
        
        #sz <- table(grp)[names(table(grp))==i]
        
        #cvm <- matrix(0, p, p)
        
        #for  (j in 1:sz) {
        #  cvm <- cvm + ((X[grp.i,][j,]-U.k[i,]) %*% t(X[grp.i,][j,]-U.k[i,]) / sz)
        #  #print(cvm[1,1])
        #}
        
        U.k[i,] <- colMeans(X[grp.i,])                        # update centers
        
        #V.k[,,i] <- cvm #find best way
        V.k[,,i] <- cov(X[grp.i,] - U.k[i,])                  # update cov. mat.
        
        #print(V.k[,,i] - cvm)
        
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

#acp.iris <- princomp(iris[1:4])

grp.iris <- kmeans.addaptative(as.matrix(iris[,1:4]), 3, ness = 20)

plot(acp.iris$scores[,2]~acp.iris$scores[,1], col = c("red","green","blue")[iris[,5]])
plot(acp.iris$scores[,2]~acp.iris$scores[,1], col = c("red","green","blue")[grp.iris$groups], pch = c(2, 4, 8)[iris[,5]])
grp.iris$centers
