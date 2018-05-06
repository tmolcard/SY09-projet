source("./fonctions/distXY.r")

KmeansAdaptative <- function(X, centers.init, v=rep(1, K), niter=100, ness=1, eps=1e-5){
  count1 = 1
  count2 = 1
  
  # --- preparation ---
  if(!is.matrix(X)) X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  crit.opt <- Inf
  res <- NA
  
  for(ess in 1:ness){
    
    # --- initialisation ---
    if(class(centers.init) == "numeric"){
      K <- centers.init
      centers <- X[sample(n, K),]
    }
    else if(ncol(centers) == p){
      centers <- as.matrix(centers.init)
      K <- nrow(centers)
    }
    else{
      cat("Error : Format of centers not correct !")
      return(res)
    }
    
    # initialisation V
    V <- sapply(v, function(x) x^(-1/p) * diag(p), simplify = "array")
    
    # formation des premiers groupes avec distance de Mahalanobis
    distance <- matrix(data = NA, nrow = n, ncol = K)
    for(k in 1:K){
      distance[,k] <- distXY(X, centers[k,], solve(V[,,k]))
    }
    
    P <- apply(distance, 1, which.min)
    
    
    for(i in 1:niter){
      
      # --- mise a jour des parametres ---
      
      # sauvegarder la centre precendante
      centers.old <- centers
      
      # pour chaque partition
      for(k in 1:K) {
        
        grp_k <- P == k
        grp_nb <- nrow(X[grp_k,])
        
        if(is.null(grp_nb) || det(cov(X[grp_k,])) < 1e-10 ){
          break
        }
        
        # nouveau centre de partition
        centers[k,] <- colMeans(X[grp_k,])
        
        # nouvelle covariance corrigée au sein de la partition
        V[,,k] <- cov(X[grp_k,]) * (grp_nb-1) / grp_nb
        V[,,k] <- (v[k] * det(V[,,k]))^(-1/p) * V[,,k]
        
        # mise à jour de la distance
        distance[,k] <- distXY(X, centers[k,], solve(V[,,k]))
      }
      
      # nouveaux groupes
      P <- apply(distance, 1, which.min)
      
      # test de la convergence
      if(sum((centers - centers.old)^2) < eps) break()
      
    }
    
    # mise à jour du critère
    crit <- sum(sapply(1:K, function(k) sum(distance[P==k,k])))
    
    # si le résultat est de meilleur qualité
    if(crit < crit.opt){
      crit.opt <- crit
      # sauvegarder les resultats
      res <- list(critere = crit.opt, nb_iteration = i, partition = P, centres = centers, 
                  cov_normalisee = V)
    }
  }
  return(res)
}