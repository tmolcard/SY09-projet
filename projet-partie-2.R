kmeans.addaptative <- function(x, k, volk = rep(1, k), niter = 100, ness = 1, eps = 10^-5) {

  n <- nrow(x)
  p <- ncol(x)
  
  # initialisation
  
  U.k <- x[sample(n, k), ]                        # center of groups
  
  V.k <- array(diag(p), c(p, p, k)) * volk^(-1/p) # inv. mat. cov.
  
  grp <- array(NA, n)                             # groupes de chaque indiv. par indices
  
  # iterations
  
}

kmeans.addaptative(ingredients.pays, 3)
