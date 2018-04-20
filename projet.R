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

