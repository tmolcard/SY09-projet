# projet SY09
# Cuisine
# Q1 Q2
recettesP <- read.csv("donnees/recettes-pays.data", na.strings="NA", header=T, dec=",")
dim(recettesP)
summary(recettesP)
recettesP[,2:51] <- as.numeric(as.character(unlist(recettesP[,2:51])))
row.names(recettesP) <- recettesP[,1]
recettesP <- recettesP[,2:51]
boxplot(recettesP[,1:9])
corr <- cor(recettesP[,1:51])
plot(recettesP[,c(3,11)])

recettesPM <- as.matrix(recettesP)
recettesPM <- scale(recettesPM, scale = FALSE)
covPM <- cov(recettesPM)
lambda <- eigen(covPM)$values # apres 36 negative
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


data("iris")
class(iris)

recettesE <- read.csv("donnees/recettes-echant.data", na.strings="", header=T, dec=",")
summary(recettesE)
