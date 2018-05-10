#### projet SY09

### Exercice 1

## 1 - description
recettes.pays <- read.csv("donnees/recettes-pays.data", row.names = 1)
na.fail(recettes.pays)
dim(recettes.pays)

rowSums(recettes.pays)
summary(recettes.pays)

boxplot(recettes.pays, las = 2)
hist(recettes.pays$olive_oil)
hist(recettes.pays$cayenne)
hist(recettes.pays$soy_sauce)
hist(recettes.pays$sesame_oil)
boxplot(recettes.pays[colMeans(recettes.pays) > 0.15], las = 2) # most used ingredients
## correlation
M <- cor(recettes.pays)    # correlation entre les ingrédients
M2 <- cor(t(recettes.pays)) # correlation entre pays
# install.packages("corrplot")
library(corrplot)
corrplot(M, type="upper", tl.col = "black", tl.srt = 45)
corrplot(M2, type="upper", tl.col = "black", tl.srt = 45)
sum <- length(cor(recettes.pays)[upper.tri(cor(recettes.pays))]) - 50 # nom total de cor entre vars
correlation <- cor(recettes.pays)[upper.tri(cor(recettes.pays))] # valeurs de cor
# parce que > 0.3 cor moyenne, > 0.5 cor forte
(length(correlation[abs(correlation) >= 0.3]) - 50 )/sum * 100 # % de cor > 0.3
(length(correlation[abs(correlation) >= 0.5]) - 50 )/sum * 100 # % de cor > 0.5
# on peut aussi utiliser chart.Correlation(recettes.pays)


## 2 - ACP

acp.recettes.pays <- prcomp(recettes.pays)  # ACP - variables = ingrédients

acp.recettes.pays$sdev # racine carrée des valeurs absolues
acp.recettes.pays.inertie <- acp.recettes.pays$sdev ^ 2 # valeurs absolues de la matrice de covarience

eigen(cov(recettes.pays))$values
# on retrouve à peu près les mêmes résultat
# on peut considérer les valeurs d'indice supérieur à 26 égales à 0, toutes ces valeurs sont inferieur à 10^-16
# elles ne sont dû qu'à des approximations, en effet on part d'une matrice de rang maximum 26
# on ne peut avoir plus de 26 valeurs propres non nulles.

inertie_total <- sum(acp.recettes.pays.inertie)

acp.recettes.pays.inertiep <- acp.recettes.pays.inertie / inertie_total * 100
inertie_explique <- apply(t(1:26), 2, function(x) sum(acp.recettes.pays.inertiep[1:x]))

plot(acp.recettes.pays.inertiep,
     main = "Inertie expliquée par composante",
     xlab = "Nombre de composantes",
     ylab = "Inertie expliquée (%)"
)
plot(inertie_explique,
     main = "Inertie expliquée par les n premières composantes",
     xlab = "Nombre de composantes",
     ylab = "Inertie expliquée (%)",
     ylim = c(0, 100)
)
lines(inertie_explique, col = "red")
abline(h=90, lty=2, col = "blue")


plot(acp.recettes.pays$x[,1:2])
text(acp.recettes.pays$x[,1:2], row.names(recettes.pays), pos=3)

plot(acp.recettes.pays$x[,c(1,3)])
text(acp.recettes.pays$x[,c(1,3)], row.names(recettes.pays), pos=3)


par(mfrow=c(1,2))
biplot(acp.recettes.pays, choices = c(1,2))
biplot(acp.recettes.pays, choices = c(1,3))
par(mfrow=c(1,1))


## 3 - Analyse ascendante hiérarchique
# Manhattan = somme|xj − yj|

dist.recettes.pays <-dist(recettes.pays, method = "manhattan")
hclust.recettes.pays <- hclust(dist.recettes.pays, method = "ward.D")
plot(hclust.recettes.pays, main = "Classification Ascendante Hiérarchique (CAH)",
     ylab="Distance calculee par le critere de Ward")

## 4 - K-means

kmeans.inertie <- function(tab, maxK = 10, n = 20) {
  inertie <- vector(length = maxK)
  for(k in 1:maxK) {
    inertie[k] <- sum(kmeans(tab, k, nstart = n)$withinss)
  }
  
  return(inertie)
}

inertie.centres <- kmeans.inertie(recettes.pays, 10, 20)
plot(inertie.centres,
     ylab = "Inertie total",
     xlab = "Nombre de centres",
     main = "Inertie totale en fonction du nombre de centres"
)
lines(inertie.centres)
abline(v=3, lty=2, col = "blue")
abline(v=5, lty=2, col = "red")


kmeans.recette.pays <- kmeans(recettes.pays, centers = 3, nstart = 20)
kmeans.recette.pays <- kmeans(recettes.pays, centers = 7, nstart = 20)


plot(acp.recettes.pays$x[,1:2],
     col = c("red","green","blue","purple","orange")[kmeans.recette.pays$cluster]
)
text(acp.recettes.pays$x[,1:2],
     row.names(recettes.pays), pos=3,
     col = c("red","green","blue","purple","orange")[kmeans.recette.pays$cluster]
)



## 6 - description des données

recettes.echant <- read.csv("donnees/recettes-echant.data")
na.fail(recettes.echant)

summary(recettes.echant)

dim(recettes.echant)

barplot(sort(table(recettes.echant[,1]), decreasing = T), las = 2, cex.names = 0.7,
        main = "Occurences des origines")


## 7 - group ingrédients

ingredients.pays <- t(recettes.echant[,2:51])
disim.ingredients <- dist(ingredients.pays, method = "binary")

## 8 - classif ascendante hierarchique

hclust.ingredients <- hclust(disim.ingredients, method = "ward.D2")

plot(hclust.ingredients,
     main = "Classification ascendante hiérarchique (Ward.D2)",
     xlab = "Ingrédients", sub = "")


## 9 - k-médoïdes

library(cluster)

for (i in c(2,3,5,8)) {
  
  print(paste("K =", i))
  
  pam.ingredients <- pam(disim.ingredients, i)
  clusplot(pam.ingredients, labels = 3, color=TRUE, shade=TRUE, lines=0, col.p="black",
           main = paste("K-Medïodes (k=", i, ")", sep=""))
  
  print(pam.ingredients$medoids)
  print(pam.ingredients$clusinfo)
  
}









