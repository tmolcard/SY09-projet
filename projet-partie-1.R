#### projet SY09

### Exercice 1

## 1 - description
recettes.pays <- read.csv("donnees/recettes-pays.data", row.names = 1)
na.fail(recettes.pays)
rowSums(recettes.pays)
dim(recettes.pays)
summary(recettes.pays)

cor(recettes.pays)    # correlation entre les ingrédients
cor(t(recettes.pays)) # correlation entre pays

boxplot(recettes.pays, las = 2)
boxplot(recettes.pays[colMeans(recettes.pays) > 0.15], las = 2) # most used ingredients

# plot(recettes.pays[cor(recettes.pays) > 0.9])

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


plot(acp.recettes.pays$x[,1:2])
text(acp.recettes.pays$x[,1:2], row.names(recettes.pays), pos=3)

plot(acp.recettes.pays$x[,c(1,3)])
text(acp.recettes.pays$x[,c(1,3)], row.names(recettes.pays), pos=3)

biplot(acp.recettes.pays) # illisible

# TODO : contributions relatives aux axes - individus ?
# plot des anciens axes dans le nouveau - à voir - certainement ilisible

plot(acp.recettes.pays$rotation[,1:2])
text(acp.recettes.pays$rotation[,1:2], colnames(recettes.pays), pos=3)

## 3 - Analyse ascendante hiérarchique

dist.recettes.pays <-dist(recettes.pays, method = "manhattan")

hclust.recettes.pays <- hclust(dist.recettes.pays) # methodes : "single", "average", ...
plot(hclust.recettes.pays)


## 4 - K-means

kmeans.inertia <- function(tab, maxK = 10, n = 20) {
  inertia <- vector(length = maxK)
  for(k in 1:maxK) {
    inertia[k] <- sum(kmeans(tab, k, nstart = n)$withinss)
  }
  
  return(inertia)
}

plot(kmeans.inertia(recettes.pays, 10, 20),
     ylab = "Inertie total",
     xlab = "Nombre de centres"
)

kmeans.recette.pays <- kmeans(recettes.pays, centers = 3, nstart = 20)
kmeans.recette.pays <- kmeans(recettes.pays, centers = 7, nstart = 20)


plot(acp.recettes.pays$x[,1:2],
     col = c("red","green","blue","purple","orange")[kmeans.recette.pays$cluster]
)
text(acp.recettes.pays$x[,1:2],
     row.names(recettes.pays), pos=3,
     col = c("red","green","blue","purple","orange")[kmeans.recette.pays$cluster]
)

## 5 - Classification géographique des origines

# s'ils nous demande de dire que  les mexicains sont loins des allemands et de dire qu'on le retrouve 
# dans la classif obtenue ok, sinon je ne vois pas comment faire.


## 6 - description des données

recettes.echant <- read.csv("donnees/recettes-echant.data")
na.fail(recettes.echant)

summary(recettes.echant)

dim(recettes.echant)

barplot(sort(table(recettes.echant[,1]), decreasing = T), las = 2, cex.names = 0.7)


# TODO : blabla

## 7 - group ingrédients

pays <- unique(recettes.echant[,1])
ingredients <- colnames(recettes.echant[,2:ncol(recettes.echant)])

ingredients.pays <- data.frame(matrix(nrow = length(ingredients), ncol = length(pays)))
colnames(ingredients.pays) <- pays
row.names(ingredients.pays) <- ingredients

for (i in 1:length(ingredients)) {
  ingredient <- ingredients[i]
  
  for (j in 1:length(pays)) {
    p <- pays[j]
    
    ingredients.pays[i, j] <- mean(recettes.echant[recettes.echant[,1] == p,][,ingredient])
  }
}

# On retrouve a quelques facteurs et approximations pres les donnees du precedent dataset.
acp.p.r <- prcomp(t(ingredients.pays))
plot(acp.p.r$x[,1], -acp.p.r$x[,2])
text(acp.p.r$x[,1], -acp.p.r$x[,2], row.names(t(ingredients.pays)), pos=3)


# TODO : similarités - disimilarités
disim.ingredients.pays <- dist(ingredients.pays, "euclidian") # pris au hasard, y reflechir !!!

# test
library(philentropy)
m <- as.matrix(t(recettes.echant[,2:51]))
colnames(m) <- recettes.echant[,1]
disim.ingredients.pays <- distance(t(recettes.echant[,2:51]), method = "jaccard")

colnames(disim.ingredients.pays) <- colnames(recettes.echant[,2:51])
rownames(disim.ingredients.pays) <- colnames(recettes.echant[,2:51])

## 8 - classif ascendante hierarchique

hclust.ingredients.pays <- hclust(as.dist(disim.ingredients.pays)) # methodes : "single", "average", ...
plot(hclust.ingredients.pays)


## 9 - k-médoïdes

library(cluster)
pam.recettes.pays <- pam(ingredients.pays, 3)
pam.recettes.pays <- pam(as.dist(disim.ingredients.pays), 3)
plot(pam.recettes.pays)













