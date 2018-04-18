#### projet SY09

### Exercice 1

## 1 - description
recettes.pays <- read.csv("donnees/recettes-pays.data", row.names = 1)
na.fail(recettes.pays)

dim(recettes.pays)
summary(recettes.pays)

cor(recettes.pays)    # correlation entre les ingrédients
cor(t(recettes.pays)) # correlation entre pays

boxplot(recettes.pays, las = 2)
boxplot(recettes.pays[colMeans(recettes.pays) > 0.15], las = 2) # most used ingredients

# plot(recettes.pays[cor(recettes.pays) > 0.9])

## 2 - ACP

acp.recettes.pays <- prcomp(recettes.pays)  # ACP - variables = ingrédients

plot(acp.recettes.pays$x[,1:2])
text(acp.recettes.pays$x[,1:2], row.names(recettes.pays), pos=3)

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

# TODO : contributions relatives aux axes - individus ?
# plot des anciens axes dans le nouveau - à voir - certainement ilisible

## 3 - Analyse ascendante hiérarchique

dist.recettes.pays <-dist(recettes.pays, method = "manhattan")

hclust.recettes.pays <- hclust(dist.recettes.pays) # methodes : "single", "average", ...
plot(hclust.recettes.pays)


## 4 - K-means

kmeans.recette.pays <- kmeans(recettes.pays, rbind(recettes.pays["American",], recettes.pays["African",], recettes.pays["Asian",]), iter.max = 30)
plot(acp.recettes.pays$x[,1:2], col = c("red","green","blue")[kmeans.recette.pays$cluster])
text(acp.recettes.pays$x[,1:2], row.names(recettes.pays), pos=3, col = c("red","green","blue")[kmeans.recette.pays$cluster])

## 5 - Classification géographique des origines

# s'ils nous demande de dire que  les mexicains sont loins des allemands et de dire qu'on le retrouve 
# dans la classif obtenue ok, sinon je ne vois pas comment faire.


## 6 - description des données

recettes.echant <- read.csv("donnees/recettes-echant.data")
na.fail(recettes.echant)

summary(recettes.echant)

# TODO : blabla - dénombrer les noms des pays

## 7 - group ingrédients

pays <- unique(recettes.echant[,1])
ingredients <- colnames(recettes.echant[,2:ncol(recettes.echant)])

ingredient.pays <- data.frame(matrix(nrow = length(ingredients), ncol = length(pays)))
colnames(ingredient.pays) <- pays
row.names(ingredient.pays) <- ingredients

for (i in 1:length(ingredients)) {
  ingredient <- ingredients[i]
  
  for (j in 1:length(pays)) {
    p <- pays[j]
    
    ingredient.pays[i, j] <- sum(recettes.echant[recettes.echant[,1] == p,][,ingredient]) mean
  }
}

# On compte le nombre de fois ou chaque ingrédients apparait dans chaque pays différents.
# On pourait aussi faire la moyenne pour chaque pays des fois ou il apparait dans les recettes.
# -> 1 s'il apparait à chaque fois 0 sinon -- surement plus malin

# en tout cas on doit centrer en colonnes si on fait la somme.

scale(ingredient.pays, center = F, scale = T)

# TODO : similarités - disimilarités





