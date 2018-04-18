#### projet SY09

### Exercice 1

## 1 - description
recettes.pays <- read.csv("donnees/recettes-pays.data", row.names = 1)
dim(recettes.pays)
summary(recettes.pays)

cor(recettes.pays)    # correlation entre les ingrédients
cor(t(recettes.pays)) # correlation entre pays

boxplot(recettes.pays, las = 2)
boxplot(recettes.pays[colMeans(recettes.pays) > 0.15], las = 2) # most used ingredients

#plot(recettes.pays[cor(recettes.pays) > 0.9])

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









