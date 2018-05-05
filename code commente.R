# projet SY09

#### Cuisine

### charger des donnees
recettes.pays <- read.csv("donnees/recettes-pays.data", row.names = 1)
na.fail(recettes.pays)

### Question 1 exploratoire -------------------------
## boxplot
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

### Question 2 PCA -------------------------


### Question 3 Analyse Ascendante Hiérarchique -------------------------
# Manhattan = somme|xj − yj|
dist.recettes.pays <-dist(recettes.pays, method = "manhattan")
hclust.recettes.pays <- hclust(dist.recettes.pays, method = "ward.D")
plot(hclust.recettes.pays, main = "Classification Ascendante Hiérarchique (CAH)",
     ylab="Distance calculee par le critere de Ward")

