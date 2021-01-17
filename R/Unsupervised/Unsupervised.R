# Libraries
install.package("caret")
install.package("dplyr")
install.packages("rattle")

library(caret)
library(dplyr)

library(rpart)
library(rattle)
library(RColorBrewer)

# Dataset and utils
vgsales_preprocessed <- read.csv("..data/vgsales_preprocessed_count_platform.csv", sep = ",", head = TRUE)

# Delete unused variables
vgsales_preprocessed <- select(vgsales_preprocessed, -c(row.ID))

####################### Training ####################### 

set.seed(1)
dt <- sort(sample(nrow(vgsales_preprocessed), nrow(vgsales_preprocessed)*.9))
train_set<-vgsales_preprocessed[dt,]
test_set<-vgsales_preprocessed[-dt,]

head(train_set)

train_set <- select(train_set, -c(row.ID, Name, First.Genre., First.Publisher.))
train_set <- select(train_set, -c(Sum.JP_Sales., Sum.NA_Sales., Sum.EU_Sales., Sum.Other_Sales.))
head(train_set)

km_train <- kmeans(train_set, centers = 3, nstar = 20)
km_train

plot(train_set,col=km_train$cluster, main=km_train$tot.withinss)

# Ver el comportamiento según el inicio del kmeans
# La aleatoriedad implica diferentes resultados
# Por cada plot mostramos el WSS, que queremos minimizar

for(i in 1:6){
  km_puntos <- kmeans(puntos, center = 3, nstar = 1)
  plot(puntos,col=km_puntos$cluster, main=km_puntos$tot.withinss)
}

# Estudio del parámetro K
wss <- 0
for (i in 1:15) {
  km.out <- kmeans(puntos, centers = i, nstar=20)
  wss[i] <- km.out$tot.withinss
}
plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")

# ====================================
# Ejercicio 2 - Clustering jerárquico
# ====================================

set.seed(1)
dt <- sort(sample(nrow(vgsales_preprocessed), nrow(vgsales_preprocessed)*.01))
train_set<-vgsales_preprocessed[dt,]
test_set<-vgsales_preprocessed[-dt,]

train_set <- select(train_set, -c(Name, Platform, Year, Genre, Publisher))


matriz_distancias <- dist(train_set)
m <- as.matrix(matriz_distancias)

# Construir clustering jerárquico
hclust_aux <- hclust(matriz_distancias)
summary(hclust_aux)
print(hclust_aux)

plot(hclust_aux)
abline(h = 6, col = "red")

# Elegimos cómo cortar
# Cortar por altura
cutree(hclust_aux, h = 0.1)
# Cortar por número de clusters
cutree(hclust_aux, k = 1)


# Cluster usando método completo
hclust.complete <- hclust(dist(train_set), method = "complete")
plot(hclust.complete, main = "Distancia máxima: complete")
# Average linkage: hclust.average
hclust.average <- hclust(dist(train_set), method = "average")
plot(hclust.average, main = "Distancia media: average")
# Single linkage
hclust.single <- hclust(dist(train_set), method = "single")
plot(hclust.single, main = "Distancia mínima: single")
# Plot dendrogram de hclust.complete, hclust.average y hclust.single

# El más balanceado en este caso es el complete y el menos el single



# Escalamos:
colMeans(train_set)
apply(train_set, 2, sd)
puntos_escalado <- scale(train_set)
colMeans(puntos_escalado)
apply(puntos_escalado, 2, sd)






















# Split data into training and testing set
set.seed(1)
dt <- sort(sample(nrow(vgsales_preprocessed_concatenated_platform_dummies), nrow(vgsales_preprocessed_concatenated_platform_dummies)*.9))
train_set<-vgsales_preprocessed_concatenated_platform_dummies[dt,]
test_set<-vgsales_preprocessed_concatenated_platform_dummies[-dt,]

# Remove not useful columns
train_set <- select(train_set, -c(Name, First.Publisher., Sum.NA_Sales., Sum.EU_Sales., Sum.JP_Sales., Sum.Other_Sales., Sum.Global_Sales., Min..Year.))

# Train and plot tree
tree <- rpart(Supersale ~ ., train_set, method = "class")
fancyRpartPlot(tree)


pred <- predict(tree, test_set, type = "class")

# Construye la matriz de confusión
conf <- table(test_set$Supersale, pred)

# Calcula accuracy
acc <- sum(diag(conf)) / sum(conf)
print(acc)

# Punto 5, parece que sobreajusta
set.seed(1)
tree <- rpart(Survived ~., train, method="class",
              control=rpart.control(cp=0.00001))
fancyRpartPlot(tree)
