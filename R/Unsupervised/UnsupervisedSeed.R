install.packages("GGally")
install.packages("foreign")
install.packages("cluster")
install.packages("vegan")

library(dplyr)
library(tidyr)
library(GGally)
library(gridExtra)
library(factoextra)
library(FactoMineR)
library(foreign)
library(cluster)
library(vegan)

############ KMEANS ############
seed <- read.csv("../../Datasets/Seed_Data.csv")
glimpse(seed)

# check missing value
anyNA(seed)

# rename column and correcting data type
names(seed) <- c("Area", "Perimeter", "Compactness", "Length", "Width", "Asymetry.coef", "Grove.length", "Type")
seed
seed$Type <- as.numeric(seed$Type+1)
str(seed)

types<-seed$Type

seed <- select(seed, -c('Type'))
head(seed)
# Data scalated
scalated_seed <- scale(seed)
scalated_seed

# Calculate distance matrix
distance_matrix<-dist(scalated_seed)

# Calculate kmeans
km_seed<-kmeans(scale(seed),centers = 3, nstart = 20)

# Plot kmeans
plot(seed,col=(km_seed$cluster)*9, main=km_seed$tot.withinss)
# fviz_cluster(km_seed, data = seed)

summary(km_seed)

# Compare clusters based on type
table(km_seed$cluster,types)

conf <- table(km_seed$cluster, types)

# Accuracy metric
acc <- sum(diag(conf)) / sum(conf)
acc

#Silhouette

kmeans_dist = vegdist(seed)
kmeans_sil = silhouette (km_seed$cluster,kmeans_dist) # or use your cluster vector
windows() # RStudio sometimes does not display silhouette plots correctly
plot(kmeans_sil)




############ PAM (KMEDIOIDES) ############
seed_kmd <- read.csv("../../Datasets/Seed_Data.csv")
glimpse(seed_kmd)

# check missing value
anyNA(seed_kmd)

# rename column and correcting data type
names(seed_kmd) <- c("Area", "Perimeter", "Compactness", "Length", "Width", "Asymetry.coef", "Grove.length", "Type")

types<-seed_kmd$Type

#seed_kmd <- select(seed_kmd, c('Area', 'Perimeter'))
seed_kmd
# seed$Type <- as.factor(seed$Type)
str(seed_kmd)



# Data scalated
scalated_seed_mkd <- scale(seed_kmd)
scalated_seed_mkd

# Calculate distance matrix
distance_matrix<-dist(scalated_seed_mkd)

# Calculate pam
km_seed_kmd<-pam(scale(seed_kmd), 3)

plot(km_seed_kmd,col=(km_seed_kmd$cluster)*10, main=km_seed_kmd$tot.withinss)

summary(km_seed_kmd)

# Compare clusters based on type
table(km_seed_kmd$cluster,types)


#Silhouette
dis = vegdist(seed_kmd)
sil = silhouette (km_seed_kmd$cluster,dis) # or use your cluster vector
windows() # We noticed RStudio sometimes does not display silhouette plots correctly
plot(sil)










############ COSAS RARAS DE ALFONSO QUE NO SABEMOS DE DONDE HAN SALIDO ############
seed_3 <- read.csv("../../Datasets/Seed_Data.csv")
glimpse(seed_3)

# check missing value
anyNA(seed_3)

# rename column and correcting data type
names(seed_3) <- c("Area", "Perimeter", "Compactness", "Length", "Width", "Asymetry.coef", "Grove.length", "Type")
seed_3
seed_3$Type <- as.numeric(seed_3$Type+1)
str(seed_3)

types_3<-seed_3$Type

seed_3 <- select(seed_3, -c('Type'))
head(seed_3)
# Data scalated
scalated_seed_3 <- scale(seed_3)
scalated_seed_3

# Calculate distance matrix
distance_matrix<-dist(scalated_seed_3)


# Hierarchical clusterization method
ag1 = agnes(seed_3, metric = "euclidean", stand=TRUE) #STAND TRUE es pa los datos tipificados
summary(ag1)

plot(ag1, which.plots=c(2), main="Dendograma")



