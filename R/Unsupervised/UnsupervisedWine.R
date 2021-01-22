install.packages("factoextra")
install.packages("fastDummies")
install.packages("psych")
install.packages("GPArotation")

library(factoextra)
library(fastDummies)
library(psych)
library(GPArotation)

library(datasets)
library(readr)
library(data.table)
library(dplyr)

# Load Data
path <- file.path("../../Datasets/StatewiseTestingDetails.csv", fsep="/") #Set path
#mydataraw <- read_sav(path) # Read .sav file from the path
mydata <- read.csv(path) #Read .csv file
head(mydata)

# Prepare Data

# Remove variables to be excluded from analysis
#mydata <- mydata[-c(1)]

# Remove missing values
mydata <- na.omit(mydata)
head(mydata)

# Transform categorial variables into numerical flag values
mydata <- data.frame(model.matrix(~  .- 1, data = mydata))
head(mydata)


# Standardize data
mydatastd <- scale(mydata)
head(mydatastd)

ncol <- ncol(mydata)
ncol

# Store variable names
var <- list()
for(i in 1:ncol){
  var[[i]] <- names(mydata)[i]
}
var

# Rename columns for easier use
names(mydata)[1:ncol] <- paste("var", 1:ncol, sep="")

head(mydata)


### Hierarchical Clustering 
# Determine the number of clusters

# Elbow method
fviz_nbclust(mydatastd, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(mydatastd, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


# Set cluster number
m <- 3 ### CHANGE

# Set cluster name
c1 <- "cluster1"
c2 <- "cluster2"
c3 <- "cluster3"


# Compute
# Note: in this case, clustering works well in standardized dataset. 
# For some dataset, clustering works well in original dataset.
cluster <- mydatastd %>% 
  dist(method = "euclidean") %>% 
  hclust(method="ward.D")

ggdendrogram(cluster)

# Visualize
fviz_dend(cluster, k = m, # Cut in m groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)



# Bind data with respective clusters
groups <- cutree(cluster, k=m) # cut tree into 5 clusters

databind <- cbind(mydata, Cluster = groups)  # Bind data with respective clusters
head(databind)

# Histogram
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
ggplot(databind, aes(Cluster)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


# Visualization of variables and clusters
library(ggplot2)
library(gridExtra)

xy <- data.frame(cmdscale(dist(mydata)), factor(groups))
names(xy) <- c("x", "y", "cluster")
xy$model <- rownames(xy)

for(i in 2:ncol){
  for(j in i: ncol){
    p <- ggplot(xy, aes(mydata[[i-1]], mydata[[j]])) + geom_point(aes(colour=cluster)) + labs(x = var[[i-1]], y = var[[j]])         
    print(p)
  }
}

## Creating a summary table for clusters
Meanvar1 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = mean(var1))

SDvar1 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = sd(var1))


# Calculating mean, Standard Deviation of Var2 by cluster
Meanvar2 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = mean(var2))

SDvar2 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = sd(var2))

# Calculating mean, Standard Deviation of Var3 by cluster
Meanvar3 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = mean(var3))
SDvar3 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = sd(var3))

# Calculating mean, Standard Deviation of Var3 by cluster
Meanvar4 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = mean(var4))

SDvar4 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = sd(var4))

# Calculating mean, Standard Deviation of Var5 by cluster
Meanvar5 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = mean(var5))

SDvar5 <- databind %>% 
  group_by(Cluster) %>% 
  summarise(average = sd(var5))

clustercount <- t(data.frame(sum(databind$Cluster == 1), sum(databind$Cluster == 2), sum(databind$Cluster == 3),stringsAsFactors=FALSE))

# Combining data frames
var1table <- merge(Meanvar1, SDvar1, by = "Cluster", sort = TRUE)
colnames(var1table) <- c("Cluster","Mean1", "SD1")

var2table <- merge(Meanvar2, SDvar2, by = "Cluster", sort = TRUE)
colnames(var2table) <- c("Cluster","Mean2", "SD2")

var3table <- merge(Meanvar3, SDvar3, by = "Cluster", sort = TRUE)
colnames(var3table) <- c("Cluster","Mean3", "SD3")

var4table <- merge(Meanvar4, SDvar4, by = "Cluster", sort = TRUE)
colnames(var4table) <- c("Cluster","Mean4", "SD4")

var5table <- merge(Meanvar5, SDvar5, by = "Cluster", sort = TRUE)
colnames(var5table) <- c("Cluster","Mean5", "SD5")

table <- var1table %>% merge(var2table, by = "Cluster", sort = TRUE) %>% merge(var3table, by = "Cluster", sort = TRUE) %>%
  merge(var4table, by = "Cluster", sort = TRUE) %>%  merge(var5table, by = "Cluster", sort = TRUE) 

table <- cbind(table, "Elements in Cluster" =clustercount[, 1])



table <- table[c(0,1, 12, 2, 3,4,5,6,7,8,9,10,11)] #Change the order of columns

labels <- data.frame( "", "", var[[1]], var[[1]], var[[2]], var[[2]],  var[[3]], var[[3]],  var[[4]], var[[4]],  var[[5]], var[[5]])
colnames(labels) <- c("Cluster","Elements in Cluster", "Mean1", "SD1",  "Mean2", "SD2",  "Mean3", "SD3", "Mean4", "SD4", "Mean5", "SD5")

table <- rbind(table, labels)

rownames(table) <- c(c1,c2,c3,"")
table


### K-means Clustering
set.seed(123)
clustering <- kmeans(mydata, centers = m, nstart = 20)
clustering

# Get Average silhouette width
sil <- silhouette(clustering$cluster, dist(mydata))
fviz_silhouette(sil)

# Bind data with respective clusters
result = kmeans(mydata, m, nstart = 50, iter.max = 10)
databind <- cbind(mydata, Cluster = result$cluster)
head(databind)

# Histogram
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
ggplot(databind, aes(Cluster)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

# Visualize clusters and variables
plot(mydata,col=result$cluster)
points(result$center,col=1:2,pch=8,cex=1)
