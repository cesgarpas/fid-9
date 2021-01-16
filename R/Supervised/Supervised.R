# Libraries
install.package("caret")
install.package("dplyr")
library(caret)
library(dplyr)

install.packages("rattle")
library(rpart)
library(rattle)
library(RColorBrewer)

# Dataset and utils
vgsales_preprocessed <- read.csv("data/vgsales_preprocessed.csv", sep = ",", head = TRUE)
vgsales_preprocessed_concatenated_platform <- read.csv("data/vgsales_preprocessed_concatenated_platform.csv", sep = ",", head = TRUE)

disctinct_platforms <- distinct(vgsales_preprocessed,Platform)

print(disctinct_platforms)

####################### Extra processing ####################### 

# Group by name
vgsales_preprocessed_concatenated_platform_dummies <- vgsales_preprocessed_concatenated_platform

# Create dummy variables depending on if the attribute Concatenate.Platform contains the platform name previously distincted
for (i in 1:length(disctinct_platforms[,])) {
  platform_dummy_name <- paste("Platform-", disctinct_platforms[i,], sep="")
  
  vgsales_preprocessed_concatenated_platform_dummies <-
    vgsales_preprocessed_concatenated_platform_dummies %>%
    mutate(!!toString(platform_dummy_name) := 
             (grepl(disctinct_platforms[i,], vgsales_preprocessed_concatenated_platform_dummies$Concatenate.Platform, fixed = TRUE)))
}


# Delete unused variables
vgsales_preprocessed_concatenated_platform_dummies <- select(vgsales_preprocessed_concatenated_platform_dummies, -c(Concatenate.Platform., row.ID))

print(vgsales_preprocessed_concatenated_platform_dummies[2,])
head(vgsales_preprocessed_concatenated_platform_dummies)

####################### Training ####################### 

train<-read.delim("data/train.csv", sep = "\t", head = TRUE)
rownames(train) <- train$id
train$id <- NULL

train <- 

set.seed(1)

tree <- rpart(Survived~., train, method = "class")

fancyRpartPlot(tree)


test<-read.delim("data/test.csv", sep = "\t", head = TRUE)
rownames(test) <- test$id
test$id <- NULL

pred <- predict(tree, test, type = "class")

# Construye la matriz de confusión
conf <- table(test$Survived, pred)

# Calcula accuracy
acc <- sum(diag(conf)) / sum(conf)
print(acc)

# Punto 5, parece que sobreajusta
set.seed(1)
tree <- rpart(Survived ~., train, method="class",
              control=rpart.control(cp=0.00001))
fancyRpartPlot(tree)
