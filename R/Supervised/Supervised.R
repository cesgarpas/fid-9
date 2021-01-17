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
vgsales_preprocessed <- read.csv("data/vgsales_preprocessed.csv", sep = ",", head = TRUE)
vgsales_preprocessed_concatenated_platform <- read.csv("data/vgsales_preprocessed_concatenated_platform.csv", sep = ",", head = TRUE)

disctinct_platforms <- distinct(vgsales_preprocessed,Platform)

print(disctinct_platforms)

####################### Extra processing ####################### 

# Group by name
vgsales_preprocessed_concatenated_platform_dummies <- vgsales_preprocessed_concatenated_platform

# Create dummy variables depending on 
#   if the attribute Concatenate.Platform contains the platform name previously distincted
#   if it is a "superventas" 
for (i in 1:length(disctinct_platforms[,])) {
  platform_dummy_name <- paste("Platform-", disctinct_platforms[i,], sep="")
  
  vgsales_preprocessed_concatenated_platform_dummies <-
    vgsales_preprocessed_concatenated_platform_dummies %>%
    mutate(!!toString(platform_dummy_name) := 
             (grepl(disctinct_platforms[i,], vgsales_preprocessed_concatenated_platform_dummies$Concatenate.Platform, fixed = TRUE))) %>% 
    mutate(Supersale = (vgsales_preprocessed_concatenated_platform_dummies$Sum.Global_Sales. >= 0.2))
}

head(vgsales_preprocessed_concatenated_platform_dummies)

# Delete unused variables
vgsales_preprocessed_concatenated_platform_dummies <- select(vgsales_preprocessed_concatenated_platform_dummies, -c(Concatenate.Platform., row.ID))

####################### Training ####################### 

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
