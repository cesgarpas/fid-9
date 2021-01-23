####################### Libraries #######################
install.package("caret")
install.package("dplyr")
install.packages("rattle")
install.packages("e1071")


library(caret)
library(dplyr)

library(rattle)
library(RColorBrewer)

library(rpart)
library(e1071)
library(party)


####################### Read dataset #######################
vgsales_preprocessed <- read.csv("../../Datasets/vgsales_preprocessed.csv", sep = ",", head = TRUE)
vgsales_preprocessed_concatenated_platform <- read.csv("../../Datasets/vgsales_preprocessed_concatenated_platform.csv", sep = ",", head = TRUE)


####################### Config variables #######################
filter_year_min <- 2000
filter_year_max <- 2015
supersale_threshold_percent <- 0.5
train_set_percentage <- 0.75


####################### Extra processing #######################
# Distinct platforms
disctinct_platforms <- distinct(vgsales_preprocessed,Platform)

# Variable for processing the dataset
vgsales_preprocessed_concatenated_platform_dummies <- vgsales_preprocessed_concatenated_platform


######## Filter by year ######## 
vgsales_preprocessed_concatenated_platform_dummies <-
  vgsales_preprocessed_concatenated_platform_dummies %>%
  filter(Min..Year. <= filter_year_max & Min..Year. >= filter_year_min)

######## Generate supersale threshold ########
# Get index
supersale_threshold_percent <- 0.5
supersale_threshold_index <- round(supersale_threshold_percent * length(vgsales_preprocessed_concatenated_platform_dummies[,1]))

# Sort by global sales
vgsales_preprocessed_concatenated_platform_dummies <- 
  vgsales_preprocessed_concatenated_platform_dummies[order(-vgsales_preprocessed_concatenated_platform_dummies$Sum.Global_Sales.),]

# Get supersales threshold value
supersale_threshold <- vgsales_preprocessed_concatenated_platform_dummies[supersale_threshold_index,]$Sum.Global_Sales.

vgsales_preprocessed_concatenated_platform_dummies <- vgsales_preprocessed_concatenated_platform_dummies %>% 
  mutate(Supersale = (vgsales_preprocessed_concatenated_platform_dummies$Sum.Global_Sales. >= supersale_threshold))


######## Create dummy variables for platform ########
#   if the attribute Concatenate.Platform contains the platform name previously distincted
for (i in 1:length(disctinct_platforms[,])) {
  platform_dummy_name <- paste("Platform-", disctinct_platforms[i,], sep="")
  
  vgsales_preprocessed_concatenated_platform_dummies <-
    vgsales_preprocessed_concatenated_platform_dummies %>%
    mutate(!!toString(platform_dummy_name) := 
             (grepl(disctinct_platforms[i,], vgsales_preprocessed_concatenated_platform_dummies$Concatenate.Platform, fixed = TRUE)))
}


######## Add market dummies ########
vgsales_preprocessed_concatenated_platform_dummies <-
  vgsales_preprocessed_concatenated_platform_dummies %>%
  mutate(Selling_EU = vgsales_preprocessed_concatenated_platform_dummies$Sum.EU_Sales. >= 0.01) %>%
  mutate(Selling_NA = vgsales_preprocessed_concatenated_platform_dummies$Sum.NA_Sales. >= 0.01) %>%
  mutate(Selling_JP = vgsales_preprocessed_concatenated_platform_dummies$Sum.JP_Sales. >= 0.01) %>%
  mutate(Selling_Other = vgsales_preprocessed_concatenated_platform_dummies$Sum.Other_Sales. >= 0.01)

######## Delete unused variables ########
vgsales_preprocessed_concatenated_platform_dummies <- select(vgsales_preprocessed_concatenated_platform_dummies, 
                                                             -c(Name, First.Publisher., Sum.NA_Sales., Sum.EU_Sales., Sum.JP_Sales., Sum.Other_Sales., Sum.Global_Sales., Min..Year., Concatenate.Platform.))

head(vgsales_preprocessed_concatenated_platform_dummies)
####################### Training #######################
################ rpart Tree ################ 
# Split data into training and testing set
set.seed(1)
dt <- sort(sample(nrow(vgsales_preprocessed_concatenated_platform_dummies), nrow(vgsales_preprocessed_concatenated_platform_dummies) * train_set_percentage))
train_set<-vgsales_preprocessed_concatenated_platform_dummies[dt,]
test_set<-vgsales_preprocessed_concatenated_platform_dummies[-dt,]

# Remove not useful columns
#train_set <- select(train_set, c(First.Genre., Supersale))

# Train and plot tree
tree <- rpart(Supersale ~ ., train_set, method = "class")
fancyRpartPlot(tree)

######## Metrics ######## 
# Predict with the test_set using the tree
pred <- predict(tree, test_set, type = "class")

# Conf. matrix creation and metrics
conf <- table(test_set$Supersale, pred)
confusionMatrix(conf)

# Summary
summary(tree)



################ Naive Bayes Tree################ 
# Split data into training and testing set
set.seed(1)
dt <- sort(sample(nrow(vgsales_preprocessed_concatenated_platform_dummies), 
                  nrow(vgsales_preprocessed_concatenated_platform_dummies) * train_set_percentage))
train_set<-vgsales_preprocessed_concatenated_platform_dummies[dt,]
test_set<-vgsales_preprocessed_concatenated_platform_dummies[-dt,]

# Train and plot tree
tree <- naiveBayes(Supersale ~ ., train_set)

######## Metrics ######## 
# Predict with the test_set using the tree
pred <- predict(tree, test_set)

# Conf. matrix creation and metrics
conf <- table(test_set$Supersale, pred)
confusionMatrix(conf)



################ Ctree2 Tree################ 
# Split data into training and testing set
set.seed(1)
dt <- sort(sample(nrow(vgsales_preprocessed_concatenated_platform_dummies), 
                  nrow(vgsales_preprocessed_concatenated_platform_dummies) * train_set_percentage))
train_set<-vgsales_preprocessed_concatenated_platform_dummies[dt,]
test_set<-vgsales_preprocessed_concatenated_platform_dummies[-dt,]

train_set <- train_set %>%
  mutate(Supersale = factor(train_set$Supersale))
test_set <- test_set %>%
  mutate(Supersale = factor(test_set$Supersale))

tree <- train(
  Supersale ~., data = train_set, method = "ctree2",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(maxdepth = 3, mincriterion = 0.95 )
)
plot(tree$finalModel)

######## Metrics ######## 
# Predict with the test_set using the tree
pred <- predict(tree, test_set, type = "raw")

# Conf. matrix creation and metrics
conf <- table(test_set$Supersale, pred)
confusionMatrix(conf)
