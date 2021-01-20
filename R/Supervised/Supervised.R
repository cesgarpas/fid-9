####################### Libraries #######################
install.package("caret")
install.package("dplyr")
install.packages("rattle")

library(caret)
library(dplyr)

library(rpart)
library(rattle)
library(RColorBrewer)


####################### Read dataset #######################
vgsales_preprocessed <- read.csv("data/vgsales_preprocessed.csv", sep = ",", head = TRUE)
vgsales_preprocessed_concatenated_platform <- read.csv("data/vgsales_preprocessed_concatenated_platform.csv", sep = ",", head = TRUE)


####################### Config variables #######################
filter_year_min <- 2000
filter_year_max <- 2015
supersale_threshold_percent <- 0.5
train_set_percentage <- 0.8
set.seed(1)


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
supersale_threshold_index <- round(supersale_threshold_percent * length(vgsales_preprocessed_concatenated_platform_dummies[,1]))

# Sort by global sales
vgsales_preprocessed_concatenated_platform_dummies <- vgsales_preprocessed_concatenated_platform_dummies[order(-vgsales_preprocessed_concatenated_platform_dummies$Sum.Global_Sales.),]

# Get index
supersale_threshold <- vgsales_preprocessed_concatenated_platform_dummies[supersale_threshold_index,]$Sum.Global_Sales.

######## Create dummy variables for platform and supersale variable depending on the threshold ########
#   if the attribute Concatenate.Platform contains the platform name previously distincted
#   if it is a "supersale" 
for (i in 1:length(disctinct_platforms[,])) {
  platform_dummy_name <- paste("Platform-", disctinct_platforms[i,], sep="")
  
  vgsales_preprocessed_concatenated_platform_dummies <-
    vgsales_preprocessed_concatenated_platform_dummies %>%
    mutate(!!toString(platform_dummy_name) := 
             (grepl(disctinct_platforms[i,], vgsales_preprocessed_concatenated_platform_dummies$Concatenate.Platform, fixed = TRUE))) %>% 
    mutate(Supersale = (vgsales_preprocessed_concatenated_platform_dummies$Sum.Global_Sales. >= supersale_threshold))
}


######## Add market dummies ########
vgsales_preprocessed_concatenated_platform_dummies <-
  vgsales_preprocessed_concatenated_platform_dummies %>%
  mutate(Selling_EU = vgsales_preprocessed_concatenated_platform_dummies$Sum.EU_Sales. >= 0.01) %>%
  mutate(Selling_NA = vgsales_preprocessed_concatenated_platform_dummies$Sum.NA_Sales. >= 0.01) %>%
  mutate(Selling_JP = vgsales_preprocessed_concatenated_platform_dummies$Sum.JP_Sales. >= 0.01) %>%
  mutate(Selling_Other = vgsales_preprocessed_concatenated_platform_dummies$Sum.Other_Sales. >= 0.01)

######## Delete unused variables ########
vgsales_preprocessed_concatenated_platform_dummies <- select(vgsales_preprocessed_concatenated_platform_dummies, -c(Concatenate.Platform., row.ID))


####################### Training #######################
######## rpart Tree ######## 
# Split data into training and testing set
dt <- sort(sample(nrow(vgsales_preprocessed_concatenated_platform_dummies), nrow(vgsales_preprocessed_concatenated_platform_dummies) * train_set_percentage))
train_set<-vgsales_preprocessed_concatenated_platform_dummies[dt,]
test_set<-vgsales_preprocessed_concatenated_platform_dummies[-dt,]

# Remove not useful columns
train_set <- select(train_set, -c(Name, First.Publisher., Sum.NA_Sales., Sum.EU_Sales., Sum.JP_Sales., Sum.Other_Sales., Sum.Global_Sales., Min..Year.))
#train_set <- select(train_set, c(First.Genre., Supersale))

# Train and plot tree
tree <- rpart(Supersale ~ ., train_set, method = "class")
fancyRpartPlot(tree)

# Print train tree error
printcp(tree)

######## Metrics ######## 
# Predict with the test_set using the tree
pred <- predict(tree, test_set, type = "class")

# Conf. matrix creation
conf <- table(test_set$Supersale, pred_class)

# Accuracy metric
acc <- sum(diag(conf)) / sum(conf)
print(acc)



############################################################################
# Recall metric
#recall(test_set, relevant = rownames(test_set)[1])

#pred_noclass <- predict(tree, test_set)
# Sensitivity metric
#sensitivity(factor(round(pred[,2]), factor(as.numeric(test_set$Supersale))))

# Specificity metric
#specificity(factor(pred[,2]), factor(as.numeric(test_set$Supersale)))


