#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 2: Principal Component Analysis
## 10/27/2016
##
## Communities_PCA_CV.R
##############################

# HAD BEEN TRYING SOMETHING OUT IN AN EXAMPLE I WAS LOOKING AT
# require(rpart)
# 
# # Add dependent variable back into training set and select only first 6 PCs
# train.data <- data.frame(train.dep, prin.Comp$x)
# train.data <- train.data[,1:7]
# 
# # Process decision tree
# rpart.model <- rpart(train.dep ~ ., data = train.data, method = "anova")
# rpart.model
# 
# # Transform test data set into PCA
# test.data <- predict(prin.Comp, newdata = subset(test, select = -ViolentCrimesPerPop))
# test.data <- as.data.frame(test.data)
# test.data <- test.data[,1:6]
# 
# #make prediction on test data
# predict(rpart.model, test.data)
