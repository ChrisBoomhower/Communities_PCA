#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 2: Principal Component Analysis
## 10/27/2016
##
## Communities_PCA_CV.R
##############################

require(pls)

set.seed(10)

PCR.out <- function(mymodel){
    print(summary(mymodel))
    
    validationplot(mymodel) #Plot root mean squared error
    validationplot(mymodel,val.type="MSEP") #Plot mean squared error
    validationplot(mymodel,val.type="R2") #Plot R2
    predplot(mymodel) #Plot predicted vs. measured values
}

comm2_pcr_cv <- pcr(ViolentCrimesPerPop~., data = comm2, scale = TRUE, validation = "LOO")#, segments = 10)
PCR.out(comm2_pcr_cv)

comm2_pcr <- pcr(ViolentCrimesPerPop~., data = comm2, scale = TRUE)
comm2_cv  <- crossval(comm2_pcr, length.seg = 1)
comm2_cv$validation$PRESS
PCR.out(comm2_pcr)

comm2_pcr.ncomp <- pcr(ViolentCrimesPerPop~., data = comm2, ncomp = 7, scale = TRUE, validation = "none")#, segments = 10)
PCR.out(comm2_pcr.ncomp)

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
