#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 2: Principal Component Analysis
## 10/27/2016
##
## Communities_PCA_CV.R
##############################

require(pls)

# Set seed for repeatability
set.seed(10)

# Function to provide output plots of PCR model
PCR.out <- function(mymodel){
    print(summary(mymodel))
    
    validationplot(mymodel) #Plot root mean squared error
    validationplot(mymodel,val.type="MSEP") #Plot mean squared error
    validationplot(mymodel,val.type="R2") #Plot R2
    predplot(mymodel) #Plot predicted vs. measured values
}

# Commented out because would rather see PRESS values than coefficent of variance (cv) values for cross-validation
# comm2_pcr_cv <- pcr(ViolentCrimesPerPop~., data = comm2, scale = TRUE, validation = "LOO")#, segments = 10)
# PCR.out(comm2_pcr_cv)

# Perform PCR with leave-one-out cross-validation (PRESS statistic) to see how many components PCR identifies as appropriate
comm2_pcr <- pcr(ViolentCrimesPerPop~., data = comm2, scale = TRUE)
comm2_cv  <- crossval(comm2_pcr, length.seg = 1) #Cross-validate using leave-one-out
comm2_cv$validation$PRESS #NOTICE PRESS VALUE DECREASES THROUGH 15 PCs AND THEN RISES AT 16 PCs
PCR.out(comm2_pcr)
#CROSS-VALIDATION PROCEDURE IDENTIFIES 15 PCs AS BEING THE APPROPRIATE NUMBER OF COMPONENTS

# Observe PCR results for first 15 components
comm2_pcr.ncomp <- pcr(ViolentCrimesPerPop~., data = comm2, ncomp = 15, scale = TRUE, validation = "none")#, segments = 10)
PCR.out(comm2_pcr.ncomp)