#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 2: Principal Component Analysis
## 10/27/2016
##
## Communities_PCA_CV.R
##############################

# Set seed for repeatability
set.seed(10)

# Function to provide output plots of PCR model
PCR.out <- function(mymodel){
    print(summary(mymodel))
    
    validationplot(mymodel) #Plot root mean squared error
    validationplot(mymodel,val.type="MSEP") #Plot mean squared error
    validationplot(mymodel,val.type="R2") #Plot R2
    predplot(mymodel,line = TRUE) #Plot predicted vs. measured values
}

# Obtain Root Mean Square Error coefficent of variance (cv) values for leave-one-out cross-validation
train_pcr_cv <- pcr(ViolentCrimesPerPop~., data = train, scale = TRUE, validation = "LOO")#, segments = 10)
PCR.out(train_pcr_cv)

# Perform PCR with leave-one-out cross-validation (PRESS statistic) to see how many components PCR identifies as appropriate
train_pcr <- pcr(ViolentCrimesPerPop~., data = train, scale = TRUE)
train_cv  <- crossval(train_pcr, length.seg = 1) #Cross-validate using leave-one-out
train_cv_melt <- melt(train_cv$validation$PRESS)
names(train_cv_melt) <- c("Response.Var", "PC", "PRESS")
formattable(train_cv_melt) #NOTICE PRESS VALUE DECREASES THROUGH 15 PCs AND THEN RISES AT 16 PCs
PCR.out(train_pcr)
#CROSS-VALIDATION PROCEDURE IDENTIFIES 15 PCs AS BEING THE APPROPRIATE NUMBER OF COMPONENTS

# Observe PCR results for first 20 components
train_pcr.ncomp <- pcr(ViolentCrimesPerPop~., data = train, ncomp = 20, scale = TRUE, validation = "none")#, segments = 10)
PCR.out(train_pcr.ncomp)

# Calculate R^2 and adjusted R^2
Rsqr <- R2(train_pcr.ncomp)
Rsqr.adj <- as.vector(1-(1-Rsqr$val)*(803/(804-Rsqr$comps-1))) # Adjusted R^2 calculation
Rsqr.data <- data.frame(Rsqr$comps, Rsqr$val[1:21], Rsqr.adj)
names(Rsqr.data) <- c("PC", "Rsqr", "Rsqr.adj")
formattable(Rsqr.data)

# Plot adjusted R^2
ggplot(data=Rsqr.data, aes(x=PC, y=Rsqr.adj, group=1)) +
    geom_line(colour="firebrick", size=1) + 
    geom_point(colour="dodgerblue2", size=3, shape=21, fill="white") +
    ggtitle("Adjusted-R-Squared Values")

# Observe loadings again, but in plot form
plot(train_pcr.ncomp, "loadings", comps = 1:2, legendpos = "topright", xlab = "Variables by Number",
     ylim = c(-0.3,0.3))
abline(h = 0)#, v = seq(1,88,1))




# Compare predicted root-mean-square error against test set root-mean-square-error
RMSEP(train_pcr.ncomp, newdata = test)

# Predict ViolentCrimesPerPop using principal components model (currently comparing 7 and 15 PCs)
formattable(as.data.frame(predict(train_pcr.ncomp, ncomp = c(7,15), newdata = test)))
predplot(train_pcr.ncomp, ncomp = c(7,15), newdata = test, line = TRUE, col = "dodgerblue2")
