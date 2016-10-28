#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 2: Principal Component Analysis
## 10/27/2016
##
## Makelike.R
##############################

setwd("Analysis/Data/")
source('Communities_clean.R', echo = TRUE)
setwd('../')
source('Communities_PCA.R', echo = TRUE)
source('Communities_PCA_CV.R', echo = TRUE)
setwd('../')
