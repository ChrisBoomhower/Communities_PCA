#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 2: Principal Component Analysis
## 10/27/2016
##
## Communities_PCA.R
##############################

# Create principal components (CURRENTLY USING HALF OF ORIGINAL DATA VIA test.expl.
# TO USE FULL DATA SET REPLACE test.expl WITH comm2.expl)
prin.Comp <- prcomp(test.expl, center = TRUE, scale. = TRUE)

# Review principle component loadings
formattable(prin.Comp$rotation)
biplot(prin.Comp, scale = 0)

# Output other PC descriptives
prin.Comp$center # Output variable means
prin.Comp$scale  # Output variable scale values
prin.Comp$sdev   # Output the standard deviations of the principal components

# Compute variance of PCs to see which PCs explain the most variance
prin.Comp.var <- (prin.Comp$sdev)^2
#format(prin.Comp.var, scientific = FALSE)

# Compute proportion of explained variance for each PC
PCvar.prop <- prin.Comp.var/sum(prin.Comp.var)
#format(PCvar.prop, scientific = FALSE)

# Compute cumulative proportion of explained variance
PCvar.cumprop <- cumsum(PCvar.prop)

# Summarize the data
#summary(prin.Comp)
PC.summary <- data.frame(Eigenvalue = prin.Comp.var, Variance.Proportion = PCvar.prop,
                         Variance.CumProportion = PCvar.cumprop)
formattable(PC.summary)

# Output Scree Plot
plot(PCvar.prop, xlab = "Principal Components",
     ylab = "Proportion of Explained Variance", type = "o", col = "dodgerblue2",
     main = "Scree Plot")

# Output Cumulative Variance Plot
plot(cumsum(PCvar.prop), xlab = "Principal Components",
     ylab = "Cumulative Proportion of Explained Variance", type = "o", col = "darkorchid3",
     main = "Cumulative Variance Plot")

# I THINK THE FIRST 7 PCs SHOULD BE USED BASED ON THE PLOT RESULTS
# theta <- seq(0,2*pi,length.out = 100)
# circle <- data.frame(x = cos(theta), y = sin(theta))
# p <- ggplot(circle,aes(x,y)) + geom_path()
# 
# loadings <- data.frame(prin.Comp$rotation, 
#                        .names = row.names(prin.Comp$rotation))
# p + geom_text(data=loadings, 
#               mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
#     coord_fixed(ratio=1) +
#     labs(x = "PC1", y = "PC2") + theme(legend.position="none")
