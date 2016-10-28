#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 2: Principal Component Analysis
## 10/27/2016
##
## Communities_PCA.R
##############################

require(formattable)

# Create principal components
prin.Comp <- prcomp(comm2.expl, center = TRUE, scale. = TRUE)
prin.Comp$center # Output variable means
prin.Comp$scale  # Output variable scale values

# Review principle component loadings
formattable(prin.Comp$rotation)
biplot(prin.Comp, scale = 0)

# Output other PC descriptives
prin.Comp$center # Output variable means
prin.Comp$scale  # Output variable scale values
prin.Comp$sdev   # Output the standard deviations of the principal components

# Compute variance of PCs to see which PCs explain the most variance
prin.Comp.var <- (prin.Comp$sdev)^2
format(prin.Comp.var, scientific = FALSE)

# Compute proportion of explained variance for each PC
PCvar.prop <- prin.Comp.var/sum(prin.Comp.var)
format(PCvar.prop, scientific = FALSE)

# Output Scree Plot
plot(PCvar.prop, xlab = "Principal Components",
     ylab = "Proportion of Explained Variance", type = "o")

# Output Cumulative Variance Plot
plot(cumsum(PCvar.prop), xlab = "Principal Components",
     ylab = "Cumulative Proportion of Explained Variance", type = "o")
