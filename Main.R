# Team: ZeaPol
# Team Members: Roeland de Koning / Barbara Sienkiewicz
# Date: 12/01/2015
# Exercise 7
library(raster)
library(knitr)

load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")
load("data/trainingPoly.rda")

########################################################################################
########################################################################################

# Part 1
# Produce one or more plots that demonstrate the relationship between the Landsat
# bands and the VCF tree cover. What can you conclude from this/these plot(s)?
# Create the basic gewata raster and adjust it also produce the first required pairsplot:

gewata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
names(gewata) <- c("band1", "band2", "band3", "band4", "band5", "band7")
vcfGewata[vcfGewata > 100] <- NA
names(vcfGewata) <- c("VCF")
covs <- addLayer(gewata, vcfGewata)
pairs(covs)

# Conclusion: Bands 3 and 7 are the best correlated. Correlation coefficient for this pair is 0.96.
# in general the correlation is rather low for all the bands however they are the best

######################################################################################
######################################################################################

# Part 2
# Create an lm() model and show a summary (e.g. using summary()) of the model object you created.
# Which predictors (bands) are probably most important in predicting tree cover?

# Create covs data frame:
covstable <- getValues(covs)
covstable2 <- na.omit(covstable)
covsDF <- as.data.frame(covstable2)
head(covsDF, n = 10)

# run the linear model on the gewatavcfDF data frame use most predicitve bands (3 and 7)

lm.trees <- lm(formula = VCF ~ band1 + band2 + band3 + band5 + band7, data = covsDF)
summary(lm.trees)

############################################################################################
###########################################################################################

# Part 3
# Plot the predicted tree cover raster and compare with the original VCF raster.

predfullVCF <- predict(covs, model=lm.trees, filename = 'output/predfullVCF', progress = 'text', overwrite = T, na.omit = T)
predfullVCF[predfullVCF < 0] <- NA
predfullVCF[predfullVCF > 100] <- NA
difference <- overlay(vcfGewata, predfullVCF, fun = function(x, y){(x-y)}, overwrite = T,
                      filename = "output/VCFGewataDiff")
par(mfrow=c(1, 3))
cols <- c("orange", "dark green", "light blue")
plot(predfullVCF, col=cols, zlim=c(0,100), main = "Predicted Gewata VCF")
plot(vcfGewata, col=cols, main = "Actual Gewata VCF")
plot(difference, main = "Difference in VCF", zlim=c(-30,30), col = colorRampPalette(c("yellow", "white", "black"))(30))
par(mfrow = c(1,1))

############################################################################################
###########################################################################################

# Part 4
# compute the RMSE between your predicted and the actual tree cover values

source("R/RMSE.R")

fullVCFrmse <- RMSE(vcfGewata, predfullVCF)
fullVCFrmse

# Full RMSE
# 9.412138

############################################################################################
###########################################################################################

# Part 5
# Are the differences between the predicted and actual tree cover the same for all of the 3 classes we used
# for the random forest classfication? Using the training polygons from the random forest classification,
# calculate the RMSE separately for each of the classes and compare. Hint - see ?zonal().

# Create the polygon training classification sytem, data for the subsets:

trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
classes <- rasterize(trainingPoly, covs , field='Code')
names(classes) <- c("class")

# create a new raster stack version of the classes cov that includes pred, actual and classes:

covs.classes <- addLayer(predfullVCF, vcfGewata, classes)
covs.classes <- mask(covs.classes, classes)
names(covs.classes) <- c("predfullVCF", "actualVCF", "class")

# mask  layers using the classes training polygons and calculate RMSE for the three
# sub classes
covs.classes.masked <- mask(covs.classes, classes)
difference.covs <- overlay(covs.classes.masked$actualVCF, covs.classes.masked$predfullVCF, fun = function(x, y){(x-y)})
diff.cov.squared <- difference.covs^2
diff.cov.mean <- zonal(diff.cov.squared, covs.classes.masked$class, fun = 'mean')
RMSE.cov <- sqrt(diff.cov.mean[, 2])
RMSE.cov

# RMSE Result Crop Forest Wetland
# 10.511162  5.969928 10.922856

# the RMSE is different for all three subclasses crops, wetlands and forests. 
# the only one to present a lower RMSE  then for the full VCF was the second class forest. 
# this possibly suggests the other classes increase the relative error overall
