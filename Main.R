# Team: ZeaPol   
# Team Members: Roeland de Koning / Barbara Sienkiewicz    
# Date: 12/01/2015       
# Exercise 7

library(raster)
library(rgeos)
library(rgdal)
library(maptools)



load("data/GewataB1.rda")
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
load("data/vcfGewata.rda")
load("data/trainingPoly.rda")
load("data/lulcGewata.rda")
load("data/LUTGewata.rda")


########################################################################################
########################################################################################

# Part 1
# Produce one or more plots that demonstrate the relationship between the Landsat 
# bands and the VCF tree cover. What can you conclude from this/these plot(s)?


# Create the basic gewata raster and adjust it also produce the first required pairsplot:
gewata <- brick(GewataB1,GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)
names(gewata) <- c("band1", "band2", "band3", "band4",  "band5", "band7")
gewata <- calc(gewata, fun = function(x) x / 10000)

# Change VCF values larger than 100 into NA and plot the result:
vcfGewata[vcfGewata > 100] <- NA


names(vcfGewata) <- c("VCF")

gewatavcf <- addLayer(gewata, vcfGewata)

pairs(gewatavcf)

# Conclusion: Bands 3 and 7 are best corelated. Correlation coefficient for this pair is 0.96.

######################################################################################
######################################################################################

# optional data sources

# create NDVI raster:

# create basic mask versions (forest) of basic gewata raster as option:
lulc <- as.factor(lulcGewata)
levels(lulc) <- LUTGewata
classes <- layerize(lulc)
names(classes) <- LUTGewata$Class
plot(classes, legend = FALSE)
forest <- classes$forest
forest[forest == 0] <- NA
gewataforest <- mask(gewatavcf, forest, filename = "output/LandSatBandsVCFForestRelationship", overwrite = T)
plot(gewataforest)

############################################################################################
###########################################################################################

# Part 2
# Create an lm() model and show a summary (e.g. using summary()) of the model object you created. 
# Which predictors (bands) are probably most important in predicting tree cover?


# Create the polygon training classification sytem, data for the lm:
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
classes <- rasterize(trainingPoly, gewatavcf , field='Code')
names(classes) <- c("class")
cols <- c("orange", "dark green", "light blue")

# create a masked version of the training model:
gewatatraining <- mask(gewatavcf, classes)
gewatatraining <- addLayer(gewatatraining,classes)
gewatatraining[gewatatraining < 0] <- NA
gewatatraining[gewatatraining > 100] <- NA

# Create gewatatraining data frame:

gewatatrainingtable <- getValues(gewatatraining)
gewatatrainingtable2 <- na.omit(gewatatrainingtable)
gewatatrainingDF <- as.data.frame(gewatatrainingtable2)

head(gewatatrainingDF, n = 10)

# run the linear model on the gewatavcfDF data frame use most predicitve bands (3 and 7)
lm.a1 <- lm(formula = VCF ~ band2 + band3 + band7, data = gewatatrainingDF)
summary(lm.a1)

############################################################################################
###########################################################################################

# Part 3
# Plot the predicted tree cover raster and compare with the original VCF raster.

lmgewatapredict <- predict(gewatavcf, model=lm.a1, filename = 'output/lmgewatapredict', progress = 'text', overwrite = T, na.omit = T)

lmgewatapredict[lmgewatapredict < 0] <- NA
lmgewatapredict[lmgewatapredict > 100] <- NA

cols <- c("orange", "dark green", "light blue")

difference <- vcfGewata - lmgewatapredict

par(mfrow=c(1, 3))

plot(lmgewatapredict, col=cols, main = "Predicted")

plot(vcfGewata, col=cols, main = "Actual")

plot(difference, col=cols, main = "Difference")


############################################################################################
###########################################################################################

# Part 4
# compute the RMSE between your predicted and the actual tree cover values

source("R/RMSE.R")

gewatarmse <- RMSE(vcfGewata, lmgewatapredict)

gewatarmse

############################################################################################
###########################################################################################

# Part 5
# Are the differences between the predicted and actual tree cover the same for all of the 3 classes we used 
# for the random forest classfication? Using the training polygons from the random forest classification, 
# calculate the RMSE separately for each of the classes and compare. Hint - see ?zonal().

gewatatrainingDF$class <- factor(gewatatrainingDF$class, levels = c(1:3))

gewata_crop <- subset(gewatatrainingDF, class == 1)
gewata_forest <- subset(gewatatrainingDF, class == 2)
gewata_wetland <- subset(gewatatrainingDF, class == 3)

lm.c <- lm(formula = VCF ~ band2 + band3 + band7, data = gewata_crop)
summary(lm.c)
lm.f <- lm(formula = VCF ~ band2 + band3 + band7, data = gewata_forest)
summary(lm.f)
lm.w <- lm(formula = VCF ~ band2 + band3 + band7, data = gewata_wetland)
summary(lm.w)

gewata_croppred <- predict(gewatavcf, model=lm.c, filename = 'output/gewata_croppred', progress = 'text', overwrite = T, na.omit = T)
gewata_forestpred <- predict(gewatavcf, model=lm.f, filename = 'output/gewata_forestpred', progress = 'text', overwrite = T, na.omit = T)
gewata_wetlandpred <- predict(gewatavcf, model=lm.w, filename = 'output/gewata_wetlandpred', progress = 'text', overwrite = T, na.omit = T)

croprmse <- RMSE(vcfGewata, gewata_croppred)
forestrmse <- RMSE(vcfGewata, gewata_forestpred)
wetlandrmse <- RMSE(vcfGewata, gewata_wetlandpred)

croprmse
forestrmse
wetlandrmse


