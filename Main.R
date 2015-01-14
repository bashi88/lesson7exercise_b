# Team: ZeaPol   
# Team Members: Roeland de Koning / Barbara Sienkiewicz    
# Date: 12/01/2015       
# Exercise 7

library(raster)
library(rgeos)
library(rgdal)
library(maptools)
library(randomForest)
library(Metrics)

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
plot(vcfGewata)

names(vcfGewata) <- c("VCF")
summary(vcfGewata)

gewatavcf <- addLayer(gewata, vcfGewata)

hist(gewatavcf)
pairs(gewatavcf)

# Conclusion: Bands 3 and 7 are best corelated. Correlation coefficient for this pair is 0.96.

######################################################################################
######################################################################################

# optional data sources

# create NDVI raster:
ndvi <- overlay(GewataB4, GewataB3, fun = function(x,y){(x-y)/(x+y)})

# create basic mask versions (forest) of basic gewata raster as option:
lulc <- as.factor(lulcGewata)
levels(lulc) <- LUTGewata
classes <- layerize(lulc)
names(classes) <- LUTGewata$Class
plot(classes, legend = FALSE)
forest <- classes$forest
forest[forest == 0] <- NA
plot(forest, col = "dark green", legend = FALSE)
gewataforest <- mask(gewatavcf, forest, filename = "output/LandSatBandsVCFForestRelationship", overwrite = T)
plot(gewataforest)

############################################################################################
###########################################################################################

# Part 2
# Create an lm() model and show a summary (e.g. using summary()) of the model object you created. 
# Which predictors (bands) are probably most important in predicting tree cover?


# Create the polygon training classification sytem, data for the lm:
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
classes <- rasterize(trainingPoly, ndvi, field='Code')
names(classes) <- c("class")
cols <- c("orange", "dark green", "light blue")
plot(classes, col=cols, legend=FALSE)
legend("topright", legend=c("cropland", "forest", "wetland"), fill=cols, bg="white")

# create a masked version of the training model:
gewatatraining <- mask(gewatavcf, classes)
gewatatraining <- addLayer(gewatatraining,classes)
plot(gewatatraining)

# Create the gewataavf and gewatatraining data frame:
gewatavcftable <- getValues(gewatavcf)
gewatavcftable2 <- na.omit(gewatavcftable)
gewatavcfDF <- as.data.frame(gewatavcftable2)

#OKAY IM DONE NA OMIT DOES NOT SEEM TO BE WORKING NEED TO USE THE DATAFRAME IN THE MODELRF TO PREDICT THE FOREST
#ANYWAY DOESNT WORK FOR NOW GOOD LUCK ILL TRY IT TOO

gewatatrainingtable <- getValues(gewatatraining)
gewatatrainingtable2 <- na.omit(gewatatrainingtable)
gewatatrainingDF <- as.data.frame(gewatatrainingtable2)

head(gewatavcfDF, n = 10)
head(gewatatrainingDF, n = 10)

# run the linear model on the gewatavcfDF data frame use most predicitve bands (3 and 7)
lm.a1 <- lm(band3 ~ band7, data = gewatavcfDF)
summary(lm.a1)



############################################################################################
###########################################################################################

# Part 3
# Plot the predicted tree cover raster and compare with the original VCF raster.

model.rf <- randomForest(x = gewatatrainingDF[ ,c(3,6)], y = gewatatrainingDF$class, importance = TRUE)

predLC <- predict(gewatatraining, model=lm.a1, na.rm=TRUE)

cols <- c("orange", "dark green", "light blue")
plot(predLC, col=cols, legend = FALSE)


par(mfrow=c(1, 1))
gewatalm <-  addLayer(predLC,vcfGewata)
plot(gewatalm)
gewatalmforest <- mask(gewatalm,forest)
plot(gewatalmforest)


############################################################################################
###########################################################################################

# Part 4
# compute the RMSE between your predicted and the actual tree cover values

predLCandVCFrmse <- rmse(vcfGewata, predLC, na.rm=TRUE)
plot(predLCandVCFrmse)




############################################################################################
###########################################################################################

# Part 5
# Are the differences between the predicted and actual tree cover the same for all of the 3 classes we used 
# for the random forest classfication? Using the training polygons from the random forest classification, 
# calculate the RMSE separately for each of the classes and compare. Hint - see ?zonal().

zonal(())

zonal(vcfGewata, predLC, fun ='rmse', digits = 0, na.rm = TRUE)
