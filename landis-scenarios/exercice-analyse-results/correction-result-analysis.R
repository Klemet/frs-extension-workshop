####################################################################################
#### Author : Clément Hardy (clem.hardy@outlook.fr)                                #
####                                                                               #
#### This script is related to the training workshop of the FRS module.            #
#### It serves to analyse the results of the LANDIS-II scenarios with and without  #
#### cut aggregation.                                                              #
#### As such, it serves as a correction for the period where the attendees will    #
#### try to analyze the results by computing road density, different fragmentation #
#### indices, and road costs.                                                      #
####                                                                               #
####################################################################################

#### 1) Install and load the needed packages ####

install.packages("gtools", "dplyr", "raster", "landscapemetrics")
library(gtools)
library(dplyr)
library(raster)
library(landscapemetrics)

###############################################################################

#### 2) Defining the paths to the files we're looking for ####

# Set ourselves where the present file is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Define the different paths that we will need
roadOutputsScenarioNoAgg <- "../scenario-no-aggregation/output/disturbances/roads/"
roadOutputsScenarioWithAgg <- "../scenario-with-aggregation/output/disturbances/roads/"
sharedRasters <- "../shared-rasters/"

###############################################################################

#### 3) Get the list of road raster files we need ####

# We get all of the rasters of road density
print("Reading the raster road files for scenario \"no aggregation\".")

# List all the files in the folder
listOfRoadRasterFilesNoAggregation = list.files(roadOutputsScenarioNoAgg)
# Keep the files with .tif in them, and without the word "flux" or "Cost" in them
listOfRoadRasterFilesNoAggregation = Filter(function(x) grepl("tif", x, fixed = TRUE), listOfRoadRasterFilesNoAggregation)
listOfRoadRasterFilesNoAggregation = Filter(function(x) !(grepl("Flux|Cost|xml|aux", x, fixed = FALSE)), listOfRoadRasterFilesNoAggregation)
# Order the files in natural order
listOfRoadRasterFilesNoAggregation = mixedsort(listOfRoadRasterFilesNoAggregation, decreasing = TRUE)

# Same thing for the scenario with aggregation
print("Reading the raster road files for scenario \"with aggregation\".")
listOfRoadRasterFilesWithAggregation = list.files(roadOutputsScenarioNoAgg)
listOfRoadRasterFilesWithAggregation = Filter(function(x) grepl("tif", x, fixed = TRUE), listOfRoadRasterFilesWithAggregation)
listOfRoadRasterFilesWithAggregation = Filter(function(x) !(grepl("Flux|Cost|xml|aux", x, fixed = FALSE)), listOfRoadRasterFilesWithAggregation)
listOfRoadRasterFilesWithAggregation = mixedsort(listOfRoadRasterFilesWithAggregation, decreasing = TRUE)

###############################################################################

#### 4) Getting the ecoregion raster ready ####

# We are going to reclassify the ecoregion raster to
# define two types of pixels : not forest (0), and
# forest (every other number).

# We load the raster of ecoregions right now
ecoregionsRaster <- raster(paste(sharedRasters, "ecoregions.tif", sep = ''))

# We define the vector for reclassification
# This 
reclass_vector_ecoregions <- c(-1, 1, 0,
                               1, Inf, 1)

# Reclassify the raster
forestRasters <- reclassify(ecoregionsRaster, reclass_vector_ecoregions)
# Importantly, we fill the NA values in the raster with 0. This is important
# to avoid errors when the fragmentation indices are computed.
forestRasters[is.na(forestRasters[])] <- 0 

###############################################################################

#### 5) Computing and plotting the road density #####

# We will fill a dataframe with the percentage of cells in the landscape
# occupied by roads at any point.
preAllocationLength <- length(listOfRoadRasterFilesWithAggregation)
roadDensityDf <- data.frame(Timestep = integer(preAllocationLength),
                            Road_Density_No_Agg = double(preAllocationLength),
                            Road_Density_With_Agg = double(preAllocationLength))
colnames(roadDensityDf) = c("Timestep", "Road density - No-Agg scenario", "Road density - With-Agg scenario")

# We loop around each raster file
for(i in seq(0, length(listOfRoadRasterFilesWithAggregation) - 1))
{
  # We get the number of pixels with roads (!= 0) in the road raster
  # for the no-aggregation scenario
  roadRasterNoAggregation <- raster(paste(roadOutputsScenarioNoAgg, listOfRoadRasterFilesNoAggregation[i+1], sep = ''))
  totalNumberOfPixels <- sum(freq(roadRasterNoAggregation)[,2])
  numberOfRoadPixels <- sum(freq(roadRasterNoAggregation)[2:nrow(freq(roadRasterNoAggregation)),2])
  roadDensityNoAggregation <- (numberOfRoadPixels/totalNumberOfPixels) * 100
  
  # We do the same for the with-aggregation scenario
  roadRasterWithAggregation <- raster(paste(roadOutputsScenarioWithAgg, listOfRoadRasterFilesWithAggregation[i+1], sep = ''))
  totalNumberOfPixels <- sum(freq(roadRasterWithAggregation)[,2])
  numberOfRoadPixels <- sum(freq(roadRasterWithAggregation)[2:nrow(freq(roadRasterWithAggregation)),2])
  roadDensityWithAggregation <- (numberOfRoadPixels/totalNumberOfPixels) * 100
  
  # We insert the results into the data frame
  roadDensityDf[i+1,] <- c(i*10, roadDensityNoAggregation, roadDensityWithAggregation)
}

# We plot the first line, for the no-agg scenario
plot(roadDensityDf$`Road density - No-Agg scenario`~roadDensityDf$Timestep,
     main = "Road density throught time\nin LANDIS-II simulations",
     type="b",
     bty="l",
     xlab="Time",
     ylab="Road density (%)",
     col=rgb(0.2,0.4,0.1,0.7),
     lwd=3,
     pch=17)

# We plot the second line
lines(roadDensityDf$`Road density - With-Agg scenario`~roadDensityDf$Timestep,
      col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )

# We add a legend to the plot
legend("bottomright", 
       legend = c("No-Agg Scenario", "With-Agg Scenario"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

###############################################################################

#### 6) Computing the road costs ####

# The road cost are inside the file "Forest Roads Construction Log.csv" at the root
# of each LANDIS-II scenario. Let's get them.

roadLogNoAgg <- read.csv(file="../scenario-no-aggregation/Forest Roads Construction Log.csv",
                       header=TRUE,
                       sep=",")
roadLogWithAgg <- read.csv(file="../scenario-with-aggregation/Forest Roads Construction Log.csv",
                         header=TRUE,
                         sep=",")

# We plot the data with the same functions as before.

# We plot the first line, for the no-agg scenario
plot(roadLogNoAgg$CostOfConstructionAndRepairs~roadLogWithAgg$Timestep,
     main = "Road costs throught time\nin LANDIS-II simulations",
     type="b",
     bty="l",
     xlab="Time",
     ylab="Costs of construction and repairs of roads ($CA)",
     col=rgb(0.2,0.4,0.1,0.7),
     lwd=3,
     pch=17)

# We plot the second line
lines(roadLogWithAgg$CostOfConstructionAndRepairs~roadLogWithAgg$Timestep,
      col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )

# We add a legend to the plot
legend("topright", 
       legend = c("No-Agg Scenario", "With-Agg Scenario"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

###############################################################################

#### 7) Computing fragmentation indices ####

# This computation is the most difficult, and will require us
# to "Mask" a raster containing the pixels where forests are with
# the rasters with roads; and then, to compute fragmentation indices
# on the resulting raster with the landscapemetrics package.

# First, we prepare a vector to reclassify the road raster
# With this, every pixel without a road will be 1, and every pixel
# with a road will be 0, allowing us to "mask" another raster.
reclass_vector_road <- c(-1, 1, 1,
                         1, Inf, 0)

# We prepare the list of results
listOfResultsNoAgg = list()
listOfResultsWithAgg = list()
# We loop around the raster files
for(i in seq(0, length(listOfRoadRasterFilesWithAggregation) - 1))
{
  # We load the road raster for the no-agg scenario
  roadRasterNoAggregation <- raster(paste(roadOutputsScenarioNoAgg, listOfRoadRasterFilesNoAggregation[i+1], sep = ''))
  
  # We give him the same extent (position in space) than the raster with the forest pixels
  # (This is because LANDIS-II outputs are no georeferenced)
  roadRasterNoAggregation = setExtent(roadRasterNoAggregation, forestRasters)
  
  # We reclassify the road raster
  roadRasterNoAggregation <- reclassify(roadRasterNoAggregation, reclass_vector_road)
  
  # Now, we mask the foret raster with the road raster;
  # This means that any pixel of raster with a road pixel on it
  # will be equal to 0, making it as it has no forest.
  maskedForestNoAgg = mask(forestRasters, roadRasterNoAggregation, maskvalue = 0, updatevalue = 0)
  
  # Now, we compute two fragmentation indices : Clumpy and TCA
  ClumpyResults = lsm_c_clumpy(maskedForestNoAgg)
  resultsNoAgg = ClumpyResults
  resultsNoAgg = bind_rows(resultsNoAgg,
                           lsm_c_tca(maskedForestNoAgg, directions = 4))
  
  # We put the results in the list of results
  listOfResultsNoAgg[[listOfRoadRasterFilesNoAggregation[i+1]]] <- resultsNoAgg
  
  # We do the same with the with-agg scenario
  roadRasterWithAggregation <- raster(paste(roadOutputsScenarioWithAgg, listOfRoadRasterFilesWithAggregation[i+1], sep = ''))
  roadRasterWithAggregation = setExtent(roadRasterWithAggregation, forestRasters)
  roadRasterWithAggregation <- reclassify(roadRasterWithAggregation, reclass_vector_road)
  maskedForestWithAgg = mask(forestRasters, roadRasterWithAggregation, maskvalue = 0, updatevalue = 0)
  ClumpyResults = lsm_c_clumpy(maskedForestWithAgg)
  resultsWithAgg = ClumpyResults
  resultsWithAgg = bind_rows(resultsWithAgg,
                             lsm_c_tca(maskedForestWithAgg, directions = 4))
  listOfResultsWithAgg[[listOfRoadRasterFilesWithAggregation[i+1]]] <- resultsWithAgg
}

# Now, we catch the results in the tibbles that landscapemetrics outputs
# into a dataframe, so that we can plot everything.
preAllocationLength <- length(listOfRoadRasterFilesWithAggregation)
fragmentationDF <- data.frame(Timestep = integer(preAllocationLength),
                            Clumpy_No_Agg = double(preAllocationLength),
                            Clumpy_With_Agg = double(preAllocationLength),
                            TCA_No_Agg = double(preAllocationLength),
                            TCA_With_Agg = double(preAllocationLength))
colnames(fragmentationDF) = c("Timestep", "Clumpy - No-Agg scenario", "Clumpy - With-Agg scenario",
                            "TCA - No-Agg scenario", "TCA - With-Agg scenario")
for(i in seq(0, length(listOfRoadRasterFilesWithAggregation) - 1))
{
  # We insert the results into the data frame
  fragmentationDF[i+1,] <- c(i*10, listOfResultsNoAgg[[i+1]]$value[2], listOfResultsWithAgg[[i+1]]$value[2],
                             listOfResultsNoAgg[[i+1]]$value[4], listOfResultsWithAgg[[i+1]]$value[4])
}

# Now, we plot the evolution of Clumpy between scenarios
plot(fragmentationDF$`Clumpy - No-Agg scenario`~fragmentationDF$Timestep,
     main = "Clumpy throught time\nin LANDIS-II simulations",
     type="b",
     bty="l",
     xlab="Time",
     ylab="Clumpy",
     col=rgb(0.2,0.4,0.1,0.7),
     lwd=3,
     pch=17,
     ylim = c(0.4,1))
lines(fragmentationDF$`Clumpy - With-Agg scenario`~fragmentationDF$Timestep,
      col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )
legend("topright", 
       legend = c("No-Agg Scenario", "With-Agg Scenario"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

# We do the same with TCA
plot(fragmentationDF$`TCA - No-Agg scenario`~fragmentationDF$Timestep,
     main = "TCA throught time\nin LANDIS-II simulations",
     type="b",
     bty="l",
     xlab="Time",
     ylab="TCA",
     col=rgb(0.2,0.4,0.1,0.7),
     lwd=3,
     pch=17)
lines(fragmentationDF$`TCA - With-Agg scenario`~fragmentationDF$Timestep,
      col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )
legend("topright", 
       legend = c("No-Agg Scenario", "With-Agg Scenario"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

