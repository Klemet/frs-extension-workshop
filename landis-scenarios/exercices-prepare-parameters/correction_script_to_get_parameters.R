####################################################################################
#### Author : Clément Hardy (clem.hardy@outlook.fr)                                #
####                                                                               #
#### This script is related to the training workshop of the FRS module.            #
#### It serves to compute parameters needs for the FRS module to function :        #
#### The basal construction cost, and the additional costs due to the slope.       #
#### As such, it serves as a correction for the period where the attendees will    #
#### try to compute those costs.                                                   #
####                                                                               #
####################################################################################

# 1) Loading the right working directory.
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# 2) Reading the .csv file containing the construction costs.
data <- read.csv(file="road_costs_data.csv",
                 header=TRUE,
                 sep=",")

# 3) We make sure that the different columns are treated as factors.
data$road_type <- as.factor(data$road_type)
data$slope <- as.factor(data$slope)
                        
# 4) We define the reference slope, that will be inside the basal cost : the 0_8% category.
data$slope <- relevel(data$slope, ref="0_8%")

# 5) We select only the data related to the type of path of reference.
dataSubset <- data[data$road_type == "Tertiary",]

# 6) We create the linear model expressing the total cost of construction
# according to an intercept (the basal cost of construction), and the effect
# of the 3 levels of slope.
linearModel <- lm(cost ~ slope, data=dataSubset)

# 7) We look at the summary of the model
summary(linearModel)

# 8) We check if the linear regression's assumptions are validated
mean(linearModel$residuals) # Mean of residuals is near zero, OK
plot(linearModel) # Homoscedasticity of residuals is OK
acf(linearModel$residuals) # No autocorrelation in residuals
cor.test(dataSubset$cost, linearModel$residuals) # Variable (cost) and residuals are not correlated

# 9) We gather the coefficients that we need to parameterize the FRS
# module : the intercept, and the additional cost due to the slope
coefficients <- summary(linearModel)$coefficients
print(coefficients)

# 10) Now, we get the mean cost of construction value for all of the types of road,
# regardless of the slope.

meanCostPrimaryRoads <- mean(data[data$road_type == "Primary","cost"])
meanCostSecondaryRoads <- mean(data[data$road_type == "Secondary","cost"])
meanCostTertiaryRoads <- mean(data[data$road_type == "Tertiary","cost"])

# 11) Then, we compute the multiplicative cost for each road type by
# doing the ratio of the mean cost of construction for this road type,
# divided by the mean cost for the reference road type (here, "tertiary")

multiplicativeValuePrimary <- meanCostPrimaryRoads / meanCostTertiaryRoads
multiplicativeValueSecondary <- meanCostSecondaryRoads / meanCostTertiaryRoads
multiplicativeValueTertiary <- 1 # it's one, since it's the reference

print(multiplicativeValuePrimary)
print(multiplicativeValueSecondary)
print(multiplicativeValueTertiary)
