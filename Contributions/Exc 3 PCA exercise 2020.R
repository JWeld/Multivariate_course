# Exercise 3: PCA


### <span style="color:cornflowerblue">Question 3: In this	exercise we will use Principal Components Analysis on the dune meadow explanatory data. We will also explore the effects of different kinds of scaling on the ordination plot. NOTE that the scaling issues applies to all ordination techniques! </span>

#### <span style="color:forestgreen"> Compare the plots on the PCA with and without scaling (PCA plot 4) and discuss the differences. 
#Is there any way to see that one is on scaled data and one is not? 
#What is the difference between PCA plot 4 and PCA plot 5? 
#Which one is best to use? Discuss the difference between the plots in plot PCA plot 6 and 7!
#</span>



library(tidyverse)
library(vegan)
library(corrplot)

data("dune")
data("dune.env")
dummy_management <- as.data.frame(model.matrix( ~ Management - 1, data=dune.env )) 
#add these to the dataset
dune.env.original <- dune.env #we keep a copy of the original version
dune.env <- dune.env %>% select(A1, Moisture, Manure, Use) %>% cbind(.,dummy_management) 
dune.env$Moisture <- as.numeric(as.character(dune.env$Moisture)) #make numeric
dune.env$Manure <- as.numeric(as.character(dune.env$Manure))
dune.env$Use <- as.numeric(dune.env$Use)
#make column names shorter
dune.env <- dune.env %>% rename(BF = ManagementBF, HF = ManagementHF,
                                NM = ManagementNM, SF = ManagementSF)

## PCA plot 1:
# Start by looking at pairwise correlations among the variables.
dune.env_cor<-cor(dune.env, method = "kendall")
corrplot(dune.env_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

## PCA plot 2:
# Then make a PCA, with scaling of the data
env.pca <- rda(dune.env, scale = TRUE) #vegan uses the same function for PCA and RDA, just depends on if it is constrained or not.
biplot(env.pca) #plot the results using the default plot scaling which is "species"
env.pca #summarise results
summary(eigenvals(env.pca)) #see variance explained

## PCA plot 3:
# Continue with a PCA on the same data, but without scaling of the data
env.pca2 <- rda(dune.env, scale = FALSE) #vegan uses the same function for PCA and RDA, just depends on if it is constrained or not.
biplot(env.pca2)#plot the results
env.pca2 #summarise results
summary(eigenvals(env.pca2)) #see variance explained

## PCA plot 4:
# Compare the PCAs with and without scaling of data
# Default scaling of the plots, which is "species"
par(mfrow = c(1,2))
biplot(env.pca, main = "PCA with scaling of data")
biplot(env.pca2, main = "PCA without scaling of data")
par(mfrow = c(1,1))

## PCA plot 5:
# Plots from the same PCA, but with plot scaling focused on sites and on species
par(mfrow = c(1,2))
biplot(env.pca, scaling = "sites", main = "Plot scaling on sites")
biplot(env.pca, scaling = "species", main = "Plot scaling on species")
par(mfrow = c(1,1))

## PCA plot 6:
# Then, a plot from the PCA on un-scaled data, 
# but with plot scaling to mimic a PCA on scaled data (correlation = TRUE)
# Second plot is the PCA on scaled data
# Both plots with plot scaling focused on species
par(mfrow = c(1,2))
biplot(env.pca2, scaling = "species", correlation = TRUE, main = "PCA on un-sclaed data,\nplot scaling on correlations")
biplot(env.pca, scaling = "species", correlation = FALSE, main = "PCA on scaled data")
par(mfrow = c(1,1))

## PCA plot 7:
# Finally a plot from the PCA on un-scaled data,
# but with plot scaling to mimic a PCA on scaled data
# Second plot is the PCA on scaled data
# Both plots with plot scaling focused on sites
par(mfrow = c(1,2))
biplot(env.pca2, scaling = "sites", correlation = TRUE, main = "PCA on un-sclaed data,\nplot scaling on correlations")
biplot(env.pca, scaling = "sites", main = "PCA on scaled data")
par(mfrow = c(1,1))


