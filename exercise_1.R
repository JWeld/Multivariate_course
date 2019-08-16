#Excercise 1
library(tidyverse)
library(vegan) # Package for multivariate analyses of ecological data



setwd("/home/james/Documents/R/Multivariate_course")



##### Read data ###########################

# Read dune species dataset
dune <- read.table("Data/Data_for_R/dune.txt", sep=";",header=T,row.names=1)

# Read environmental  dataset
env<-read.table("Data/Data_for_R/env.txt", sep="\t",header=T,row.names=1)
env$Management <- as.factor(env$Management) #convert to factor
#Management factor is character but we need it to be numeric for analyses. Convert to "dummy" variables...
#note! if you are converting a factor that is numbers already you need as.numeric(as.character(x))
management <- env$Management #keep the character data in case we need it later
env$Management <- as.numeric(env$Management)



###########################################

#Question 2: Do a PCA on the environmental data related to the Dune meadow dataset

env.pca <- rda(env, scale = TRUE)
plot(env.pca)
biplot(env.pca)
env.pca

#Question 3 : Try a CA on the environmental data

#unconstrained ordination on environmental data (CA)
env.ca <- cca(env) #yes, the function is called cca instead of ca
plot(env.ca)
env.ca

#Question 4 : Do a CA-ordination on the Dune Meadow species dataset
#unconstrained ordination on species (CA)
dune.ca <- cca(dune)
plot(dune.ca)
ordipointlabel(dune.ca)#easier to read

dune.cca
