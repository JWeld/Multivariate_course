# Script for thesing data quality, by Ulf Grandin 2015

setwd("C:/Users/grandin/Documents/Ulf IVM/Undervisning/MVA molecular data/2015/Exercises")
library(tidyverse)

library("vegan") # Package for multivariate analyses of ecological data
library("labdsv") # Another package for multivariate analyses of ecological data
library("reshape2")
library("dendextend")
library("sparcl")
library("outliers")
library("gdata")


##### Read data ###########################
# Read small species dataset
spe<-read.table("Data/species12x10.txt", sep="\t",header=T,row.names=1)

# Read environmental  dataset
env<-read.table("Data/env12x5.txt", sep="\t",header=T,row.names=1)

#################################
#### Check of data quality #######

# Create some outliers in a temporary dataset called mvo (acronym for Multi Variate Outlier)
mvo<-as.matrix(spe)
n_outl<- 3 # Give number of outliers to be created
v_outl<-30 # Give value of outlier(s)
mvo[base::sample(1:length(mvo),n_outl, replace = FALSE)]<-v_outl # Replace some data with outliers, at random positions
mvo
# The line below is for saving the mvo data set to your working directory
#write.table(mvo, file="mvo.txt",quote = F, sep = "\t", dec = ",", na = "NA") # to save mvo as a text file


# Cleveland plot to inspect data
dotchart(mvo, lcolor=col(mvo), main="Cleveland plot", xlab="Observed value") # on a small dataset


# Find data > 9, as no value should be higher than 9 in this data set
GT9 <- which(mvo > 9, arr.ind=TRUE)
GT9

# To scan all variables with Grubbs test for outliers
mvo_melt<-melt(mvo) # Restructure data

#spe_gather <- gather(spe)

do.call("rbind", with(mvo_melt, tapply(value, Var2, function(x) 
  unlist(grubbs.test(x)[c("statistic", "p.value", "alternative")]))))

# Indicative search for outliers. Many peaks and high SD indicate outliers may occur
dist1<-vegdist(mvo, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
d1<-data.frame(unmatrix(as.matrix(dist1), byrow = TRUE))
d1r<-t(data.frame(strsplit(row.names(d1), ":")))
d1rr<-data.frame(d1,d1r)
d1mean<-tapply(d1rr$unmatrix.as.matrix.dist1.., d1rr$X1, mean)
sd(d1mean) # Standard deviation of the mean distances. If > 3: strong outlier
plot(density(d1mean), col = "red", xlab="Distance between observations",main = "Indictive test of multivariate outliers")
legend("topright",paste("SD =",round(sd(d1mean),1))) # Legend with standard deviation

# Same as above but on the spe dataset (i.e. without outliers)
# Copy the 9 lines above and change "mvo" to "spe" in first line
dist1<-vegdist(spe, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = FALSE) 
d1<-data.frame(unmatrix(as.matrix(dist1), byrow = TRUE))
d1r<-t(data.frame(strsplit(row.names(d1), ":")))
d1rr<-data.frame(d1,d1r)
d1mean<-tapply(d1rr$unmatrix.as.matrix.dist1.., d1rr$X1, mean)
sd(d1mean) # Standard deviation of the mean distances. If > 3: strong outlier
plot(density(d1mean), col = "red", xlab="Distance between observations",main = "Indictive test of multivariate outliers")
legend("topright",paste("SD =",round(sd(d1mean),1))) # Legend with standard deviation


### Try ordinations to look for strange patterns
par(mfrow = c(1, 2)) # to get two plots side by side
# 1. on data with outliers
ca_outl<-cca(mvo)
ca_outl
plot(ca_outl,main="Data with outliers")

#2. On data without outliers
ca_orig<-cca(spe)
ca_orig
plot(ca_orig, main = "CA, Original data")
par(mfrow = c(1, 1)) # reset to 1 plot per page

# Test of gradient length
(DCA1<-decorana(spe))
plot(DCA1, main ="DCA, Original data")

#Species area curve ("sac") to understand data structure
sac<-specaccum(mvo,method = "random", permutations = 200) # Random order of samples
plot(sac, col="blue", main = "Species area curve")

# Estimates of total species richness
richness <- poolaccum(mvo, permutations = 200)
summary(richness, display = c("S","chao"))
plot(richness, main = "Estimates of total species richness") # Not so informative for small data sets, but good for large

# Compare with similar analysis on the Swiss data set
swiss_spe<-read.table("Data/swiss_spe.txt", sep="\t", header=T,row.names=1)
swiss_richness<-poolaccum(swiss_spe, permutations = 200)
plot(swiss_richness)

### Detection of missing data #######
# Modify the "spe" dataset to have 20% missing values and empty samples and non-occurring species
spe2<-as.data.frame(lapply(spe, function(x) "is.na<-"(x, sample(seq(x), floor(length(x) * runif(1, 0, 0.2))))))
spe2$Sp1<-0
spe2[3,]<-0
row.names(spe2)<-row.names(spe)
#write.table(spe2, file = "spe2.txt",quote = F, sep = "\t", dec = ",", na = "NA") # to save mvo as a text file



# Counut number of missing values
sum(is.na(spe2))
spe2 #Look at the dataset

# Two ways of visualisation of missing data using the package "VIM"
library("VIM")
aggr(as.matrix(spe2[-c(3),-c(1)])) # spe2 without row 3 and column 1 as they are set to zeros above
matrixplot(spe2[-c(3),-c(1)]) # Quick visualisation of missing values


# Imputation of missing values. Do not put too much trust in the modelled values!
# Use visual results from matrixplot to find variables with missing data to speify the model
spe2Imp<-regressionImp(formula = Sp3~Sp2+Sp5+Sp6+Sp8+Sp9+Sp10, data = spe2[-c(3),-c(1)]) #Modify the formula accoring to your data
spe2Imp
matrixplot(spe2Imp[,-c(10)])


#### E N D :  Check of data quality #######
###########################################

#########################
#### START of exploartive analyses   

# Try a correspondennce analysis on the original data set
spe.CA<-cca(spe)
spe.CA
plot(spe.CA, main="CA plot")


# Try a principal components analysis (PCA) on the data set
env.PCA<-rda(env, cor=T)
env.PCA
plot(env.PCA, main="PCA plot")

# Convert the factor variables to Dummy variables
library(caret) # contains function dummyVars
env1<-env
env1[,c(6:12)] <- predict(dummyVars(~ Site + Fertilizer + Dose, data = env1), newdata = env1)
names(env1)[c(6:12)]<-c("Site_a","Site_b","Fert", "No_fert", "D_Small", "D_Mid","D_Hi")
env1

# Try a new principal components analysis (PCA) on the data set
env1.PCA<-rda(env1[,c(1:2,6:12)], cor=T)
env1.PCA
plot(env1.PCA, main="PCA plot, dummy transformed data")


# Try a correspondennce analysis on the data set with missing values
spe2.CA<-cca(spe2)

# Remove rows with missing values
spe3<-spe2[complete.cases(spe2),]
spe3

# Try a correspondennce analysis on the new data set
spe3.CA<-cca(spe3)

# Remove rows(=samples) and columns(=species) with  with no species
spe4<-spe3[!rowSums(spe3, na.rm=T)==0,!colSums(spe3, na.rm=T)==0]
spe4

# Try a correspondennce analysis on the new data set
spe4.CA<-cca(spe4)
spe4.CA
plot(spe4.CA, main= "CA, cleared data set")


# To remove rows in the environmental data that are not any longer in the species data
env_spe<-merge(env,spe4, by=0)
env_spe
row.names(env_spe)<-env_spe$Row.names
env_spe$Row.names<-NULL
env_spe

# Split env_spe into two separate data sets
spe5<-env_spe[,c(6:ncol(env_spe))]
spe5
env3<-env_spe[,c(1:ncol(env))]
env3
str(env3)


# Test the correlation between species and environmental data in the reduced data set
# 1. Make a new ordination in the new speies data
spe5.CA<-cca(spe5)
#2. Test the correlation
envfit(ord=spe5.CA, env=env3, permutations = 999)

# Compare with "envfit" on the original data sets
envfit(spe.CA, env, 999)


# End
##################


























# Figures for ppt.
CCA4<-cca(formula=spe~Site, data=env)
CCA4
plot(CCA4, main = "CCA Factor env.var.")
with(env, ordispider(CCA4, group=Site, lty=3, col="blue"))

CCA5<-cca(formula=spe~Env1+Env2, data=env)
CCA5
plot(CCA5, main = "CCA Continous env. var.")

CCA6<-cca(formula=spe~Env1+Site, data=env)
CCA6
plot(CCA6, main = "CCA Continous  and factorial env. var.")
with(env, ordispider(CCA6, group=Site, lty=3, col="blue"))

# Removed from exercise:
# Finally, try a "shotgun" CCA with all data
all.CCA<-cca(formula=spe~., data=env)
all.CCA
plot(all.CCA, main = "CCA all data")
with(env, ordispider(all.CCA, group=Site, lty=3, col="blue"))

# Compare the CCA with a CA
par(mfrow=c(1,2))
plot(all.CCA, display="sites", main = "CCA all data")
plot(spe.CA, display="sites", main = "CA all data")
par(mfrow=c(1,1))
