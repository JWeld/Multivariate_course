#
# Comparing ordinations
# 
# These are multiple ways of comparing ordinations or the structure in related multivariate datasets.
# Here, we will use Procrustes analysis, Co-correspondence analysis and perform a Mantel test. 
#
# These methods are used to compare how objects differ in ordination space when they are described by
# different sets of descriptors (e.g., plants and insects), or if there are different ordiantion methods
# (e.g., CA and PCA) applied to the same datasets.
#
# In this exercise, we will compare DCAs of the dune meadow sites based on plant and insect data
#
# Questions:
#  (i)  How similar is the arrangemont of sites in ordination space based on plant and insect data?
#  (ii) Do the three analyses (Procrustes, Co-Correspondence and Mantel) indicate similar odinations of 
#       sites based on plant vs. insect data?
#
#
# some housekeeping
#
if(!is.element("cocorresp",installed.packages()[,1])) {install.packages("cocorresp") }
library(cocorresp)
library(vegan)
data(dune)
#
# DCA on the plant species data 
#
plants.DCA <- decorana(dune)
#
# DCA on the insect data (called Bugs)
#
insects <- readRDS("Bugs.RDS")
insects.DCA <- decorana(insects)
#
# Have a look at the DCA based on insects
#
insects.DCA
#
# perform a Procrustes analysis on the two DCAs from different datasets
#
plants.and.insects.from.DCA <- procrustes(plants.DCA, insects.DCA)
plot(plants.and.insects.from.DCA, kind=1, type="text")
#
# Labels show the position of the samples in the second ordination, and arrows point to their positions in the
# target ordination Function protest tests the non-randomness (`significance') between two configurations
#
ProCsig <- protest(plants.DCA, insects.DCA)
ProCsig
#
# Co-correspondence analysis
#
CoCor <- coca(dune ~ ., data = insects, method = "symmetric")
summary(CoCor)
corAxis(CoCor)
#
# set up and perform the Mantel test
#
insects.distance<-vegdist(insects)
plants.distance<-vegdist(dune)
#
# what does mantel() do?
#
help(mantel)
#
# perform the mantel test using parametric and non-parametric correlations
mantel(insects.distance,plants.distance)
mantel(insects.distance,plants.distance,method="spear")