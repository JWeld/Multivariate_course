#
# Classification 1
#
# Start by testing the non-hierarchical K-means clustering, using Multivar / K-Means. Note that it is
# recommended that K-means need at least 100 observations (500 according to some sources) to be reliable!
# Let us ignore the sample size issue for the moment, and ask for 4 clusters (for later comparison with the
# four management types). Test different hierarchical agglomeration algorithms and similarity indices. Use
# at least the Euclidian and the Bray-Curtis similarity measures for the hierarchical clustering technique.
# Also try Ward's method 
#
# Questions:
# (i) 	How many clusters seem to exist in the Dune data set?
# (ii)	Is a 3- or 4-cluster solution a better match to the management type in the dune.env dataset?
# (iii) Do the two approaches to hierachical clustering reveal similar patterns in the Dune data set?
# (iv) Interpret the Hopkins statistic for the Dune data set
#
if(!is.element("clustertend",installed.packages()[,1])) {install.packages("clustertend") }
if(!is.element("factoextra",installed.packages()[,1])) {install.packages("factoextra") }
if(!is.element("NbClust",installed.packages()[,1])) {install.packages("NbClust") }
#
# Have a look to see what NbClust does
#
help(NbClust)
#
# make a distance matrix using the Bray-Curtis method
#
dune.dist <- vegdist(dune, method = "bray")
#
# see how many clusters there appear to be in the data
#
res<-NbClust(dune, diss=dune.dist, distance = NULL, 
	min.nc=2, max.nc=12, method = "kmeans", index = "kl") 
	
library(dendextend)
#
# K-Means Cluster Analysis based on the previously identified number of clusters
#
dune.fit.3 <- kmeans(dune, 3, nstart = 50) 
#
# As the final result of k-means clustering result is sensitive to the random starting assignments, 
# we specify nstart = 50. This means that R will try 50 different random starting assignments and then
# select the best results corresponding to the one with the lowest within cluster variation.
# Note that k-means clustering involves randomness so you won't neccessarily get exactly the same results 
# if you repeat it.
#
# There are also several different algorithms that can be used in k-means clustering, and different software 
# have can use different defaults.Check out the help file for the kmeans function if you want to know more
# about these.
#
#cluster number for each data point
#
dune.fit.3$cluster
#
#cluster sizes
#
dune.fit.3$size
#
#compare clusters to management category
#
table(dune.fit.3$cluster, dune.env.original$Management)
#
# there are four management categories, try k-means clustering with k=4
#
dune.fit.4 <- kmeans(dune, 4, nstart = 50) 
#
#cluster number for each data point
#
dune.fit.4$cluster
#
#cluster sizes
#
dune.fit.4$size
#
#compare clusters to management category
#
table(dune.fit.4$cluster, dune.env.original$Management)
#
#try some approaches to hierarchical clustering
#
dend2 <- dune %>% # data
        dist(method = "euclidean") %>% # calculate a distance matrix, choose method 
        hclust(method = "ward.D") %>% # Hierarchical clustering, choose method 
        as.dendrogram # Turn the object into a dendrogram.
dend2 %>% set("labels_color") %>% set("branches_lwd") %>%  set("branches_k_color", k = 4) %>% plot
#
#the dist() function does not include Bray-Curtis but we have already made a distance matrix using 
#the vegdist() function from vegan.
#
dend3 <- dune.dist %>% # data (Bray-Curtis)
        hclust(method = "aver") %>% # Hierarchical clustering, choose method 
        as.dendrogram # Turn the object into a dendrogram.
dend3 %>% set("labels_color") %>% set("branches_lwd") %>%  set("branches_k_color", k = 4) %>% plot
#
#we can colour the labels based on their grouping in the kmeans analysis to compare clusters
#
dend3 %>% set("labels_color", fit$cluster) %>% set("branches_lwd") %>%  set("branches_k_color", k = 4) %>% plot
#
#tanglegram is a nice function to compare dendrograms that you might want to try out as an extra
#
tanglegram(dend2, dend3)
#
# finally, should the Dune data be clustered at all?
#
library(clustertend)
#
# Compute Hopkins statistic for the Dune dataset
#
set.seed(123)
#
hopkins(dune, n = nrow(dune)-1)