# PCoA Exercise
#
# Principal Coordinates Analysis (PCoA), also known as Multidimensional Scaling (MDS), is a generalization
# of the ordination techniques we have already seen as it can be estimated with any distance matrix. In
# contrast, PCA is restricted to Euclidean distances while CA preserves Chi-Square distances.
#
# Here, we will explore PCoA on four different distance matrices: 
#	(i)   Bray-Curtis
#	(ii)  Jaccard
#	(iii) Euclidean
#	(iv)  Mahalanobis
#
# The R base statistics library offers the routine cmdscale() for performing MDS. There are numerous 
# other libraries offering PCoA analysis
#
# After performing the four PCoA analyses, please answer the following questions:
#	(i) 	How similar are the ordination plots from the four distance matrices?
#   (ii)	Why are some plots more similar than others?
#	(iii)	Which PCoA plot is most similar to the output from a PCA?
#	(iv)	Which PCoA plot is most similar to the output from a CA?
#
# generate the four distance matrices
#
dune.bray.distance.matrix<-vegdist(dune,method="bray")
dune.jaccard.distance.matrix<-vegdist(dune,method="jaccard")
dune.euclidean.distance.matrix<-vegdist(dune,method="euclidean")
dune.mahalanobis.distance.matrix<-vegdist(dune,method="mahalanobis")
#
# what can be accomplished with the MDS routine?
#
help(cmdscale)
#
# run an MDS using cmdscale() for each of the four distance matrices
#
dune.bray.location<-cmdscale(dune.bray.distance.matrix)
dune.jaccard.location<-cmdscale(dune.jaccard.distance.matrix)
dune.euclidean.location<-cmdscale(dune.euclidean.distance.matrix)
dune.mahalanobis.location<-cmdscale(dune.mahalanobis.distance.matrix)
#
# now take a look at some plots
#
plot(dune.bray.location, xlab="PCoA 1", ylab="PCoA 2")
title(main="PCoA on Bray Curtis distances of dune sites")
text(dune.bray.location,labels=row.names(dune.bray.location),pos=2)
#
plot(dune.jaccard.location, , xlab="PCoA 1", ylab="PCoA 2")
title(main="PCoA on Jaccard distances of dune sites")
text(dune.jaccard.location,labels=row.names(dune.jaccard.location),pos=2)
#
plot(dune.euclidean.location, xlab="PCoA 1", ylab="PCoA 2")
title(main="PCoA on Euclidean distances of dune sites")
text(dune.euclidean.location,labels=row.names(dune.euclidean.location),pos=2)
#
plot(dune.mahalanobis.location, xlab="PCoA 1", ylab="PCoA 2")
title(main="PCoA on Mahalanobis distances of dune sites")
text(dune.mahalanobis.location,labels=row.names(dune.mahalanobis.location),pos=2)