#
# NMDS - Non Metric Multidimensional Scaling
#
# At the conclusion of this exercise, you should be able to:
# (i)	State which metrics are supported in the vegan implementation of NMDS
# (ii)	Evaluate the appropriate number of dimensions for the ordination
# (iii)	Evaluate the consequences of using different distance metrics
# (iv)  Compare the vegan default NMDS to the results of a CA and a DCA
#
help(metaMDS)
#
# run NMDS with the defaults and 2 dimensions, what is the stress?
#
dune.NMDS.bray.2<-metaMDS(dune,k=2)
#
# run NMDS with the defaults and 3 dimensions, what is the stress ?
#
dune.NMDS.bray.2<-metaMDS(dune,k=3)
#
# what happens when Euclidean distances are used?
#
dune.NMDS.euclidean.2<-metaMDS(dune,k=2)
#
# plot some results
#
ordiplot(dune.NMDS.bray.2, type="text", main="NMDS from Bray Curtis distance, 2 Dimensions")
ordiplot(dune.NMDS.euclidean.2, type="text", main="NMDS from Euclidean distance, 2 Dimensions")
#
# create Shepard plots to show how the stress evolves
#
stressplot(dune.NMDS.bray.2)
stressplot(dune.NMDS.euclidean.2)
#
# generate the CA and DCA results
#
dune.CA<-cca(dune)
dune.DCA<-decorana(dune)
#
# plot the CA and DCA results
#
ordiplot(dune.CA, type="text", main="CA on Dune Meadow data")
ordiplot(dune.DCA, type="text", main="DCA on Dune Meadow data")


