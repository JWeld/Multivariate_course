

### <span style="color:cornflowerblue">Question 28: Differences bwtween groups. ANOSIM,adonis and SIMPER</span>

#### <span style="color:forestgreen"> What does the results from anosim and adonis tell you? Give one other example of predefined groups that could be uses as the grouping variable(s)! </span>

#ANOSIM (ANalysis Of Similarities) is a non-parametric test of significant difference between two or more groups, based on any distance measure. In this case, use the clusters from the K-means exercise. Change to Bray-Curtis similarity index.

# Start with a CA with Management as environmental factor and hulls around the different management types
dune.ca<-cca(dune)
fit.mgm<-envfit(dune.ca~Management, dune.env, perm = 0)
plot(dune.ca, type = "n", scaling = "symmetric")
with(dune.env, points(dune.ca, display = "sites", scaling = "symmetric", col = as.numeric(Management), pch=16))
with(dune.env, ordispider(dune.ca, Management, scaling = "symmetric", col="skyblue"))
with(dune.env, ordihull(dune.ca, Management, scaling = "symmetric",label = TRUE))

dune.dist <- vegdist(dune, method = "bray") #create distance matrix based on Bray-Curtis method
dune.ano <- anosim(dune.dist, dune.env$Management) #Comparing groupings based on management
summary(dune.ano)
plot(dune.ano, xlab = "Anosim, Dune meadow management types")

#ANOSIM gives you the P value and a R value. R value close to 1 indicates high separation between levels of your factor while R value #close to 0 indicate no separation between levels of your factor.

# Try also the recommended alternative "adonis" - Analysis of variance using distance matrices - for partitioning distance matrices among sources of variation and fitting linear models (e.g., factors, polynomial regression) to distance matrices; uses a permutation test with pseudo-F ratios.

dune.ado<-adonis(formula = dune~Management, data = dune.env, method = "bray")
dune.ado

# You can also have more advanced models in adonis
dune.ado2<-adonis(dune~Management*A1, data = dune.env)
dune.ado2


#  After a significant ANOSIM, you may want to know which species are primarily responsible for the observed difference between clusters. SIMPER (Similarity Percentage) will do this for you. The test does not come with significance testing. In the output tables, taxa are sorted in descending order of contribution to group difference. 

#### <span style="color:forestgreen"> Compare the different groups and check which species that contributes mostly to the difference between pairs of clusters.  </span>


sim <- simper(dune, dune.env$Management) #try management groupings
summary(sim)

# Visually compare the result from simper with species positions in a biplot
plot(dune.ca, type = "n", scaling = "species")
with(dune.env, points(dune.ca, display = "sites", scaling = "species", col = as.numeric(Management), pch=16))
with(dune.env, orditorp(dune.ca, display = "species", scaling = "species"))
with(dune.env, ordihull(dune.ca, Management, scaling = "species",label = TRUE))

