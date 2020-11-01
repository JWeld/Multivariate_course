#
# Principal Response Curves
#
# In the following exercise, we will attempt to identify temporal structure in a data set in two ways.
# First, we will perform an RDA against time, we will then perform a more appropriate analysis using 
# Principal Response Curves (PRC).
#
# At the conclusion of this exercise, you should be able to state:
# (i) Was there an effect of dose over time on the invertebrate community?
# (ii) Which species changed most under the highest dose compared to the control ?
#
# We will use the pyrifos data set, which we have not previously seen.
#
help(pyrifos)
#
# As you will have seen from the description of the pyrifos data set, we need to do some data manipulation
# before it will be possible to perform any analysis.
#
data(pyrifos)
ditch_ <- gl(12, 1, length=132)
week_ <- gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24))
dose_ <- factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)) 
#
# first, let's see if there is temporal structure in the data by performing an RDA with time as the 
# predictor variable
#
pyrifos.RDA.week<-rda(pyrifos ~ week_)
summary(pyrifos.RDA.week)
#
# Display the ordination plot to see if there may be interpretable temporal structure
#
pyrifos.RDA.week.plot<-ordiplot(pyrifos.RDA.week)
text(pyrifos.RDA.week.plot,"centroids", col="blue",pos=2,cex=2)
#
# now can we say anything about dose effects?
#
pyrifos.RDA.dose<-rda(pyrifos ~ dose_)
summary(pyrifos.RDA.dose)
#
# Display the ordination plot to see if there is interpretable structure related to dose
#
pyrifos.RDA.dose.plot<-ordiplot(pyrifos.RDA.dose)
text(pyrifos.RDA.dose.plot,"centroids", col="blue",pos=2,cex=2)
#
# Now perform the appropriate analysis for exploring temporal structure
#
pyrifos.PRC<-prc(pyrifos, treatment=dose_, time=week_)
summary(pyrifos.PRC)   
#
# plot some results
#
plot(pyrifos.PRC)
#
# Plotting the total result set is a bit messy, plot only the common species based on sum of
# abundances
#
pyrifos.SumOfAbundances<-colSums(pyrifos)
plot(pyrifos.PRC,select=pyrifos.SumOfAbundances > 100)
logabu <- colSums(pyrifos)
plot(pyrifos.PRC, select = logabu > 100)
#
## Ditches are randomized, we have a time series, and are only
## interested in the first axis
pyrifos.PRC.ctrl<- how(plots = Plots(strata = ditch_,type = "free"),
	within = Within(type = "series"), nperm = 99)
anova(pyrifos.PRC, permutations = pyrifos.PRC.ctrl, first=TRUE)