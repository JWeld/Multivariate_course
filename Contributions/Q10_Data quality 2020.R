# Question 10 MVA course 2020

###<span style="color:cornflowerblue">Question 10: Data Quality Assessment</span>
#  These are only a few examples on how to assess data quality. 

#### <span style="color:forestgreen">Is there a pattern in the distribution of missing values that may lead to the omission of a species or a sample? Is Grubbs test a good method for finding outliers in a multivariate data set? Would you say that the original Dune Meadow data set is OK for further analyses? Motivate your answer!</span>


library("vegan") 
library("reshape2")
library("outliers") # For Grubbs test
library("gdata") # for unmatrix
library("VIM") # For matrixplot


data(dune)



# Create some outliers and missing values in a temporary dataset called mvo.dune (acronym for MultiVariate Outlier)
mvo.dune<-as.matrix(dune)
n_outl<- 5 # Give number of outliers to be created, here 5
v_outl<-30 # Give value of the outlier(s), here 30
mvo.dune[sample(1:length(mvo.dune),n_outl, replace = FALSE)]<-v_outl # Replace some data with outliers, at random positions
mvo.dune[sample(1:length(mvo.dune),n_outl, replace = FALSE)]<-NA # Replace some data with NAs, at random positions


# Cleveland plot to inspect data
dotchart(as.matrix(mvo.dune), lcolor=col(mvo.dune), main="Cleveland plot", xlab="Observerd value")

# Find data > 9, as no value should be higher than 9 in the dune meadow data set
(GT9 <- which(mvo.dune > 9, arr.ind=TRUE))


# Find missing values
(mvo_miss<-which(is.na(mvo.dune), arr.ind=TRUE))

# Two ways of visualising missing data, using the package "VIM"
aggr(as.matrix(mvo.dune)) 
matrixplot(mvo.dune) 


# To scan all variables with Grubbs test for outliers
# Note: Only indicative!
mvo.dune_melt<-melt(mvo.dune) # Restructure data
a<-as.data.frame(do.call("rbind", with(mvo.dune_melt, tapply(value, Var1, function(x) 
  unlist(grubbs.test(x)[c("statistic", "p.value", "alternative")])))))
(a[which(a$p.value < 0.05),])


# Indicative search for outliers in a multivariate data set. 
# Multiple peaks and high SD indicate outliers may occur
dist1<-vegdist(mvo.dune, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = TRUE) 
d1<-data.frame(unmatrix(as.matrix(dist1), byrow = TRUE))
d1r<-t(data.frame(strsplit(row.names(d1), ":")))
d1rr<-data.frame(d1,d1r)
d1mean<-tapply(d1rr$unmatrix.as.matrix.dist1.., d1rr$X1, mean)
sd(d1mean) # Standard deviation of the mean distances. If > 3: strong outlier(s)
plot(density(d1mean), col = "red", xlab="Distance between observations", main = "Indictive test of multivariate outliers\nData with noise")
legend("topright",paste("SD =",round(sd(d1mean),1))) # Legend with standard deviation

# Same as above but on the original dune meadow dataset (i.e. without outliers)
dist1<-vegdist(dune, method="euclidean", binary=FALSE, diag=FALSE, upper=FALSE, na.rm = TRUE) 
d1<-data.frame(unmatrix(as.matrix(dist1), byrow = TRUE))
d1r<-t(data.frame(strsplit(row.names(d1), ":")))
d1rr<-data.frame(d1,d1r)
d1mean<-tapply(d1rr$unmatrix.as.matrix.dist1.., d1rr$X1, mean)
sd(d1mean) # Standard deviation of the mean distances. If > 3: strong outlier(s)
plot(density(d1mean), col = "red", xlab="Distance between observations",main = "Indictive test of multivariate outliers\nOriginal Dune Meadow data")
legend("topright",paste("SD =",round(sd(d1mean),1))) # Legend with standard deviation

