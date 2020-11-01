# Classification 2
# Partition and Regression Trees
#
# Most of the code is taken from 
# https://jonlefcheck.net/2015/02/06/a-practical-guide-to-machine-learning-in-ecology/
#
# Here, we will use the iris data. This is a "classic" data set used for exploring multivariate 
# methods for discrimination.
#
# https://en.wikipedia.org/wiki/Iris_flower_data_set
#
# When you have completed the exercise, you should be able to answer the following questions:
# (i)	Based on a visual analysis, which dimensions provide the best separation between iris species?
# (ii)	Are your insights from the visual analysis confirmed by the partition tree analysis?
# (iii)	Does the partition tree provide a perfect species identification or are there misclassifications?
#
# Before starting the analysis, we will generate pairwise scatterplots of the data
#
# the following line will first check to see inf the "psych" library is loaded, and only 
# load it if it is not present
#
if(!is.element("psych",installed.packages()[,1])) {install.packages("psych") }
library(psych)
#
# create pairwise plots of the variables in the iris data set
#
pairs.panels(iris[,-5],gap=0,bg=c("red","blue","yellow")[iris$Species],pch=21)
#
# what does rpart() do ?
#
help(rpart)
#
# set up the partition tree model for the iris data
#
iris.tree.model<-rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
# 
# display the partition tree
#
plot(iris.tree.model)
text(iris.tree.model)
#
# plots for confirmatory visual analysis
#
plot(iris$Petal.Length, col = c("red", "blue", "forestgreen")[iris$Species], xlab = "", ylab = "Petal Length (cm)")
legend(120, 2, c("setosa","versicolor","virginica"), col = c("red", "blue", "forestgreen"), pch = 1)
abline(h = 2.45)
#
plot(iris$Petal.Width, col = c("red", "blue", "forestgreen")[iris$Species], xlab = "", ylab = "Petal Width (cm)")
legend(20, 1.5, c("setosa","versicolor","virginica"), col = c("red", "blue", "forestgreen"), pch = 1)
abline(h = 1.75)
