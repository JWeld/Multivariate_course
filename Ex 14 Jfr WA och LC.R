# Exercise 14 in 2020


### <span style="color:cornflowerblue">Question 14:	Site scores can be calculated in two ways; Weighted Average and Linear Combination. Here you will do both and compare the results. Both options are available in most software, but the default varies.</span>

#### <span style="color:forestgreen"> What is the main difference between the two ways. How to know which method to select? What does the display = "cn" in the last plots mean?</span>


library(vegan)


#### Illustration of difference between WA and LC scores

data("dune")
data("dune.env")
dune.env$Moisture<-as.numeric(dune.env$Moisture) #Set Moisture to numeric for the illustration of WA and LC

# Make a CCA with Management and Moisture as explanatory variables
cca.dune<-cca(dune~Management + Moisture, dune.env)
# Look at the result
anova(cca.dune)
summary(eigenvals(cca.dune))

# Make two plots, one with weighted average (WA) and one with Linear Combination (LC) to calculate the site scores. 
par(mfrow= c(1,2)) # To get two plots beside each other
plot(cca.dune, type = "n", display = c("wa"), scaling = "sites", main = "WA", pch = 3)
points(cca.dune, display = c("wa"), scaling = "sites", pch = 3)
text(cca.dune, display = "wa", scaling = "sites", pos = 2)
ordispider(cca.dune, dune.env$Management, col = "black", label= TRUE, scaling = "sites", display = "wa")

plot(cca.dune, type = "n", display = c("lc"), scaling = "sites", main = "LC", pch = 3)
points(cca.dune, display = c("lc"), scaling = "sites", col = "red", pch = 19)
text(cca.dune, display = "lc", col = "red", scaling = "sites", pos = 3)
ordispider(cca.dune, dune.env$Management, col = "red", label= TRUE, scaling = "sites", display = "lc")
par(mfrow= c(1,1))

# Same as above, but here with sites and species
par(mfrow= c(1,2))
plot(cca.dune, display = c("sp", "wa", "cn"), scaling = "species", main = "Weighted average")
plot(cca.dune, display = c("sp", "lc", "cn"), scaling = "species", main = "Linear combination")
par(mfrow= c(1,1))


