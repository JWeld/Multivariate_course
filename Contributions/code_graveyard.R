#Q21
#
#adjR2.cca <- RsquareAdj (cca1)$adj.r.squared 

#alternatively
#sel.osR2 <- ordiR2step (cca0, scope = formula (cca1), R2scope = adjR2.cca, direction = 'forward', permutations = 9999)
#sel.osR2$anova # N F T L S sig

#or model selection using permutations
# By terms
#anova(cca1, by="term", perm.max = 1500)

#PLS
#pcr.fit <- plsr(`Abs_F 420 (/5cm)` ~ ., data = data.pls[c(11:35)],scale = TRUE, validation = "CV")

#But how do we know from what number of predictors to choose? We can check the output of #summary(plsFit), which is truncated into two parts. We should look for component number which adequately explains both predictors and response variances.

#The first section displays the root mean squared error of prediction (RMSEP), cross-validation estimate, as well as the adj-CV, which #is adjusted for bias. Take note of the dimensions of X and Y of the data towards the top of the output.

#The next section shows the percent of variance explained by components for predictors and response. See how the variance explained #rises quickly from 1 component and stabilizes above 10..13 components. That would be a good component range for the pls model.

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(dune, nc=6) 
