# The original PLS code in Exercise 29 had four problems.
# 
# Wrong number of components. The cross-validation results for a two-response model are stored as a three-dimensional array, one slice per response. The line which.min(cv$val[estimate = "adjCV", , ]) - 1 ran which.min over the whole two by nineteen matrix and returned the position in the flattened matrix, not a component count. The comment said three components were optimal, but the code actually refitted the model with nine. The named argument estimate = "adjCV" was also silently ignored by the subsetting operator, so the code only picked the right slice by luck of position.
# 
# Coefficients normalised across the wrong set. The absolute coefficients were summed over both responses together, then only the A1 column was plotted. The bars therefore did not sum to one hundred as the comment claimed, and Moisture was never shown even though question 29.1 asks about both responses.
# 
# Mislabelled correlation plot. The correlation plot was given labels = names(coefficients), where coefficients had just been sorted by value. The labels were applied in variable order, so most species names ended up next to the wrong points.
# 
# Plots tied to the refitted model. The score and correlation plots asked for components one to three from the refitted model. That works only if the selected number of components is at least three, which the corrected selection does not guarantee.
# 
# The revised chunk selects components from the adjCV slice by averaging each response's error relative to its intercept-only error, normalises coefficients per response and plots both, draws the score and correlation plots from the full cross-validated fit with labels = "names", and then deliberately overrides the selection to three components for illustration, with a note in the text explaining why.


library(pls)


#This function takes the same form as a lot of regression models in R:
#plsr(Response variable ~ Explanatory Variables, data = yourdata, scale = TRUE/FALSE)
#However the response variable can be a matrix of variables.

#Here we will use the dune species to predict environmental variables A1 and moisture.
#We have mostly done this in the other direction, but correlations go both ways! 
pls.response <- dplyr::select(dune.env.original, A1, Moisture)
pls.response$Moisture <-  as.numeric(pls.response$Moisture)
pls.response <- as.matrix(pls.response)
pls.exp <- as.matrix(dune)
pls.fit <- plsr(pls.response ~ pls.exp,
                na.action = na.omit,
                validation = "LOO")

summary(pls.fit)

#Cross validation is used to help us find the optimal number of retained dimensions.
#Then the model is rebuilt with this optimal number of dimensions.
cv <- RMSEP(pls.fit)

#cv$val is a 3-d array: estimate (CV / adjCV) x response (A1, Moisture) x
#number of components. Taking the adjCV slice gives a response x components matrix.
adjCV <- cv$val["adjCV", , ]
round(adjCV, 3)

#Each response on its own would choose a different number of components:
best.per.response <- apply(adjCV, 1, which.min) - 1
best.per.response

#To choose ONE number of components for the joint model, first express each
#response's error relative to its intercept-only error (so that A1, measured in
#cm, does not dominate Moisture, measured on a 1-5 scale), then average the two
#responses and take the minimum.
rel.adjCV <- adjCV / adjCV[, "(Intercept)"]
mean.rel <- colMeans(rel.adjCV)
round(mean.rel, 3)
best.dims <- unname(which.min(mean.rel) - 1)
best.dims

#Cross validation suggests a single component, which is a rather thin model
#for illustrating the plots below. For teaching purposes we keep three
#components instead. Try setting this to the cross-validated value (or to
#best.per.response["Moisture"]) and see how the results change!
best.dims <- 3

# Rerun the model with optimal dimensions
pls.fit2 <-
  plsr(pls.response ~ pls.exp, ncomp = best.dims, na.action = na.omit)
summary(pls.fit2)

#Finally, we extract the useful information and format the output.
#coef() returns a species x response x ncomp array; take the species x response
#matrix for the fitted number of components (columns = A1 and Moisture).
coefficients <- coef(pls.fit2)[, , 1]

#Normalise EACH response separately so that the absolute values of the
#coefficients sum to 100 within a response. That makes the two responses
#comparable and means the bars really do show "percent of total effect".
coefficients <- sweep(coefficients, 2, colSums(abs(coefficients)), "/") * 100
colSums(abs(coefficients)) # check: both should be 100

#Plot the three strongest positive and three strongest negative predictors
#for each response (question 29.1 asks about both A1 and Moisture).
par(mfrow = c(2, 2), mar = c(6, 4, 3, 1))
for (resp in colnames(coefficients)) {
  co <- sort(coefficients[, resp])
  barplot(tail(co, 3), main = paste(resp, "- strongest positive"),
          las = 2, cex.names = 0.8)
  barplot(head(co, 3), main = paste(resp, "- strongest negative"),
          las = 2, cex.names = 0.8)
}
par(mfrow = c(1, 1))

#We can plot the scores. Use the full cross-validated fit here, since pls.fit2
#may have fewer than 3 components. The first components are the same in both fits.
#This gives a pairwise plot of the correlation of each species with the three first components.
corrplot(pls.fit,
         comps = 1:3,
         labels = "names")

#This gives a pairwise plot of the score values for the three first components.
#Score plots are often used to look for patterns, groups or outliers in the data.
#The scores represent the different sites.
plot(pls.fit, plottype = "scores", comps = 1:3)

#Study the predicted vs. measured plot to see if the data needs to be transformed.
plot(pls.fit2,
     ncomp = best.dims,
     asp = 1,
     line = TRUE)

