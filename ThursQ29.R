library(pls)
#Note: loading pls masks the scores() function from vegan (the same kind of
#package conflict described at the start of Friday's notebook). This is the last
#exercise today so it does no harm here, but if you go back to earlier exercises
#in the same session, call vegan::scores() explicitly.

#This function takes the same form as a lot of regression models in R:
#plsr(Response variable ~ Explanatory Variables, data = yourdata, scale = TRUE/FALSE)
#However the response variable can be a matrix of variables.

#Here we will use the dune species to predict environmental variables A1 and moisture.
#We have mostly done this in the other direction, but correlations go both ways! 
pls.response <- dplyr::select(dune.env.original, A1, Moisture)
#Moisture is stored as a factor with levels 1, 2, 4 and 5. as.numeric() on a
#factor returns the level codes (1, 2, 3, 4), not the values, so we go via
#as.character() to get the real moisture classes back.
pls.response$Moisture <- as.numeric(as.character(pls.response$Moisture))
pls.response <- as.matrix(pls.response)
pls.exp <- as.matrix(dune)
pls.fit <- plsr(pls.response ~ pls.exp,
                na.action = na.omit,
                validation = "LOO")
#Note that we have not scaled the responses. A1 (cm) has about three times the
#variance of Moisture (1-5 classes), so the PLS components are pulled somewhat
#more towards A1. Try adding scale = TRUE to see how much this matters.

summary(pls.fit)

#Cross validation is used to help us find the optimal number of retained dimensions.
#Then the model is rebuilt with this optimal number of dimensions.
cv <- RMSEP(pls.fit)

#cv$val is a 3-d array: estimate (CV / adjCV) x response (A1, Moisture) x
#number of components. Taking the adjCV slice gives a response x components matrix.
adjCV <- cv$val["adjCV", , ]
round(adjCV, 3)

#Each response on its own may choose a different number of components
#(here they happen to agree, but that is not guaranteed):
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
#components instead. Try setting this to the cross-validated value and see
#how the results change!
best.dims <- 3

# Rerun the model with the chosen number of dimensions
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

#The next two plots use the full cross-validated fit (pls.fit) rather than
#pls.fit2, so that they always have three components to show whatever value
#you give best.dims above. PLS components are nested, so the first three
#components are identical in the two fits.

#This gives a pairwise plot of the correlation of each species with the three first components.
corrplot(pls.fit,
         comps = 1:3,
         labels = "names")

#This gives a pairwise plot of the score values for the three first components.
#Score plots are often used to look for patterns, groups or outliers in the data.
#The scores represent the different sites; labels = "names" prints the site numbers
#so that you can see which site is which (question 29.2).
plot(pls.fit, plottype = "scores", comps = 1:3, labels = "names")

#Study the predicted vs. measured plot to see if the data needs to be transformed.
plot(pls.fit2,
     ncomp = best.dims,
     asp = 1,
     line = TRUE)
