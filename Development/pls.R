#PLS regression, like PCA, seeks to find components which maximize the variability of predictors 
#but differs from PCA as PLS requires #the components to have maximum correlation with the response.
#Change 'Antal arter' (number of species in Swedish) to your choice of response variable(s)
pls.fit <- plsr(`Antal arter` ~ ., data = data.pls[c(9:35)], scale = TRUE, validation = "CV")
#The predictor variables are mapped to a smaller set of variables, and within that smaller 
#space we perform a regression against the #outcome variable. PLS aims to choose new mapped variables
#that maximally explain the outcome variable.
#The next step is to remove unwanted variables and then build a model.  
#Cross validation is used to find the optimal number of retained dimensions.
#Then the model is rebuilt with this optimal number of dimensions. 
#Find the number of dimensions with lowest cross validation error
cv <- RMSEP(pls.fit)
best.dims <- which.min(cv$val[estimate = "adjCV", , ]) - 1
best.dims 
# Rerun the model
pls.fit2 <-  plsr(`Antal arter` ~ ., data = data.pls[c(6:35)], ncomp = best.dims)
#Finally, we extract the useful information and format the output.
coefficients <-  coef(pls.fit2)
sum.coef <-  sum(sapply(coefficients, abs))
coefficients <-  coefficients * 100 / sum.coef
coefficients <-  sort(coefficients[, 1 , 1])
barplot(tail(coefficients, 5))
#The regression coefficients are normalized so their absolute sum is 100 and the result is sorted.
#The results below show that 'Tot-N_TNb' and 'Mn' are the two most importnat positive predictors of 'Antal arter'.
#You could run the code #barplot(head(coefficients, 5)) to see that at the other end of the
#scale for negative predictors.
summary(pls.fit2)
plot(pls.fit)
validationplot(pls.fit2, val.type = "MSEP")
predplot(pls.fit2)
coefplot(pls.fit2)
