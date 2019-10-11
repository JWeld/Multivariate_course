#Excercise 1
library(tidyverse)


#constrained ordination
setwd("C:/Users/grandin/Documents/Ulf IVM/Undervisning/MVA molecular data/2015/Exercises")

library("vegan") # Package for multivariate analyses of ecological data
library("labdsv") # Another package for multivariate analyses of ecological data
library("reshape2")
library("dendextend")
library("sparcl")
library("outliers")
library("gdata")
library("caret")


##### Read data ###########################
Bugs <- column_to_rownames(Bugs,var = "ID")
Coord <- column_to_rownames(Coord,var = "ID")
Species <- column_to_rownames(Species,var = "ID")
EnvVar <- column_to_rownames(EnvVar,var = "ID")
Mean_Ellenberg <- column_to_rownames(Mean_Ellenberg, var = "ID")

EnvTS <- column_to_rownames(EnvTS,var = "ID")
SpeTS <- column_to_rownames(SpeTS,var = "ID")

#dune.env <- EnvVar
#dune <- Species

Trend_Lakes_2015_PLS <- read_excel("Data/Data_for_PLS/Trend Lakes 2015_PLS.xls")

save(Bugs, Coord, Species, EnvVar, Mean_Ellenberg, Ellenberg, EnvTS, SpeTS, Trend_Lakes_2015_PLS, file = "MVM_data.RDS")


# Read small species dataset
spe<-read.table("Data/species12x10.txt", sep="\t",header=T,row.names=1)

# Read environmental  dataset
env<-read.table("Data/env12x5.txt", sep="\t",header=T,row.names=1)

#Ulf's version with ells
dune.u <- read.table("Data/Data_for_R/dune.txt", sep=";",header=T,row.names=1)
dune.mean.ell <- read_csv("Data/exports_from_canoco/meanell.csv", col_types = cols(Plot = col_skip()))
dune.env.u <- read.table("Data/Data_for_R/env.txt", sep="\t",header=T,row.names=1)

#Zeleny version with traits and ellenbergs
# Species and environmental data
dune2.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.spe.txt', row.names = 1)
dune2.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.env.txt', row.names = 1)

# Species attributes
dune2.traits <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.traits.txt', row.names = 1)
dune2.ell <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.ell.txt', row.names = 1)

###########################################
#CCA analysis
CCA.Ellenberg.all <- cca(dune2.spe ~ ., data = dune2.ell)

#########################
#### START of constrained analyses


###########
## Screening for relationships
# Make a CA
spe.CA<-cca(spe)

par(mfrow=c(1,4)) # Three plots per page
test_env<-envfit(spe.CA, env, permutations = 999)#, display = "sites")
test_env
plot(spe.CA, main="CA, all env.var.", display="sites", type = "n", cex.main=1.4)
text(spe.CA, display = "sites", cex=1.2)
plot(test_env,cex=1.7) # Plot only vars with p-values max = 0.05

plot(spe.CA, main="CA, env. p<0.05", display="sites", type="n", cex.main=1.4)
text(spe.CA, display = "sites", cex=1.2)
plot(test_env, p.max=0.05, cex=1.7) # Plot only vars with p-values max = 0.05

#Compare with a shotgun CCA with all environmental data
shotgun<-cca(formula = spe~., data = env) # the dot means all variables in data set env
shotgun
plot(shotgun, type="n",main="CCA all env vars", cex.main=1.4)
text(shotgun, dis="cn", col = "blue", cex=1.7)
text(shotgun, "sites", cex=1.2)

# Compare CCA with only "Site" and "Dose" as explanatoty variables
new_cca<-cca(formula = spe~Site+Dose, data = env)
plot(new_cca, type="n",main="CCA with sign. expl. var.", cex.main=1.4)
text(new_cca, dis="cn", col = "blue", cex=1.7)
text(new_cca, "sites", cex=1.2)
par(mfrow=c(1,1)) # Reset to one plot per page

# Test the variance inflation factor
# 1. Run on original data
vif.cca(shotgun)

# 2. Create a new env data set with one correlated variable
env4<-env
env4$Env3<-env4$Env2+rnorm(mean = 0, sd = 0.6,n=12)
env4

#Test the correlation
cor.test(env4$Env2, env4$Env3)
plot(env4$Env2, env4$Env3)
abline(lm(env4$Env2~env4$Env3))

# 3. Make a new CCA
shotgun2<-cca(spe~., env4)
plot(shotgun2)

# 4. Check variance inflation factor using the env. data
vif.cca(shotgun2)

#### END screening #############
##########################################


#############################################
### Different ways of building models

# Start by make ordinations
cca0<-cca(formula = spe~1, data = env) # model with only intercept
cca1<-cca(spe~.,env) # full model (with all explanatory variables)
cca1
plot(cca1, main="CCA, all data")

# 1. Stepwise approach, using "step"
step1f<-step(cca0, scope = formula(cca1), direction = "forward")
step1f$anova
plot(step1f, main="Forward selection")
step1b<-step(cca0, scope = formula(cca1), direction = "backward")
step1b$anova
plot(step1b, main="Backward elimination")

# 2. Stepwise approach, using "ordistep"
step2<-ordistep(cca0, scope = formula(cca1), perm.max=200)
step2$anova


# 3. Model selection using permutations

# 3a. Overall test
anova(cca1)

# 3b. By terms
anova(cca1, by="term", perm.max = 200)
# 3b. By margin
anova(cca1, by="margin", perm.max = 200)
# 3b. By axis
anova(cca1, by="axis", perm.max = 200)

# New model with only significant explanatory variables
cca3<-cca(spe~Site+Dose, env)
cca3
plot(cca3, main="CCA with sign. expl. var.")
with(env, ordispider(cca3, group=Site, lty=3, col="blue"))

# Test of significane in the new model
anova(cca3, by="term", perm =200)
anova(cca3, by="margin", perm =200)
anova(cca3, by="axis", perm =200)


# Site scores as WA or LC?
plot(cca3, display = c("lc", "wa"), type = "t", main = "LC vs. WS scores")
ordispider(cca3, col="blue")



#############################
#### Partial Ordinations

# Partial CCA with "Site" as explanatory var. and effect of "Dose" removed
(pCCA<-cca(spe~Site+Condition(Dose), env))

plot(pCCA, main = "pCCA")
with(env, ordispider(pCCA, group=Site, lty=3, col="blue"))

anova(pCCA)
anova(pCCA, by="margin")
anova(pCCA, by="axis")

# CCA and pCCA beside each other
par(mfrow=c(1,2))
plot(cca3, main="CCA with sign. expl. var.")
with(env, ordispider(cca3, group=Site, lty=3, col="blue"))
plot(pCCA, main = "pCCA")
with(env, ordispider(pCCA, group=Site, lty=3, col="blue"))
par(mfrow=c(1,1))

# Compare CCA and pCCA, using procrustes rotation
(pro1<-protest(X = cca3, Y = pCCA, scores = "sites"))
plot(pro1, main = "Comparing CCA and pCCA")
text(pro1, cex=0.7)
plot(pro1, kind = 2, xlab = "Sample number")



#####################################
## Variance partitioning with two factors
(v_part2<-varpart(spe, ~Site, ~Dose, data = env, transfo = "hellinger"))
showvarparts(2, labels = c("Site","Site+Dose","Dose",""), bg = c("hotpink","skyblue"))
plot(v_part2, bg = c("hotpink","skyblue"), cex=1.5)

# Add a third factor
(v_part3<-varpart(spe, ~Site, ~Dose, ~Fertilizer , data = env, transfo = "hellinger"))
showvarparts(3, bg = c("hotpink","skyblue","Goldenrod"))
plot(v_part3, bg = c("hotpink","skyblue","Goldenrod"), cex=1.5)

# END
#######################

data(varespec)
data(varechem)
## Common but bad way: use all variables you happen to have in your
## environmental data matrix
vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca)
## Formula interface and a better model
vare.cca <- cca(varespec ~ Al + P*(K + Baresoil), data=varechem)
vare.cca
plot(vare.cca)
## Partialling out and negative components of variance
v.cca <- cca(varespec ~ Ca, varechem)
plot(v.cca)
vv.cca <- cca(varespec ~ Ca + Condition(pH), varechem)
plot(vv.cca)
## RDA
data(dune)
data(dune.env)
dune.Manure <- rda(dune ~ Manure, dune.env)
plot(dune.Manure) 


barplot(tail(coefficients, 5))



mod <- rda(dune ~ Management, data = dune.env)
mod <- dune.rda2
model.matrix(mod)
with(dune.env, levels(Management))
scores(mod, display = "cn")
ordiplot(mod)
text(mod, display = "cn")

dune.env.original$Management <-  as.factor(dune.env.original$Management)

# RDA 
dune.rda <- rda(Species ~ . , dune.env.original)
dune.rda2 <- rda(Species ~ ., dune.env)
dune.rda
ordiplot(dune.rda, type="text", main="RDA")
ordiplot(dune.rda2, type="text", main="RDA2")
plot(dune.rda, display = "sites")
text(dune.rda$CCA$envcentre, display = "cn")

ord <- dune.rda
layout(matrix(1:2, ncol = 2))
plot(ord, choices = c(1,2))
plot(ord, choices = c(2,3))
layout(1)
# CCA 
dune.cca <- cca(Species, dune.env.original)
dune.cca2 <- cca(Species, dune.env)
dune.cca
ordiplot(dune.cca, type="text", main="CCA")
ordiplot(dune.cca2, type="text", main="CCA2")
