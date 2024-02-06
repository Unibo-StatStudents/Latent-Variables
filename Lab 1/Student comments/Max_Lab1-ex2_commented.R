#1 ####
cormat <- read.table("socmob.txt")
cormat
# some rather high correlations; 0.3 / 0.4 in real data is already good; so we 
# have some factors


#2 exploratory factor analysis to find number of factors ####
?factanal
str(cormat)
cormat <- as.matrix(cormat)
n <- 713
formula <- "V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10"
f1 <- factanal(formula, factors = 1, covmat = cormat, n.obs = n, rotation = "none")
f2 <- factanal(formula, factors = 2, covmat = cormat, n.obs = n, rotation = "none")
f3 <- factanal(formula, factors = 3, covmat = cormat, n.obs = n, rotation = "none")
f4 <- factanal(formula, factors = 4, covmat = cormat, n.obs = n, rotation = "none")
names(f1)
Chisq <- round(c(f1$STATISTIC, f2$STATISTIC, f3$STATISTIC, f4$STATISTIC), 3)
Chisq
df <- c(f1$dof, f2$dof, f3$dof, f4$dof)
pvalues <- round(c(f1$PVAL, f2$PVAL, f3$PVAL, f4$PVAL), 4)
pvalues
f4 # best model, explains 53.5% of the cumulative variance
f3


#3 uniqueness model ####
# choose f3 instead
names(f3)
comm <- 1 - f3$uniquenesses
comm # 
percVar <- sum(comm)/nrow(cormat) 
percVar
# cumulative variance = 43%


#4 check if reproduced correlation matrix is close to the one we observed ####
repcorr <- loadings(f3) %*% t(loadings(f3)) # matrix product (10 x 3) * (3 x 10)
round(cormat - repcorr, 3) # 3-factor model is still a good solution


#5 orthogonal and oblique rotations ####
# all variables are still loaded in the first factor, so we create a rotation
loadings(f3)
print(f3, cutoff = 0.2)
library(GPArotation)
Varimax(loadings(f3))
quartimax(loadings(f3))
oblimin(loadings(f3)) # best
# variable 1, 2, 5, 10 are all about occupational status -> factor 1
# variable 8, 9 are all about the Firstborn's education -> factor 2
# variable 3, 4, 6, 7 are all about parent's education/occupation -> factor 3


#6 confirmatory factor analysis ####
library(lavaan)
?cfa 
# have to specify the variables for each loading; F2 V10 has to be tested
socmob.model <- " 
F1 =~ V1 + V2 + V5 + V10
F2 =~ V8 + V9 + V10
F3 =~ V3 + V4 + V6 + V7"
# we need to enter either the data or the sample covariance matrix
# std.lv = TRUE allows to fix the variances of the latent variables equal to 1 
# and make the loading estimation free
fit <- cfa(socmob.model, sample.cov = cormat, sample.nobs = n, std.lv = TRUE)
summary(fit, fit.measures = T)
# CFI: should be over 0.8 or 0.9 -> pretty good here
# TLI: should be over 0.8 or 0.9 -> pretty good here
# RMSEA: should lower than 0.08 -> not, but the confidence includes it

# Lastly: If we remove V10 from F2 it gets even worse -> keep it