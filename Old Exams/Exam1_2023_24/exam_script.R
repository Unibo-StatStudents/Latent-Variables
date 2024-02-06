rm(list=ls())

# I) Factor Analysis [y: metric] & [x: metric] - HolzingerSwineford1939 ####


## 1) Setup ####
# install.packages('lavaan')
# install.packages('GPArotation')
library(lavaan)
library(GPArotation)

data = HolzingerSwineford1939[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9")]   


## 2) Exploratory Factor Analysis ####

# Check if there is correlation among the MVs; this should be the case if we 
# assume LVs
matcorr = cor(data)
matcorr # correlations around 0.3 or 0.4 in real data are already good 
fa = factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=data, factors=3, rotation='varimax')
# rotation = 'varimax' by default (-> orthogonal, aims at a few large and many 
# zero loadings)
# x = ~x1+x2+x3+x4+x5+x6+x7+x8+x9 (here the formula interface was used)

loadings(fa) # since it is an orthogonal rotation, these can be 
# interpreted as correlations between LVs and MVs

# Data can also be provided in form of a correlation matrix
fa_alt = factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9, covmat = as.matrix(matcorr), factors=3, n.obs=301)
# have to provide n.obs here, otherwise it is not equivalent to fa


## 3) Goodness of Fit ####
Chisq = fa$STATISTIC
DoF = fa$dof
pvalue = fa$PVAL
print(fa, cutoff=0.3)


## 4) Variance Decomposition ####
# total variance = number of total variables

# SS loadings
sum(fa$loadings[, 1]^2) # Factor1
sum(fa$loadings[, 2]^2) # Factor2
sum(fa$loadings[, 3]^2) # Factor3

# Proportion Var
sum(fa$loadings[, 1]^2)/9 # Factor1
sum(fa$loadings[, 2]^2)/9 # Factor2
sum(fa$loadings[, 3]^2)/9 # Factor3

# Cumulative Var
# Communalities (for each MV i, this is the sum over q squared loadings)
comm = rowSums((fa$loadings)^2)
# also:
# comm = 1 - fa$uniqueness
sum(comm)/9

# Uniquenesses (for each MV i, this is the specific variance phi_i)
1 - comm
fa$uniquenesses


## 5) Reproduced correlation matrix ####
repcorr = loadings(fa)%*%t(loadings(fa))
repcorr

# Difference between observed and theoretical correlation
round(matcorr - repcorr, 3)
# highest acceptable cell (found so far):


## 6) Rotations ####

# orthogonal
Varimax(loadings(fa))
# similar to factanal(), (because factanal uses varimax by default)
# a few large and many zero loadings
quartimax(loadings(fa))
# aims to reproduce the correspondence between observed and latent factors

# oblique
oblimin(loadings(fa))
# tends to produce varimax-looking factors which are oblique however
promax(loadings(fa))
# aims at a simple structure with low correlation between factors


## 7) Confirmatory Factor Analysis ####

model1 = 'visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9'
model2 = 'visual =~ x1 + x2 + x3 + x9
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9'

fit1 = cfa(model=model1, data=data)
summary(fit1, fit.measures=TRUE)

fit2 = cfa(model=model2, data=data)
summary(fit2, fit.measures=TRUE)

fit3 = cfa(model=model2, data=data, std.lv=TRUE)
summary(fit3, fit.measures=TRUE)
# std.lv: If TRUE, the metric of each latent variable is determined by fixing 
# their (residual) variances to 1.0. If FALSE, the metric of each latent 
# variable is determined by fixing the factor loading of the first indicator 
# to 1.0. If there are multiple groups, std.lv = TRUE and "loadings" is included 
# in the group.equal argument, then only the latent variances of the first group 
# will be fixed to 1.0, while the latent variances of other groups are set free.
# Course: Allows to fix the variances of the latent variables equal to 1 and 
# make the loading estimation free

fit4 = cfa(model=model2, data=data, orthogonal=TRUE)
summary(fit4, fit.measures=TRUE)
# orthogonal: If TRUE, all covariances among latent variables are set to zero.

# cfa output: (Use CFI, TLI and RMSEA to check if certain MVs are affected by 
# ambiguous LVs)

# Model Test User Model:
# - Test statistic: Model chi-square is the chi-square statistic we obtain from 
# the maximum likelihood statistic

# User Model versus Baseline Model:

# - Comparative Fit Index (CFI): CFI is the Comparative Fit Index – values can 
# range between 0 and 1 (values greater than 0.90, conservatively 0.95 indicate 
# good fit)
# Course: should be over over 0.8 or 0.9

# - Tucker-Lewis Index (TLI): Also ranges between 0 and 1 (if it’s greater than 
# 1 it should be rounded to 1) with values greater than 0.90 indicating good 
# fit. If the CFI and TLI are less than one, the CFI is always greater than the 
# TLI.
# Course: should be over over 0.8 or 0.9

# Root Mean Square Error of Approximation:
# -RMSEA: Root mean square error of approximation. In lavaan, you also obtain a 
# p-value of close fit, that the RMSEA < 0.05. If you reject the model, it means 
# your model is not a close fitting model.
# Course: should be below 0.08 -> if not, check also if the confidence interval includes it

# Variances: 
# - Estimate: Are they all > 0? Otherwise we have Heywood cases
# Std.Err: Are they all the same magnitude?




# II) Latent Trait Analysis [y: metric] & [x: cat (bin)] - Abortion ####


## 1) Setup ####
# install.packages('ltm')
library(ltm)

data(Abortion)
?Abortion

dim(Abortion)
head(Abortion)
# total number of response patterns = 2^p = 2^4 = 16


## 2) Checking for significant association ####
dsc = descript(Abortion, chi.squared=TRUE)
plot(dsc) # shows the total score on the x-axis, and the proportion of correct 
# answers for each item on the y-axis -> Note: The highest total score is not 
# shown (because it implies that all items were correct anyway)
# chi.squared=TRUE: logical; if TRUE the chi-squared test for the pairwise 
# associations between items is performed.

# Percentages of agreements for each item
dsc$perc
# The logit for Item 1 is negative as there were less positive than negative 
# answers. It is calculated as:
# log(0.4379947 / (1 - 0.4379947))

# Number of agreements over all items
dsc$items
# 103 is the number individuals replied 0 to all the items
# 141 is the number of individuals replying 1 to all items (1111 -> 4 times 1)

# Chi-squared test for the pairwise associations between items
dsc$pw.ass
# for all pairs of items we have a significant chi-squared . The latent 
# variables are associated -> so it makes sense to estimate a linear trait model
# Note, here: Low p-value -> High association (!)


## 3) Latent trait model ####
?ltm
# fits latent trait model to the data
# requires a formula

# IRT.param: (FHH: 'FALSE: HIGH -> HIGH')

# - FALSE: The Two-parameter logistic (2PL) model uses the other latent trait 
# parametrization, where: beta1 = alpha1, but: beta0 = - alpha0/alpha1
# logit(pi) = alpha0 + alpha1 * z
# -> The HIGHER alpha0, the higher is the probability of a positive response

# - TRUE: The Two-parameter logistic (2PL) model uses the usual Item Response 
# Theory (IRT) parametrization, with a negative difficulty parameter
# logit(pi) = beta1 * (z - beta0)
# -> The LOWER beta0, the higher is the probability of a positive response
#           = beta1 * z - beta1 * beta0
#           = beta1 * z - beta1 * (- alpha0/alpha1)
#           = alpha1 * z + alpha1 * alpha0/alpha1
#           = alpha0 + alpha1 * z

# 2-parameter model (difficulty & discrimination)
ltm1 = ltm(Abortion~z1, IRT.param=FALSE)
ltm2 = ltm(Abortion~z1, IRT.param=TRUE) 

summary(ltm2)
# - Here: Using the IRT.param=TRUE parametrization, the difficulty parameter is
# negative. Therefore, the item with the most negative difficulty parameter is
# the one with the highest probability of a positive answer. Here, it is item 3,
# and we could already see this with dsc$perc. Similarly, item 1 had more
# negative answers, which is why (under this parametrization) its coefficient is 
# the only positive.
# - Item 3 also discriminates the most
# - method: Gauss-Hermite (to approximate the integral; used in E-M algorithm)

summary(ltm1)
# Given that we have a positive difficulty parameter, the item with the most
# negative parameter is the one with the lowest possibility of a positive answer
# --> low values = difficult items
# --> NOTE: Values are the same for the discrimination parameter

# Example: Changing (negative parameter) beta0 to (positive parameter) alpha0:
-summary(ltm2)$coefficients[1,1]*summary(ltm2)$coefficients[5,1]
summary(ltm1)$coefficients[1,1]


## 4) Alpha and standardized alpha ####

alpha = ltm1$coeff[, 2]
stalpha = alpha/sqrt(1+alpha^2)
# stalpha can be read as the correlation between x and the LV

## 5) Median individual and pi(y=0) ####
# Definition: Probability of a positive response from an individual with y = 0
# Note, the notation is different: y = z
# pi(z=0) = logistic(eta(z=0)) = 1/(1 + exp(-alpha0))
coef(ltm1, prob=TRUE, order=TRUE)
# solution in terms of these probabilities
# same as before, but we also have the probabilities of the median individual.
# The median individual will have the highest probabilities for the 'easiest'
# item 3


## 5) Item characteristic curves / item response functions
plot(ltm2, legend = TRUE, cx = "bottomright", xlab="Attitude toward
abortion", lwd = 3, cex.main = 1.5, cex.lab = 1.3, cex = 1.1)
# We can see as many probabilities of positive responses as there are items
# - Difficulty parameters: Are related to the shift: Item 1 was the most difficult
# so that the probabilities of a positive response is the lowest. For item 3 
# (=easiest item) it is shifted upwards
# - Discrimination parameters: These influence the steepness of the 
# curve: the higher, the steeper. Item 3 has the highest discrimination 
# parameter, but note that in terms of stalpha, these are very close to each
# other, which means the steepness of the curves looks really similar


## 6) Goodness of Fit ####

# Pearson chi-squared & LR test [29/33]
# We have 2^p = 2^4 = 16 response patterns
E = fitted(ltm2)[, 5] # expected frequencies related to each response pattern
# Note: We have not observed all possible response patterns, only 14 out of 16:
fitted(ltm2)
O = ltm2$pattern$obs
cbind(ltm2$pattern$X, O, E)
# Note: We have expected frequencies < 5. -> sparse data! First, we calculate
# the chi-squared and the LR test statistics

# Pearson chi-squared
Chisq = sum((O - E)^2/E) # need DoF to decide if this is significant
DoF = 14 - 4*2 - 1 
# = number of observations - number of parameters - 1
# = 2^p - p*(q + 1) - 1 # IMPORTANT: We take the ACTUALLY OBSERVED number of
# observations here: 14 instead of 16
cbind(ltm2$pattern$X, O, E, R)
pvalueC = 1 - pchisq(Chisq, DoF) # upper tail of the chi-squared
pvalueC # -> on significance level 0.05 Pearson chi-squared would reject the
# model

# LR test statistic
LR = 2*sum(O * log(O/E))
pvalueLR = 1 - pchisq(LR, DoF) # DoF is the same has in Chisq
pvalueLR # --> again: model rejected on a significance level of 0.01
# Both tests reject, BUT we still we are not sure if this is due to sparse data
# --> we could collapse the response patterns with low frequencies; drawback:
# might obtain negative DOF
# --> bootstrapping
# --> look at the residuals (done here: with the margins() command)


# Two-way and Three-way chi-squared residuals [30/33]
?margins
# Checks the fit on the two- and three-way margins for grm, ltm, rasch and tpm 
# objects.
# NOTE: goodness of fit is exactly the same for both parametrizations of the 
# model
margins(ltm2)
# for all pairs of items, we get the frequencies of various (bivariate) response 
# patterns

# Note: 
# - this table only shows the HIGHEST residuals; that is why we don't see all 
# possible combinations
# - We calculate and check R > 3.5 or R > 4
# - If three-way looks bad, but two-way looks good -> reject
# we can see that we don't get close to 3.5 or 4; meaning that sparse data is 
# NOT affecting our results

# Now: Just print the first two residuals for the three-way responses:
margins(ltm2, type="three-way", nprint=2)
# Note: For response pattern (110) (for items 1, 2, 4) we have R > 3.5: So 
# there exists a triplet for which it is not a good fit; but this is negligible

# Repeat with nprint=3
margins(ltm2, type="three-way", nprint=3)


## 7) Factor scores: Values of LVs for each individual (response pattern) ####

?factor.scores
eap = factor.scores(ltm2, method="EAP")
# EAP: expected apriori pattern (-> also called: expected mean score)
eap
# Response pattern (0000) gets the lowest factor score for the most 'difficult' 
# LV

# Note: Can also use sufficiency principle ()
comp = factor.scores(ltm2, method="Component")
comp
# Associated to each response pattern, we get a component (lin. comb. of obs 
# values) -> therefore z1 (component is highest for high obs values)
# NOTE: The ranking between EAP (above) and Component (below) is the same!

# Compare with a simple score over each pattern
resp.pattern = eap$score.dat[, 1:4] # -> just gives the response pattern
total.score = apply(X=resp.pattern, MARGIN=1, FUN=sum) 
# MARGIN=1 indicates that we sum over the rows
# -> just adding up row-wise: e.g., 0 + 0 + 0 + 1 for the second response 
# pattern
total.score

# Compare the total.score (from magazines) to the EAP and Component scores
# We order by the total.score and compare if the ranking is the same:
eap_z1 = eap$score.dat[, 7]
comp_z1 = comp$score.dat[, 7]
table = round(cbind(resp.pattern, eap_z1, comp_z1, total.score)[order(total.score),], 3)
table
# We can see that the total.score ranking does not provide the same results. The 
# total.score ranking cannot differentiate between individuals

# Compute factor scores for those response patterns which were not observed
factor.scores(ltm2, resp.pattern=rbind(c(1,0,0,1),c(1,0,1,0)))
# fitted() is the wrong function here
# Negative scores: These are response patterns for individuals have a negative 
# view towards abortion




# III) Latent Trait Analysis [y: metric] & [x: cat (bin)] - LSAT ####


## 1) Setup ####

# All discrimination parameters are equal

data('LSAT')
?LSAT
dim(LSAT)
head(LSAT)
dsc = descript(LSAT)

dsc$perc
# item3 is the most difficult item; it is the one with the highest number of 
# wrong answers

dsc$items
# huge number of people who responded correctly

dsc$pw.ass
# not all items are associated (1,5) (1,4) (3,5)
# will later be used for goodness-of-fit; we can see this already when running
# dsc

plot(dsc)
# item 1 is the one with the highest proportion of correct responses for each 
# value of total score; item 3 is the one with the lowest proportions..


## 2) Rasch model ####

rasch1 = rasch(LSAT, IRT.param=FALSE, constraint=cbind(ncol(LSAT) + 1, 1))
rasch2 = rasch(LSAT, IRT.param=TRUE, constraint=cbind(ncol(LSAT) + 1, 1))
# constraint=cbind(ncol(LSAT) + 1, 1): The first element means that we are 
# addressing the parameter 'p+1' which is 'z' (= discrimination parameter). The 
# second element means that we are setting that selected parameter to 1 (here).
# -> So: Assuming that the discrimination parameter equals 1

summary(rasch1) 
# latent trait parametrization -> FHH: item 1 is easiest
summary(rasch2)
# IRT parametrization -> item 1 is easiest


## 3) Median individual and pi(y=0) ####
coef(rasch1, prob=TRUE, order=TRUE) # again: the probability that the median 
# individual corresponds 1 (= correct) to an item is highest for the easiest 
# item


## 4) Bootstrapped p-value ####
E = fitted(rasch1)[, 6] # expected frequencies related to each response pattern
# Note: We have not observed all possible response patterns, only 30 out of 32:
fitted(rasch1)
O = rasch1$pattern$obs
cbind(rasch1$pattern$X, O, E)
# Note: We have expected frequencies < 5. -> sparse data! First, we calculate
# the chi-squared and the LR test statistics

# Pearson chi-squared
Chisq = sum((O - E)^2/E) # need DoF to decide if this is significant
DoF = 30 - 6 - 1 
pvalueC = 1 - pchisq(Chisq, DoF) # upper tail of the chi-squared
pvalueC # no rejection of the model

# LR test statistic
LR = 2*sum(O * log(O/E))
pvalueLR = 1 - pchisq(LR, DoF) # DoF is the same has in Chisq
pvalueLR # --> again: model not rejected. But: We have sparse data

# Bootstrapped p-value
# use this when chi-squared and LR test don't hold asymptotically
pval.boot = GoF.rasch(rasch2, B = 199, seed = 221019)
pval.boot$Tobs # empirical value of the Pearson chi-squared distribution
pval.boot # p-value: 0.26 -> we do not reject the H0
# asymptotic p-value could also be asked here


# Two-way and Three-way chi-squared residuals [30/33]
margins(m1) # two-way
margins(m1, type = "three-way", nprint = 2)
# looks bad with 000: so we also run it with nprint=3:
margins(m1, type = "three-way", nprint = 3)
# association between the two triplets from above can still not be explained by 
# the model, the rest is all lower than 4 -> good


## 5) Unconstrained Rasch model ####
rasch3 = rasch(LSAT, IRT.param=FALSE) # GLLVM
rasch4 = rasch(LSAT, IRT.param=TRUE) # IRT
# -> again with two parametrizations possible

# Comparing GLLVM: Constrained vs. Unconstrained
summary(rasch1) # from difficult to easy: 3, 2, 4, 5, 1
summary(rasch3) # from difficult to easy: 3, 2, 4, 5, 1

# Comparing IRT: Constrained vs. Unconstrained
summary(rasch2) # from difficult to easy: 3, 2, 4, 5, 1
summary(rasch4) # from difficult to easy: 3, 2, 4, 5, 1

# Showing how we get from one parametrization to the other
-summary(rasch2)$coefficients[1, 1]*summary(rasch2)$coefficients[6, 1]
summary(rasch1)$coefficients[1, 1]
# -> just with negative sign, multiplied by discrimination parameter (=1 here)


## 6) ANOVA: Constrained Rasch vs. Unconstrained Rasch ####
anova(rasch2, rasch4)
# rasch4 has a significantly better (= higher) loglikelihood. The unconstrained 
# Rasch model is better than the constrained Rasch model. The discrimination 
# parameters are NOT all equal to 1
anova(rasch1, rasch3) # same result
# AIC/BIC confirms this result. The better model has the lower AIC/BIC

margins(rasch4)
# -> no problems in the 2way margin
margins(rasch4, type = "three-way", nprint = 2)
# -> no problem in the 3way margin


## 7) Compare with 2-parameter model (difficulty & discrimination) ####
ltm3 = ltm(LSAT~z1, IRT.param=FALSE)
ltm4 = ltm(LSAT~z1, IRT.param=TRUE)

summary(ltm3)
summary(ltm4)
# -> signficant coefficients: z-vals > 3 for every coefficient
# difficulty parameters are not the same in both models (different 
# parametrization)
# discriminatory parameters for items 1, 2, 3 are higher than for items 4, 5 in 
# both models

# Compare again with the unconstrained Rasch model 
anova(rasch4, ltm4)
# We do not reject H0: The 2-parameter model is NOT significantly better than
# the unconstrained Rasch model. That means the discimination parameters are not
# significantly different from another

# SO: They are NOT assumed to be equal to 1, but they are also NOT assumed to be
# different(?)

plot(rasch4, legend=TRUE, cx='bottomright', lwd=3, cex.main=1.5, cex.lab=1.3, 
     cex=1.1)
# -> curves are parallel because they have the same discriminatory parameters
# item 1 is the easiest, item 3 is the most difficult


## 8) Factor scores ####
# finding the 'best' students (response patterns)
eap = factor.scores(rasch4, method='EAP')
eap
plot(eap$score.dat$z1)
resp.pattern = eap$score.dat[, 1:5]
total.score = apply(X=resp.pattern, MARGIN=1, FUN=sum)
total.score
table = round(eap$score.dat[order(total.score), ], 3)
table

# Compute factor scores for those response patterns which were not observed
factor.scores(rasch4, resp.pattern=rbind(c(0,1,1,0,0), c(0,1,0,1,0)))
# -> 5^2 = 32 items, but we observe only 30; the ones above are the two missing
# response patterns




# IV) Latent Class Analysis [y: cat (poly)] & [x: cat (bin)] - carcinoma ####

## 1) Setup ####
# install.packages('poLCA')
library(poLCA)

data(carcinoma)
?carcinoma
head(carcinoma)
carcinoma


## 2) Latent Class Analysis ####
formula = cbind(A,B,C,D,E,F,G)~1
lca2 = poLCA(formula=formula, data=carcinoma, nclass=2, nrep=10, verbose=FALSE)
lca3 = poLCA(formula=formula, data=carcinoma, nclass=3, nrep=10, verbose=FALSE)
lca4 = poLCA(formula=formula, data=carcinoma, nclass=4, nrep=10, verbose=FALSE)
# - verbose=FALSE: Logical, indicating whether poLCA should output to the screen 
# the results of the model. If FALSE, no output is produced. The default is TRUE.
# - nrep: Number of times to estimate the model

# Compare estimated frequencies of the response patterns
est_freq = data.frame(lca2$predcell[1:9], lca3$predcell[9], lca4$predcell[9])
est_freq
# can directly see that the 2-class model is not the best

# Compare the model in terms of log-likelihood, number of free parameters, 
# likelihood ratio test, Pearson chi-square test (and associated p-value), AIC, 
# BIC.
K = c('2', '3', '4')
llike = c(lca2$llik, lca3$llik, lca4$llik)
npar = c(lca2$npar, lca3$npar, lca4$npar)
LR = round(c(lca2$Gsq, lca3$Gsq, lca4$Gsq), 3)
Chisq = round(c(lca2$Chisq, lca3$Chisq, lca4$Chisq), 3)
DoF = c(lca2$resid.df, lca3$resid.df, lca4$resid.df)
pval = round(1 - pchisq(Chisq, DoF), 4)
AIC = round(c(lca2$aic, lca3$aic, lca4$aic), 3)
BIC = round(c(lca2$bic, lca3$bic, lca4$bic), 3)
table = data.frame(K, llike, npar, LR, Chisq, DoF, pval, AIC, BIC)
table

# loglikelihood increases with number of classes (typical)
# number of parameters will increase
# According to AIC/BIC, we should choose the 3-class model, BUT: according to LR
# and Chi-square test they are all not significant; would even prefer 2-class 
# model; Explanation (in book): With expected frequencies less than 5, LR and 
# Chi-Square tests will explode. BUT: With expected frequencies below 0.1, we 
# have the exact opposite.
# -->  Should rely on AIC/BIC here (we can also see that 2-class model doesn't 
# fit well)

## 3) Display parameter estimates for the 3-class model ####

# Prior probabilities
round(lca3$P, 4)

# Estimated class-conditional response probabilities.
lapply(lca3$probs, round, 3)
# Here, we have a 'switching problem' since there is indeterminacy of the 
# latent classes. We Want to give the same order to the classes

# Now: Give an order to the priors in increasing order of the estimated priors
probs_start = lca3$probs.start
# probs.start: A list of matrices containing the class-conditional response 
# probabilities used as starting values in the estimation algorithm. If the 
# algorithm needed to restart (see eflag), then this contains the starting 
# values used for the final, successful, run.

# New order given by priors
new_probs_start = poLCA.reorder(probs_start, order(lca3$P))

# Refit the model, but use the ordered solutions in 'probs.start' argument
lca3_ord = poLCA(formula=formula, data=carcinoma, nclass=3, 
                 probs.start=new_probs_start, verbose=FALSE)
round(lca3_ord$P, 4)

## 4) Interpret the three latent classes ####
lapply(lca3_ord$probs, round, 3)
# Summary: 

# class 3: out of 7, 5 are in agreement that class 3 is the type of tissue with 
# carcinoma
# class 2: 7 out of 7 agree that this class is composed of tissues where all 
# pathologists agree there is an absence of carcinoma
# class 1: completely split in two; this class is the class of inconsistent 
# diagnoses; recall that this class is the least numerous (0.1817); so very 
# small


## 5) Compute the estimated probability of response pattern (1 1 1 1 1 1 1) ####
# -> Compute the estimate of the probability of the agreement of the 
# pathologists in the negative diagnosis.

head(carcinoma)
# first row: All tissues give negative diagnosis: We want to compute the 
# probability of that response pattern
# f(x_A=1, x_B=1, ..., x_G=1) = pi\1 = sum_^3_{j=0} eta_j \prod^G_{i=A} \pi_{ij}^x{ij} (1-\pi_{ij})^{1-x_{ij}}

# lca3$P[1] = eta_1
# lca3$P[2] = eta_2
# lca3$P[3] = eta_3

# lca3$probs$A[1,1] = 1-\pi_{A1}^{1-x_A1} # --> we are multiplying by the 
# (1-\pi_{ij})^{1-x_{ij}} part of the formula (since it captures the NEGATIVE 
# response! - the POSITIVE response will be 1)

# lca3$probs$A[1,1] = probability of the first response pattern associated to the first class
p.dnc1 = lca3$P[1]*lca3$probs$A[1,1]*lca3$probs$B[1,1]*lca3$probs$C[1,1]*lca3$probs$D[1,1]*lca3$probs$E[1,1]*lca3$probs$F[1,1]*lca3$probs$G[1,1]

# lca3$probs$A[2,1] = probability of the first response pattern associated to the second class
p.dnc2 = lca3$P[2]*lca3$probs$A[2,1]*lca3$probs$B[2,1]*lca3$probs$C[2,1]*lca3$probs$D[2,1]*lca3$probs$E[2,1]*lca3$probs$F[2,1]*lca3$probs$G[2,1]

# lca3$probs$A[3,1] = probability of the first response pattern associated to the third class
p.dnc3 = lca3$P[3]*lca3$probs$A[3,1]*lca3$probs$B[3,1]*lca3$probs$C[3,1]*lca3$probs$D[3,1]*lca3$probs$E[3,1]*lca3$probs$F[3,1]*lca3$probs$G[3,1]

# Summing over:
p.dn = p.dnc1 + p.dnc2 + p.dnc3
round(p.dn,4) # Result: 0.2869


## 6) Compute estimated number of tissues where pathologist agree in negative diagnosis ####
round(p.dn*lca3$N, 4) # = 0.2869 * 118 = 33.8491
118*p.dn

## 7) Compute estimated posterior probability of response pattern (1 1 1 1 1 1 1) ####

# Display the posterior probability estimates of the response pattern 
# (1,1,1,1,1,1,1) for the three latent classes. To which class is the sample of 
# tissue correspondent to this response pattern allocated?

posterior_est = round(lca3_ord$posterior[1, ], 5)
# 0 1 0: -> Result is 1: quite expected: Probability of belonging to the 
# second class for the first response pattern; this is the latent class of 
# negative diagnosis.


## 8) Display the latent class in which the samples of tissue are allocated according to highest posterior probability ####

# class vector (according to posterior probability)
lca3_ord$predclass
# 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 2 2 3 1 1 1 1 1 1 1 1 1 1 1 1 3 1 3 3 3 3 3
# 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# 3 3 3 3 3 3 3

# Create a frequency table of this predicted class
table(lca3_ord$predclass)
#1  2  3 
#23 44 51 

# Result is 3

## 9) 
# Select the samples of tissues allocated to the class of inconsistent diagnosis 
# and evaluate the correspondent response pattern.

sel = carcinoma[lca3_ord$predclass==1,] # select all tissues with predicted 
# class==1

# Evaluation:
sel
# 23 tissues: At most 4 pathologists give a positive diagnosis in that class;
# again this is the 'inconsistent' class




# V) Latent Class Analysis [y: cat (poly)] & [x: cat (bin)] - Mobility ####

## 1) Setup ####
# install.packages('ltm')
library(ltm)

?Mobility
str(Mobility)

names(Mobility)<-c("Item1","Item2","Item3","Item4",
                   "Item5","Item6","Item7","Item8")
Mobility[Mobility==1] = 2
Mobility[Mobility==0] = 1
# this is because poLCA codes from 1 onwards (0 does not mean anything)
# new values: 2 yes; 1 no


## 2) Latent Class Analysis ####
formula = cbind(Item1,Item2,Item3,Item4,Item5,Item6,Item7,Item8)~1
lca2 = poLCA(formula=formula, data=Mobility, nclass=2, nrep=10, verbose=FALSE)
lca3 = poLCA(formula=formula, data=Mobility, nclass=3, nrep=10, verbose=FALSE)
lca4 = poLCA(formula=formula, data=Mobility, nclass=4, nrep=10, verbose=FALSE)

# Compare estimated frequencies of the response patterns
est_freq = data.frame(lca2$predcell[1:10], lca3$predcell[10], lca4$predcell[10])
est_freq
# It is possible to see that there are some expected frequencies that are LESS 
# THAN 5. So we cannot rely on the test statistics that use the chi-square 
# approximation

# Compare the three models considering the following measures: log-likelihood, 
# number of free parameters, likelihood ratio test, Pearson chi-square test (and 
# associated p-value), AIC, BIC.
K = c("2", "3", "4")
llik = c(lca2$llik, lca3$llik, lca4$llik)
npar = c(lca2$npar, lca3$npar, lca4$npar)
Gsq = round(c(lca2$Gsq, lca3$Gsq, lca4$Gsq), 3)
Chisq = round(c(lca2$Chisq, lca3$Chisq, lca4$Chisq), 3)
DoF = c(lca2$resid.df, lca3$resid.df, lca4$resid.df)
pvalue = round(1-pchisq(Chisq, DoF), 4)
AIC = round(c(lca2$aic, lca3$aic, lca4$aic), 3)
BIC = round(c(lca2$bic, lca3$bic, lca4$bic), 3)
table = data.frame(K, llik, npar, Gsq, Chisq, DoF, pvalue, AIC, BIC)
table
# According to the Chi^2 test, by seeing the p-value we should refuse the 
# hypothesis that this model is a good model for the data. But since the 
# expected frequencies of some patterns are lower than 5 than we should not rely 
# on this result. Instead, we should look to AIC and BIC. From these model 
# selection criteria the best model is the one with 4 classes.
# answer: 4-class model

## 3) Display parameter estimates for the 4-class model ####
round(lca4$P, 4)
lapply(lca4$probs, round, 4) # Class conditional response probability
# It is possible that we get the classes in a different order because of the 
# problem of the indeterminacy of the latent class.
?poLCA.reorder # helper function
probs_start = lca4$probs.start # starting probability used in model lca4
new_probs_start = poLCA.reorder(probs_start, order(lca4$P)) # we reorder the 
# probabilities according to the probabilities of belonging to each class

# Refit the model, but use the ordered solutions in 'probs.start' argument
lca4_ord = poLCA(formula, Mobility, nclass=4, probs.start=new_probs_start, verbose=FALSE)
# For Item 1, 2, 3, 4, 5, 6, 7, 8, how do the different classes respond?
# all girls belonging to class 1 have a higher probability of responding yes (2)
# to item 1

round(lca4_ord$P, 4) # The classes are ordered following the size of each class 
# and this means that class 1 has the lowest size while class 4 has the greatest 
# size estimated class-conditional response probabilities.
lapply(lca4_ord$probs, round, 4) # estimated class-conditional response probabilities.


## 4) Interpret the four latent classes ####

# Class 1 It is the class that has the highest conditional probability referred 
# to the outcome YES, in ALL ITEMS. So this is the class of the women that have 
# the highest mobility and so that respond YES to almost all the items.

# Class 3 It is the class where we have the LOWEST conditional probability of 
# getting a YES and this is true for all the items. So this class contains the 
# women with the lowest mobility.

# Class 2 It is the class that seems to have the highest probability referred to 
# the YES = 2 outcome after class 1. So these women in this class have less 
# mobility than women in class 1 but more mobility than women in class 3. 
# This is the medium-high class in terms of mobility.

# Class 4 The women in this class has more mobility than class 3 but less mobility 
# than class 2 and 1 so this is the medium-low class in terms of mobility.


## 5) Compute estimated posterior probability of response pattern (1 1 1 1 1 1 1) and (2 2 2 2 2 2 2) ####
# In which classes are the samples of women corresponding to these response patterns 
# allocated?

# (1, 1, 1, 1, 1, 1, 1, 1)
a = as.numeric(row.names(Mobility[which(Mobility$Item1 == 1 & Mobility$Item2 == 1 & 
                                           Mobility$Item3 == 1 & Mobility$Item4 == 1 & 
                                           Mobility$Item5 == 1 & Mobility$Item6 == 1 & 
                                           Mobility$Item7 == 1 & Mobility$Item8 == 1),]))
est_posterior_a = round(lca4_ord$posterior[a[1], ], 5) 
# Result: Such a person will probably belong to the 3rd class

# (2, 2, 2, 2, 2, 2, 2, 2)
b = as.numeric(row.names(Mobility[which(Mobility$Item1 == 2 & Mobility$Item2 == 2 & 
                                           Mobility$Item3 == 2 & Mobility$Item4 == 2 &
                                           Mobility$Item5 == 2 & Mobility$Item6 == 2 & 
                                           Mobility$Item7 == 2 & Mobility$Item8 == 2),]))
est_posterior_b = round(lca4_ord$posterior[b[1], ], 5)
# Result: Such a person will probably belong to the 1st class


## 6) Display the latent class in which the samples of women are allocated according to highest posterior probability ####

# class vector (according to posterior probability)
lca4_ord$predclass

# Create a frequency table of this predicted class
table(lca4_ord$predclass)


## 7) Display the latent class for the women with the least mobility ####
# Select the samples of women allocated to the class with less mobility and 
# evaluate the corresponding response patterns
sel = Mobility[lca4_ord$predclass == 3, ]
unique(sel)
# -> women in this class were able to do at most 2 things on the list


## 8) Compute the predicted cell probability from the latent class model of the response pattern (2, 1, 2, 1, 2, 1, 2, 1) ####
# It is not observed in the dataset, and allocate it according to the highest 
# posterior probability. We want to predict the predicted probability of a 
# response pattern that is not showed in the data set.

# Predicted percentage of this response pattern
poLCA.predcell(lca4_ord, c(2, 1, 2, 1, 2, 1, 2, 1)) 
# the predicted percentage of this response pattern is almost zero (it is chosen 
# to be contradictory; the girl can go out, but cannot go shopping for example).


## 9) Compute estimated posterior probability of response pattern (2 1 2 1 2 1 2 1) ####
# We can classify this pattern not observed in the data set. This pattern is 
# classified in class 2.
poLCA.posterior(lca4_ord, y = c(2, 1, 2, 1, 2, 1, 2, 1)) 
unique(Mobility)

# In order to know which are the not observed pattern, if there are many, we can
# use as command the following:
c = as.numeric(row.names(Mobility[which(Mobility$Item1 == 2 & Mobility$Item2 == 1 & 
                                           Mobility$Item3 == 2 & Mobility$Item4 == 1 & 
                                           Mobility$Item5 == 2 & Mobility$Item6 == 1 & 
                                           Mobility$Item7 == 2 & Mobility$Item8 == 1),]))
# We get a result of 0 because this pattern is not observed in the data set.