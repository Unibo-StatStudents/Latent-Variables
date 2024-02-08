#------------------------ FACTOR ANALISYS LAB --------------------------------#
# The data are based on information provided by 713 male or female married 
# respondents to a survey carried out in 1949. The variables relate to the 
# respondent, his or his spouse, father, father-in-law, and firstborn son. 
# The file socmob.txt contains the full correlation matrix. 
# The 10 variables are coded as follows:
# X1 = Husband’s father’s occupational status 
# X2 = Wife’s father’s occupational status 
# X3 = Husband’s further education 
# X4 = Husband’s qualifications 
# X5 = Husband’s occupational status 
# X6 = Wife’s further education
# X7 = Wife’s qualifications
# X8 = Firstborn’s further education
# X9 = Firstborn’s qualifications
# X10 = Firstborn’s occupational status

# Perform first an exploratory factor analysis and then a confirmatory 
# factor analysis on this data set.


# 1 - Load the correlation matrix socmob.txt and explore the correlation matrix 
# in order to evaluate if a factor model can be fitted.

# In this case this is not a raw data set but it is a CORRELATION MATRIX, 
# so we don't have information about individuals.
cormat <- read.table("Lab 1/socmob.txt")
cormat

# It is a correlation matrix where between V8 and V9 (variables related to firstborn qualification 
# and education) correlation is 0.5; and it is 0.43 between V9 and V10; V9 and V4;... There are a 
# lot of variables very correlated one to each other.



# 2 - Estimate a factor model with 1, 2, 3 and 4 factors using the Maximum 
# Likelihood method and select the best model using the Chi-square test. 
# Use the function factanal.

# We perform an EXPLORATORY FACTOR ANALYSIS in order to how many factors there are under this 
# correlation matrix. It is very probable, by looking at the matrix, that we will have 3 or 4 
# factors. 
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
df <- c(f1$dof, f2$dof, f3$dof, f4$dof)
pvalues <- round(c(f1$PVAL, f2$PVAL, f3$PVAL, f4$PVAL), 4)
Chisq # It decreases from the 1-factor solution to the 4-factor solution
df # The same is true for the degrees of freedom because we consider more parameters to be estimated
pvalues # The only p-value for which we DON'T REJECT H0 is the one of the FOURTH SOLUTION, 4-FACTOR MODEL


# So we choose the 4-FACTOR MODEL.
# We print the solution.
f4

# We can see that in principle is a good solution that is able to explain the 53.5% of the overall 
# variability and the p-value tell us to not reject.
# But if we look at the uniqueness we see that there is the variance of the error term of V6 that 
# is more or less equal to zero, and if we compute the COMMONALITIES:
comm <- 1-f4$uniqueness
round(comm, 3)

# We see that this is almost equal to 1. This is quite strange because the commonalities express 
# the percentage of the variability explained by the factor model for each variable.
# This is what we call HEYHOOD-CASE.

# In order to solve this problem we can do different things:
# Run a 4-factors model after deleting the V6 item --> the problem IN THIS CASE won't be solved anyway.
# In this case, even if the fit is not very good, the best choice is to run a 3-factors model.

f3

# The percentage of the overall variability explained by the model is 43%, so this is still a good 
# result.
# 
# -In general from the solution we see that we have quite high loading related to the FIRST factor 
# for the first 3 items, 5 item and 10 item. This is more related to the husband and wife father 
# occupations, so it is a factor related to the occupational status of all the family.
# 
# -For the SECOND factor we have high loading in relation to the son (last three items); this factor 
# capture the first son condition in terms of education, qualification... and this factor has a not 
# negligible loading also in correspondence of the 4 items that is related to the father education.
# 
# -For the THIRD factor we have high loading for the 3,4,6,7 items and it is related to the parent 
# status in terms of education and qualification.
#


# 3 - Compute the communalities and comment them. Which percentage of the 
# variance of the model is explained by the three-factor model?
comm <- 1 - f3$uniquenesses
comm
percVar <- sum(comm)/nrow(cormat)
percVar


# 4 - Compute the reproduced correlation matrix and the discrepancy between the 
# observed and reproduced correlation.
repcorr <- loadings(f3) %*% t(loadings(f3))
round(cormat - repcorr, 3)
# In general the 3-model solution is not bad (residuals are pretty close to 0).


# 5 - Apply different orthogonal and oblique rotations and interpret the solutions obtained.
loadings(f3)
print(f3, cutoff = 0.2)
library(GPArotation)
Varimax(loadings(f3))
quartimax(loadings(f3))
oblimin(loadings(f3))

# The interpretation of the factor is similar to the previous one:
#   
# 1st FACTOR = we have high loading corresponding to all the items that express to general occupational 
# status of the family and so item 1,2,5,10. So high score of this factor correspond to a good 
# occupational status of the family.
# 
# 2nd FACTOR = it is related to firstborn condition (occupation, qualification, education) because we have
# high loading in item 8,9,10
# 
# 3rd FACTOR = high loading in item 3,4,6,7, which are items related to parents conditions.
# 
# The phi matrix that express the a posterior correlation of the latent variables has a NOT NEGLIGIBLE 
# CORRELATION, so it make sense to assume that this factors are correlated.
# 
# So of course the best solution in terms of the goodness of fit test is the 4-factor solution but since 
# in that case we have a specific variance very close to zero it is better to consider the 3-factor 
# model even if the overall goodness of fit test is not so good, it is well interpretable and the 
# residuals are good.


# CONFIRMATORY FACTOR ANALYSIS
# 6 - On the basis of the previous analysis perform a confirmatory factor 
# analysis using the function cfa in lavaan.
library(lavaan)
?cfa
socmob.model <- "
F1 =~ V1 + V2 + V5 + V10
F2 =~ V8 + V9 + V10
F3 =~ V3 + V4 + V6 + V7"

fit <- cfa(socmob.model, sample.cov = cormat, sample.nobs = n, std.lv = TRUE)
# std.lv = T allow to fix the variances of the latnet variables equal to 1 and 
# make the loading estimation free
summary(fit, fit.measures = T)


# We have the MLE for the model, the chi-square reject the null hypothesis that the model is a good model 
# but for the confirmatory factor model the chi-square is influenced on the sample size so we are not sure 
# if the rejection depends on the large sample size or on the fact that the model is not a good model 
# so we look at the CFI and TLI.
# They are close to 0.9 so there are some improvements that can be done like for example considering also 
# other items in the factor that have small but not so small loading.
# The RMSEA has cut point = 0.08 so this model is good but not the best model that we can have.
# All the loading are significant.
