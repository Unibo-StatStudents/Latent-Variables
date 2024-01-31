#----------------------- MOBILITY DATA -------------------------------------#
# A rural subsample of 8445 women from the Bangladesh Fertility Survey of 1989.
# The dimension of interest is women’s mobility of social freedom. 
# Women were asked whether they could engage in the following activities alone 
# (1 = yes, 0 = no):
# Item 1: Go to any part of the village/town/city. 
# Item 2: Go outside the village/town/city.
# Item 3: Talk to a man you do not know.
# Item 4: Go to a cinema/cultural show.
# Item 5: Go shopping.
# Item 6: Go to a cooperative/mothers’ club/other club. 
# Item 7: Attend a political meeting.
# Item 8: Go to a health centre/hospital.



# 1 - Load the dataset Mobility (library ltm)
library(ltm)
?Mobility
str(Mobility)


# 2 - Recode the dataset properly in order to use the function poLCA.
names(Mobility) <- c("Item1", "Item2","Item3","Item4",
                   "Item5","Item6","Item7","Item8")

# The function poLCA does not accept "0" entries. 
# Manifest variables must contain only integer values, and must be coded with 
# consecutive values from 1 to the maximum number of outcomes for each variable.

# Create new values: 2=yes 1=no
Mobility[Mobility == 1] <- 2
Mobility[Mobility == 0] <- 1


# 3 - Use the function poLCA to fit the models with 2, 3 and 4 latent classes to the data.
library(poLCA)
formula <- cbind(Item1, Item2, Item3, Item4, Item5, Item6, Item7, Item8) ~ 1 # 1 because we don't have covariates in this analysis
m.2 <- poLCA(formula, Mobility, nclass = 2, nrep = 10, verbose = FALSE)
m.3 <- poLCA(formula, Mobility, nclass = 3, nrep = 10, verbose = FALSE)
m.4 <- poLCA(formula, Mobility, nclass = 4, nrep = 10, verbose = FALSE)
# nrep = n° of time that we fit the model with different initial values


# 4 - Compare the estimates of the expected frequencies for the response pattern 
# obtained with the three models.
freq.estim <- data.frame(m.2$predcell[1:10], m.3$predcell[10], m.4$predcell[10])
freq.estim

# We get a data-frame that contains the responses pattern and the observed frequencies that are
# common to all the three models and the expected frequencies under the model with 2 , 3 and 4
# classes.

# It is possible to see that there are some expected frequencies that are LESS THAN 5. So we
# cannot rely on the test statistics that use the chi-square approximation


# 5 - Compare the three models considering the following measures: 
# log-likelihood, number of free parameters, likelihood ratio test, 
# Pearson chi-square test (and associated p-value), AIC, BIC.
K <- c("2", "3", "4")
llik <- c(m.2$llik, m.3$llik, m.4$llik) # log-likelihood
npar <- c(m.2$npar, m.3$npar, m.4$npar) # number of free parameters
Gsq <- round(c(m.2$Gsq, m.3$Gsq, m.4$Gsq), 3) # G^2
Chisq <- round(c(m.2$Chisq, m.3$Chisq, m.4$Chisq), 3) # Pearson chi-square test
df <- c(m.2$resid.df, m.3$resid.df, m.4$resid.df)
pvalue <- round(1 - pchisq(Chisq, df), 4) # p-value
AIC <- round(c(m.2$aic, m.3$aic, m.4$aic), 3) # AIC
BIC <- round(c(m.2$bic, m.3$bic, m.4$bic), 3) # BIC
summary <- data.frame(K, llik, npar, Gsq, Chisq, df, pvalue, AIC, BIC)
summary


# 6 - Which model shows the best fit to the observed data?
# According to the Chi^2 test, by seeing the p-value we should refuse the hypothesis that this
# model is a good model for the data. But since the expected frequencies of some patterns are
# lower than 5 than we should not rely on this result. 

# We should look to AIC and BIC. From these model selection criteria the best model is the one
# with 4 classes.


# 7 - Display the parameter estimates of the four-class model and compare 
# this solution with those obtained with your colleagues. Which problem arises?
round(m.4$P, 4)
lapply(m.4$probs, round, 4) # Class conditional response probability
# It is possible that we get the classes in a different order because of the problem of the
# indeterminacy of the latent class.



# 8 - Use the function poLCA.reorder to solve the indeterminacy of the ranking of the latent classes.
?poLCA.reorder
probs.start.m4 <- m.4$probs.start # Starting probability used in the model m.4
new.probs.start.m4 <- poLCA.reorder(probs.start.m4, order(m.4$P)) # We reorder the probability according to the order of m.4$P
m.4.ord <- poLCA(formula, Mobility, nclass = 4, probs.start = new.probs.start.m4, verbose = FALSE)
m.4.ord
round(m.4.ord$P, 4) # The classes are ordered following the size of each class and 
# this means that class 1 has the lowest size while class 4 has the greatest size
lapply(m.4.ord$probs, round, 4) # estimated class-conditional response probabilities.


# 9 - Interpret the four latent classes.
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


# 10 - Display the posterior probability estimates of the response pattern 
# (1, 1, 1, 1, 1, 1, 1, 1) and (2, 2, 2, 2, 2, 2, 2, 2) for the four latent classes. 
# In which classes the samples of women corresponding to these response patterns are allocated?

# posterior: matrix of posterior class membership probabilities
# a = object that includes all the observations that have response pattern = ALL NO
a <- as.numeric(row.names(Mobility[which(Mobility$Item1 == 1 & Mobility$Item2 == 1 & 
                                           Mobility$Item3 == 1 & Mobility$Item4 == 1 & 
                                           Mobility$Item5 == 1 & Mobility$Item6 == 1 & 
                                           Mobility$Item7 == 1 & Mobility$Item8 == 1),]))
post.1 <- m.4.ord$posterior[a[1],]
round(post.1, 5)

# (2, 2, 2, 2, 2, 2, 2, 2)
b <- as.numeric(row.names(Mobility[which(Mobility$Item1 == 2 & Mobility$Item2 == 2 & 
                                           Mobility$Item3 == 2 & Mobility$Item4 == 2 &
                                           Mobility$Item5 == 2 & Mobility$Item6 == 2 & 
                                           Mobility$Item7 == 2 & Mobility$Item8 == 2),]))
post.2 <- m.4.ord$posterior[b[1],]
round(post.2, 5)


# 11 - Display the latent class in which the samples of women are allocated 
# according to highest posterior probability.
m.4.ord$predclass
table(m.4.ord$predclass)


# 12 - Select the samples of women allocated to the class with less mobility and 
# evaluate the corresponding response patterns.
cl3 <- Mobility[m.4.ord$predclass == 3,] # To select this particular subset of women
# I get all the observations with the response pattern
unique(cl3) # We get the UNIQUE response patterns classified in class 3


# 13 - Compute the predicted cell probability from the latent class model of the 
# response pattern (2, 1, 2, 1, 2, 1, 2, 1), that is not observed in the dataset, 
# and allocate it according to the highest posterior probability.
# We want to predict the predicted probability of a response pattern that is not showed in the data set
?poLCA.predcell
poLCA.predcell(m.4.ord, c(2, 1, 2, 1, 2, 1, 2, 1)) # Predicted percentage of this response pattern
poLCA.posterior(m.4.ord, y = c(2, 1, 2, 1, 2, 1, 2, 1)) # We can classify this pattern not observed in the
# data set. This pattern is classified in class 2.
unique(Mobility)

# In order to know which are the not observed pattern, if there are many, we can use as command the following:
c <- as.numeric(row.names(Mobility[which(Mobility$Item1 == 2 & Mobility$Item2 == 1 & 
                                         Mobility$Item3 == 2 & Mobility$Item4 == 1 & 
                                         Mobility$Item5 == 2 & Mobility$Item6 == 1 & 
                                         Mobility$Item7 == 2 & Mobility$Item8 == 1),]))
# We get a result of 0 because this pattern is not observed in the data set.






















