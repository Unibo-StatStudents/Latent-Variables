######Lab2#Item response theory

library(ltm)

data(Abortion)
?Abortion

dim(Abortion)
# 379   4

head(Abortion) # first 6 individuals reply '1'

#  Item 1 Item 2 Item 3 Item 4
#1      1      1      1      1
#2      1      1      1      1
#3      1      1      1      1
#4      1      1      1      1
#5      1      1      1      1
#6      1      1      1      1

?descript 
# comment of ltm
# computes the descriptive statistics 

dsc <- descript(Abortion)

dsc$perc 
# percentages of agrees for each items
# of course, the first logit will be negative since the proportion of positive
# answers is less than negative answers

#             0         1      logit
#Item 1 0.5620053 0.4379947 -0.2493044
#Item 2 0.4063325 0.5936675  0.3791478
#Item 3 0.3641161 0.6358839  0.5575432
#Item 4 0.3825858 0.6174142  0.4785874

dsc$items
# 103 is the number individuals replied 0 to all the items
# 141 is the number of individuals replying 1 to all items (1111 = 4)

#      0  1  2  3   4
#Freq 103 33 37 65 141

dsc$pw.ass
# for all pairs of items we have a significant chi-squared 
# latent variables are associated -> so it makes sense to estimate a linear 
# trait model

#Item i Item j p.value
#1      1      4  <2e-16
#2      1      3  <2e-16
#3      2      4  <2e-16
#4      1      2  <2e-16
#5      2      3  <2e-16
#6      3      4  <2e-16


# 2.1) - 2.5) ####
?ltm 
# fits latent trait model to the data
# requires a formula
# IRT.param: see documentation: we use FALSE, and the the difficulty parameter
# will be negative

m1<-ltm(Abortion~z1)
# Abortion has dependent on the latent variable z1; IRT.param=TRUE: 2-param 
# logistic model

m1.rip<-ltm(Abortion~z1,IRT.param=FALSE)
# latent trait parametrization

summary(m1)
# Given that we have a negative difficulty parameter: low values = easy items

#Call:
#ltm(formula = Abortion ~ z1)

#Model Summary:
#   log.Lik      AIC      BIC
# -706.3369 1428.674 1460.174

#Coefficients:
#                value std.err  z.vals
#Dffclt.Item 1  0.1697  0.0659  2.5749  (more 0 replies than 1 -> positive parameter)
#Dffclt.Item 2 -0.2362  0.0618 -3.8198
#Dffclt.Item 3 -0.3428  0.0671 -5.1062 (easiest items: we can see above it has the highest percentage of postive answers)
#Dffclt.Item 4 -0.3165  0.0654 -4.8411
#Dscrmn.Item 1  4.4532  1.0245  4.3468
#Dscrmn.Item 2  4.3226  0.6824  6.3340
#Dscrmn.Item 3  5.6639  0.9957  5.6883 (also item 3 is the one that discriminates the most)
#Dscrmn.Item 4  3.6254  0.5570  6.5087

# asymptotic standard errors and zeta-values
# Item3: The woman is not married and does not wish to marry the man

#Integration:
#method: Gauss-Hermite (to approximate the integral; used in E-M algorithm)
#quadrature points: 21 

#Optimization:
#Convergence: 0 
#max(|grad|): 0.0039 
#quasi-Newton: BFGS 


summary(m1.rip)
# Given that we have a positive difficulty parameter: low values = difficult items
# --> NOTE: Values are the same for the discrimination parameter


#Call:
#ltm(formula = Abortion ~ z1, IRT.param = FALSE)
#
#Model Summary:
#   log.Lik      AIC      BIC
# -706.3369 1428.674 1460.174 (--> same as above)

#Coefficients: --> note difficulty parameters are now reversed
#                     value std.err  z.vals
#(Intercept).Item 1 -0.7555  0.2836 -2.6643
#(Intercept).Item 2  1.0211  0.2857  3.5738
#(Intercept).Item 3  1.9416  0.4197  4.6262
#(Intercept).Item 4  1.1474  0.2667  4.3021
#z1.Item 1           4.4532  1.0245  4.3468
#z1.Item 2           4.3226  0.6824  6.3340
#z1.Item 3           5.6639  0.9957  5.6883
#z1.Item 4           3.6254  0.5570  6.5087

#Integration:
#method: Gauss-Hermite
#quadrature points: 21 

#Optimization:
#Convergence: 0 
#max(|grad|): 0.0039 
#quasi-Newton: BFGS --> different from above

# alpha_i1 = beta_i1
# -(alpha_i0/alpha_i1) = -(alpha_i0/beta_i1) = beta_i0
# summary(m1)$coefficients[1,1]: first diff param in the first parametrization
# summary(m1)$coefficients[5,1]: discrimination param in both parametrizations
-summary(m1)$coefficients[1,1]*summary(m1)$coefficients[5,1]
-0.7555329

summary(m1.rip)$coefficients[1,1]

alpha<-m1.rip$coeff[,2] # discrimination parameters
stalpha<-alpha/sqrt(1+alpha^2) 
# reminder: that is the standardized alpha: Correlation between xi
# Cov(x_i, Xi_i) = lambda_ij (...)
# stalpha can be read as the correlation between x and the latent variable;
# which is standardized to be 0 < stalpha < 1
# see slide: 26/33

# Xi~ N(0,1) 
stalpha
#   Item 1    Item 2    Item 3    Item 4 
#0.9757019 0.9742691 0.9847692 0.9639998 

coef(m1.rip,prob=TRUE,order=TRUE) 
# solution in terms of these probabilities
# same as before but we also have the probabilities of the median individual
# the median individual will have the highest probabilities for the 'easiest' 
# item 3
#       (Intercept)       z1 P(x=1|z=0)
#Item 1  -0.7555329 4.453179  0.3196169
#Item 2   1.0211338 4.322628  0.7351934
#Item 4   1.1473662 3.625383  0.7590295
#Item 3   1.9416173 5.663928  0.8745297

# 2.6) ####
plot(m1, legend = TRUE, cx = "bottomright", xlab="Attitude toward
abortion", lwd = 3, cex.main = 1.5, cex.lab = 1.3, cex = 1.1)
# we can see as many probabilities of positive responses as there are items
# difficulty params are related to the shift: Item 1 was the most difficult
# so that the probabilities of a positive response is the lowest. For item 3 
# (=easiest item) it is shifted upwards
# in terms of discrimination parameters: these influence the steepness of the 
# curve: the higher, the steeper. Item 3 has the highest discrimination 
# parameter, but note that in terms of stalpha, these are very close to each
# other, which means the steepness of the curves looks really similar


# 2.7) ####
# Interpretation: We have a better understanding of the parameters now, but we 
# have to check the goodness if fit of the model:

# Pearson chi-squared & LR test
# slides 29/33
# We should have 16 response patterns
E<-fitted(m1)[,5] # fitted gives us information about the fitted objects of the 
# model
# expected frequencies related to each response pattern. 

# Indeed, we don't have observed all the patterns: Only 14 out of 16:
fitted(m1)

O<- m1$patterns$obs

cbind(m1$pattern$X,O,E)

> cbind(m1$pattern$X,O,E)
#          O           E
#0 0 0 0 103 103.4122369 --> 1st response pattern: very good fit
#0 0 0 1  13  15.5202810
#0 0 1 0  10  11.1475316
#0 0 1 1  21  14.3156121
#0 1 0 0   9   8.9141331
#0 1 0 1   6   6.8099834
#0 1 1 0   7  12.0534014
#0 1 1 1  44  41.8185891
#1 0 0 0   1   1.3985312
#1 0 1 1   6   7.1931090
#1 1 0 0   3   0.8956743
#1 1 0 1   3   2.5554663
#1 1 1 0  12   7.1052145
#1 1 1 1 141 142.7076472

# IMPORTANT: definition of sparse data: Are some expected frequencies < 5?
# YES! (response patterns: 1000, 1011):
# Now, we compute the statistic:

Chisq<-sum((E-O)^2/E)
# is this significant? We need degrees of freedom

DOF<-14-2*4-1 
# information - #params - 1
# VERY COMMON MISTAKE: taking the theoretical information (=16) instead of the 
# observed information (=14)

pvalueC<-1-pchisq(Chisq,DOF) # upper tail of the chi-squared
pvalueC
#0.01170012 -> on significance level 0.05 Pearson chi-squared would reject the
# model

# Now: LR test statistic
LR<-2*sum(O*log(O/E))
pvalueLR<-1-pchisq(LR,DOF) # DOF are always the same
pvalueLR
#0.002319567 --> again: model rejected
# Both tests reject, BUT we still we are not sure if this is due to sparse data
# --> we could collapse the response patterns with low frequencies; drawback:
# might obtain negative DOF
# --> bootstrapping
# --> look at the residuals (done here: with the margins() command)


# 2.8) ####
?margins
# Checks the fit on the two- and three-way margins for grm, ltm, rasch and tpm 
# objects.
# NOTE: goodness of fit is exactly the same for both parametrizations of the 
# model
margins(m1)

#Call:
#ltm(formula = Abortion ~ z1)

#Fit on the Two-Way Margins

#Response: (0,0) --> for all pairs of items, we get the frequencies of the (0,0) response pattern
#  Item i Item j Obs    Exp (O-E)^2/E  
#1      1      4 129 135.53      0.31   
#2      2      3 117 121.46      0.16  
#3      2      4 114 117.99      0.13  

#Response: (1,0)
#  Item i Item j Obs   Exp (O-E)^2/E  
#1      1      2   7 11.74      1.92  
#2      1      4  16 11.43      1.83  
#3      3      4  29 32.33      0.34  

#Response: (0,1)
#  Item i Item j Obs   Exp (O-E)^2/E  
#1      3      4  22 26.01      0.62  
#2      1      4  84 78.46      0.39  
#3      1      2  66 69.60      0.19  

#Response: (1,1)
#  Item i Item j Obs    Exp (O-E)^2/E  
#1      1      2 159 153.26      0.21  
#2      3      4 212 206.03      0.17  
#3      1      4 150 153.58      0.08 

# NOTE: 
# - this table only shows the HIGHEST residuals; that is why we don't see all 
# possible combinations
# - We calculate and check R > 3.5 or R > 4
# - If three-way looks bad, but two-way looks good -> reject
# we can see that we don't get close to 3.5 or 4; meaning that sparse data is 
# NOT affecting our results
# slides 30/33

# NOW: Just print the first two residuals for the three-way responses
margins(m1,type="three-way",nprint=2)

#Call:
#ltm(formula = Abortion ~ z1)

#Fit on the Three-Way Margins

#Response: (0,0,0)
#  Item i Item j Item k Obs    Exp (O-E)^2/E  
#1      1      2      3 116 118.93      0.07  
#2      1      2      4 113 114.56      0.02  

#Response: (1,0,0)
#  Item i Item j Item k Obs  Exp (O-E)^2/E  
#1      1      2      4   1 3.43      1.72  
#2      1      3      4   4 2.29      1.27  

#Response: (0,1,0)
#  Item i Item j Item k Obs   Exp (O-E)^2/E  
#1      1      3      4  17 23.20      1.66  
#2      1      2      4  16 20.97      1.18  

#Response: (1,1,0)
#  Item i Item j Item k Obs  Exp (O-E)^2/E    
#1      1      2      4  15 8.00      6.12 *** --> NOTE: > 3.5: So there exists a triplet for which it is not a good fit; but this is negligible
#2      1      2      3   6 3.45      1.88    

#Response: (0,0,1)
#  Item i Item j Item k Obs   Exp (O-E)^2/E  
#1      1      2      3  31 25.46       1.2  
#2      2      3      4  13 16.65       0.8  

#Response: (1,0,1)
#  Item i Item j Item k Obs  Exp (O-E)^2/E  
#1      1      2      3   6 9.22      1.12  
#2      1      2      4   6 8.32      0.65  

#Response: (0,1,1)
##  Item i Item j Item k Obs   Exp (O-E)^2/E  
#1      2      3      4  27 21.51       1.4  
#2      1      3      4  65 56.13       1.4  

#Response: (1,1,1)
#  Item i Item j Item k Obs    Exp (O-E)^2/E  
#1      1      2      3 153 149.81      0.07  
#2      1      3      4 147 149.90      0.06  

#'***' denotes a chi-squared residual greater than 3.5 

margins(m1,type="three-way",nprint=3)

#Call:
#ltm(formula = Abortion ~ z1)

#Fit on the Three-Way Margins

#Response: (0,0,0)
#  Item i Item j Item k Obs    Exp (O-E)^2/E  
#1      1      2      3 116 118.93      0.07  
#2      1      2      4 113 114.56      0.02  
#3      2      3      4 104 104.81      0.01  

#Response: (1,0,0)
#  Item i Item j Item k Obs  Exp (O-E)^2/E  
#1      1      2      4   1 3.43      1.72  
#2      1      3      4   4 2.29      1.27  
#3      1      2      3   1 2.52      0.92  

#Response: (0,1,0)
#  Item i Item j Item k Obs   Exp (O-E)^2/E  
#1      1      3      4  17 23.20      1.66  
#2      1      2      4  16 20.97      1.18  
#3      2      3      4  10 13.17      0.77  

#Response: (1,1,0)
#  Item i Item j Item k Obs  Exp (O-E)^2/E    
#1      1      2      4  15 8.00      6.12 ***
#2      1      2      3   6 3.45      1.88    
#3      1      3      4  12 9.13      0.90    

#Response: (0,0,1)
#  Item i Item j Item k Obs   Exp (O-E)^2/E  
#1      1      2      3  31 25.46      1.20  
#2      2      3      4  13 16.65      0.80  
#3      1      2      4  34 29.84      0.58  

#Response: (1,0,1)
#  Item i Item j Item k Obs  Exp (O-E)^2/E  
#1      1      2      3   6 9.22      1.12  
#2      1      2      4   6 8.32      0.65  
#3      1      3      4   3 3.68      0.13  

#Response: (0,1,1)
#  Item i Item j Item k Obs   Exp (O-E)^2/E  
#1      2      3      4  27 21.51      1.40  
#2      1      3      4  65 56.13      1.40  
#3      1      2      3  51 53.87      0.15  

#Response: (1,1,1)
#  Item i Item j Item k Obs    Exp (O-E)^2/E  
#1      1      2      3 153 149.81      0.07  
#2      1      3      4 147 149.90      0.06  
#3      1      2      4 144 145.26      0.01  

#'***' denotes a chi-squared residual greater than 3.5 


# 2.9) ####
?factor.scores

fs<-factor.scores(m1,method="EAP")
# EAP: expected apriori pattern
fs

#Call:
#ltm(formula = Abortion ~ z1)

#Scoring Method: Expected A Posteriori

#Factor-Scores for observed response patterns:
#   Item 1 Item 2 Item 3 Item 4 Obs     Exp     z1 se.z1
#1       0      0      0      0 103 103.412 -1.163 0.550 --> lowest factor score for the most 'difficult' LV
#2       0      0      0      1  13  15.520 -0.643 0.285
#3       0      0      1      0  10  11.148 -0.449 0.337
#4       0      0      1      1  21  14.316 -0.085 0.244
#5       0      1      0      0   9   8.914 -0.585 0.297
#6       0      1      0      1   6   6.810 -0.188 0.310
#7       0      1      1      0   7  12.053 -0.048 0.214
#8       0      1      1      1  44  41.819  0.121 0.280
#9       1      0      0      0   1   1.399 -0.573 0.301
#10      1      0      1      1   6   7.193  0.131 0.288
#11      1      1      0      0   3   0.896 -0.118 0.270
#12      1      1      0      1   3   2.555  0.047 0.216
#13      1      1      1      0  12   7.105  0.199 0.334
#14      1      1      1      1 141 142.708  0.961 0.602

# Note: Can also use sufficiency principle ()
Comp<-factor.scores(m1,method="Component")
Comp
# Associated to each response pattern, we get a component (lin. comb. of obs 
# values) -> therefore z1 (component is highest for high obs values)
# NOTE: The ranking between EAP (above) and Component (below) is the same!

#Call:
#ltm(formula = Abortion ~ z1)

#Scoring Method: Component

#Factor-Scores for observed response patterns:
#   Item 1 Item 2 Item 3 Item 4 Obs     Exp     z1
#1       0      0      0      0 103 103.412  0.000
#2       0      0      0      1  13  15.520  3.625
#3       0      0      1      0  10  11.148  5.664
#4       0      0      1      1  21  14.316  9.289
#5       0      1      0      0   9   8.914  4.323
#6       0      1      0      1   6   6.810  7.948
#7       0      1      1      0   7  12.053  9.987
#8       0      1      1      1  44  41.819 13.612
#9       1      0      0      0   1   1.399  4.453
#10      1      0      1      1   6   7.193 13.742
#11      1      1      0      0   3   0.896  8.776
#12      1      1      0      1   3   2.555 12.401
#13      1      1      1      0  12   7.105 14.440
#14      1      1      1      1 141 142.708 18.065



resp.pattern<-fs$score.dat[,1:4] # -> just gives the response pattern
total.score<-apply(resp.pattern,1,sum) # -> just adding up: e.g., 0 + 0 + 0 + 1 for the second response pattern
total.score
#0 1 1 2 1 2 2 3 1 3 2 3 3 4

Cp<- Comp$score.dat[,7]

tab<-cbind(fs$score.dat,Cp,total.score)

# create a table to compare the method used in magazines to our EAP and component scores
round(tab,3)

#   Item 1 Item 2 Item 3 Item 4 Obs     Exp     z1 se.z1     Cp total.score
#1       0      0      0      0 103 103.412 -1.163 0.550  0.000           0
#2       0      0      0      1  13  15.520 -0.643 0.285  3.625           1
#3       0      0      1      0  10  11.148 -0.449 0.337  5.664           1
#4       0      0      1      1  21  14.316 -0.085 0.244  9.289           2
#5       0      1      0      0   9   8.914 -0.585 0.297  4.323           1
#6       0      1      0      1   6   6.810 -0.188 0.310  7.948           2
#7       0      1      1      0   7  12.053 -0.048 0.214  9.987           2
#8       0      1      1      1  44  41.819  0.121 0.280 13.612           3
#9       1      0      0      0   1   1.399 -0.573 0.301  4.453           1
#10      1      0      1      1   6   7.193  0.131 0.288 13.742           3
#11      1      1      0      0   3   0.896 -0.118 0.270  8.776           2
#12      1      1      0      1   3   2.555  0.047 0.216 12.401           3
#13      1      1      1      0  12   7.105  0.199 0.334 14.440           3
#14      1      1      1      1 141 142.708  0.961 0.602 18.065           4

# Now: Order the table according to total.score and see if the ranking 
# correspond to our two scores
# We can see that
# - 0000: matches
# - 0001: matches
# - 1111: matches
# - HOWEVER: in the middle the scores don't match perfectly
# So: EAP or Component scores have the be used
round(tab[order(total.score),],3)

#  Item 1 Item 2 Item 3 Item 4 Obs     Exp     z1 se.z1     Cp total.score
#1       0      0      0      0 103 103.412 -1.163 0.550  0.000           0
#2       0      0      0      1  13  15.520 -0.643 0.285  3.625           1
#3       0      0      1      0  10  11.148 -0.449 0.337  5.664           1
#5       0      1      0      0   9   8.914 -0.585 0.297  4.323           1
#9       1      0      0      0   1   1.399 -0.573 0.301  4.453           1
#4       0      0      1      1  21  14.316 -0.085 0.244  9.289           2
#6       0      1      0      1   6   6.810 -0.188 0.310  7.948           2
#7       0      1      1      0   7  12.053 -0.048 0.214  9.987           2
#11      1      1      0      0   3   0.896 -0.118 0.270  8.776           2
#8       0      1      1      1  44  41.819  0.121 0.280 13.612           3
#10      1      0      1      1   6   7.193  0.131 0.288 13.742           3
#12      1      1      0      1   3   2.555  0.047 0.216 12.401           3
#13      1      1      1      0  12   7.105  0.199 0.334 14.440           3
#14      1      1      1      1 141 142.708  0.961 0.602 18.065           4


# 2.10) ####
# Now compute the factor scores for these response patterns which are NOT observed
# Note: Assignment slides use the fitted function here  which is wrong
factor.scores(m1,resp.pattern=rbind(c(1,0,0,1),c(1,0,1,0)))

#Call:
#ltm(formula = Abortion ~ z1)

#Scoring Method: Empirical Bayes

#Factor-Scores for specified response patterns:
#  Item 1 Item 2 Item 3 Item 4 Obs   Exp     z1 se.z1
#1      1      0      0      1   0 1.125 -0.248 0.231
#2      1      0      1      0   0 2.027 -0.135 0.240

# Negative scores: These are response patterns for individuals having a negative 
# view towards abortion
