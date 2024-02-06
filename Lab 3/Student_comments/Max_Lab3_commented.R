#Latent class analysis on carcinoma data set / 04/12/2023


# 1. #### 

library(poLCA) 

data(carcinoma)
?carcinoma

head(carcinoma) 

# A B C D E F G
#1 1 1 1 1 1 1 1
#2 1 1 1 1 1 1 1
#3 1 1 1 1 1 1 1
#4 1 1 1 1 1 1 1
#5 1 1 1 1 1 1 1
#6 1 1 1 1 1 1 1


# 2. ####

?poLCA

formula<-cbind(A,B,C,D,E,F,G)~1
# matrix of oberserved variables
# putting 1 because we don't have covariates

# Fitting 3 different models
m.2<-poLCA(formula,carcinoma,nclass=2,nrep=10,verbose=FALSE)
m.3<-poLCA(formula,carcinoma,nclass=3,nrep=10,verbose=FALSE)
m.4<-poLCA(formula,carcinoma,nclass=4,nrep=10,verbose=FALSE)


# 3. ####

m.2$predcell
# matrix which contains the 20 response patterns with observed and 
# expected frequencies

freq.estim<-data.frame(m.2$predcell[1:9],m.3$predcell[9],m.4$predcell[9])
freq.estim
# for 1st observation: expected.1 and expected.2 are much closer to observed
# than expected (also for observation 3 and 12); we can already see that the 2-
# class model is not the best

# Also: for observation 8, we see low expected frequencies; will see later what 
# that means

#   A B C D E F G observed expected expected.1 expected.2
#1  1 1 1 1 1 1 1       34   23.049     33.849     33.810
#2  1 1 1 1 2 1 1        2    6.612      1.973      1.994
#3  1 2 1 1 1 1 1        6   12.651      6.323      6.364
#4  1 2 1 1 1 1 2        1    1.668      1.548      1.477
#5  1 2 1 1 2 1 1        4    3.629      3.045      2.911
#6  1 2 1 1 2 1 2        5    0.479      4.660      4.830
#7  2 1 1 1 1 1 1        2    3.039      2.058      2.074
#8  2 1 2 1 2 1 2        1    0.197      0.186      0.981
#9  2 2 1 1 1 1 1        2    1.668      1.284      1.256
#10 2 2 1 1 1 1 2        1    0.299      1.630      1.709
#11 2 2 1 1 2 1 1        2    0.479      2.892      3.008
#12 2 2 1 1 2 1 2        7    3.668      6.494      6.368
#13 2 2 1 1 2 2 2        1    2.640      1.446      0.945
#14 2 2 1 2 1 1 2        1    0.093      0.100      0.108
#15 2 2 1 2 2 1 2        2    4.250      2.552      2.185
#16 2 2 1 2 2 2 2        3    3.112      2.049      2.958
#17 2 2 2 1 2 1 2       13   11.470      9.563     13.026
#18 2 2 2 1 2 2 2        5    8.399      8.701      5.107
#19 2 2 2 2 2 1 2       10   13.523     13.550      9.897
#20 2 2 2 2 2 2 2       16    9.902     12.328     15.990


# 4. ####

K<-c("2", "3", "4")

# loglikelihood
llik<-c(m.2$llik,m.3$llik,m.4$llik)

# increasing number of parameters
npar<-c(m.2$npar,m.3$npar,m.4$npar) 

# LR test
Gsq<-round(c(m.2$Gsq,m.3$Gsq,m.4$Gsq),3) 

# Chi-squared test (now don't have to do it manually)
Chisq<-round(c(m.2$Chisq,m.3$Chisq,m.4$Chisq),3) 

# DoF
df<-c(m.2$resid.df,m.3$resid.df,m.4$resid.df)

# p-value - we are in the right tail
pvalue<-round(1-pchisq(Chisq,df),4)

# AIC/BIC
AIC<-round(c(m.2$aic,m.3$aic,m.4$aic),3)
BIC<-round(c(m.2$bic,m.3$bic,m.4$bic),3)

# Summary table
summary<-data.frame(K,llik,npar,Gsq,Chisq,df,pvalue,AIC,BIC)
summary

#  K      llik npar    Gsq  Chisq  df pvalue     AIC     BIC
#1 2 -317.2568   15 62.365 92.648 103 0.7581 664.514 706.074
#2 3 -293.7050   23 15.262 20.503  95 1.0000 633.410 697.136
#3 4 -289.2858   31  6.423 10.086  87 1.0000 640.572 726.463

# loglikelihood increases with number of classes (typical)
# number of parameters will increase
# According to AIC/BIC, we should choose the 3-class model, BUT: according to LR
# and Chi-square test they are all not significant; would even prefer 2-class 
# model; Explanation (in book): With expected frequencies less than 5, LR and 
# Chi-Square tests will explode. BUT: With expected frequencies below 0.1, we 
# have the exact opposite.
# -->  Should rely on AIC/BIC here (we can also see that 2-class model doesn't 
# fit well)


# 7. #### 

round(m.3$P,4) # extract prior probabilities

# 0.3736 0.4447 0.1817

lapply(m.3$probs,round,4)

# Switching problem; indeterminacy of the latent classes(?)
# Want to give the same order to the classes (see 8.)

#$A
#          Pr(1)  Pr(2)
#class 1:  0.9427 0.0573
#class 2:  0.0000 1.0000
#class 3:  0.4872 0.5128

#$B
#           Pr(1)  Pr(2)
#class 1:  0.8621 0.1379
#class 2:  0.0191 0.9809
#class 3:  0.0000 1.0000

#$C
#           Pr(1)  Pr(2)
#class 1:  1.0000 0.0000
#class 2:  0.1425 0.8575
#class 3:  1.0000 0.0000

#$D
#           Pr(1)  Pr(2)
#class 1:  1.0000 0.0000
#class 2:  0.4138 0.5862
#class 3:  0.9424 0.0576

#$E
#           Pr(1)  Pr(2)
#class 1:  0.9449 0.0551
#class 2:  0.0000 1.0000
#class 3:  0.2494 0.7506

#$F
#           Pr(1)  Pr(2)
#class 1:  1.0000 0.0000
#class 2:  0.5236 0.4764
#class 3:  1.0000 0.0000

#$G
#           Pr(1)  Pr(2)
#class 1:  1.0000 0.0000
#class 2:  0.0000 1.0000
#class 3:  0.3693 0.6307


# 8. ####

# Now: Give an order to the priors

probs.start.m3<-m.3$probs.start

# Reorder the solutions in increasing order of m.3$P (estimated prior prob)
new.probs.start.m3<-poLCA.reorder(probs.start.m3,order(m.3$P))

# Refit the model, but use the ordered solutions in 'probs.start' argument
m.3.ord<-poLCA(formula,carcinoma,nclass=3,probs.start=new.probs.start.m3,verbose=FALSE)
round(m.3.ord$P,4)

# 0.1817 0.3736 0.4447

# 9. ####

# now looking at conditional probabilities of the ordered solution
lapply(m.3.ord$probs,round,4)


# again: 
# -1=absence 
# -2=presence 
# of carcinoma

#$A
#           Pr(1)  Pr(2)
#class 1:  0.4872 0.5128
#class 2:  0.9427 0.0573
#class 3:  0.0000 1.0000 # class 3: tissues, we there is carcinoma for sure

#$B
#           Pr(1)  Pr(2)
#class 1:  0.0000 1.0000
#class 2:  0.8621 0.1379
#class 3:  0.0191 0.9809

#$C
#           Pr(1)  Pr(2)
#class 1:  1.0000 0.0000
#class 2:  1.0000 0.0000
#class 3:  0.1425 0.8575

#$D
#           Pr(1)  Pr(2)
#class 1:  0.9424 0.0576
#class 2:  1.0000 0.0000
#class 3:  0.4138 0.5862 # here: almost half and half

#$E
#           Pr(1)  Pr(2)
#class 1:  0.2494 0.7506
#class 2:  0.9449 0.0551
#class 3:  0.0000 1.0000

#$F
#           Pr(1)  Pr(2)
#class 1:  1.0000 0.0000
#class 2:  1.0000 0.0000
#class 3:  0.5236 0.4764 # in disagreement

#$G
#           Pr(1)  Pr(2)
#class 1:  0.3693 0.6307
#class 2:  1.0000 0.0000
#class 3:  0.0000 1.0000

# Summary: 

# class 3: out of 7, 5 are in agreement that class 3 is the type of tissue with 
# carcinoma
# class 2: 7 out of 7 agree that this class is composed of tissues where all 
# pathologists agree there is an absence of carcinoma
# class 1: completely split in two; this class is the class of inconsistent 
# diagnoses; recall that this class is the least numerous (0.1817); so very 
# small


# 10. ####

head(carcinoma)
# first row: All tissues give negative diagnosis: We want to compute the 
# probability of that response pattern
# f(x_A=1, x_B=1, ..., x_G=1) = pi\1 = sum_^3_{j=0} eta_j \prod^G_{i=A} \pi_{ij}^x{ij} (1-\pi_{ij})^{1-x_{ij}}

# m.3$P[1] = eta_1
# m.3$probs$A[1,1] = 1-\pi_{A1}^{1-x_A1} # --> we are multiplying by the 
# (1-\pi_{ij})^{1-x_{ij}} part of the formula
p.dnc1<-m.3$P[1]*m.3$probs$A[1,1]*m.3$probs$B[1,1]*m.3$probs$C[1,1]*m.3$probs$D[1,1]*m.3$probs$E[1,1]*m.3$probs$F[1,1]*m.3$probs$G[1,1]

# m.3$P[2] = \eta_2
# m.3$probs$A[2,1] = probability of the first response pattern associated to the second class
p.dnc2<-m.3$P[2]*m.3$probs$A[2,1]*m.3$probs$B[2,1]*m.3$probs$C[2,1]*m.3$probs$D[2,1]*m.3$probs$E[2,1]*m.3$probs$F[2,1]*m.3$probs$G[2,1]
p.dnc3<-m.3$P[3]*m.3$probs$A[3,1]*m.3$probs$B[3,1]*m.3$probs$C[3,1]*m.3$probs$D[3,1]*m.3$probs$E[3,1]*m.3$probs$F[3,1]*m.3$probs$G[3,1]
p.dn<-p.dnc1+p.dnc2+p.dnc3
round(p.dn,4)
# 0.2869


# 11. ####

round(p.dn*m.3$N,4)
# 33.8491
118*p.dn # number of tissues in which the pathologists agree in a negative 
# diagnosis


# 12. ####

post.1<-m.3.ord$posterior[1,] # posterioir of the first response pattern 
# (= agreement in a negative diagnosis)
round(post.1,5)
#0 1 0
# quite expected: Probability of belonging to the second class for the first 
# response pattern; this is the latent class of negative diagnosis.


# 13. ####

# class vector (according to posterior probability)
m.3.ord$predclass
# 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 2 2 3 1 1 1 1 1 1 1 1 1 1 1 1 3 1 3 3 3 3 3
# 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# 3 3 3 3 3 3 3

# create a frequency table of this predicted class
table(m.3.ord$predclass)
#1  2  3 
#23 44 51 


# 14. ####

# Select the samples of tissues allocated to the class of inconsistent diagnosis 
# and evaluate the correspondent response pattern.
sel<-carcinoma[m.3.ord$predclass==1,] # select all tissues with predicted 
# class==1
sel
#   A B C D E F G
#43 1 2 1 1 1 1 2
#44 1 2 1 1 2 1 1
#45 1 2 1 1 2 1 1
#46 1 2 1 1 2 1 1
#47 1 2 1 1 2 1 1
#48 1 2 1 1 2 1 2
#49 1 2 1 1 2 1 2
#50 1 2 1 1 2 1 2
#51 1 2 1 1 2 1 2
#52 1 2 1 1 2 1 2
#56 2 2 1 1 1 1 1
#57 2 2 1 1 1 1 1

# 23 tissues: At most 4 pathologists give a positive diagnosis in that class;
# again this is the 'inconsistent' class




