######Lab2#Item response theory

library(ltm)

data(Abortion)
?Abortion

dim(Abortion)
# 379   4

head(Abortion)

#  Item 1 Item 2 Item 3 Item 4
#1      1      1      1      1
#2      1      1      1      1
#3      1      1      1      1
#4      1      1      1      1
#5      1      1      1      1
#6      1      1      1      1

?descript

dsc <- descript(Abortion)

dsc$perc

#             0         1      logit
#Item 1 0.5620053 0.4379947 -0.2493044
#Item 2 0.4063325 0.5936675  0.3791478
#Item 3 0.3641161 0.6358839  0.5575432
#Item 4 0.3825858 0.6174142  0.4785874

dsc$items

#      0  1  2  3   4
#Freq 103 33 37 65 141

dsc$pw.ass

#Item i Item j p.value
#1      1      4  <2e-16
#2      1      3  <2e-16
#3      2      4  <2e-16
#4      1      2  <2e-16
#5      2      3  <2e-16
#6      3      4  <2e-16

?ltm

m1<-ltm(Abortion~z1)
m1.rip<-ltm(Abortion~z1,IRT.param=FALSE)

summary(m1)

#Call:
#ltm(formula = Abortion ~ z1)

#Model Summary:
#   log.Lik      AIC      BIC
# -706.3369 1428.674 1460.174

#Coefficients:
#                value std.err  z.vals
#Dffclt.Item 1  0.1697  0.0659  2.5749  
#Dffclt.Item 2 -0.2362  0.0618 -3.8198
#Dffclt.Item 3 -0.3428  0.0671 -5.1062
#Dffclt.Item 4 -0.3165  0.0654 -4.8411
#Dscrmn.Item 1  4.4532  1.0245  4.3468
#Dscrmn.Item 2  4.3226  0.6824  6.3340
#Dscrmn.Item 3  5.6639  0.9957  5.6883
#Dscrmn.Item 4  3.6254  0.5570  6.5087

#Integration:
#method: Gauss-Hermite
#quadrature points: 21 

#Optimization:
#Convergence: 0 
#max(|grad|): 0.0039 
#quasi-Newton: BFGS 


summary(m1.rip)

#Call:
#ltm(formula = Abortion ~ z1, IRT.param = FALSE)
#
#Model Summary:
#   log.Lik      AIC      BIC
# -706.3369 1428.674 1460.174

#Coefficients:
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
#quasi-Newton: BFGS 

-summary(m1)$coefficients[1,1]*summary(m1)$coefficients[5,1]
 -0.7555329

summary(m1.rip)$coefficients[1,1]

alpha<-m1.rip$coeff[,2]
stalpha<-alpha/sqrt(1+alpha^2)

stalpha
#   Item 1    Item 2    Item 3    Item 4 
#0.9757019 0.9742691 0.9847692 0.9639998 

coef(m1.rip,prob=TRUE,order=TRUE)
#       (Intercept)       z1 P(x=1|z=0)
#Item 1  -0.7555329 4.453179  0.3196169
#Item 2   1.0211338 4.322628  0.7351934
#Item 4   1.1473662 3.625383  0.7590295
#Item 3   1.9416173 5.663928  0.8745297

plot(m1, legend = TRUE, cx = "bottomright", xlab="Attitude toward
abortion", lwd = 3, cex.main = 1.5, cex.lab = 1.3, cex = 1.1)

E<-fitted(m1)[,5]
O<- m1$patterns$obs

cbind(m1$pattern$X,O,E)

> cbind(m1$pattern$X,O,E)
#          O           E
#0 0 0 0 103 103.4122369
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

Chisq<-sum((E-O)^2/E)
DOF<-14-2*4-1
pvalueC<-1-pchisq(Chisq,DOF)
pvalueC
#0.01170012

LR<-2*sum(O*log(O/E))
pvalueLR<-1-pchisq(LR,DOF)
pvalueLR
#0.002319567

?margins
margins(m1)

#Call:
#ltm(formula = Abortion ~ z1)

#Fit on the Two-Way Margins

#Response: (0,0)
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
#1      1      2      4  15 8.00      6.12 ***
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


?factor.scores

fs<-factor.scores(m1,method="EAP")
fs

#Call:
#ltm(formula = Abortion ~ z1)

#Scoring Method: Expected A Posteriori

#Factor-Scores for observed response patterns:
#   Item 1 Item 2 Item 3 Item 4 Obs     Exp     z1 se.z1
#1       0      0      0      0 103 103.412 -1.163 0.550
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


Comp<-factor.scores(m1,method="Component")
Comp

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



resp.pattern<-fs$score.dat[,1:4]
total.score<-apply(resp.pattern,1,sum)
total.score
 #0 1 1 2 1 2 2 3 1 3 2 3 3 4
 
Cp<- Comp$score.dat[,7]

tab<-cbind(fs$score.dat,Cp,total.score)

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


factor.scores(m1,resp.pattern=rbind(c(1,0,0,1),c(1,0,1,0)))

#Call:
#ltm(formula = Abortion ~ z1)

#Scoring Method: Empirical Bayes

#Factor-Scores for specified response patterns:
#  Item 1 Item 2 Item 3 Item 4 Obs   Exp     z1 se.z1
#1      1      0      0      1   0 1.125 -0.248 0.231
#2      1      0      1      0   0 2.027 -0.135 0.240


