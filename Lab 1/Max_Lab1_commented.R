library(lavaan)
data(HolzingerSwineford1939)

?HolzingerSwineford1939

#x1 Visual perception
#x2 Cubes
#x3 Lozenges
#x4 Paragraph comprehension
#x5 Sentence completion
#x6 Word meaning
#x7 Speeded addition
#x8 Speeded counting of dots
#x9 Speeded discrimination straight and curved capitals

# x1-x2-x3= Visualization factor
# x4-x5-x6= Verbal intelligence/ textual factor
# x7-x8-x9= Speed factor



head(HolzingerSwineford1939)

#sex ageyr agemo  school grade       x1   x2    x3       x4   x5        x6       x7   x8       x9
#1  1   1    13     1 Pasteur     7 3.333333 7.75 0.375 2.333333 5.75 1.2857143 3.391304 5.75 6.361111 
#2  2   2    13     7 Pasteur     7 5.333333 5.25 2.125 1.666667 3.00 1.2857143 3.782609 6.25 7.916667 
#3  3   2    13     1 Pasteur     7 4.500000 5.25 1.875 1.000000 1.75 0.4285714 3.260870 3.90 4.416667 
#4  4   1    13     2 Pasteur     7 5.333333 7.75 3.000 2.666667 4.50 2.4285714 3.000000 5.30 4.861111 
#5  5   2    12     2 Pasteur     7 4.833333 4.75 0.875 2.666667 4.00 2.5714286 3.695652 6.30 5.916667 
#6  6   2    14     1 Pasteur     7 5.333333 5.00 2.250 1.000000 3.00 0.8571429 4.347826 6.65 7.500000      


data<-HolzingerSwineford1939[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9")]   

matcor<-cor(data)

matcor 

#           x1          x2         x3        x4         x5        x6          x7           x8        x9    
#x1 1.00000000  0.29734551 0.44066800 0.3727063 0.29344369 0.3567702  0.06686392   0.22392677 0.3903404    
#x2 0.29734551  1.00000000 0.33984898 0.1529302 0.13938749 0.1925319 -0.07566892   0.09227923 0.2060406    
#x3 0.44066800  0.33984898 1.00000000 0.1586396 0.07719823 0.1976610  0.07193105   0.18601263 0.3286506    
#x4 0.37270627  0.15293019 0.15863957 1.0000000 0.73317017 0.7044802  0.17382912   0.10689838 0.2078483    
#x5 0.29344369  0.13938749 0.07719823 0.7331702 1.00000000 0.7199555  0.10204475   0.13866998 0.2274664    
#x6 0.35677019  0.19253190 0.19766102 0.7044802 0.71995554 1.0000000  0.12110170   0.14961132 0.2141617    
#x7 0.06686392 -0.07566892 0.07193105 0.1738291 0.10204475 0.1211017  1.00000000   0.48675793 0.3406457    
#x8 0.22392677  0.09227923 0.18601263 0.1068984 0.13866998 0.1496113  0.48675793   1.00000000 0.4490154    
#x9 0.39034041  0.20604057 0.32865061 0.2078483 0.22746642 0.2141617  0.34064572   0.44901545 1.0000000    
   

f1<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=data,1)
f2<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=data,2)
f3<-factanal(~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=data,3)

Chisq<-round(c(f1$STATISTIC,f2$STATISTIC,f3$STATISTIC),3)

df<-c(f1$dof,f2$dof,f3$dof)

pvalue<-round(c(f1$PVAL,f2$PVAL,f3$PVAL),4)  

Chisq 
#objective objective objective 
#  306.558   127.637    22.377 pvalue

df     
# 27 19 12  

pvalue

#objective objective objective 
#   0.0000    0.0000    0.0335        

loadings(f3)

#Loadings:
#   Factor1 Factor2 Factor3
#x1  0.277   0.623   0.151 
#x2  0.105   0.489         
#x3          0.663   0.130 
#x4  0.827   0.165         
#x5  0.861                 
#x6  0.801   0.212         
#x7                  0.696 
#x8          0.162   0.709 
#x9  0.132   0.406   0.524 

#               Factor1 Factor2 Factor3
#SS loadings      2.185   1.343   1.327
#Proportion Var   0.243   0.149   0.147
#Cumulative Var   0.243   0.392   0.539       

print(f3,cutoff=0.3)

#Call:
#factanal(x = ~x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, factors = 3,     data = data)

#Uniquenesses:
#   x1    x2    x3    x4    x5    x6    x7    x8    x9 
#0.513 0.749 0.543 0.279 0.243 0.305 0.502 0.469 0.543 

#Loadings:
#   Factor1 Factor2 Factor3
#x1          0.623         
#x2          0.489         
#x3          0.663         
#x4  0.827                 
#x5  0.861                 
#x6  0.801                 
#x7                  0.696 
#x8                  0.709 
#x9          0.406   0.524 

#               Factor1 Factor2 Factor3
#SS loadings      2.185   1.343   1.327
#Proportion Var   0.243   0.149   0.147
#Cumulative Var   0.243   0.392   0.539

#Test of the hypothesis that 3 factors are sufficient.
#The chi square statistic is 22.38 on 12 degrees of freedom.
#The p-value is 0.0335 
 
sum(loadings(f3)[,1]^2)   
#  2.184693      SS loadings Factor 1  

sum(loadings(f3)[,2]^2)  
# 1.342721     SS loadings Factor 2

sum(loadings(f3)[,3]^2)  
#1.327258       SS loadings Factor 3   

sum(loadings(f3)[,1]^2)/9   
#0.2427436        Proportion Var Factor 1

sum(loadings(f3)[,2]^2)/9   
#0.1491912       Proportion Var Factor 2

sum(loadings(f3)[,2]^2)/9 
# 0.1474731     Proportion Var Factor 3

comm<-rowSums(loadings(f3)^2)

   x1        x2        x3        x4        x5        x6        x7        x8        x9 
0.4874721 0.2512648 0.4572236 0.7208072 0.7571231 0.6947835 0.4977990 0.5314433 0.4567549   

percVar<-sum(comm)/9

# 0.5394079  Cumulative Var  Fact1+Fact2+Fact3

uniq=1-comm

round(uniq,3)

#  x1    x2    x3    x4    x5    x6    x7    x8    x9 
#0.513 0.749 0.543 0.279 0.243 0.305 0.502 0.469 0.543 

repcorr<-loadings(f3)%*%t(loadings(f3))
round(matcor-repcorr,3)

#      x1     x2     x3     x4     x5     x6     x7     x8     x9
#x1  0.513 -0.032 -0.001  0.026 -0.013 -0.011 -0.018  0.002  0.021
#x2 -0.032  0.749  0.015 -0.012  0.009  0.007 -0.031  0.027  0.007
#x3 -0.001  0.015  0.543  0.008 -0.021  0.018  0.026 -0.015 -0.013
#x4  0.026 -0.012  0.008  0.279 -0.002 -0.002  0.042 -0.032 -0.020
#x5 -0.013  0.009 -0.021 -0.002  0.243  0.004 -0.033  0.016  0.031
#x6 -0.011  0.007  0.018 -0.002  0.004  0.305  0.002  0.012 -0.024
#x7 -0.018 -0.031  0.026  0.042 -0.033  0.002  0.502  0.001 -0.006
#x8  0.002  0.027 -0.015 -0.032  0.016  0.012  0.001  0.469  0.005
#x9  0.021  0.007 -0.013 -0.020  0.031 -0.024 -0.006  0.005  0.543


library(GPArotation)

Varimax(loadings(f3))

#Orthogonal rotation method varimax converged.
#Loadings:
#   Factor1 Factor2 Factor3
#x1  0.3202  0.6066  0.1301
#x2  0.1353  0.4809 -0.0409
#x3  0.0795  0.6619  0.1133
#x4  0.8379  0.1131  0.0767
#x5  0.8667  0.0323  0.0703
#x6  0.8151  0.1617  0.0658
#x7  0.1019 -0.0624  0.6954
#x8  0.0776  0.1744  0.7036
#x9  0.1699  0.4088  0.5106

#Rotating matrix:
#       [,1]   [,2]    [,3]
#[1,] 0.9977 -0.065 -0.0212
#[2,] 0.0645  0.998 -0.0244
#[3,] 0.0227  0.023  0.9995

quartimax(loadings(f3))

#Orthogonal rotation method Quartimax converged.
#Loadings:
#   Factor1 Factor2 Factor3
#x1   0.353  0.5897  0.1244
#x2   0.158  0.4740 -0.0423
#x3   0.115  0.6566  0.1140
#x4   0.844  0.0719  0.0557
#x5   0.869 -0.0102  0.0483
#x6   0.823  0.1215  0.0456
#x7   0.116 -0.0710  0.6923
#x8   0.104  0.1666  0.7021
#x9   0.202  0.3974  0.5078

#Rotating matrix:
#       [,1]    [,2]    [,3]
#[1,] 0.9925 -0.1132 -0.0469
#[2,] 0.1123  0.9934 -0.0219
#[3,] 0.0491  0.0165  0.9987

oblimin(loadings(f3))

#Oblique rotation method Oblimin Quartimin converged.
#Loadings:
#   Factor1 Factor2  Factor3
#x1  0.1910  0.6020  0.03094
#x2  0.0437  0.5054 -0.11662
#x3 -0.0695  0.6893  0.02307
#x4  0.8405  0.0218  0.00531
#x5  0.8882 -0.0674  0.00756
#x6  0.8076  0.0775 -0.01093
#x7  0.0436 -0.1516  0.72310
#x8 -0.0327  0.1042  0.70150
#x9  0.0348  0.3661  0.46318

#Rotating matrix:
#       [,1]    [,2]    [,3]
#[1,]  1.055 -0.1764 -0.0836
#[2,] -0.141  1.0657 -0.1640
#[3,] -0.090 -0.0835  1.0329

#Phi:
#      [,1]  [,2]  [,3]
#[1,] 1.000 0.326 0.216
#[2,] 0.326 1.000 0.270
#[3,] 0.216 0.270 1.000


HS.model <- 'visual =~ x1 + x2 + x3
textual =~x4 + x5 + x6
speed =~x7 + x8 + x9'

fit <- cfa(HS.model, data=data)

summary(fit,fit.measures=TRUE)

#lavaan 0.6-12 ended normally after 35 iterations

#  Estimator                                         ML
 # Optimization method                           NLMINB
 # Number of model parameters                        21

 # Number of observations                           301

#Model Test User Model:
                                                      
 # Test statistic                                85.306
#  Degrees of freedom                                24
 # P-value (Chi-square)                           0.000

#Model Test Baseline Model:

 # Test statistic                               918.852
 # Degrees of freedom                                36
 # P-value                                        0.000

#User Model versus Baseline Model:

 # Comparative Fit Index (CFI)                    0.931
 # Tucker-Lewis Index (TLI)                       0.896

#Loglikelihood and Information Criteria:

#  Loglikelihood user model (H0)              -3737.745
 # Loglikelihood unrestricted model (H1)      -3695.092
                                                      
#  Akaike (AIC)                                7517.490
#  Bayesian (BIC)                              7595.339
#  Sample-size adjusted Bayesian (BIC)         7528.739

#Root Mean Square Error of Approximation:

 # RMSEA                                          0.092
#  90 Percent confidence interval - lower         0.071
#  90 Percent confidence interval - upper         0.114
#  P-value RMSEA <= 0.05                          0.001

#Standardized Root Mean Square Residual:

 # SRMR                                           0.065

#Parameter Estimates:

#  Standard errors                             Standard
#  Information                                 Expected
#  Information saturated (h1) model          Structured

#Latent Variables:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  visual =~                                           
#    x1                1.000                           
#    x2                0.554    0.100    5.554    0.000
#    x3                0.729    0.109    6.685    0.000
#  textual =~                                          
#    x4                1.000                           
#    x5                1.113    0.065   17.014    0.000
#    x6                0.926    0.055   16.703    0.000
#  speed =~                                            
#    x7                1.000                           
#    x8                1.180    0.165    7.152    0.000
#    x9                1.082    0.151    7.155    0.000

#Covariances:
 #                  Estimate  Std.Err  z-value  P(>|z|)
#  visual ~~                                           
#    textual           0.408    0.074    5.552    0.000
#    speed             0.262    0.056    4.660    0.000
#  textual ~~                                          
#    speed             0.173    0.049    3.518    0.000

#Variances:
#                   Estimate  Std.Err  z-value  P(>|z|)
#   .x1                0.549    0.114    4.833    0.000
#   .x2                1.134    0.102   11.146    0.000
#   .x3                0.844    0.091    9.317    0.000
#   .x4                0.371    0.048    7.779    0.000
#   .x5                0.446    0.058    7.642    0.000
#   .x6                0.356    0.043    8.277    0.000
#   .x7                0.799    0.081    9.823    0.000
#   .x8                0.488    0.074    6.573    0.000
#   .x9                0.566    0.071    8.003    0.000
#    visual            0.809    0.145    5.564    0.000
#    textual           0.979    0.112    8.737    0.000
#    speed             0.384    0.086    4.451    0.000

HS.model1 <- 'visual =~ x1 + x2 + x3+ x9
textual =~x4 + x5 + x6
speed =~x7 + x8 + x9'

fit1 <- cfa(HS.model1, data=data)

summary(fit1,fit.measures=TRUE)

#lavaan 0.6-12 ended normally after 34 iterations

#  Estimator                                         ML
#  Optimization method                           NLMINB
#  Number of model parameters                        22

 # Number of observations                           301

#Model Test User Model:
                                                      
#  Test statistic                                52.382
#  Degrees of freedom                                23
#  P-value (Chi-square)                           0.000

#Model Test Baseline Model:

#  Test statistic                               918.852
#  Degrees of freedom                                36
#  P-value                                        0.000

#User Model versus Baseline Model:

#  Comparative Fit Index (CFI)                    0.967
#  Tucker-Lewis Index (TLI)                       0.948

#Loglikelihood and Information Criteria:

 # Loglikelihood user model (H0)              -3721.283
 # Loglikelihood unrestricted model (H1)      -3695.092
                                                      
 # Akaike (AIC)                                7486.566
 # Bayesian (BIC)                              7568.123
 # Sample-size adjusted Bayesian (BIC)         7498.351

#Root Mean Square Error of Approximation:

#  RMSEA                                          0.065
#  90 Percent confidence interval - lower         0.042
#  90 Percent confidence interval - upper         0.089
#  P-value RMSEA <= 0.05                          0.133

#Standardized Root Mean Square Residual:

#  SRMR                                           0.045

#Parameter Estimates:

#  Standard errors                             Standard
#  Information                                 Expected
#  Information saturated (h1) model          Structured

#Latent Variables:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  visual =~                                           
#    x1                1.000                           
#    x2                0.578    0.098    5.918    0.000
#    x3                0.754    0.103    7.291    0.000
#    x9                0.437    0.081    5.367    0.000
#  textual =~                                          
#    x4                1.000                           
#    x5                1.115    0.066   17.016    0.000
#    x6                0.926    0.056   16.685    0.000
#  speed =~                                            
#    x7                1.000                           
#    x8                1.207    0.185    6.540    0.000
#    x9                0.675    0.112    6.037    0.000

#Covariances:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  visual ~~                                           
#    textual           0.396    0.072    5.506    0.000
#    speed             0.177    0.055    3.239    0.001
#  textual ~~                                          
#    speed             0.136    0.051    2.675    0.007#

#Variances:
#                   Estimate  Std.Err  z-value  P(>|z|)
#   .x1                0.576    0.100    5.731    0.000
#   .x2                1.120    0.100   11.153    0.000
#   .x3                0.830    0.087    9.515    0.000
#   .x9                0.558    0.060    9.336    0.000
#   .x4                0.373    0.048    7.800    0.000
#   .x5                0.444    0.058    7.602    0.000
#   .x6                0.357    0.043    8.285    0.000
#   .x7                0.740    0.086    8.595    0.000
#   .x8                0.375    0.094    3.973    0.000
#    visual            0.783    0.134    5.842    0.000
#    textual           0.978    0.112    8.728    0.000
#    speed             0.444    0.097    4.567    0.000


fit1 <- cfa(HS.model1, data = HolzingerSwineford1939, std.lv = TRUE)

summary(fit1,fit.measures=TRUE)
#lavaan 0.6-12 ended normally after 20 iterations

#  Estimator                                         ML
#  Optimization method                           NLMINB
#  Number of model parameters                        22

#  Number of observations                           301

#Model Test User Model:
                                                      
#  Test statistic                                52.382
#  Degrees of freedom                                23
#  P-value (Chi-square)                           0.000

#Model Test Baseline Model:

#  Test statistic                               918.852
#  Degrees of freedom                                36
#  P-value                                        0.000
#
#User Model versus Baseline Model:

 # Comparative Fit Index (CFI)                    0.967
 # Tucker-Lewis Index (TLI)                       0.948

#Loglikelihood and Information Criteria:

 # Loglikelihood user model (H0)              -3721.283
 # Loglikelihood unrestricted model (H1)      -3695.092
                                                      
 # Akaike (AIC)                                7486.566
 # Bayesian (BIC)                              7568.123
 # Sample-size adjusted Bayesian (BIC)         7498.351

#Root Mean Square Error of Approximation:

 # RMSEA                                          0.065
 # 90 Percent confidence interval - lower         0.042
 # 90 Percent confidence interval - upper         0.089
 # P-value RMSEA <= 0.05                          0.133

#Standardized Root Mean Square Residual:

 # SRMR                                           0.045

#Parameter Estimates:

 # Standard errors                             Standard
 # Information                                 Expected
  #Information saturated (h1) model          Structured

#Latent Variables:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  visual =~                                           
#    x1                0.885    0.076   11.685    0.000
#    x2                0.511    0.076    6.698    0.000
#    x3                0.667    0.072    9.241    0.000
#    x9                0.387    0.064    6.034    0.000
#  textual =~                                          
#    x4                0.989    0.057   17.455    0.000
#    x5                1.103    0.063   17.602    0.000
#    x6                0.916    0.054   17.073    0.000
#  speed =~                                            
#    x7                0.666    0.073    9.134    0.000
#    x8                0.804    0.074   10.934    0.000
#    x9                0.450    0.066    6.824    0.000

#Covariances:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  visual ~~                                           
#    textual           0.453    0.062    7.242    0.000
#    speed             0.301    0.080    3.763    0.000
#  textual ~~                                          
#    speed             0.206    0.070    2.937    0.003

#Variances:
#                   Estimate  Std.Err  z-value  P(>|z|)
#   .x1                0.576    0.100    5.731    0.000
#   .x2                1.120    0.100   11.153    0.000
#   .x3                0.830    0.087    9.515    0.000
#   .x9                0.558    0.060    9.336    0.000
#   .x4                0.373    0.048    7.800    0.000
#   .x5                0.444    0.058    7.602    0.000
#   .x6                0.357    0.043    8.285    0.000
#   .x7                0.740    0.086    8.595    0.000
 #  .x8                0.375    0.094    3.973    0.000
 #   visual            1.000                           
 #   textual           1.000                           
 #   speed             1.000                         
 
 fit.HS.ortho <- cfa(HS.model1, data = HolzingerSwineford1939, orthogonal=TRUE)
 summary(fit.HS.ortho,fit.measure=TRUE)
#lavaan 0.6-12 ended normally after 31 iterations

#  Estimator                                         ML
#  Optimization method                           NLMINB
#  Number of model parameters                        19

#  Number of observations                           301

#Model Test User Model:
                                                      
#  Test statistic                               106.240
#  Degrees of freedom                                26
#  P-value (Chi-square)                           0.000

#Model Test Baseline Model:

 # Test statistic                               918.852
 # Degrees of freedom                                36
 # P-value                                        0.000

#User Model versus Baseline Model:

 # Comparative Fit Index (CFI)                    0.909
 # Tucker-Lewis Index (TLI)                       0.874

#Loglikelihood and Information Criteria:

 # Loglikelihood user model (H0)              -3748.212
 # Loglikelihood unrestricted model (H1)      -3695.092
                                                      
 # Akaike (AIC)                                7534.424
 # Bayesian (BIC)                              7604.860
 # Sample-size adjusted Bayesian (BIC)         7544.602

#Root Mean Square Error of Approximation:

 # RMSEA                                          0.101
 # 90 Percent confidence interval - lower         0.082
 # 90 Percent confidence interval - upper         0.122
 # P-value RMSEA <= 0.05                          0.000

#Standardized Root Mean Square Residual:

 # SRMR                                           0.140

#Parameter Estimates:

 # Standard errors                             Standard
 # Information                                 Expected
 # Information saturated (h1) model          Structured

#Latent Variables:
 #                  Estimate  Std.Err  z-value  P(>|z|)
 # visual =~                                           
 #   x1                1.000                           
 #   x2                0.704    0.120    5.885    0.000
 #   x3                0.940    0.140    6.709    0.000
 #   x9                0.554    0.092    6.022    0.000
 # textual =~                                          
 #   x4                1.000                           
 #   x5                1.133    0.067   16.906    0.000
 #   x6                0.924    0.056   16.391    0.000
 # speed =~                                            
 #   x7                1.000                           
 #   x8                1.056    0.164    6.431    0.000
 #   x9                0.697    0.104    6.679    0.000

#Covariances:
 #                  Estimate  Std.Err  z-value  P(>|z|)
 # visual ~~                                           
 #   textual           0.000                           
 #   speed             0.000                           
 # textual ~~                                          
 #   speed             0.000                           

#Variances:
#                   Estimate  Std.Err  z-value  P(>|z|)
#   .x1                0.737    0.104    7.113    0.000
#   .x2                1.073    0.101   10.604    0.000
#   .x3                0.726    0.096    7.594    0.000
#   .x9                0.525    0.063    8.359    0.000
#   .x4                0.382    0.049    7.805    0.000
#   .x5                0.416    0.059    7.038    0.000
#   .x6                0.369    0.044    8.367    0.000
#   .x7                0.676    0.091    7.404    0.000
#   .x8                0.457    0.089    5.113    0.000
#    visual            0.621    0.126    4.946    0.000
#    textual           0.969    0.112    8.640    0.000
#    speed             0.507    0.108    4.713    0.000
