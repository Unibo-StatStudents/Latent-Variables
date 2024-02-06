#Latent class analysis on carcinoma data set

1. 

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

2.

?poLCA

formula<-cbind(A,B,C,D,E,F,G)~1

m.2<-poLCA(formula,carcinoma,nclass=2,nrep=10,verbose=FALSE)
m.3<-poLCA(formula,carcinoma,nclass=3,nrep=10,verbose=FALSE)
m.4<-poLCA(formula,carcinoma,nclass=4,nrep=10,verbose=FALSE)

3.

freq.estim<-data.frame(m.2$predcell[1:9],m.3$predcell[9],m.4$predcell[9])
freq.estim

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

4.

K<-c("2", "3", "4")
llik<-c(m.2$llik,m.3$llik,m.4$llik)
npar<-c(m.2$npar,m.3$npar,m.4$npar)
Gsq<-round(c(m.2$Gsq,m.3$Gsq,m.4$Gsq),3)
Chisq<-round(c(m.2$Chisq,m.3$Chisq,m.4$Chisq),3)
df<-c(m.2$resid.df,m.3$resid.df,m.4$resid.df)
pvalue<-round(1-pchisq(Chisq,df),4)
AIC<-round(c(m.2$aic,m.3$aic,m.4$aic),3)
BIC<-round(c(m.2$bic,m.3$bic,m.4$bic),3)
summary<-data.frame(K,llik,npar,Gsq,Chisq,df,pvalue,AIC,BIC)
summary

#  K      llik npar    Gsq  Chisq  df pvalue     AIC     BIC
#1 2 -317.2568   15 62.365 92.648 103 0.7581 664.514 706.074
#2 3 -293.7050   23 15.262 20.503  95 1.0000 633.410 697.136
#3 4 -289.2858   31  6.423 10.086  87 1.0000 640.572 726.463


7. 

round(m.3$P,4)

# 0.3736 0.4447 0.1817

lapply(m.3$probs,round,4)

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

8.

probs.start.m3<-m.3$probs.start

new.probs.start.m3<-poLCA.reorder(probs.start.m3,order(m.3$P))

m.3.ord<-poLCA(formula,carcinoma,nclass=3,probs.start=new.probs.start.m3,verbose=FALSE)
round(m.3.ord$P,4)

# 0.1817 0.3736 0.4447

9.

lapply(m.3.ord$probs,round,4)

#$A
#           Pr(1)  Pr(2)
#class 1:  0.4872 0.5128
#class 2:  0.9427 0.0573
#class 3:  0.0000 1.0000

#$B
#           Pr(1)  Pr(2)
#class 1:  0.0000 1.0000
#class 2:  0.8621 0.1379
#class 3:  0.0191 0.9809

#$C
           Pr(1)  Pr(2)
#class 1:  1.0000 0.0000
#class 2:  1.0000 0.0000
#class 3:  0.1425 0.8575

#$D
#           Pr(1)  Pr(2)
#class 1:  0.9424 0.0576
#class 2:  1.0000 0.0000
#class 3:  0.4138 0.5862

#$E
#           Pr(1)  Pr(2)
#class 1:  0.2494 0.7506
#class 2:  0.9449 0.0551
#class 3:  0.0000 1.0000

#$F
#           Pr(1)  Pr(2)
#class 1:  1.0000 0.0000
#class 2:  1.0000 0.0000
#class 3:  0.5236 0.4764

#$G
#           Pr(1)  Pr(2)
#class 1:  0.3693 0.6307
#class 2:  1.0000 0.0000
#class 3:  0.0000 1.0000


10.

p.dnc1<-m.3$P[1]*m.3$probs$A[1,1]*m.3$probs$B[1,1]*m.3$probs$C[1,1]*m.3$probs$D[1,1]*m.3$probs$E[1,1]*m.3$probs$F[1,1]*m.3$probs$G[1,1]
p.dnc2<-m.3$P[2]*m.3$probs$A[2,1]*m.3$probs$B[2,1]*m.3$probs$C[2,1]*m.3$probs$D[2,1]*m.3$probs$E[2,1]*m.3$probs$F[2,1]*m.3$probs$G[2,1]
p.dnc3<-m.3$P[3]*m.3$probs$A[3,1]*m.3$probs$B[3,1]*m.3$probs$C[3,1]*m.3$probs$D[3,1]*m.3$probs$E[3,1]*m.3$probs$F[3,1]*m.3$probs$G[3,1]
p.dn<-p.dnc1+p.dnc2+p.dnc3
round(p.dn,4)
# 0.2869


11.

round(p.dn*m.3$N,4)
# 33.8491

12.

post.1<-m.3.ord$posterior[1,]
round(post.1,5)
#0 1 0

13.

m.3.ord$predclass
# 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 2 2 3 1 1 1 1 1 1 1 1 1 1 1 1 3 1 3 3 3 3 3
# 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# 3 3 3 3 3 3 3

table(m.3.ord$predclass)
#1  2  3 
#23 44 51 

14.
sel<-carcinoma[m.3.ord$predclass==1,]
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



