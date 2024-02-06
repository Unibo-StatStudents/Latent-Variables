setwd('C:\\Users\\maxim\\Desktop\\exam')
load('lat.dat')
lat = read.table('lat.dat', header=TRUE)

dim(lat)
head(lat)
library(poLCA)
table(lat)

formula = cbind(Writi,Throw,Hamme,Washi,Combi,Lifti)~1
lca2 = poLCA(formula,lat,nclass=2,nrep=10,verbose=FALSE)
lca3 = poLCA(formula,lat,nclass=3,nrep=10,verbose=FALSE)
lca4 = poLCA(formula,lat,nclass=4,nrep=10,verbose=FALSE)
lca5 = poLCA(formula,lat,nclass=5,nrep=10,verbose=FALSE)

lca2$predcell
round(lca1$P, 4)
lapply(lca1$probs, round, 3)

dsc = descript(lat, chi.squared=TRUE)
library(ltm)
dsc$perc

# Compare estimated frequencies of the response patterns
est_freq = data.frame(lca2$predcell[1:8], lca3$predcell[8], lca4$predcell[8], lca5$predcell[8])
est_freq

K = c('2', '3', '4', '5')
llike = c(lca2$llik, lca3$llik, lca4$llik,lca5$llik)
npar = c(lca2$npar, lca3$npar, lca4$npar, lca5$npar)
LR = round(c(lca2$Gsq, lca3$Gsq, lca4$Gsq, lca5$Gsq), 3)
Chisq = round(c(lca2$Chisq, lca3$Chisq, lca4$Chisq, lca5$Chisq), 3)
DoF = c(lca2$resid.df, lca3$resid.df, lca4$resid.df, lca5$resid.df)
pval = round(1 - pchisq(Chisq, DoF), 4)
AIC = round(c(lca2$aic, lca3$aic, lca4$aic,lca5$aic), 3)
BIC = round(c(lca2$bic, lca3$bic, lca4$bic,lca5$bic), 3)
table = data.frame(K, llike, npar, LR, Chisq, DoF, pval, AIC, BIC)
table

lca3$predcell
round(lca3$P, 4)
lapply(lca3$probs, round, 3)
probs_start = lca3$probs.start
new_probs_start = poLCA.reorder(probs_start, order(lca3$P))
lca3_ord = poLCA(formula=formula, data=lat, nclass=3, 
                 probs.start=new_probs_start, verbose=FALSE)
round(lca3_ord$P, 4)

lapply(lca3_ord$probs, round, 3)
lca3_ord$posterior

# lca3$probs$A[1,1] = probability of the first response pattern associated to the first class
p.dnc1 = lca3$P[1]*lca3$probs$Writi[1,1]*lca3$probs$Throw[1,1]*lca3$probs$Hamme[1,1]*lca3$probs$Washi[1,1]*lca3$probs$Combi[1,1]*lca3$probs$Lifti[1,1]

# lca3$probs$A[2,1] = probability of the first response pattern associated to the second class
p.dnc2 = lca3$P[2]*lca3$probs$Writi[2,1]*lca3$probs$Throw[2,1]*lca3$probs$Hamme[2,1]*lca3$probs$Washi[2,1]*lca3$probs$Combi[2,1]*lca3$probs$Lifti[2,1]

# lca3$probs$A[3,1] = probability of the first response pattern associated to the third class
p.dnc3 = lca3$P[3]*lca3$probs$Writi[3,1]*lca3$probs$Throw[3,1]*lca3$probs$Hamme[3,1]*lca3$probs$Washi[3,1]*lca3$probs$Combi[3,1]*lca3$probs$Lifti[3,1]

# Summing over:
p.dn = p.dnc1 + p.dnc2 + p.dnc3
round(p.dn,4) # Result:


# lca3$probs$A[1,1] = probability of the first response pattern associated to the first class
p.dnc1 = lca3_ord$P[1]*lca3_ord$probs$Writi[1,1]*lca3_ord$probs$Throw[1,1]*lca3_ord$probs$Hamme[1,1]*lca3_ord$probs$Washi[1,1]*lca3_ord$probs$Combi[1,1]*lca3_ord$probs$Lifti[1,1]

# lca3$probs$A[2,1] = probability of the first response pattern associated to the second class
p.dnc2 = lca3_ord$P[2]*lca3_ord$probs$Writi[2,1]*lca3_ord$probs$Throw[2,1]*lca3_ord$probs$Hamme[2,1]*lca3_ord$probs$Washi[2,1]*lca3_ord$probs$Combi[2,1]*lca3_ord$probs$Lifti[2,1]

# lca3$probs$A[3,1] = probability of the first response pattern associated to the third class
p.dnc3 = lca3_ord$P[3]*lca3_ord$probs$Writi[3,1]*lca3_ord$probs$Throw[3,1]*lca3_ord$probs$Hamme[3,1]*lca3_ord$probs$Washi[3,1]*lca3_ord$probs$Combi[3,1]*lca3_ord$probs$Lifti[3,1]

# Summing over:
p.dn = p.dnc1 + p.dnc2 + p.dnc3
round(p.dn,4) # Result: 0.2869

# (2, 2, 2, 2, 2, 2, 2, 2)
b = as.numeric(row.names(lat[which(lat$Writi == 2 & lat$Throw == 2 & 
                                     lat$Hamme == 2 & lat$Washi == 2 &
                                     lat$Combi == 2 & lat$Lifti == 2),]))
est_posterior_b = round(lca3_ord$posterior[b[1], ], 5)

posterior_est = round(lca3_ord$posterior[5, ], 5)
table(lca3_ord$predclass)
