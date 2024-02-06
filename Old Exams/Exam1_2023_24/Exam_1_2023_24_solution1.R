##LAB

lat <- read.csv("lat.dat", sep = " ")
nrow(unique(lat))

#How many are the possible response patterns? And the obs ones?



#What is the proportion of "right" for each item?

nrow(lat[lat$Writi == 1,])/nrow(lat)
nrow(lat[lat$Throw == 1,])/nrow(lat)
nrow(lat[lat$Hamme == 1,])/nrow(lat)
nrow(lat[lat$Washi == 1,])/nrow(lat)
nrow(lat[lat$Combi == 1,])/nrow(lat)
nrow(lat[lat$Lifti == 1,])/nrow(lat)

#does is makes sense to perform a LCA? Why?

#Fit the poLCA with 2,3,4,5 latent classes

formula <- cbind(Writi, Throw, Hamme, Washi, Combi, Lifti)~1

m.2<-poLCA(formula,lat,nclass=2,nrep=10,verbose=FALSE)
m.3<-poLCA(formula,lat,nclass=3,nrep=10,verbose=FALSE)
m.4<-poLCA(formula,lat,nclass=4,nrep=10,verbose=FALSE)
m.5<-poLCA(formula,lat,nclass=5,nrep=10,verbose=FALSE)

#Choose the best model

K<-c("2", "3", "4", "5")
llik<-c(m.2$llik,m.3$llik,m.4$llik, m.5$llik)
npar<-c(m.2$npar,m.3$npar,m.4$npar, m.5$npar)
Gsq<-round(c(m.2$Gsq,m.3$Gsq,m.4$Gsq, m.5$Gsq),3)
Chisq<-round(c(m.2$Chisq,m.3$Chisq,m.4$Chisq, m.5$Gsq),3)
df<-c(m.2$resid.df,m.3$resid.df,m.4$resid.df,m.5$resid.df )
p_value<-round(1-pchisq(Chisq,df ),4)
AIC<-round(c(m.2$aic,m.3$aic,m.4$aic, m.5$bic),3)
BIC<-round(c(m.2$bic,m.3$bic,m.4$bic, m.5$bic),3)
summary<-data.frame(K,llik,npar,Gsq,Chisq,df,p_value,AIC,BIC)
summary

#Write the LCA model analytically and interpret the chosen solution

.......

#poLCA.reorder (not requested)

probs.start.m3<-m.3$probs.start
new.probs.start.m3<-poLCA.reorder(probs.start.m3,order(m.3$P))
m.3.ord<-poLCA(formula,lat,nclass=3,probs.start=new.probs.start.m3
               ,verbose=FALSE)
round(m.3.ord$P,4)

#compute the probability of using "right" to all the items

nrow(lat[lat$Writi==1 & lat$Throw == 1 & lat$Hamme == 1 & lat$Washi ==1 & lat$Combi == 1 & lat$Lifti == 1,])/nrow(lat)

#compute the posterior probability estimates of using "left" to all items for the latent classes. To which class is 
#this response pattern allocated?


post.16<-m.3.ord$posterior[16,]
round(post.16,5)

#display the latent class in which the individuals are allocated accoedring to the highest posterior probability

m.3.ord$predclass
table(m.3.ord$predclass)

##THEORY

#Illustrate the specification of the latent variable model for binary data with the Underlying Variable approach

#In the normal linear factor model derive the cov bw the components and the factors