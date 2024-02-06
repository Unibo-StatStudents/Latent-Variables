#1
library(ltm)
?Mobility
str(Mobility)


#2
names(Mobility)<-c("Item1","Item2","Item3","Item4",
                   "Item5","Item6","Item7","Item8")
Mobility[Mobility==1]<-2
Mobility[Mobility==0]<-1
# this is because poLCA codes from 1 onwards (0 does not mean anything)

#New values: 2 yes 1 no


#3.
# Use poLCA to fit 2-, 3-, 4-latent class models
?poLCA
library(poLCA)
formula<-cbind(Item1,Item2,Item3,Item4,Item5,Item6,Item7,Item8)~ 1
m.2<-poLCA(formula,Mobility,nclass=2,nrep=10,verbose=FALSE)
m.3<-poLCA(formula,Mobility,nclass=3,nrep=10,verbose=FALSE)
m.4<-poLCA(formula,Mobility,nclass=4,nrep=10,verbose=FALSE)


#4.
freq.estim<-data.frame(m.2$predcell[1:10],m.3$predcell[10],m.4$predcell[10])
freq.estim


#5. 
# cannot rely on Chi-squared here; we use AIC/BIC and decide for the 4-class 
# solution
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


#6.
# answer: 4-class model
round(m.4$P,4)
lapply(m.4$probs,round,4)


#7.
# each one of us will have a different order of these probabilities in 
# round(m.4$P,4) (->indeterminacy of the latent class)
?poLCA.reorder # helper function
probs.start.m4<-m.4$probs.start # starting probability used in model m.4
new.probs.start.m4<-poLCA.reorder(probs.start.m4,order(m.4$P)) # we reorder the 
# probabilities according to the probabilities of belonging to each class
m.4.ord<-poLCA(formula,Mobility,nclass=4,probs.start=new.probs.start.m4,verbose=FALSE)
# For Item 1, 2, 3, 4, 5, 6, 7, 8, how do the different classes respond?
# all girls belonging to class 1 have a higher probability of responding yes (2)
# to item 1

#9.
# class 1: has a high probability for positive answers (2) in the items; class of 
# women with the highest mobility
# class 2: positive answer in item 1,2,3,4; negative answers in 5,6,7,8; they 
# don't have a huge mobility; they only did half of the things on the list 
# (medium-high class)
# class 3: high values for 1; a lot of negative answers; lowest mobility
# class 4: more mobility in class 3 but less than 2 and 1 -> medium-low class
#sizes of each latent class;
round(m.4.ord$P,4)
#estimated class-conditional response probabilities.
lapply(m.4.ord$probs,round,4)


#10.
#posterior: matrix of posterior class membership probabilities
a<-as.numeric(row.names(Mobility[which(Mobility$Item1==1&Mobility$Item2==1&Mobility$Item3==1&Mobility$Item4==1&Mobility$Item5==1&Mobility$Item6==1&Mobility$Item7==1&Mobility$Item8==1),]))
# a = object that includes all the observations that have a 
# response pattern = ALL NO

post.1<-m.4.ord$posterior[a[1],] # for the first person
round(post.1,5) # such a person will probabily belong to the third class

b<-as.numeric(row.names(Mobility[which(Mobility$Item1==2&Mobility$Item2==2&Mobility$Item3==2&Mobility$Item4==2&Mobility$Item5==2&Mobility$Item6==2&Mobility$Item7==2&Mobility$Item8==2),]))
# b = ALL YES
post.2<-m.4.ord$posterior[b[1],] # such a person is probably in the first class
round(post.2,5)

#11.
m.4.ord$predclass # class vector
table(m.4.ord$predclass) # if we table this value, we can see that the majority 
# of women are in class 4 (medium-low mobility)


#12.
cl3<-Mobility[m.4.ord$predclass==3,] # take only the rows of observations 
# belonging to class 3
unique(cl3) # we get the unique response patterns clasified in class 3


#13.
?poLCA.predcell # calculates the predicted cell percentages of a latent class 
# model
poLCA.predcell(m.4.ord,c(2,1,2,1,2,1,2,1)) # the predicted percentage of this 
# response pattern is almost zero (it is chosen to be contradictory; the girl 
# can go out, but cannot go shopping for example)
poLCA.posterior(m.4.ord,y=c(2,1,2,1,2,1,2,1)) # this would probably go to group 2

# In order to know which are the not observed pattern, if there are many, we can 
# use as command 
