
library(lavaan)

data<-read.table("scieR.txt",header=T)

#Model 1factor

model.f1<-'f1=~x1+x3+x4+x7'

fit<-cfa(model.f1,data= data[,c(1,3,4,7)], ordered=c("x1","x3","x4","x7"), std.lv = TRUE)
summary(fit,fit.measures=TRUE)


#Model 2factors

model.f2<-'f1=~x1+x3+x4+x7
f2=~x2+x5+x6'


fit2<-cfa(model.f2,data= data[,c(1,2,3,4,5,6,7)], ordered=c("x1","x2","x3","x4","x5","x6","x7"), std.lv = TRUE)

summary(fit2,fit.measures=TRUE)
