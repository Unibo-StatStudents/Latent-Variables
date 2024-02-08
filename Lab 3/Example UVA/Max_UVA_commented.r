# PART B)

setwd('C:\\Users\\maxim\\Documents\\Unibo\\3_Latent Variables_85177\\Lab3\\Example UVA')
library(lavaan)

# slide 20/21: 2-class model showed a good fit 
# class 1: technology
# class 2: social

# Now, taking advantage of this solution (knowing which items relate to which 
# class), and try the underlying variable approach, and show it it relates to 
data = read.table("Lab 3/Example UVA/scie7i.dat", header=F)


# 1.) Model 1 factor ####
model.f1 = 'f1=~V1+V3+V4+V7'
fit = cfa(model.f1,data= data[,c(1,3,4,7)],
          ordered=c("V1","V3","V4","V7"), std.lv = TRUE) 
# ordered: declare them ordinal for lavaan

summary(fit, fit.measures=TRUE)

# multiply by 1.8 to get to the other models
# looking at thresholds


# 2.) Model 2 factor ####
model.f2 = 'f1=~V1+V3+V4+V7
f2=~V2+V5+V6'
fit2 = cfa(model.f2,data= data[,c(1,2,3,4,5,6,7)],
           ordered=c("V1","V2","V3","V4","V5","V6","V7"), std.lv = TRUE)
summary(fit2,fit.measures=TRUE)
# Two different solution for standard and scaled -> we can choose
# RMSEA is on the threshold, much better than 1-class model
# multiply each loading by 1.8
# thresholds: 'Covariances' shows the factors are not significantly correlated
# 