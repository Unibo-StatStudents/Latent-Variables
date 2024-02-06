# Latent Variables Lab - 12/12/2023


#1 ####
library(ltm)
data("LSAT")
?LSAT
dim(LSAT)


#2 ####

dsc <- descript(LSAT)

dsc$perc
# item3 is the most difficult item; it is the one with the highest number of 
# wrong answers

dsc$items
# huge number of people who responded correctly

dsc$pw.ass
# not all items are associated (1,5) (1,4) (3,5)
# will later be used for goodness-of-fit

plot(dsc)
# item 1 is the one with the highest proportion of correct responses for each 
# value of total score; item 3 is the one with the lowest proportions..


#3 ####
?rasch
m1 <- rasch(LSAT, IRT.param = TRUE, constraint = cbind(ncol(LSAT) + 1, 1))
# -> this is how we fit the contrainst; note: They don't have to be 1, just 
# equal
# only the difficulty parameters; the lowest parameter corresponds to the
# easiest; the easiest is item 1, the most difficult is item 3

m1.rip <- rasch(LSAT, IRT.param = FALSE, constraint = cbind(ncol(LSAT)+ 1, 1))
# -> parametrized in the other way: IRT.param = FALSE


#3.1 #### (see above)
summary(m1)
summary(m1.rip)


#3.2 ####
coef(m1.rip, prob = TRUE, order = TRUE)
# -> prob(median individual responds 1 (=correctly) to item 3)
# -> 


#3.3 ####
# use this when chi-squared and LR test don't hold asymptotically
pval.boot <- GoF.rasch(m1, B = 199, seed = 221019)
pval.boot$Tobs # empirical value of the Pearson chi-squared distribution
pval.boot # p-value: 0.26 -> we do not reject the H0
# asymptotic p-value could also be asked here


#3.4 ####
margins(m1) # two-way
margins(m1, type = "three-way", nprint = 2)
# looks bad with 000: so we also run it with nprint=3:

margins(m1, type = "three-way", nprint = 3)
# association between the two triplets from above can still not be explained by 
# the model
# -> all lower than 4 -> good


# margins can generally be used to check if there are problems in the
# association; good alternative if we can't use chi-squared

#4 ####
m2 <- rasch(LSAT)# LRT parametrization
m2.rip <- rasch(LSAT,IRT.param=FALSE) # GLLVM parametrization
# -> again with two parametrizations (no matter if constrained or unconstrained)


#4.1 ####
summary(m2) # unconstrained

# item 3 is the most difficult, item 1 is the easiest
summary(m1) # constrained
# item 1 easiest

summary(m1.rip)
# -> item 3 is the easiest, then items 3, 2, 4, 5, 1

summary(m2.rip)
# -> same: item 3 is the most difficult; then item 2, 4, 5, 1


#4.2 ####
-summary(m2)$coefficients[1,1]*summary(m2)$coefficients[6,1]
summary(m2.rip)$coefficients[1,1]
# -> we can go from one parametrization to the other
# just with negative sign, multiplied by discrimination parameter


#4.3 ####
anova(m1,m2)
# -> reject the hypothesis; model 1 is the null hypothesis; model 2 is the 
# alternative
# m2 is better! meaning: all the discriminant parameters are not all equal to 1
# and model 2 is better
# this could also be seen from AIC and BIC


#4.4 ####
margins(m2)
# -> no problems in the 2way margin

margins(m2, type = "three-way", nprint = 2)
# -> no problem in the 3way margin

# AIC/BIC was already checked in 4.3


#5 ####
m3 <- ltm(LSAT ~ z1)
m3.rip <- ltm(LSAT ~ z1, IRT.param = FALSE)
# -> all significant (z values > 3)

#5.1 ####
summary(m3)
# compare to:
summary(m3.rip)
# parameters are the same; can be interpreted as loadings; first 3 items have a 
# higher loading than the last 2; first 3 are discriminatory


#5.2 ####
anova(m2,m3)
# -> now we accept the H0: discriminant parameters are not significantly 
# different
# m2 is better than m3; also confirmed by AICBIC
# -> no significant difference between the discriminant parameters between the 
# items


#6 ####
plot(m2, legend = TRUE, cx = "bottomright", lwd = 3, cex.main = 1.5,
     cex.lab = 1.3, cex = 1.1)
# -> curves are parallel because they have the same discriminatory parameters
# item 1 is the easiest, item 3 is the most difficult


#7 ####
fs<-factor.scores(m2, method="EAP")
fs
plot(fs$score.dat$z1)
resp.pattern <- fs$score.dat[,1:5]
total.score <- apply(resp.pattern,1,sum)
total.score
round(fs$score.dat[order(total.score),],3)
# -> if we check the entire data sets there are lots of frequencies lower than 5
# thatswhy is good to check the model with the margins
# lowest score: 00000
# highest score: 11111
# latent variable represents the ability of the student
# the total score: if we order the table according to the total score
# those who answer correctly to item ???


#8 ####
factor.scores(m2,resp.pattern=rbind(c(0,1,1,0,0), c(0,1,0,1,0)))
# -> 5^2 = 32 items, but we observe only 30; the ones above are the two missing
# repsonse patterns