
#1
library(ltm)
data("LSAT")
?LSAT
dim(LSAT)

#2
dsc <- descript(LSAT)
dsc$perc
dsc$items
dsc$pw.ass
plot(dsc)

#3
?rasch
m1 <- rasch(LSAT, IRT.param = TRUE, constraint = cbind(ncol(LSAT) + 1, 1))
m1.rip <- rasch(LSAT, IRT.param = FALSE, constraint = cbind(ncol(LSAT)+ 1, 1))

#3.1
summary(m1.rip)

#3.2
coef(m1.rip, prob = TRUE, order = TRUE)

#3.3
pval.boot <- GoF.rasch(m1, B = 199, seed = 221019)
pval.boot$Tobs
pval.boot

#3.4
margins(m1)
margins(m1, type = "three-way", nprint = 2)
margins(m1, type = "three-way", nprint = 3)

#4
m2 <- rasch(LSAT)
m2.rip <- rasch(LSAT,IRT.param=FALSE)
#4.1
summary(m2.rip)
#4.2
summary(m2)$coefficients[1,1]*summary(m2)$coefficients[6,1]
summary(m2.rip)$coefficients[1,1]
#4.3
anova(m1,m2)
#4.4
margins(m2)
margins(m2, type = "three-way", nprint = 2)

#5
m3 <- ltm(LSAT ~ z1)
m3.rip <- ltm(LSAT ~ z1, IRT.param = FALSE)
#5.1
summary(m3.rip)
#5.2
anova(m2,m3)

#6
plot(m2, legend = TRUE, cx = "bottomright", lwd = 3, cex.main = 1.5,
cex.lab = 1.3, cex = 1.1)

#7
fs<-factor.scores(m2, method="EAP")
fs
plot(fs$score.dat$z1)
resp.pattern <- fs$score.dat[,1:5]
total.score <- apply(resp.pattern,1,sum)
total.score

#8
factor.scores(m2,resp.pattern=rbind(c(0,1,1,0,0), c(0,1,0,1,0)))
