

library(ltm)
cheating <- read.table("cheating.txt")

#1.
#a.
2^4

#b
?descript
dsc <- descript(cheating)
dsc$perc

#c
dsc$items

#d
dsc$pw.ass



#3.
#a
m1.rip <- ltm(cheating ~ z1,IRT.param=FALSE)
summary(m1.rip)
#b
alpha <- m1.rip$coefficients[,2]
alpha

#4.
stalpha <- alpha/sqrt(1+alpha^2)
stalpha
coef(m1.rip, prob = TRUE, order = TRUE)
#5.
E <- fitted(m1.rip)[,5]
O <- m1.rip$patterns$obs
cbind(m1.rip$patterns$X, O, E)
Chisq <- sum((E-O)^2/E)
Chisq
DOF <- 16 - 4*2 -1
DOF
pvalueC <- 1 - pchisq(Chisq, DOF)
pvalueC
LR <- 2 * sum(O * log(O/E))
LR
pvalueLR <- 1 - pchisq(LR, DOF)
pvalueLR

#6
margins(m1.rip)
#7.
fs1 <- factor.scores(m1.rip, method = "Component")
fs <- factor.scores(m1.rip, method = "EAP")
resp.pattern <- fs$score.dat[,1:4]
total.score <- apply(resp.pattern, 1, sum)
total.score
round(fs$score.dat[order(total.score),],3)
