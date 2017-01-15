
# 1 Read file Surv.csv

surv <- read.csv('~/Documents/courses/stats/Ex_13/surv.csv', sep=';')
str(surv)
View (surv)

# crearing opposite to life variable

surv$died <- ifelse(surv$lives == 0, 1, 0);
surv$tq <- factor(surv$tq)

# make a life table
library(KMsurv)
library(survival)

tis <- c(1:6)
nsubs <- matrix(nrow=6, ncol=1)
nevent <- matrix(nrow=6, ncol=1)
nlost <-matrix(nrow=6, ncol =1)
for (i in 1:6) {
  agei <- subset(surv, age>=i)
  nsubs[i] <- nrow(agei)
  agei.i <- subset (agei, age ==i)
  nevent[i] <- sum(agei.i$died)
  nlost[i] <- nrow(agei.i)-nevent[i]
}

lifetab(tis, nsubs[1], nlost, nevent)

#2 Estimate survival function

surv$ar2.2 <- ifelse(is.na(surv$ar2), 97, surv$ar2)
surv$time <- surv$ar2.2 - surv$ar1

msurv <- Surv(surv$age, surv$time, surv$died, type='interval')
fit <- survfit(msurv~1)
summary(fit)

plot(fit)

#3 Use sex as factor to test if different sexes have better survival

fit.sex <- survfit(msurv~surv$sex)
plot(fit.sex)

msurv2 <- Surv(surv$age, surv$died)
survdiff(msurv2~surv$sex)

# 4 Use tq as a factor to test differences

fit.tq <- survfit(msurv~surv$tq)
plot(fit.tq, col = c('blue','red'))

survdiff(msurv2~surv$tq)

# Cox regression

surv.coxph3 <- coxph(msurv2~surv$rep*surv$tq*surv$sex)
AIC(surv.coxph3)
library(car)
Anova(surv.coxph3, Type='II')

surv.coxph3.best <- coxph(msurv2~surv$rep+surv$tq+surv$sex)
AIC(surv.coxph3.best)
anova(surv.coxph3.best)
Anova(surv.coxph3.best,type=2)

# 6 Plotting the model
plot(surv$age~surv$tq)

# 3D visualization

bh <- basehaz(surv.coxph3.best, centered = TRUE)
for (i in 1:6) {
  surv$bh[surv$age==i] <- bh$hazard[i]
}
coefs <- as.numeric(coefficients(surv.coxph3.best))

coef.rep <- coefs[1]
coef.tq <- coefs[2]
coef.sex <- coefs[3]

surv$haz <- surv$age*surv$bh*exp((coef.rep*surv$rep)+coef.tq*(as.numeric(surv$tq))
                                 +coef.sex*(as.numeric(surv$sex)))

tq1 <- subset(surv, surv$tq==1)
tq2 <- subset(surv, surv$tq==2)
library(rgl)
plot3d(tq1$age,tq1$rep,tq1$haz)
plot3d(tq2$age, tq2$rep, tq2$haz, col='red', add=TRUE)
