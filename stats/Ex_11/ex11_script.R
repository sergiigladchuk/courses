# PART A

#1 Read tiger.csv
tiger <- read.csv('~/Documents/courses/stats/Ex_10/tiger.csv', sep=';')
str(tiger)
View(tiger)

# convert to counts
counts <- cbind(tiger$n[c(1,3,5,7)],tiger$n[c(2,4,6,8)])
counts
rownames(counts) <- c('s1','s2','s3','s4')
colnames(counts) <- c('col1','col2')

chisq.test(counts)
fisher.test(counts)

#2 Analysis with hierarchical log-linea analysis

tiger$seas <- factor(tiger$seas)
tiger$color <- factor(tiger$color)

glm.model <- glm(n~seas*color, data=tiger, family=poisson)
anova(glm.model, test='Chisq')

library(MASS)
loglm(n~seas+color, data=tiger)

# PART B
rm(list=ls())

# 1 read DDT
ddt <- read.csv('~/Documents/courses/stats/Ex_11/DDT.csv', sep=';')
str(ddt)
View(ddt)

# model selection routine
library(glmulti)

full.model <- glm(n~psite*sex*mort, data=ddt, family=poisson)
summary(full.model)

model.sel <- glmulti(full.model, level = 2, crit='aic')
summary(model.sel)

weightable(model.sel)

best.model <- glm(n ~ 1 + psite + sex + mort + mort:psite + mort:sex, family=poisson, data = ddt)
summary(best.model)

anova(best.model, test='Chisq')

AIC(full.model)
AIC(best.model)

full.model2 = glm(n ~ sex * psite * mort, family = poisson, data = ddt)
summary(full.model2)

model2.sel <- glmulti(full.model2, level = 2, crit = 'aic')
summary(model2.sel)

weightable(model2.sel)

best.model2 <- glm(n ~ 1 + sex + psite + mort + mort:sex + mort:psite, family =poisson, data=ddt)
summary(best.model2)
anova(best.model2, test = 'Chisq')
AIC(best.model2)

library(car)
Anova(best.model, type = 3)

# PART C
rm(list = ls())

# 1 Read mice data and fit best model
mice <- read.csv('~/Documents/courses/stats/Ex_11/mice.csv', sep=';')
str(mice)
View(mice)

full.model <- glm (n ~ treat + age + mort, family=poisson, data=mice)
summary(full.model)

model.sel <- glmulti(full.model, level=2, crit='aic')
summary(model.sel)
weightable(model.sel)

best.model <- glm(n ~ 1 + treat + age + mort + mort:treat + mort:age, family=poisson, data=mice)
summary(best.model)
anova(best.model, test = 'Chisq')
Anova(best.model, type=3)
AIC(full.model)
AIC(best.model)

# reducing model by excluding non-significant interaction between age and mortality
reduced.model <- glm(n ~ 1 + treat + age + mort + mort:treat, data=mice, family=poisson)
summary(reduced.model)
AIC(reduced.model)
anova(reduced.model, test='Chisq')
Anova(reduced.model, type=3)

final.model <- glm(n ~ treat + age + mort + mort*treat, data=mice, family=poisson)
anova(final.model, test='Chisq')

# 2. Extract the coefficients and calculate odd ratio
coefs <- coefficients(final.model)

age.im <- exp(0)
age.ma <- exp(coefs[3])
age.ol <- exp(coefs[4])
age.coefs <- c(age.im, age.ma, age.ol)
sum.age <- sum(age.coefs)
age <- age.coefs/sum.age
age

int <- exp(coefs[6])
int

# PART D
rm(list=ls())

# 1 Read EXCAV.csv
excav <- read.csv('~/Documents/courses/stats/Ex_11/EXCAV.csv', sep=';')
str(excav)
View(excav)

# convert to factors
excav$sex <- factor(excav$sex)
excav$season <- factor(excav$season)
excav$ampm <- factor(excav$ampm)

fit <- glm(exc ~ sex+season+ampm+sex*season+sex*ampm+season*ampm+sex*season*ampm, data=excav, family=binomial)

full.model <- glm(exc~time+sex*season*ampm, family=binomial, data=excav)
summary(full.model)
library(glmulti)
model.sel <- glmulti(full.model, level = 2, crit='aic')
summary(model.sel)
weightable(model.sel)

best.model <- glm(exc ~ season + ampm + time, family=binomial, data=excav)
summary(best.model)
anova(best.model, test='Chisq')
Anova(best.model, type=3)
AIC(full.model)
AIC(best.model)

# 2. Save predicted values into dataframe and plot it against significant variables

excav$pred <- predict(best.model, excav, type='response')
coefs <- coefficients(best.model)

boxplot(excav$pred~excav$season, xlab='Season', ylab='Excavation')
boxplot(excav$pred~excav$ampm, xlab='AmPm', ylab='Excavation')

# 3. Plot data with predicted fit lines instead

morn.spring<-subset(excav, excav$ampm == 1 & excav$season == 2)
morn.wint<-subset(excav, excav$ampm == 1 & excav$season == 1)
aft.spring<-subset(excav, excav$ampm == 2 & excav$season == 2)
aft.wint<-subset(excav, excav$ampm == 2 & excav$season == 1)
plot(morn.spring$pred~morn.spring$time, xlab="Time", ylab="Probability of
            excavation", xlim=c(0,600), ylim=c(0,1.4), pch=19, col="red")
points(morn.wint$pred~morn.wint$time, pch=19, col="blue")
points(aft.spring$pred~aft.spring$time, pch=21, col="red")
points(aft.wint$pred~aft.wint$time, pch=21, col="blue")
legend("topleft", legend=c("Spring", "Winter", "Morning", "Afternoon"), col
       = c("red","blue","black","black"), pch=c(19,19, 19, 21))

#altirnatively in coeff

plot(morn.spring$exc~morn.spring$time, xlab="Time", ylab="Probability of
excavation", xlim=c(0,600), ylim=c(0,1.4), pch=19, col="red")
points(morn.wint$exc~morn.wint$time, pch=19, col="blue")
points(aft.spring$exc~aft.spring$time, pch=21, col="red")
points(aft.wint$exc~aft.wint$time, pch=21, col="blue")
legend("topleft", legend=c("Spring", "Winter", "Morning", "Afternoon"),
         col = c("red","blue","black","black"), pch=c(19,19, 19, 21),
         lty=c(1,1,1,2))

x<-c(1:600)
y1 <- coefs[1] + coefs[4]*x
pred1 <- exp(y1)/(1+exp(y1))
y2 <- coefs[1] + coefs[2] + coefs[4]*x
pred2 <- exp(y2)/(1+exp(y2))
y3 <- coefs[1] + coefs[3] + coefs[4]*x
pred3 <- exp(y3)/(1+exp(y3))
y4 <- coefs[1] + coefs[3] + coefs[2] + coefs[4]*x
pred4 <- exp(y4)/(1+exp(y4))
lines(x, pred1, col="blue")
lines(x, pred2, col="red")
lines(x, pred3, col="blue", lty=2)
lines(x, pred4, col="red", lty=2)

# adding gitter to data because of overlap

morn.spring$exc.jit <- jitter(morn.spring$exc, amount = 0.02)
morn.wint$exc.jit <- jitter(morn.wint$exc, amount = 0.02)

aft.spring$exc.jit<-jitter(aft.spring$exc, amount = 0.02)
aft.wint$exc.jit<-jitter(aft.wint$exc, amount = 0.02)
plot(morn.spring$exc.jit~morn.spring$time, xlab="Time", ylab="Probability
of excavation", xlim=c(0,600), ylim=c(0,1.4), pch=19, col="red")

points(morn.wint$exc.jit~morn.wint$time, pch=19, col="blue")
points(aft.spring$exc.jit~aft.spring$time, pch=21, col="red")
points(aft.wint$exc.jit~aft.wint$time, pch=21, col="blue")
legend("topleft", legend=c("Spring", "Winter", "Morning", "Afternoon"),
         col = c("red","blue","black","black"), pch=c(19,19, 19, 21),
         lty=c(1,1,1,2))

x<-c(1:600)
y1 <- coefs[1] + coefs[4]*x
pred1 <- exp(y1)/(1+exp(y1))
y2 <- coefs[1] + coefs[2] + coefs[4]*x
pred2 <- exp(y2)/(1+exp(y2))
y3 <- coefs[1] + coefs[3] + coefs[4]*x
pred3 <- exp(y3)/(1+exp(y3))
y4 <- coefs[1] + coefs[3] + coefs[2] + coefs[4]*x
pred4 <- exp(y4)/(1+exp(y4))
lines(x, pred1, col="blue")
lines(x, pred2, col="red")
lines(x, pred3, col="blue", lty=2)
lines(x, pred4, col="red", lty=2)