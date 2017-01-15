# PART A
# 1 Read RMfor
RMfor <- read.csv("~/Documents/courses/stats/Ex_5/RMfor.csv", sep=';')

# scatterplots 
pairs(~oak+birch+alder+lime, data=RMfor, main="Simple Scatterplot Matrix")

# yes the are independent

#2 Faking interactions
RMfor$oak.birch.prod <- RMfor$oak * RMfor$birch
RMfor$oak.alder.prod <- RMfor$oak * RMfor$alder
RMfor$birch.alder.prod <- RMfor$alder * RMfor$birch
View(RMfor)

#3 Computaion of Pearson correlation
library(Hmisc)

rcorr(as.matrix(RMfor[4:10]), type="pearson")
# strong corelation between birch and oak close to -1 (-0.67)

#4 Partial correlatios
library (ppcor)
p.values <- rep(NA, 10)
dim(p.values) <- c(5,2)
lim_oak <-  pcor.test(RMfor$lime, RMfor$oak, RMfor$birch, method="pearson")
p.values[1,1] <- "lim_oak"
p.values[1,2] <- lim_oak$p.value

lim_ald <-  pcor.test(RMfor$lime, RMfor$alder, RMfor$birch, method="pearson")
p.values[2,1] <- "lim_ald"
p.values[2,2] <- lim_ald$p.value

lim_oxb <-  pcor.test(RMfor$lime, RMfor$oak.birch.prod, RMfor$birch, method="pearson")
p.values[3,1] <- "lim_oxb"
p.values[3,2] <- lim_oxb$p.value

lim_oxa <-  pcor.test(RMfor$lime, RMfor$oak.alder.prod, RMfor$birch, method="pearson")
p.values[4,1] <- "lim_oxa"
p.values[4,2] <- lim_oxa$p.value

lim_axb <-  pcor.test(RMfor$lime, RMfor$birch.alder.prod, RMfor$birch, method="pearson")
p.values[5,1] <- "lim_axb"
p.values[5,2] <- lim_axb$p.value

p.values

# oak has most significance so we control it in next step
#5 new partial correlation matrix with controlling both birch and oak
p.values2 <- rep(NA, 8)
dim(p.values2) <- c(4, 2)

lim_ald <-  pcor.test(RMfor$lime, RMfor$alder, RMfor[4:5], method="pearson")
p.values2[1,1] <- "lim_ald"
p.values2[1,2] <- lim_ald$p.value

lim_oxb <-  pcor.test(RMfor$lime, RMfor$oak.birch.prod, RMfor[4:5], method="pearson")
p.values2[2,1] <- "lim_oxb"
p.values2[2,2] <- lim_oxb$p.value

lim_oxa <-  pcor.test(RMfor$lime, RMfor$oak.alder.prod, RMfor[4:5], method="pearson")
p.values2[3,1] <- "lim_oxa"
p.values2[3,2] <- lim_oxa$p.value

lim_axb <-  pcor.test(RMfor$lime, RMfor$birch.alder.prod, RMfor[4:5], method="pearson")
p.values2[4,1] <- "lim_axb"
p.values2[4,2] <- lim_axb$p.value

p.values2

#exclude ald now

p.values3 <- rep(NA, 6)
dim(p.values3) <- c(3, 2)

lim_axb <-  pcor.test(RMfor$lime, RMfor$alder, RMfor[4:6], method="pearson")
p.values3[1,1] <- "lim_axb"
p.values3[1,2] <- lim_axb$p.value

lim_oxb <-  pcor.test(RMfor$lime, RMfor$oak.birch.prod, RMfor[4:6], method="pearson")
p.values3[2,1] <- "lim_oxb"
p.values3[2,2] <- lim_oxb$p.value

lim_oxa <-  pcor.test(RMfor$lime, RMfor$oak.alder.prod, RMfor[4:6], method="pearson")
p.values3[3,1] <- "lim_oxa"
p.values3[3,2] <- lim_oxa$p.value

p.values3

#exclude axb now

p.values <- rep(NA, 4)
dim(p.values) <- c(2, 2)

lim_oxa <-  pcor.test(RMfor$lime, RMfor$alder, RMfor[4:6,8], method="pearson")
p.values[1,1] <- "lim_oxa"
p.values[1,2] <- lim_oxa$p.value

lim_oxb <-  pcor.test(RMfor$lime, RMfor$oak.birch.prod, RMfor[4:6,8], method="pearson")
p.values[2,1] <- "lim_oxb"
p.values[2,2] <- lim_oxb$p.value

p.values

#6 test all possible models with lm

full.model <- lm(lime~oak+birch+alder+oak*birch+oak*alder+birch*alder, data=RMfor)
summary(full.model)

#model selection
library(glmulti)
model.sel <- glmulti(full.model, level = 2, crit='aic')
summary(model.sel)
weightable(model.sel)

#7 check for collinearity
model <- model.matrix(~oak+birch+alder+oak*birch+oak*alder+birch*alder, data=RMfor)
kappa(model)

library(car)
fit <- lm(lime~ oak+birch+alder+oak*birch+oak*alder+birch*alder, data=RMfor)
vif(fit)

#PART B
rm(list=ls())
#1 Read the file, and fit the model

PCA <- read.csv("~/Documents/courses/stats/Ex_6/PCA.csv", sep=';')
PCA.na <- na.omit(PCA)
View(PCA.na)
str(PCA.na)
PCA.na$sex <-factor(PCA.na$sex)
str(PCA.na)

#building model
fit <-lm(age~ wing+tarsv+tail+wproj+fett+weigh+bill+bihe+biwi, data = PCA.na)
# library(car)
# vif(fit)
# summary(fit)

library(glmulti)
model.sel <- glmulti(fit, level = 1, crit='aic')

sel.output<-weightable(model.sel)
head(sel.output, n=10)

fit.best <- lm(age ~ wing+weigh + bihe + biwi, data=PCA.na)
summary(fit.best)

#2. Same model selection excluding wing parametr
fit.nw <-lm(age~ tarsv+tail+wproj+fett+weigh+bill+bihe+biwi, data = PCA.na)

model.sel.nw <- glmulti(fit.nw, level = 1, crit='aic')

sel.output.nw <-weightable(model.sel.nw)
head(sel.output.nw, n=10)

fit.best.nw <- lm(age ~ wproj + weigh + bihe + biwi, data=PCA.na)
summary(fit.best.nw)

#let's check correlation between wproj

rcorr(as.matrix(PCA.na[,c(11,14,16,18,19)]), type="pearson")

# PART C
rm(list=ls())

#1 Read PHEASANT.csv

pheasant <- read.csv('~/Documents/courses/stats/Ex_6/PHEASANT.csv', sep=';')
str(pheasant)
View(pheasant)

#plot weight against ageday

plot(WEIGHT~AGEDAY,data=pheasant)

nls <- nls(pheasant$WEIGHT~a/(1 + exp(-k * (pheasant$AGEDAY - i))), data=pheasant, 
           start=list(a=1400, k=0.05, i=60), na.action = na.omit)

#saving predicted results to another variable
pheasant$PRED <- predict(nls,pheasant)
summary(nls, correlation = TRUE)

# plot predicted weight and original
plot(WEIGHT~AGEDAY,data=pheasant)
points(pheasant$AGEDAY,pheasant$PRED, col='red')

#3 If you wish
