#PART A

#1 Read in Tits file

tits <- read.csv("~/Documents/courses/stats/Ex_1/tits.csv")

str(tits)
View(tits)
#make boxplots and error bar plots to check that blue tits are as heavy as great tits

boxplot(tits$wei~tits$SPE)

#error plot
tits.BM <- subset(tits,tits$SPE  =="BM")
tits.TX <- subset(tits,tits$SPE =="TX")
tits.BM.av <- mean(tits.BM$wei, na.rm = TRUE)
tits.TX.av <- mean(tits.TX$wei, na.rm = TRUE)
tits.BM.sd <- sd(tits.BM$wei, na.rm = TRUE)
tits.TX.sd <- sd(tits.TX$wei, na.rm = TRUE)
tits.BM.se <- tits.BM.sd / sqrt(length(na.omit(tits.BM$wei)))
tits.TX.se <- tits.TX.sd / sqrt(length(na.omit(tits.TX$wei)))

mean.wei <- cbind(tits.BM.av, tits.TX.av)
se.wei <- cbind(tits.BM.se, tits.TX.se)

#build error plot

library(Hmisc)
x <- c(1:2)
plot(x, mean.wei , type='b', lty=0, xlim=c(0.5, 2.5),
     xlab="Species", ylim=c(8, 22), ylab="Weight", axes=FALSE)
axis(1, at=c(0.5, 1, 2, 2.5), labels=c("","Blue tit", "Great tit",  ""))

axis(2, at=seq(8, 22, 2))

errbar(x, mean.wei, mean.wei + se.wei, mean.wei - se.wei, add=TRUE)

# where is a significan difference between weight of two species

#2 Do t-test and ANOVA
t.test(tits$wei~tits$SPE, rm.na=TRUE, var.equal = TRUE)

aov.wei <- aov(tits$wei~tits$SPE)
summary(aov.wei)

# Significance level is almost same because ANOVA for pairwize comparizon is
# very alike to t-test. F is t-value squered (t.test should be with equal variance)

# PART B
rm(list=ls())
#1 Read in peas.csv
peas <- read.csv('~/Documents/courses/stats/Ex_7/peas.csv', sep=';')
str(peas)
View(peas)

# plots and error plots
boxplot(peas$len~peas$treat)

# error plot
library(Rmisc)
autoSE <- summarySE(peas, 'len', 'treat', na.rm = TRUE)
View(autoSE)

library(Hmisc)
x <- c(1:5)
plot(x, autoSE[ ,3] , type='b', lty=0, xlim=c(0.5, 5.5),
     xlab="Groups", ylim=c(50, 80), ylab="Len", axes=FALSE)
axis(1, at=c(0.5, 1, 2, 3, 4, 5, 5.5), labels=c("","1g1f", "2f", "2g", "2s", "c", ""))
axis(2, at=seq(50, 80, 2))
errbar(x, autoSE[,3], autoSE[,3] + autoSE[,5], autoSE[,3] - autoSE[,5], add=TRUE)

# One-way analysis of variance - ANOVA
aov.peas <- aov(peas$len~peas$treat)
summary(aov.peas)

# Null-hypothesis is that all means are equal, and it is rejected because 
# p value is very low (significant)

# testing with linear model
lm.peas <- lm(peas$len~peas$treat)
lm.peas
summary(lm.peas)

#p values is the save as an F value - though output is more descriptive and 
# gives the hint what categories are most significant

peas$resid <- residuals(lm.peas)
hist(peas$resid, breaks=10)
# test for normality

shapiro.test(peas$resid)
#p-value is not 1 so it is normal

#build qqnorm plots and line
qqnorm(peas$resid)
qqline(peas$resid)

# 6 Investigate assumption of homogeneity of variances
library(car)
leveneTest(peas$len~peas$treat)

#Yes it is fine. The most homogenic variance is in first group, but we reject 
# this hypothesis because it is significant that variances are not equal.
# main contributor is control group

# 7 Test null-hypothesis that control group is identical to all other gropus
# taken together
summary.lm(aov.peas)

# making contrast matrix for this pypthesis
contrast.1 <- cbind(c(1,1,1,1,0),
                    c(0,0,0,0,0),
                    c(0,0,0,0,0),
                    c(0,0,0,0,0))
contrasts(peas$treat)<-contrast.1
summary.lm(aov(peas$len~peas$treat))

# 8 Test null-pypothesis that mixed sugar is identical to single sugar treatments
contrast.2 <- cbind(c(0,1,1,1,-1),
                    c(0,0,0,0,-1),
                    c(0,0,0,0,-1),
                    c(0,0,0,0,-1))
contrast.2
contrasts(peas$treat)<-contrast.2
summary.lm(aov(peas$len~peas$treat))

# 9 Make past-hoc test of difference between all groups.

TukeyHSD(aov.peas)

pairwise.t.test(peas$len, peas$treat, p.adj="bonferroni", paired=F)

#10 Nonparametric version of ANOVA - Kruskal-Wall
kruskal.test(peas$len, peas$treat)
