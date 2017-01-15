# PART A

# 1
chicks <- read.csv('~/Documents/courses/stats/Ex_8/CHICKS.csv', sep=';')
str(chicks)
View(chicks)
chicks$sex <- factor(chicks$sex)
str(chicks)

# two-way ANOVA

chicks.aov <- aov(chicks$weight~chicks$sex*chicks$treat)
summary(chicks.aov)

# Is this a case of an ANOVA with replication? - yes
# If so what is the replicate? - the individual chicks are replicates
# What sort of ANOVA model is this fixed (model I), random(model II),
# or mixed (model II)? - fixed model I

# 2 equality of the variances
chicks$groups <- paste(chicks$sex, chicks$treat, sep='')

#leveneTest

library(car)
leveneTest(chicks$weight~chicks$groups)

#levens test is not significant so variances are not homogene

#normality of the residuals
chicks$resid <- residuals(aov(chicks$weight~chicks$sex*chicks$treat))
hist(chicks$resid, breaks = 10)

qqnorm(chicks$resid)
qqline(chicks$resid)

shapiro.test(chicks$resid)
# the values is far from 1 so it is normally distributed

#3 Illustrate results with clustered boxplot
library(lattice)
bwplot(weight~treat | sex, chicks)

# PART B
rm(list=ls())

#1 read ARGY.csv
argy <- read.csv('~/Documents/courses/stats/Ex_8/ARGY.csv', sep=';')
str(argy)
argy$site <- factor(argy$site)
argy$yr <- factor(argy$yr)
argy$bloom <- factor(argy$bloom)

str(argy)

#plot the data
xyplot(dens~site | bloom, argy)

# 2 precise ANOVA

library(lme4)
library(lmerTest)
model.argy <- lmer(dens~bloom+(1|site)+ (1|site:bloom), data=argy)
summary(model.argy)

rand(model.argy)

#aov model
Anova(model.argy, Type="III")

#linear model
argy.fix.lm <- lm(dens~bloom*site, data=argy)
summary(argy.fix.lm)

argy.fix.aov <- aov(argy.fix.lm)
summary(argy.fix.aov)

# PART C
rm(list=ls())

# 1 read file fort.csv

fort <- read.csv('~/Documents/courses/stats/Ex_8/fort.csv', sep=';')
str(fort)

fort$sex <- factor(fort$sex)
fort$season <- factor(fort$season)
fort$ampm <- factor(fort$ampm)

str(fort)
View(fort)

# building ANOVA model

fort.aov <- aov(fortime~sex*season*ampm, data=fort)
summary(fort.aov)

fort.aov.reorder <- aov(fortime~season*sex*ampm, data=fort)
summary(fort.aov.reorder)

Anova(lm(fortime~sex*season*ampm, data=fort), type='III')

fort$groups <- paste(fort$sex, fort$season, fort$ampm, sep='')
leveneTest(fort$fortime~fort$groups)

fort$resid <- residuals(fort.aov)
# the assumption of heterogeneity is not met

hist(fort$resid, breaks = 10)

qqnorm(fort$resid)
qqline(fort$resid)

shapiro.test(fort$resid)


# 2 transforming data with arc-sine square-root

fort$fortime.tr <- asin(sqrt(fort$fortime))

# building ANOVA model

fort.aov <- aov(fortime.tr~sex*season*ampm, data=fort)
summary(fort.aov)

fort.aov.reorder <- aov(fortime.tr~season*sex*ampm, data=fort)
summary(fort.aov.reorder)

Anova(lm(fortime.tr~sex*season*ampm, data=fort), type='III')

leveneTest(fort$fortime.tr~fort$groups)

fort$resid <- residuals(fort.aov)
# the assumption of heterogeneity is not met

hist(fort$resid, breaks = 10)

qqnorm(fort$resid)
qqline(fort$resid)

shapiro.test(fort$resid)

# 3. post-hoctest of vinter versus spring
TukeyHSD(fort.aov, "season")

# PART D
rm(list=ls())

#1 read data mosq

mosq <- read.csv('~/Documents/courses/stats/Ex_8/MOSQ.csv', sep=';')
str(mosq)
mosq$cage <- factor(mosq$cage)
mosq$id <- factor(mosq$id)
mosq$meas <- factor(mosq$meas)

str(mosq)
View(mosq)

xyplot(wlen~id | cage, mosq)

# 2
# giving uniq IDs
mosq$id2 <- paste(mosq$cage, mosq$id, sep='')

#build model
mosq.lmer <- lmer(wlen~1 + (1|cage) + (1|cage:id2), data=mosq)
summary(mosq.lmer)
rand(mosq.lmer)

#PART E
rm(list=ls())

#1 read iir.csv
iir <- read.csv('~/Documents/courses/stats/Ex_8/iir.csv', sep=';', header = TRUE, row.names = NULL)

iir <- read.csv("http://wallace.teorekol.lu.se/statistics_for_biologists/08/iir.csv", 
                header = TRUE, sep = ";", row.names = NULL)

str(iir)
iir$cage <- factor(iir$cage)
iir$nid <- factor(iir$nid)
str(iir)
View(iir)

# this is nested mixed model
iir.lmer <- lmer(intake~hab*food+(1|hab:cage)+(1|cage:id), data=iir)
summary(iir.lmer)

anova(iir.lmer)
rand(iir.lmer)
