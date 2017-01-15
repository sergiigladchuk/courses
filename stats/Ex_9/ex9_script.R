# PART A
# Read Tribol.csv

tribol <- read.csv('~/Documents/courses/stats/Ex_9/tribol.csv', sep=';')
str(tribol)

tribol$block <- factor(tribol$block)
str(tribol)

#block is a random factor

#building model with block as random factor
library(lme4)
library(lmerTest)
model.tribol <- lmer(wei~gtype+(1|block), data=tribol)
summary(model.tribol)
anova(model.tribol)
rand(model.tribol)

#do maximul likelyhood model
model.tribolML <- lmer(wei~gtype+(1|block), REML = F, data = tribol)
summary(model.tribolML)
anova(model.tribolML)
rand(model.tribolML)

#try to test without block
tribol.aov <- aov(wei~gtype, data=tribol)
summary(tribol.aov)

#F and p value are non-significant now, so correction for block is needed
library(multcomp)
tribol.test <- glht(model.tribol, linfct = mcp(gtype = "Tukey"))
summary(tribol.test)

tribol.testML <- glht(model.tribolML, linfct = mcp(gtype = "Tukey"))
summary(tribol.testML)

# 4 Planned contrast 
contr.tribol <- rbind('+b - ++' = c(1,-1,0), 'bb - ++' = c(1,0,-1))
tribol.plcont <- glht(model.tribolML, linfct = mcp(gtype = contr.tribol))
summary(tribol.plcont)

# PART B
rm(list=ls())

# 1 Read forest.csv

forest <- read.csv('~/Documents/courses/stats/Ex_3/forest.csv', sep=';')

#aggregation
str(forest)
forest$site <- factor(forest$site)
View(forest)
forest.av <- aggregate(forest[2:7] ,by = list(forest$site), FUN=mean, na.rm=TRUE )
View(forest.av)
boxplot(forest.av[c(4,5,7)])

# 2 overlay scatterplot of tree species distribution

x <- as.matrix(forest.av[c(4,5,7)])
x <- t(x)
colnames(x)<-forest.av$Group.1
barplot(x, col=c('darkblue','red','green'),legend.text = TRUE, 
        args.legend = list(x='topleft',colnames(forest.av[c(4,5,7)])), beside=TRUE)

# 3 repeat t-test (as in exercise 4)
t.test(forest.av$oak, forest.av$bir, paired = TRUE)

# 4 repeated measures ANOVA to test the same thing
# make only oak and bir mean dataframe

oak.bir <- cbind(forest.av$oak, forest.av$bir)
colnames(oak.bir) <- c("oak",  "bir")
oak.bir
mlm1 <- lm(oak.bir ~ 1)
mlm1

rfactor <- factor(c("oak", 'bir'))
rfactor
library(car)
mlm1.aov <- Anova(mlm1, idata = data.frame(rfactor), idesign=~rfactor, type='III')
summary(mlm1.aov, multivariate=FALSE)

# 5 inclusding alder so test for sphericity can be applied

oak.bir.ald <- cbind(forest.av$oak, forest.av$bir, forest.av$ald)
colnames(oak.bir.ald) <- c('oak', 'bir', 'ald')
oak.bir.ald

mlm2 <- lm(oak.bir.ald ~ 1)
rfactor2 <- factor(c('oak','bir','ald'))
mlm2.aov <- Anova(mlm2, idata = data.frame(rfactor2), idesign=~rfactor2, type='III')
summary(mlm2.aov, multivariate=FALSE)

# 6 Two-way ANOVA with the sire as random block factor and species as a fixed factor
# data rearengement
oak <- cbind(forest.av$Group.1, forest.av$oak)
colnames(oak) <- c('site', 'prop')

bir <- cbind(forest.av$Group.1, forest.av$bir)
colnames(bir) <- c('site', 'prop')

ald <- cbind(forest.av$Group.1, forest.av$ald)
colnames(ald) <- c('site', 'prop')

trees <- rbind(oak,bir, ald)
trees <- as.data.frame(trees)
trees
trees$species <- c(rep('oak', 13), rep('bir', 13), rep('ald', 13))
View(trees)
trees$site <- factor(trees$site)
trees$species <- factor(trees$species)
str(trees)

# 7 Ordinary ANOVA to test the difference in abundance between the tree species

model.trees <- lmer(prop~species+(1|site), data=trees)
summary(model.trees)
anova(model.trees)
rand(model.trees)

aov.trees <-  aov(prop~species+Error(site/species), data=trees)
summary(aov.trees)

# 8 Remove alk and do paired t-test
trees2 <- subset(trees, trees$species!='ald')
trees2

aov.trees2 <- aov(prop~species+site, data=trees2)
summary(aov.trees2)

# PART C
rm(list=ls())

#1 read anxiety2 file
anxiety <- read.csv('~/Documents/courses/stats/Ex_9/anxiety2.csv', sep=';')
str(anxiety)
View(anxiety)

anxiety$subject <- factor(anxiety$subject)
anxiety$anxiety <- factor(anxiety$anxiety)
anxiety$tension <- factor(anxiety$tension)

str(anxiety)

model.anx <- lm(cbind(trial1, trial2, trial3, trial4)~anxiety*tension, data=anxiety)
summary(model.anx)
anova(model.anx)
library(car)
Anova(model.anx)

trials <- factor(c('trial1', 'trial2', 'trial3', 'trial4' ))
mlm.anx.rep <- Anova(model.anx, idata=data.frame(trials), idesign = ~trials, type='III')
summary(mlm.anx.rep, multivariate=FALSE)

# 2 Visualizate the data
library(reshape)
str(anxiety)
anx2 <- melt(anxiety, id=c('subject', 'anxiety', 'tension'))
anx2$trial <- c(rep(1, 12), rep(2,12), rep(3, 12), rep(4, 12))
View(anx2)
str(anx2)
nsubjects <- 12#max(anx2$subject)
anx2$trial.jit <- jitter(anx2$trial)

# get the range for x and y axes
xrange <- range(anx2$trial.jit)
yrange <- range(anx2$value)

plot(xrange, yrange, type='n', xlab='Trial', ylab='# errors')
colors <-rainbow(nsubjects)
plotchar <- rep(c(18:21), times=3)
for(i in 1:nsubjects) {
  trial <- subset(anx2, anx2$subject==i)
  points(trial$trial.jit, trial$value, type='b', col=colors[i], pch=plotchar[i])
}
legend('bottomleft', c('Subject 1', 'Subject 2', 'Subject 3','Subject 4', 'Subject 5',
                       'Subject 6', 'Subject 7', 'Subject 8', 'Subject 9', 
                       'Subject 10','Subject 11','Subject 12'),
       cex=0.8, pch=plotchar, col=colors)

# graph of between-subject effects
lalt <- subset(anx2, anxiety==1 & tension==1)
laht <- subset(anx2, anxiety==1 & tension==2)
halt <- subset(anx2, anxiety==2 & tension==1)
haht <- subset(anx2, anxiety==2 & tension==2)
lalt.m <- mean(lalt$value)
laht.m <- mean(laht$value)
halt.m <- mean(halt$value)
haht.m <- mean(haht$value)

means.anx <- c(lalt.m, laht.m, halt.m, haht.m)

lalt.se <- (sd(lalt$value))/(sqrt(length(lalt$value)))
laht.se <- (sd(laht$value))/(sqrt(length(laht$value)))
halt.se <- (sd(halt$value))/(sqrt(length(halt$value)))
haht.se <- (sd(haht$value))/(sqrt(length(haht$value)))

se.anx <- c(lalt.se, laht.se, halt.se, haht.se)

library(Hmisc)
x <- c(1:4)
plot(x, means.anx, type = 'b', lty=0, xlim=c(0.5, 4.5), xlab='Treatment', 
     , ylim=c(0,14), ylab='# errors', axes=FALSE)
axis(1, at=c(0.5,1,2,3,4,4.5), labels=c('','LA/LT','LA/HT','HA/LT','HA/HT',''))
axis(2, at=seq(0,14,2))
errbar(x, means.anx, means.anx + se.anx, means.anx - se.anx, add=TRUE)

# PART D
rm(list=ls())

#1. Read breed data and make plot with regression line for each year
breed <- read.csv('~/Documents/courses/stats/Ex_9/BREED.csv', sep=';')
str(breed)
View(breed)

ar.vec <- seq(range(breed$ar)[1],range(breed$ar)[2])
#prepare plot
plot(range(breed$date),range(breed$egg), type='n', xlab='Date in May', ylab='# of Eggs')
colors <- rainbow(length(ar.vec))
#add points for each year
count <- 0
for (year in ar.vec){
  count <- count + 1
  #split data
  yearData <- subset(breed,breed$ar == year)
  
  #linear model
  lm <- lm(egg~date,data=yearData)
  
  #get coeff
  abline(lm$coefficients,col=colors[count])
  
  points(egg~date, data=yearData, col=colors[count])
  
}
# legend
legend(1, 4.5, ar.vec, cex=0.8, lty=1, col=colors, title = 'Years')

# 2. Test using one-way ANOVA difference between number of eggs for year

#get year as factor
breed$ar <- factor(breed$ar)
breed.aov <- aov(egg~ar, data=breed)
summary(breed.aov)

# 3. Add date as covariate
breed.ancova <- aov(egg~date*ar, data=breed)
summary(breed.ancova)
# removing interaction
breed.ancova.noInter <- aov(egg~date+ar, data=breed)
summary(breed.ancova.noInter)

# 4. Parameter extraction
param <- coef(breed.ancova)
breed$resid <- residuals(breed.ancova)

# 5. Plotting residuals
hist(breed$resid)
shapiro.test(breed$resid)
qqnorm(breed$resid)
qqline(breed$resid)
# plot the residuals against date
plot(resid~date, data=breed)
