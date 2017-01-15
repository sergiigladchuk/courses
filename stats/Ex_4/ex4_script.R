#PART A
#1 readd bodyweight file
setwd("~/Documents/courses/stats/Ex_4")
bodyweight <- read.csv('~/Documents/courses/stats/Ex_4/bodyweight.csv', sep=';')
View(bodyweight)
str(bodyweight)

#2 test null-hypothesis with t.test
t.test(bodyweight$bw~bodyweight$sex)

#and now with same variance
t.test(bodyweight$bw~bodyweight$sex, var.equal = TRUE)

#3 Levene's test for equality of variances
library(car)
leveneTest(bodyweight$bw~bodyweight$sex)

#4 Make an error-bar graph
#manually 
#subsets or by aggragate

males <- subset(bodyweight, bodyweight$sex == 'm')
females <- subset(bodyweight, bodyweight$sex == 'f')

av.males <- mean(males$bw, na.rm = TRUE)
av.females <- mean(females$bw, na.rm = TRUE)

sd.males <- sd(males$bw, na.rm = TRUE)
sd.females <- sd(females$bw, na.rm = TRUE)

se.males <- (sd.males/sqrt(length(males$bw)))
se.females <- (sd.females/sqrt(length(females$bw)))

ci.males <- se.males * 1.96
ci.females <- se.females * 1.96

means <- cbind(av.males, av.females)
cis <- cbind(ci.males, ci.females)

#build plot
library(Hmisc)
x <- c(1:2)
plot(x, means, type='b', lty=0, xlim=c(0.5, 2.5),
     xlab="Sex", ylim=c(0, 100), ylab="Weight", axes=FALSE)
axis(1, at=c(0.5, 1, 2, 2.5), labels=c("","Males", "Females",  ""))

axis(2, at=seq(0, 100, 5))

errbar(x, means, means + cis, means - cis, add=TRUE)

#via ready to use function
library(Rmisc)
autoSum <- summarySE(bodyweight, 'bw', 'sex', na.rm = TRUE)

#build plot with arrows
# xvals <- barplot(means, xlab="Sex", ylab="Weight", ylim=c(0,100), 
#                  axes=FALSE)
# axis(1, at=xvals, labels=c("Male","Female"))
# 
# arrows(xvals, means, means + cis, xvals, 
#        means, means - cis, code=3, angle=90, length=0.1)

#PART B
rm(list=ls())

#1 Read teats

tits <- read.csv("~/Documents/courses/stats/Ex_1/tits.csv")

#2 t-test that fledging weight does not differ

t.test(tits$wei~tits$SPE)
t.test(tits$wei~tits$SPE, var.equal = TRUE)

#check if variance should be used
library(car)
leveneTest(tits$wei~tits$SPE)

#3 build error-bar-graph
library(Rmisc)
vals <- summarySE(tits, 'wei', 'SPE', na.rm = TRUE)

#build plot
library(Hmisc)
x <- c(1:2)
plot(x, vals$wei, type='b', lty=0, xlim=c(0.5, 2.5),
     xlab="Sex", ylim=c(5, 20), ylab="Weight", axes=FALSE)
axis(1, at=c(0.5, 1, 2, 2.5), labels=c("","Great tit", "Blue tit", ""))

axis(2, at=seq(0, 20, 5))

errbar(x, vals$wei, vals$wei + vals$ci, vals$wei - vals$ci, add=TRUE)

#4 histogram or wei
hist(tits$wei, breaks = 10)

#test of normality
shapiro.test(tits$wei)
#looks like not normal because p-value is far away from 1

qqnorm(tits$wei)
qqline(tits$wei)

#5 compute errors for each clutch

tits.wei <- summarySE(tits, 'wei', 'SPE', na.rm = TRUE)
tits$res <- ifelse(tits$SPE == 'TX', tits$wei - 
                     tits.wei[2,3], tits$wei - tits.wei[1,3])
View(tits)

#explore rei with tests
hist(tits$res)
shapiro.test(tits$res)

qqnorm(tits$res)
qqline(tits$res)

#6 Explore wei within subsets
tits.TX <- subset(tits, tits$SPE == 'TX')
tits.BM <- subset(tits, tits$SPE == 'BM')

hist(tits.TX$res)
shapiro.test(tits.TX$res)

qqnorm(tits.TX$res)
qqline(tits.TX$res)

hist(tits.BM$res)
shapiro.test(tits.BM$res)

qqnorm(tits.BM$res)
qqline(tits.BM$res)

#7 

#8 U-test - nonparametric test

wilcox.test(tits.TX$wei, tits.BM$wei, paired = FALSE)

#9 just boxplot with no SE in graph since no paramets
boxplot(tits$wei~tits$SPE, xlab="Species", ylab="Weight")

#PART C
#1 read forest file

forest <- read.csv("~/Documents/courses/stats/Ex_3/forest.csv", sep=';')
str (forest)
View (forest)

#aggregation on study site with all speciace averages

av.forest <- aggregate(forest, FUN=mean, by=list(forest$site), na.rm=TRUE)

av.forest

t.test1 <- t.test(av.forest$oak,av.forest$bir, paired = TRUE)

t.test2 <- t.test(av.forest$oak,av.forest$bea, paired = TRUE)

t.test3 <- t.test(av.forest$oak,av.forest$ald, paired = TRUE)

t.test4 <- t.test(av.forest$oak,av.forest$asp, paired = TRUE)

t.test5 <- t.test(av.forest$oak,av.forest$lim, paired = TRUE)

t.test6 <- t.test(av.forest$bir,av.forest$bea, paired = TRUE)

t.test7 <- t.test(av.forest$bir,av.forest$ald, paired = TRUE)

t.test8 <- t.test(av.forest$bir,av.forest$asp, paired = TRUE)

t.test9 <- t.test(av.forest$bir,av.forest$lim, paired = TRUE)

t.test10 <- t.test(av.forest$bea,av.forest$ald, paired = TRUE)

t.test11 <- t.test(av.forest$bea,av.forest$asp, paired = TRUE)

t.test12 <- t.test(av.forest$bea,av.forest$lim, paired = TRUE)

t.test13 <- t.test(av.forest$ald,av.forest$asp, paired = TRUE)

t.test14 <- t.test(av.forest$ald,av.forest$lim, paired = TRUE)

t.test15 <- t.test(av.forest$asp,av.forest$lim, paired = TRUE)

bfc <- rep(NA,15)

bfc[1] <- t.test1$p.value
bfc[2] <- t.test2$p.value
bfc[3] <- t.test3$p.value
bfc[4] <- t.test4$p.value
bfc[5] <- t.test5$p.value
bfc[6] <- t.test6$p.value
bfc[7] <- t.test7$p.value
bfc[8] <- t.test8$p.value
bfc[9] <- t.test9$p.value
bfc[10] <- t.test10$p.value
bfc[11] <- t.test11$p.value
bfc[12] <- t.test12$p.value
bfc[13] <- t.test13$p.value
bfc[14] <- t.test14$p.value
bfc[15] <- t.test15$p.value

bfc <- bfc*15
bfc


#fdr correction

fdr <- rep(NA,15)

fdr[1] <- t.test1$p.value
fdr[2] <- t.test2$p.value
fdr[3] <- t.test3$p.value
fdr[4] <- t.test4$p.value
fdr[5] <- t.test5$p.value
fdr[6] <- t.test6$p.value
fdr[7] <- t.test7$p.value
fdr[8] <- t.test8$p.value
fdr[9] <- t.test9$p.value
fdr[10] <- t.test10$p.value
fdr[11] <- t.test11$p.value
fdr[12] <- t.test12$p.value
fdr[13] <- t.test13$p.value
fdr[14] <- t.test14$p.value
fdr[15] <- t.test15$p.value

p.adjust(fdr,method = 'fdr')

#4.  Make two sorts of graphs for the comparisons oak vs. birch, and oak vs.lime

#The first is to compute the differences between
#the pairs of variables, and then make an error-bar-graph of that difference

# no good example
av.forest$oak.bir <- av.forest$oak - av.forest$bir
av.forest$oak.lim <- av.forest$oak - av.forest$lim

boxplot(av.forest$oak.bir)

# Second  to plot the two variables against each other

plot(av.forest$oak,av.forest$bir, xlim = c(0,1), ylim= c(0,1))
abline(0,1)

plot(av.forest$oak,av.forest$lim, xlim = c(0,1), ylim= c(0,1))
abline(0,1)

#5. test normal distribution of differences

shapiro.test(av.forest$oak.bir)

#6. Conduct nonparametric test because "it is not normal"

wilcox.test(av.forest$oak,av.forest$bir, paired = TRUE)
wilcox.test(av.forest$oak,av.forest$lim, paired = TRUE)

library(BSDA)
SIGN.test(av.forest$oak,av.forest$bir)
SIGN.test(av.forest$oak,av.forest$lim)
