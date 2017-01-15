#PART A
#1. Read file Succ
setwd("~/Documents/courses/stats/Ex_3")

succ <- read.csv("succ.csv")
str(succ)

#2. Make a histogram and then display normal curve base on instruction
h <- hist(succ$succ, breaks=10)

#adding norm curve
xfit <- seq(min(succ$succ),max(succ$succ),length=40)
yfit <- dnorm(xfit,mean=mean(succ$succ),sd=sd(succ$succ))
yfit <- yfit*diff(h$mids[1:2])*length(succ$succ)
lines(xfit,yfit, col='blue', lwd=2)

#3. Save average of succ per year into new frame
aveSucc <- aggregate(succ$succ, list(succ$year), mean)
colnames(aveSucc) <- c('year', 'aveSucc')
View(aveSucc)

aveH <- hist(aveSucc$aveSucc, breaks=10, main = 'Example of Central Limit theorem')
xfit <- seq(min(aveSucc$aveSucc),max(aveSucc$aveSucc),length=40)
yfit <- dnorm(xfit,mean=mean(aveSucc$aveSucc),sd=sd(aveSucc$aveSucc))
yfit <- yfit*diff(aveH$mids[1:2])*length(aveSucc$aveSucc)
lines(xfit,yfit, col='blue', lwd=2)

#it looks like distribution of means and it is normal

#PART B
#1. Generate matrix 30 with normal distribution p = 0.2  (Bin(1;0.2))
binom <- rbinom(900, 1, 0.2)
dim(binom) <- c(30,30)
binom <- as.data.frame(binom)
View(binom)

#2 calculate mean of each col
means <- sapply(binom,mean,na.rm=TRUE)
#means are arount 0.2
stdevs <- sapply(binom,sd,na.rm=TRUE)

#3 st deviation of the means

sdOfMeans <- sd(means)
sdOfMeans

#st deviation from columns
sdOfMeans2 <- stdevs/sqrt(30)
sdOfMeans2

#4 Calculate exact standad error
sdExact <- sqrt(1*0.2*(1-0.2))
errorExact <- sdExact/sqrt(30)
errorExact

#5 New data generation from normal distribution
Normal <- rnorm(900, 5, 3)
dim(Normal) <- c(30,30)
Normal <- as.data.frame(Normal)
View(Normal)

#calculations
Normal.means <- sapply(Normal,mean,na.rm=TRUE)
Normal.means
#means are arount 5
Normal.stdevs <- sapply(Normal,sd,na.rm=TRUE)
Normal.stdevs

#st deviation of the means

Normal.sdOfMeans <- sd(Normal.means)
Normal.sdOfMeans

#st deviation from columns
Normal.sdOfMeans2 <- Normal.stdevs/sqrt(30)
Normal.sdOfMeans2

#6 rename to norm
norm <- Normal

#7 Make a correlation matrix for 10 variables
norm10 <- norm[,1:10]

norm.corr <- cor(norm10, y = NULL, use = "everything", method = "pearson")
norm.corr

rcorr(as.matrix(norm10), type="pearson")

#PART C
#1 Read in file
rm(list=ls())
tits <- read.csv("tits.csv")
str(tits)
boxplot(tits$wei~tits$SPE, xlab='Species', ylab='Weight')
boxplot(tits$egg~tits$SPE, xlab='Species', ylab='EggWeigh')

#2 Use summary comand
summary(tits)

#3 Use apply
apply(tits[,2:3], 2, FUN=mean, na.rm=TRUE)

#4 Calculating stadrad errors
#first make subset
TX <- subset(tits, SPE=='TX')
View(TX)
BM <- subset(tits, SPE=='BM')
Vies(BM)

#get all the means and Standard errors
mean.wei.TX <-(mean(TX$wei, na.rm=TRUE))
mean.wei.BM <-(mean(BM$wei, na.rm=TRUE))

mean.egg.TX <-(mean(TX$egg, na.rm=TRUE))
mean.egg.BM <-(mean(BM$egg, na.rm=TRUE))

se.wei.TX <-(sd(TX$wei, na.rm=TRUE))/(sqrt(length(na.omit(TX$wei))))
se.wei.BM <-(sd(BM$wei, na.rm=TRUE))/(sqrt(length(na.omit(BM$wei))))

se.egg.TX <-(sd(TX$egg, na.rm=TRUE))/(sqrt(length(na.omit(TX$egg))))
se.egg.BM <-(sd(BM$egg, na.rm=TRUE))/(sqrt(length(na.omit(BM$egg))))

#bind into vector
se.wei <- cbind(se.wei.TX, se.wei.BM)
se.egg <- cbind(se.egg.TX, se.egg.BM)
mean.wei <- cbind(mean.wei.TX, mean.wei.BM)
mean.egg <- cbind(mean.egg.TX, mean.egg.BM)

#build the plot for egg
library(Hmisc)
x <- c(1:2)
plot(x, mean.egg, type='b', lty=0, xlim=c(0.5, 2.5),
     xlab="Species", ylim=c(0, 14), ylab="Eggs", axes=FALSE)
axis(1, at=c(0.5, 1, 2, 2.5), labels=c("","Great tit", "Blue tit",  ""))

axis(2, at=seq(0, 14, 2))

errbar(x, mean.egg, mean.egg + se.egg, mean.egg - se.egg, add=TRUE)

#build the err plot for weight
plot(x, mean.wei, type='b', lty=0, xlim=c(0.5, 2.5),
     xlab="Species", ylim=c(10, 20), ylab="Weght", axes=FALSE)
axis(1, at=c(0.5, 1, 2, 2.5), labels=c("","Great tit", "Blue tit",  ""))

axis(2, at=seq(10, 20, 2))

errbar(x, mean.wei, mean.wei + se.wei, mean.wei - se.wei, add=TRUE)

#PART D
rm(list=ls())
setwd("~/Documents/courses/stats/Ex_3")
#1 open file forest
forest <- read.csv("forest.csv",header = TRUE, sep=';', row.names = NULL)
str(forest)
View(forest)

#2 build boxplots for different species grouped by study site
boxplot(forest$oak~forest$site, xlab="Site", ylab="Oak")

boxplot(forest$ald~forest$site, xlab="Site", ylab="Ald")

#3 descriptive statistics
summary(forest)

#4 make error bar
av.forest <- aggregate(forest, by=list(forest$site), FUN=mean, na.rm=TRUE)

var.forest <- aggregate(forest, by=list(forest$site), FUN=var, na.rm=TRUE)

site.names <- factor(av.forest$Group.1)

n.forest <- table(forest$site)
n.forest

se.forest <- sqrt(var.forest/n.forest)
se.forest <- cbind(se.forest, site.names)

xvals <- barplot(av.forest$oak, xlab="Site", ylab="Oak", ylim=c(0,1))
axis(1, at=xvals, labels=site.names)

arrows(xvals, av.forest$oak + se.forest$oak, xvals, 
       av.forest$oak - se.forest$oak, code=3, angle=90, length=0.1)

#5  Make a selection of cases such that only study sites with numbers less than, say, 30 are included
# using subset. Make some plotsto see how it affects your output.
forest.30 <- subset(forest,forest$site < 30)

str(forest.30)

boxplot(forest.30$oak~forest.30$site, xlab="Site", ylab="Oak")

boxplot(forest.30$ald~forest.30$site, xlab="Site", ylab="Ald")

av.forest.30 <- aggregate(forest.30, by=list(forest.30$site), FUN=mean, na.rm=TRUE)

var.forest.30 <- aggregate(forest.30, by=list(forest.30$site), FUN=var, na.rm=TRUE)

site.names <- factor(av.forest.30$Group.1)

n.forest.30 <- table(forest.30$site)
n.forest.30

se.forest.30 <- sqrt(var.forest.30/n.forest.30)
se.forest.30 <- cbind(se.forest.30, site.names)

xvals <- barplot(av.forest.30$oak, xlab="Site", ylab="Oak", ylim=c(0,1))
axis(1, at=xvals, labels=site.names)

arrows(xvals, av.forest.30$oak + se.forest.30$oak, xvals, 
       av.forest.30$oak - se.forest.30$oak, code=3, angle=90, length=0.1)
