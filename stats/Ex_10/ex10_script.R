#PART A

#1 Read Serp1 file

serp <- read.csv('~/Documents/courses/stats/Ex_10/serp1.csv', sep=';')
str(serp)
View(serp)

counts <- table(serp$soil, serp$leaf)
chisq.test(counts)

# the p-value is too big so data is independet

fisher.test(counts)
# also p-value is to high

# fisher one-tailed test
fisher.test(counts, alternative = 'greater')

# G-test 

library(DescTools)
GTest(counts)

#grouped barplot to visualize
barplot(counts, beside=TRUE, legend=rownames(counts))

# New data set Serp

serpN <- read.csv('~/Documents/courses/stats/Ex_10/serp.csv', sep=';')
View(serpN)
countsN <- matrix(serpN$n,nrow=2,ncol=2, byrow = TRUE)
rownames(countsN) <- c('s','n')
colnames(countsN) <- c('p','s')

# tests
chisq.test(countsN)
fisher.test(countsN)
GTest(countsN)

# PART B
rm(list=ls())
# 1 Acacia file read
acacia <- read.csv('~/Documents/courses/stats/Ex_10/acacia.csv',sep=';')

str(acacia)
View(acacia)

counts <- matrix(acacia$n,nrow=2,ncol=2, byrow= TRUE)
rownames(counts) <- c('spe1','spe2')
colnames(counts) <- c('yes','no')

chisq.test(counts)
fisher.test(counts)
GTest(counts)
barplot(counts, beside=T, legend=rownames(counts))

# 3 Calculate expected values
expVal <- matrix(NA,2,2)

expVal[1,1] <- sum(counts[1,])/sum(counts)*(sum(counts[,1])/sum(counts))*sum(counts)
expVal[2,1] <- sum(counts[2,])/sum(counts)*(sum(counts[,1])/sum(counts))*sum(counts)
expVal[1,2] <- sum(counts[1,])/sum(counts)*(sum(counts[,2])/sum(counts))*sum(counts)
expVal[2,2] <- sum(counts[2,])/sum(counts)*(sum(counts[,2])/sum(counts))*sum(counts)

# PART C
rm(list=ls())
# 1 Tiger.csv read
tiger <- read.csv('~/Documents/courses/stats/Ex_10/tiger.csv', sep=';')
str(tiger)
View(tiger)

# convert to appropriate counts matrix
counts <- cbind(tiger$n[c(1,3,5,7)],tiger$n[c(2,4,6,8)])
rownames(counts) <- c('s1','s2','s3','s4')
colnames(counts) <- c('col1','col2')

# expected freq
expVal <- matrix(NA,4,2)

expVal[1,1] <- sum(counts[1,])/sum(counts)*(sum(counts[,1])/sum(counts))*sum(counts)
expVal[2,1] <- sum(counts[2,])/sum(counts)*(sum(counts[,1])/sum(counts))*sum(counts)
expVal[3,1] <- sum(counts[3,])/sum(counts)*(sum(counts[,1])/sum(counts))*sum(counts)
expVal[4,1] <- sum(counts[4,])/sum(counts)*(sum(counts[,1])/sum(counts))*sum(counts)
expVal[1,2] <- sum(counts[1,])/sum(counts)*(sum(counts[,2])/sum(counts))*sum(counts)
expVal[2,2] <- sum(counts[2,])/sum(counts)*(sum(counts[,2])/sum(counts))*sum(counts)
expVal[3,2] <- sum(counts[3,])/sum(counts)*(sum(counts[,2])/sum(counts))*sum(counts)
expVal[4,2] <- sum(counts[4,])/sum(counts)*(sum(counts[,2])/sum(counts))*sum(counts)

expVal

# tests
GTest(counts)
chisq.test(counts)
fisher.test(counts)
barplot(t(counts), beside=T, legend=colnames(counts))

#PART D
rm(list=ls())

# 1 read pseudo 
pseudo <- read.csv('~/Documents/courses/stats/Ex_10/pseudo.csv', sep=';')
str(pseudo)
View(pseudo)

pseudo$id <- factor(pseudo$id)
library(lme4)
library(lmerTest)

pseudo.wrong <- lmer(prot~weight+(1|id), data=pseudo)
summary(pseudo.wrong)
anova(pseudo.wrong)

# Split data per individual and calculate linear model for each dependacy between 
# weight and protein content

coefs <- matrix(nrow=20,ncol=2)
for (i in 1:20) {
  ind <- subset(pseudo, id ==i)
  reg <- lm(ind$prot~ind$weight)
  coefs[i,] <- (reg$coefficients)
}

colnames(coefs) <- c('Intercept', 'Slope.of.weight')
coefs <- as.data.frame(coefs)
coefs

# 3 test that slope is not equal to 0
t.test(coefs$Slope.of.weight, mu=0)

# 4 Wilcox tets and other non-parametric
wilcox.test(coefs$Slope.of.weight, mu=0)

library(BSDA)
SIGN.test(coefs$Slope.of.weight, md= 0 )

summary(lm(Intercept~Slope.of.weight, data = coefs))

# Advanced
pseudo.right <- lmer(prot~(weight|id), data=pseudo)
summary(pseudo.right)

anova(pseudo.right)

rand(pseudo.right)
