# PART A

# 1. read PCA.csv

pca <- read.csv('~/Documents/courses/stats/Ex_12/PCA.csv', sep=';')
str(pca)
View(pca)

pcdata <- na.omit(pca[10:19])

fit <- princomp(pcdata, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit,type='lines')
biplot(fit)

# the longer arrows the higher correlation with the factor

# 2 Another PCA with only age, wing, tail, weight, tarsv
pcdata2 <- na.omit(pca[c(9,10,11,12,13,16)])
fit2 <- princomp(pcdata2[2:6], cor=TRUE)
summary(fit2)
loadings(fit2)
pcscores <- fit2$scores
plot(fit2,type='lines')
biplot(fit2)

# 3 test if there is a difference between sexes with respect to the first principal component

dim(pcdata2)
dim(pcscores)
pctest <- cbind(pcdata2,pcscores)
head(pctest)

sex.test1 <- lm(Comp.1~sex, data=pctest)
summary(sex.test1)
anova(sex.test1)
boxplot(pctest$Comp.1~pctest$sex, ylab='PC1')

# 4 test sexes differences for second PC

sex.test2 <- lm(Comp.2~sex, data=pctest)
summary(sex.test2)
anova(sex.test2)
boxplot(pctest$Comp.2~pctest$sex, ylab='PC2')

# PART B
#1 Predictig sex of the bird from its body measurements.

disc <- pca[c(9,11,12)]
disc <- na.omit(disc)
library(MASS)
fit <- lda(sex~wing + tarsv, data=disc, CV=TRUE)
ct <- table (disc$sex, fit$class)
diag(prop.table(ct,1))
sum(diag(prop.table(ct)))
# more than 90% of all birds can be classified using these traits

#2 Plot tarsv as a function of wing
males <- subset(disc, disc$sex == 'male')
females <- subset(disc, disc$sex == 'female')

plot(males$tarsv~males$wing, xlab = "Wing length", ylab='Tarsus', xlim = c(90,110), ylim = c(27,37), col='blue')
points(females$tarsv~females$wing, col='red')

#3 The same plot but sex come from descriminant prediction

disc$pred.sex <- fit$class
pmales <- subset(disc, disc$pred.sex == 'male')
pfemales <- subset(disc, disc$pred.sex == 'female')

plot(pmales$tarsv~pmales$wing, xlab = "Wing length", ylab='Tarsus', xlim = c(90,110), ylim = c(27,37), col='blue')
points(pfemales$tarsv~pfemales$wing, col='red')

#4 Adding more predictors to check the improvement of classification
disc2 <- pca[c(9:19)]
disc2 <- na.omit(disc2)
str(disc2)
fit.total <- lda(sex~age + wing + tarsv + tail + wproj + fett + weigh + bill + bihe + biwi, data=disc2, CV=TRUE)
ct2 <- table (disc2$sex, fit.total$class)
diag(prop.table(ct2,1))
sum(diag(prop.table(ct2)))
# only 2% improvement so important thing is to choose traits wisely

# PART C
rm(list=ls())

# 1 read foxdens20
fox <- read.csv('~/Documents/courses/stats/Ex_12/foxdens20.csv', sep=';')
str(fox)
View(fox)

# plot two most adundant species
no <- subset(fox, fox$foxden == 0)
yes <- subset(fox, fox$foxden == 1)
plot (no$fest_ovi~no$care_big, xlab='C. bigelowii', ylab = 'F. ovina',  xlim = c(0, 20), ylim = c(0,20), col = 'blue')
points(yes$fest_ovi~yes$care_big, col = 'green')

# 2. Discriminant analysis with 4 most abundant plants
disc <- fox[2:6]
str(disc)
fit <- lda(foxden~care_big+fest_ovi+rume_ace+vacc_vit, data=disc, CV=TRUE)
ct <- table (disc$foxden, fit$class)
diag(prop.table(ct,1))
sum(diag(prop.table(ct)))

# 3 What site was clasified wrongly
disc.class <- fit$class
fox.check <- fox[,1:2]
fox.check$disc.class <- disc.class
fox.check

# 4 Let's cluster

d <- dist(as.data.frame(fox[3:22]), method = 'euclidean')
fit <- hclust(d, method='ward.D')
plot(fit)

# 5 Cluster precisely with kmeans
foxdata = fox[3:22]
fit <- kmeans(foxdata, 2)
aggregate(foxdata,by=list(fit$cluster), FUN=mean)
fox.check$clust.class <- fit$cluster
fox.check
