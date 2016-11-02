#1. Analyzed the file iin excel

#set working directory to 
setwd("~/Documents/stats_course/Ex_1")
#2. Read file into R from converted csv
tits <- read.csv("tits.csv")

#3 inspect data by calling tits
tits
View(tits)

#4 check structure
str(tits)

#5 add names column to tits table

tits$name <- ifelse(tits$SPE == "TX","Great tit","Blue tit")

#6 other way of adding names
tits$name[tits$SPE == "BM"] <- "Blue tit"
tits$name[tits$SPE == "TX"] <- "Great tit"

#7 descriptive statistics
sapply(tits, mean, na.rm=TRUE) #mean
sapply(tits, var, na.rm=TRUE) #variance
sapply(tits, med, na.rm=TRUE) # max - does not work for this set

summary(tits) #nice view

#8 Get separate values from psych pachage
library(psych)
describeBy(tits$wei, tits$SPE)

#9 T-test for differences between species in weight and clutch size
t.test(tits$wei~tits$SPE)
t.test(tits$egg~tits$SPE)

#10 Make a scatterplot of egg by weight
plot(tits$wei, tits$egg) #simple plot
plot(tits$wei, tits$egg, xlab = "Bird weight", ylab = "Egg weight") #with nice names
plot(tits$wei, tits$egg, xlab = "Bird weight", ylab = "Egg weight", col=as.numeric(tits$SPE)) #with different colors

#11 Assign colors by making subset
TX <- subset(tits, tits$SPE == "TX")
BM <- subset(tits, tits$SPE == "BM")

#12 create plot based on subsets
plot (TX$wei, TX$egg)
points(BM$wei, BM$egg)

#fixing limits
plot (TX$wei, TX$egg, xlim = c(0,20), ylim = c(0, 20), col="black")
points(BM$wei, BM$egg, col="blue")

#adding regression lines
regWhole <- lm(tits$egg~tits$wei) #for whole set
plot(tits$wei, tits$egg, xlab = "Bird weight", ylab = "Egg weight", col=as.numeric(tits$SPE))
abline(regWhole)

regTX <- lm(TX$egg~TX$wei) #for TX set
plot(TX$wei, TX$egg, xlab = "Bird weight", ylab = "Egg weight")
abline(regTX)

regBM <- lm(BM$egg~BM$wei) #for BM set
plot(BM$wei, BM$egg, xlab = "Bird weight", ylab = "Egg weight")
abline(regBM)

#13 change properties of line
regBM <- lm(BM$egg~BM$wei) #for BM set
plot(BM$wei, BM$egg, xlab = "Bird weight", ylab = "Egg weight")
abline(regBM, col="red", lty="dashed")

#14 make scaterplot from xydata
xydata <- read.csv("xydata.csv")
View(xydata)
plot(xydata$x, xydata$y)

#plot y versus ln(x)
xydata$logx <- log(xydata$x)
plot(xydata$logx, xydata$y)
