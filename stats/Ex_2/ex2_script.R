#1 Read file GUD34
gud34 <- read.csv("GUD34.csv",sep=';')
View(gud34)
str(gud34)

#2 Read in file GUD56.csv
gud56 <- read.csv("GUD56.csv", sep=';')
View(gud56)
str(gud56)

#3 add the data via rbind

gud36 <- rbind(gud34, gud56)

#4 create agregated dataframe
aveGud <- aggregate(gud36$gud, by=list(gud36$pid), mean)
str(aveGud)
colnames(aveGud) <- c('pid','gud')
View(aveGud)

#5 Read file Fledge.csv and agragete into aveFLE
fledge <- read.csv("FLEDGE.csv",sep=';')
View(fledge)

aveFle <- aggregate(fledge$fledge, by=list(fledge$pid), mean)
colnames(aveFle) <- c('pid', 'fledge')
View(fledge)

#7 create merged dataframe
aveFleGud <- merge(aveGud, aveFle, by = 'pid', all=TRUE)

#8 make plot and calculate liniar regression

plot(aveFleGud$gud, aveFleGud$pid)

reg <- lm(aveFleGud$fledge~aveFleGud$gud)
summary(reg)
