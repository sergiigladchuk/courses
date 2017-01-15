#PART A
#1. Read in the file
rm(list=ls())
setwd("~/Documents/courses/stats/Ex_5")
ancex <- read.csv("ANCEX.csv",sep=';')
str(ancex)
View(ancex)

#test for normality
#build histograms
x <- ancex$XVAR
h <- hist(x, breaks = 10, col='red')
xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit,yfit, col='blue', lwd=2)

x <- ancex$YVAR
h <- hist(x, breaks = 10, col='red')
xfit <- seq(min(x), max(x), length=40)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit,yfit, col='blue', lwd=2)

#tests
shapiro.test(ancex$XVAR)
shapiro.test(ancex$YVAR)

qqnorm(ancex$XVAR)
qqline(ancex$XVAR)

qqnorm(ancex$YVAR)
qqline(ancex$YVAR)

#the series are not normal

#2 Pearson correlation between xvar and yvar
library(Hmisc)
rcorr(ancex$XVAR,ancex$YVAR, type="pearson")

#A correlation is a test for linear relationship. It always between -1 and
#+1.
#In this case, the correlation is close to +1, which implies a strong,
#positive linear relationship. See also the graph below.
#The p-value is very low (rounded to zero in this case), which means we can
#reject the null hypothesis.
#Assumptions of this test are:
#1. Data is continuous
#2. The relationship is linear (not non-linear)
#3. There are no outliers
#4. The variables are normally distributed

#3 Scatterplot
plot(ancex$YVAR~ancex$XVAR)
#yep it looks linear, the slope is positive,
# which was seen from correlation test

#4 Linear regression test
fit <- lm(ancex$YVAR~ancex$XVAR)
summary(fit)

#p-value is 9.05e-14
#H0 is xvar = 0

#5 print the equasion
'yvar = 2.9052 * xvar + 10.2875'

#6 extract confidence intervals
confint(fit, parm="ancex$XVAR", level=0.95)
confint(fit, parm="(Intercept)", level=0.95)
#The confidence interval for the slope β1 is [2.464, 3.347]. Zero is not
#included in that interval, which again confirms that the slope is
#significantly different from zero.

#7 getting significance val with ANOVA
fit.aov <- aov(ancex$YVAR~ancex$XVAR)
summary(fit.aov)

# 13.486**2 = F value in ANOVA. It is strong correlation

# Add regression line to the scatterplot
plot(ancex$YVAR~ancex$XVAR)
abline(fit)

#9 Analyzing residuals

ancex$resid <- residuals(fit)

#10 check normality of residuals

x <- ancex$resid
h <- hist(x, breaks = 10, col='red')
xfit <- seq(min(x), max(x), length=100)
yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit,yfit, col='blue', lwd=2)

#tests
shapiro.test(ancex$resid)

qqnorm(ancex$resid)
qqline(ancex$resid)

# yes they are

#11 check dependancy on xvar

plot(ancex$resid~ancex$XVAR)

#12 Check another assumption that variance of the residuals is indpendent
# of the variable xvar

plot(fit)

#13 New reverse regration

revFit <- lm(ancex$XVAR~ancex$YVAR)
summary(revFit)
summary(fit)

# p values are exactly the same
# regression slope is 0.29829
rcorr(ancex$XVAR,ancex$YVAR, type="pearson")


# PART B
rm(list=ls())
setwd("~/Documents/courses/stats/Ex_5")
#1 read RMfor data set

RMfor <- read.csv('RMfor.csv',sep=';')
str(RMfor)
View(RMfor)

#regression for lime on birch
fit.RMfor <- lm(RMfor$lime~RMfor$birch)
summary(fit.RMfor)
#save predicted values into one more varible
RMfor$pred.bir <- fitted(fit.RMfor)

#2 calculating MA and RMA
library(lmodel2)
fit.MA <- lmodel2(lime~birch, data=RMfor,
                  nperm=99)
fit.MA

plot(fit.MA, method="MA")

#3 check the diff between the slopes

#4 overlay scatter plot with lime and all three predicted 
# values against birch

resTable <- fit.MA$regression.results
View(resTable)

intMA <- resTable[2,2]
slopeMA <- resTable[2,3]
RMfor$pred.bir.MA <- intMA + RMfor$birch*slopeMA

intOLS <- resTable[1,2]
slopeOLS <- resTable[1,3]
RMfor$pred.bir.OLS <- intOLS + RMfor$birch*slopeOLS

intSMA <- resTable[3,2]
slopeSMA <- resTable[3,3]
RMfor$pred.bir.SMA <- intSMA + RMfor$birch*slopeSMA

#plot all of the predictions
plot(RMfor$pred.bir~RMfor$birch)
points(RMfor$pred.bir.MA~RMfor$birch, col="green")
points(RMfor$pred.bir.OLS~RMfor$birch, col="red")
points(RMfor$pred.bir.SMA~RMfor$birch, col="blue")
