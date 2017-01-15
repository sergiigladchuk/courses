# Model fitting

Ringlets <- read.csv('~/Documents/courses/modelling/Dynamics/Ringlets.csv.csv')
str(Ringlets)
View(Ringlets)

plot(Ringlets, type='b')
# in log scale
plot(Ringlets$year, log(Ringlets$N), type='b')

# vectors with corresponding population sizes
Nt <- Ringlets$N[1:23]
Ntplus1 <- Ringlets$N[2:24]

plot(Nt, Ntplus1, xlab = 'N(t)', ylab='N(t+1)')

# fitting parameters
Rickerfit <- nls(Ntplus1~Nt*exp(r0*(1-Nt/K)),data=list(Nt=Nt), start=list(r0=1,K=100))
summary(Rickerfit)

points(Nt, fitted(Rickerfit), col='red')

# residuals againts population size
plot(Nt, residuals(Rickerfit))

#switch to log scale
rt <- log (Ntplus1/Nt)
plot(Nt,rt)

#removing 1976 year

Nt <- Ringlets$N[2:23]
Ntplus1 <- Ringlets$N[3:24]
rt <- log (Ntplus1/Nt)
plot(Nt,rt)

# Predicting rt to fit Ricker model

rt_fit <- nls (rt ~ r0 *(1-Nt/K), data=list(Nt=Nt), start=list(r0=1, K=100))
summary(rt_fit)

# plots to check predicted values and residuals
points(Nt, fitted(rt_fit),col='red')
plot(residuals(rt_fit))

#checking residuals
sqrt(sum(residuals(rt_fit)^2)/(length(rt)-2))

# Alternative model
rt_fit_alt <- nls(rt ~ (log(lambda) - b * log(1 + a * Nt)), data = list(Nt=Nt), start=list(lambda=1.1,a=0.001,b=1))
summary(rt_fit_alt)
points(Nt, fitted(rt_fit_alt),col='blue')
AIC(rt_fit)
AIC(rt_fit_alt)
# the first model is better
