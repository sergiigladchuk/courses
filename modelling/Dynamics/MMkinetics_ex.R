library(deSolve)

#define the drowth function with fishing

logisticGrowthWithFishing <- function(t, c, P) {
  dcdt <- -P$Kmax * c /(P$Kn + c) * P$n
  list(dcdt)
  
}

# set the vector of time-points for the output
timevec <- seq(0, 20, by=0.1)

# list of parameters

P <- list(Kmax = 5, Kn = 40, n = 10)

c0 <- 200 #initial concentration

# call the ode function
c <- 150:1
out <- ode(y = c0, func = logisticGrowthWithFishing, times = timevec, parms = P)
dcdt <- unlist(logisticGrowthWithFishing(1, c, P))
par(mfrow = c(1,2))
plot(out[,1], out[,2], type='l', main = 'Michaelis-Menten kenetics')
plot(c, dcdt, type='l', main='dc/dt vs. c')
