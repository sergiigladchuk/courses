library(deSolve)

#define the drowth function with fishing

logisticGrowthWithFishing <- function(t, n, P) {
  dndt <- P$r0 * n * (1 - n/P$K) - P$h * n
  list(dndt)
  
}

# set the vector of time-points for the output
timevec <- seq(0, 20, by=0.1)

# list of parameters

P <- list(r0 = 1.3, K = 100, h = 0.5)

n0 <- 2 #initial population size

# call the ode function
n <- 1:150
out <- ode(y = n0, func = logisticGrowthWithFishing, times = timevec, parms = P)
dndt <- unlist(logisticGrowthWithFishing(1, x, P))
par(mfrow = c(1,2))
plot(out[,1], out[,2], type='l', main = 'Logistic growth with fishing')
plot(x, dndt, type='l', main='dn/dt vs. n')
