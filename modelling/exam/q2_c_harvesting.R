library(deSolve)
#define the drowth function with fishing
populationGrowth <- function(t, n, P) {
  dndt <- P$r0 * n * (1 - n**2/P$K**2) - n*P$h
  list(dndt)
}
# set the vector of time-points for the output
timevec <- seq(0, 20, by=0.1)
# list of parameters
P <- list(r0 = 1.5, K = 100, h = 1)
n0 <- 50 #initial population size
# call the ode function
out <- ode(y = n0, func = populationGrowth, times = timevec, parms = P)
n <- 1:110
dndt <- unlist(populationGrowth(1, n, P))
par(mfrow = c(1,2))
plot(out[,1], out[,2], type='l', main = 'Population growth', xlab='Time', ylab='Population size')
plot(n, dndt, type='l', main='Growth Function', xlab='n', ylab='dn/dt',col='red')
abline(0,0)
