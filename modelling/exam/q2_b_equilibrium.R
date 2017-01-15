library(deSolve)

#define the drowth function

populationGrowth <- function(t, n, P) {
  dndt <- P$r0 * n * (1 - n**2/P$K**2)
  list(dndt)
  
}

# set the vector of time-points for the output
timevec <- seq(0, 20, by=0.1)

# list of parameters

P <- list(r0 = 1.3, K = 100)

n0 <- 2 #initial population size

# call the ode function
n <- 1:110
out <- ode(y = n0, func = populationGrowth, times = timevec, parms = P)
dndt <- unlist(populationGrowth(1, n, P))
par(mfrow = c(1,2))
plot(out[,1], out[,2], type='l', main = 'Population growth', xlab='Time', ylab='Population size')
plot(n, dndt, type='l', main='Growth Function', xlab='n', ylab='dn/dt',col='red')
abline(0,0)
