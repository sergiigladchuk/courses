library(deSolve)

LV_sys_log <- function(t, np, P) {
  # extract vector content
  n <- np[1]
  p <- np[2]
  
  # calculate the growth rates:
  #dndt <- P$r0 * n - P$a * n * p
  dndt <- P$r * n * (1 - n / P$K) - P$a * n * p
  dpdt <- P$a * n * p - P$mu * p
  
  # output
  list(c(dndt, dpdt))
  
}

# set the vector of time-points for the output
timevec <- seq(0, 20, by=0.01)

# list of parameters

P <- list(r0 = 4, a = 2, mu = 5, K = 20)

# initial parameters
np0 <- c(n=2, p=2)


# call the ode function

out <- ode(y = np0, func = LV_sys_log, times = timevec, parms = P)

par(mfrow = c(1,2))
#ploting populations in time plots
plot(out[,1], out[,2], type='l', col='blue', main = 'Time plot',
     xlab='Time', ylab='Populations n,p')
lines(out[,1], out[,3], col='red')

#ploting phase-plane plot (p vs. n)
plot(out[,3], out[,2], type='l', main = 'Phase plot',
     xlab = 'y - predator densities', ylab='n - prey densities')
source('~/Documents/courses/modelling/Dynamics/LV_isoclines_logistics.R')
LV_isoclines_log(P$r0,P$a,P$mu,P$K)
