library(deSolve)

LV_sys <- function(t, np, P) {
  # extract vector content
  n <- np[1]
  p <- np[2]
  
  # calculate the growth rates:
  dndt <- P$r * n - P$a * n * p
  dpdt <- P$a * n * p - P$mu * p
  
  # output
  list(c(dndt, dpdt))
  
}

# set the vector of time-points for the output
timevec <- seq(0, 20, by=0.01)

# list of parameters

P <- list(r = 4, a = 2, mu = 5)

# initial parameters
np0 <- c(n=2, p=2)


# call the ode function

out <- ode(y = np0, func = LV_sys, times = timevec, parms = P)

par(mfrow = c(1,2))
#ploting populations in time plots
plot(out[,1], out[,2], type='l', col='blue', main = 'Time plot',
     xlab='Time', ylab='Populations n,p')
lines(out[,1], out[,3], col='red')

#ploting phase-plane plot (p vs. n)
plot(out[,3], out[,2], type='l', main = 'Phase plot',
     xlab = 'y - predator densities', ylab='n - prey densities')
source('~/Documents/courses/modelling/Dynamics/LV_isoclines.R')
LV_isoclines(P$r,P$a,P$mu)
