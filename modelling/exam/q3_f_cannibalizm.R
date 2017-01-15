library(deSolve)

age_population_cannibalizm <- function(t, JA, P) {
  # extract vector content
  J <- JA[1]
  A <- JA[2]
  
  # calculate the growth rates:
  #dndt <- P$r0 * n - P$a * n * p
  #dJdt <- P$b * A - P$g * J - P$mu_j * (1 + P$k * J * A) * J 
  dJdt <- P$b * A - P$g * J - P$mu_j * J - P$k * J * A
  dAdt <- P$g * J - P$mu_0 * (1 + P$c * A - P$s * P$k * J * A) * A
  
  # output
  list(c(dJdt, dAdt))
}

# set the vector of time-points for the output
timevec <- seq(0, 50, by=1)

# list of parameters
P <- list(b = 1,
          g = 0.5,
          mu_j = 0.3,
          mu_0 = 0.2,
          c = 0.01,
          k = 0.002,
          s = 0.01)

# initial parameters
JA0 <- c(J=20, A=50)

# call the ode function
out <- ode(y = JA0, func = age_population_cannibalizm, times = timevec, parms = P)

par(mfrow = c(1,2))
maxPop <- max(out[,3], out[,2]) * 1.2;
#ploting populations in time plots
plot(out[,1], out[,2], type='l', col='green', main = 'Time plot',
     xlab='Time', ylab='Populations J, A', lwd = 2, ylim= c(0,maxPop))
lines(out[,1], out[,3], col='red', lwd = 2)
legend('topright', legend = c('Juvenile','Adult'),
       lty=c(1,1), col=c('green','red'), lwd = c(2,2))

#ploting phase-plane plot (J vs. A)

plot(out[,3], out[,2], type='l', main = 'Phase plot',
     xlab = 'Adult population', ylab='Juvenile population',
     xlim = c(0,maxPop), ylim=c(0,maxPop), lwd = 2)
