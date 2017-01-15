library(deSolve)

age_population <- function(t, JA, P) {
  # extract vector content
  J <- JA[1]
  A <- JA[2]
  
  # calculate the growth rates:
  dJdt <- P$b * A - P$g * J - P$mu_j * J
  dAdt <- P$g * J - P$mu_0 * (1 + P$c * A) * A
  
  # output
  list(c(dJdt, dAdt))
}

# set the vector of time-points for the output
timevec <- seq(0, 50, by=1)

# list of parameters
P <- list(b = 0.2,
          g = 0.5,
          mu_j = 0.3,
          mu_0 = 0.2,
          c = 0.01)

# initial parameters
JA0 <- c(J=20, A=50)

# call the ode function
out <- ode(y = JA0, func = age_population, times = timevec, parms = P)

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

#add isoclines
isocline1 <- function(vec, P){
  line1 <- P$b*vec/(P$g+P$mu_j);
  line1;
}
isocline2 <- function(vec, P){
  line2 <- (P$mu_0*vec + P$mu_0 * P$c * vec**2) / P$g;
  line2;
}
vec <- seq(0,maxPop)
lines (vec, isocline1(vec, P), col = 'green', lty = 5)
lines(vec, isocline2(vec, P), col = 'red', lty = 5)

legend('topleft', legend = c('trajectory', 'J isocline','A isocline'),
       col=c('black','green','red'), lwd = c(2,1,1), lty = c(1,5,5))

