#SIR model

SIRfunc <- function(t, SIR, P) {
  beta <- 0.01 #contact rate
  rho <- 0.1 #recovery rate
  b <- 0.01 #birth rate
  d <- 0.01 #death rate
  v <- 0.01
  
  S <- SIR[1]
  I <- SIR[2]
  R <- SIR[3]
  
  dSdt <- -beta*S*I + b*(S + I + R) - d * S -v*S
  dIdt <- beta*S*I - rho*I - d * I
  dRdt <- rho * I - d * R + v*S
  
  list(c(dSdt,dIdt,dRdt))
  
}

tt <- seq(0,1000, by=1)
SIR0 <- c(S=1, I=1, R=99) #starting values of Succeptible (all polulation), infected and resestant
library(deSolve)
result <- ode(y=SIR0, func=SIRfunc, parms=NA, times=tt)
plot(result)
