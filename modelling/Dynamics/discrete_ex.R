# discret dynamic model for Beverton-Holt model

n_max = 50
n <- rep(0,n_max)
n[1] <- 10 # initial polulation size
# parameters
a <- 0.02 #dencity dependence 
lamd <- 1.02 #growth rate

for (t in 2:n_max) {
  #growth function goes here
  n[t] <- lamd * n[t-1] / (1 + a * n[t-1])
  
}

par(mfrow = c(1,2))
plot(n, type='b', xlab='time', ylab='population size')

# plot of equlibrium
Nt <- seq(0, 1, by=0.1)
Nt1 <- lamd * Nt / (1 + a * Nt)
plot(Nt1~Nt, type='l')
abline(0,1, col='green')
