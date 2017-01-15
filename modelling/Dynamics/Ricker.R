#Ricker

r <- 2.7
K <- 100
sigma <- 0.3
tmax <- 500

n <- rep(0, tmax)
n[1] <- 20

par(mfrow = c(1,1))
for (t in 2:tmax) {
  n[t] <- n[t-1]*exp(r*(1-n[t-1]/K) + sigma*rnorm(1)) # rnorm gives noise
}

plot(n, type='l')
var(n)
