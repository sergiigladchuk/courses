#AR1 (Auto-Regresive process)

a <- 0.8
sigma <- 1
tmax <- 100
x <- rep(0,tmax)
x[1] <- 20

par(mfrow = c(1,1))
for (t in 2:tmax) {
  x[t] <- a * x[t-1] + sigma*rnorm(1) # rnorm gives noise
}

plot(x, type='l')