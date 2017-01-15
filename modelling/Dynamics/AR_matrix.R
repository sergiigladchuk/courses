#AR1 (Auto-Regresive process)

a <- 0.8
sigma <- 1
tmax <- 100
n <- 100
X <- matrix(0,n, tmax)
X[,1] <- 20

par(mfrow = c(1,1))
for (t in 2:tmax) {
  X[,t] <- a * X[,t-1] + sigma*rnorm(1) # rnorm gives noise
}

plot(x=NA, y=NA, type='n', xlim=c(0,tmax), ylim=range(X))

for(i in 1:n){
  lines(1:tmax,X[i,], col=i)
}