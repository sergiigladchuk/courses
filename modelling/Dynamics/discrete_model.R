# discret dynamic model

n <- rep(0,50)
n[1] <- 10 # initial polulation size
# parameters
r <- 2.8
K <- 100

for (t in 2:50) {
  #growth function goes here
  n[t] <- n[t-1] * exp(r*(1-n[t-1]/K))
  
}

plot(n, type='b')