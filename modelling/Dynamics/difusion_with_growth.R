d <- 0.1
n <- 100
N <- rep(0,n)
N[50] <-100 #initial generation
N[60] <- 80 #other generation
tmax <- 50

#changes for local Wicker groth (or Logistics)
r <- 1
K <- 1000

# creation of universe, the particles on the edge dissapear
for(t in 2:tmax) {
  Nnew <- rep(0,n)
  Nnew[1] <- N[1]*exp(r*(1-N[1]/K) -2*d*N[1] + d*N[2])
  Nnew[n] <- N[n]*exp(r*(1-N[n]/K) -2*d*N[n] + d*N[n-1])
  
  #loop throug all the other cells
  for(i in 2:(n-1)) {
    Nnew[i] <- N[i]*exp(r*(1-N[i]/K) -2*d*N[i] + d*N[i-1]+ d*N[i+1])
    
  }
  # set 
  N <-Nnew
}

plot(N)