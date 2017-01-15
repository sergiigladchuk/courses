d <- 0.1
n <- 100
N <- rep(0,n)
N[50] <-100 #initial generation
N[60] <- 80 #other generation
tmax <- 2000

# creation of universe, the particles on the edge dissapear
for(t in 2:tmax) {
  Nnew <- rep(0,n)
  Nnew[1] <- (1-2*d)*N[1] + d*N[2]
  Nnew[n] <- (1-2*d)*N[n] +d*N[n-1]
  
  #loop throug all the other cells
  for(i in 2:(n-1)) {
    Nnew[i] <- (1-2*d)*N[i] + d*N[i-1] + d*N[i+1]
    
  }
  # set 
  N <-Nnew
}

plot(N)