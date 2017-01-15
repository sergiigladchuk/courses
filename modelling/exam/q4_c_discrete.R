
# parameters
w <- 1.2; #fitness 
N <- 100; #population size

p <- 1/(2 * N); # starting p-val

for (t in 2:1000) {
  
  w.mean <- w * p[t-1] + 1 - p[t-1];
  sigma <- 1/w.mean * sqrt(w/(2*N)*p[t-1]*(1 - p[t-1]));
  p <- c(p, 0);
  #dynamix function
  p[t] <- w / (w.mean) * p[t-1] + rnorm(1,mean=0,sd = sigma);
  
  if(p[t] <= 0 || p[t] >= 1) {
    break;
  }
}
plot(p, type='b', xlab='generations', ylab='gene relative frequency')