
q4_d_function <- function(N, w){
  simNum <- 1000
  simulations <-rep(0,simNum)
  
  p <- 1/(2 * N); # starting p-val
  for (s in 1:simNum) {
    for (t in 2:1000) {
      
      w.mean <- w * p[t-1] + 1 - p[t-1];
      sigma <- 1/w.mean * sqrt(w/(2*N)*p[t-1]*(1 - p[t-1]));
      p <- c(p, 0);
      #dynamix function
      p[t] <- w / (w.mean) * p[t-1] + rnorm(1,mean=0,sd = sigma);
      
      if(p[t] <= 0) {
        simulations[s] <- 0;
        break;
      }
      if(p[t] >= 1) {
        simulations[s] <- 1;
        break;
      }
    }
  }
  # output of fixation probability
  return(sum(simulations)/simNum)
}