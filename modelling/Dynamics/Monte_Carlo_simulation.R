# script to simulate population size
#parameters

r0 <- 1.004 #from model
K <- seq(4, 200, by=4) #
sigma <- 0.3445

#simulation
years <- 100
num_sim <- 1000
ext_vec <- rep(0,length(K))
for (Ki in 1:length(K)){
  M <- matrix(0, years, num_sim)
  M[1,] <- 35 # initial population
  
  ext_num <- 0
  for (c in 1 : num_sim) {
    
    for (r in 2 : years) {
      M[r,c] <- M[r-1,c]*exp(r0*(1-M[r-1,c]/K[Ki]) + sigma * rnorm(1))
      if (M[r,c] < 2) {
        ext_num <- ext_num + 1;
        break;
      }             
    }
  }
  ext_risk <- ext_num/num_sim
  
  #plot(M[,1], type='b', xlim=c(0,years),ylim=c(0, max(K * 2 , M[1,1])))
  plotsMax <- 20
  #for(i in 2 : plotsMax) {
   # points(M[,i], col = i, type='b')
  #}
  ext_vec[Ki] <- ext_risk
}
plot(ext_vec~K, type='b')
