#Define random walk

print('Enter the lenght of walk')
lenOfWalk <- scan(nmax=1, what=numeric())


for (i in 1:10) {

  x <- seq(100)
  y <- seq(100)

  y[1] <- 0
  for (j in 1 : (lenOfWalk - 1)) {
    y[j + 1] <- y[j] + rnorm(1)
    
    
  }
  #create plot
  if(i == 1) {
    plot(x, y, type='l', ylim=c(-20,20))
    
  }
  #add to plot
  lines(x, y)
}
