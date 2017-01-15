x <- rnorm(20)
plot(1:20, x, type='b')

for(i in 2:19) {
  if (x[i-1] < x[i] && x[i+1] < x[i]) {
    cat('Found maximum at',i,fill=T)
    points(i,x[i],col='red')
  }
}