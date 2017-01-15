
LV_isoclines_log <- function(r0, a, mu, K) {
  #first line coordinates for n population
  # n = 0
  # p = r/a
  max = 5
  x1 = c (0, r0/a, max)
  y1 = c(mu/a, mu/a, mu/a)
  # plot(x1, y1, type='l', xlim=c(0,max),ylim=c(0,max),
  #      xlab='n - prey densities', ylab = 'y - predator densities')
  lines(x1, y1, col='blue')
  
  #secondl line for p population
  y2 = seq(0, max, by=0.1)
  x2 = r0/a + y2 * r0 / (a * K)
  
  lines(x2, y2, col='red')
  #abline(r0/a, r0/(a*K), col='red')

}

