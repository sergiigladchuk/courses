
LV_isoclines <- function(r, a, mu) {
  #first line coordinates for n population
  # n = 0
  # p = r/a
  max = 5
  x1 = c (0, r/a, max)
  y1 = c(mu/a, mu/a, mu/a)
  # plot(x1, y1, type='l', xlim=c(0,max),ylim=c(0,max),
  #      xlab='n - prey densities', ylab = 'y - predator densities')
  lines(x1, y1, col='blue')
  
  #secondl line for p population
  
  x2 = c(r/a, r/a, r/ a)
  y2 = c(0, mu/a, max)
  lines(x2, y2, col='red')
}
