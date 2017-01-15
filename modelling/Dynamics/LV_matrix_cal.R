# function to create matrix
LV_matrix <- function(r, a, mu){
  
  X <- matrix(c(0, r, -1 * mu, 0),nrow = 2)
  X
  
  
}

LV_matrix(P$r,P$a,P$mu)

LV_Cycle_Period <- function (r, mu) {
  T <- 2 * pi / sqrt(mu * r)
  T
  
}