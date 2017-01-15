
# vecror of population adults
A = c(0:300);
#constrants
b <- 1;
g <- 0.5;
mu_j <- 0.3;
mu_0 <- 0.2;
c <- 0.01;

isocline1 <- function(vec){
  line1 <- b*vec/(g+mu_j);
  line1;
}
isocline2 <- function(vec){
  line2 <- (mu_0*vec + mu_0 * c * vec**2) / g;
  line2;
}

#equilibrium
J_eq <- b*(g*b-mu_0*g-mu_0*mu_j)/((g+mu_j)*(mu_0*mu_j*c+mu_0*c*g));
A_eq <- (g*b-mu_0*g-mu_0*mu_j)/(mu_0*mu_j*c+mu_0*c*g);

#plot
par(mfrow = c(1,1))
plot (A, isocline1(A), type = 'l', col = 'green',
      xlab = 'Adult population', ylab = 'Juvenile population')
lines(A, isocline2(A), col = 'red')
points(A_eq,J_eq, pch=19)
legend('bottomright', legend = c('J isocline','A isocline'),
       lty=c(1,1), col=c('green','red'))
