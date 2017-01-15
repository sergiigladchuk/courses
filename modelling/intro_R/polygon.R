phi <- seq(0, 2*pi, length.out=7)
x <- cos(phi)
y <- sin(phi)

plot(NA,NA,xlim=c(-1,1), ylim=c(-1,1), type='n', xlab='',ylab='')
polygon(x,y,col='red')

#op check
op <- par(mfrow=c(1,2))

plot(sin, -5, 5, main='The sinus function')
plot(cos, -5, 5, main='The cosinus function')

par(op)

#3D Graphics

X <- matrix(runif(100),10,10)
image(X)
contour(X)
persp(X, phi=40)
