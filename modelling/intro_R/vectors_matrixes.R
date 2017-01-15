#test matrixes and vectors

#vectors
x <- c(2,1,4)

3*x

y <- c(1,0,1)

x*y

sqrt(sum(x*y))

#matrix 
A <- matrix(c(3,7,1,8), 2, 2)
View(A)
A

B <- matrix(c(0,2,2,-1),2,2)
B

A <- matrix(c(2,-1),1,2)
A
B <- matrix(c(1,0,4,2,7,1),2,3)
B
C <- matrix(c(-1, 0, 2), 3,1)
C
A %*% B %*% C
