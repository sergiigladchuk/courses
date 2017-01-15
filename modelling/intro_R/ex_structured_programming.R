#1 mymax function

mymax <- function(a,b) {
  if (a > b) {
    a
  } else {
    b
  }
}

#2 weighted sub func

myWeightSub <- function(inVec, weiVec) {
  multVec = inVec * weiVec
  sum(multVec)
}

#3 fibonochi function
myFibonachi <- function(n) {
  
  fibo <- c(1,1)
  gold <- c(((1 + sqrt(5))/2)^1/sqrt(5),((1 + sqrt(5))/2)^2/sqrt(5))
  
  for (i in 3:n){
    fibo <- c(fibo, fibo[i-1] + fibo[i-2])
    gold <- c(gold, ((1 + sqrt(5))/2)^i/sqrt(5))
  }
  
  #plot fibo
  plot(fibo,type = 'b')
  
  #add golder ratio line
  points(gold,col='red',pch=25)
}

#4 plotmax function
plotmax <- function(n) {
  #random vector
  randVec <- runif(n)
  
  rawMatrix = NA
  
  #plots blue line
  plot(randVec, type='l', col='blue')
  
  matrixStarted <- FALSE
  
  #highlitning with red crosses
  for (i in 2:(n-1)) {
    if (randVec[i - 1] < randVec[i] && randVec[i + 1] < randVec[i]) {
      points(i, randVec[i], pch = 4, col='red')
      
      
      #add to matrix
      if (!(matrixStarted)) {
        rawMatrix <- c(i, randVec[i]);
        matrixStarted <- TRUE;
      }else{
        rawMatrix <- c(rawMatrix, i, randVec[i]);
      }
    }
  }
  #print out matrix
  convMatrix <- matrix(rawMatrix, nrow = length(rawMatrix) / 2, byrow=TRUE)
  convMatrix
  
}

# 5 buble sort
X <- matrix(runif(9), 3, 3)
X

sortrows <- function (A, c) {
  repeat {
    done <- TRUE; #set the flag
    for (i in 1 : (length(A[,c]) - 1)) {
      if (A[i, c] > A[i+1, c]) {
        swapitem <- A[i, c];
        A[i, c] <- A[i + 1, c];
        A[i + 1, c] <- swapitem;
        done <- FALSE;
        
      }
    }
    if (done){
      break;
    }
  }
  return(A)
}