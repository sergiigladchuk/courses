# R Perceptron based on perceptron.m 


StepFunction = function(Input) {
  
  if(Input <= 0) {
    Outsignal = 0
  } else {
    Outsignal = 1
  }
  return(Outsignal)
  
}

INPUT = matrix(1:8,ncol=2) # set up empty matrix to fill 

INPUT[1,1] = 0 # input set 1
INPUT[1,2] = 0
INPUT[2,1] = 1 # input set 2
INPUT[2,2] = 0
INPUT[3,1] = 0 # input set 3
INPUT[3,2] = 1
INPUT[4,1] = 1 # input set 4
INPUT[4,2] = 1

CORRECT_OUTPUT = c(1,0,0,1) # correct output

NUM_EXAMPLES = 4
NUM_INPUTS = 2

eta = 0.8
i = 1                                      # i must start as a positive number
Count = 0                                  # counter for the number of batches (a batch = all four examples)
NUMTIMES = 0


W = runif(3,-3,3) # initialize random uniform weights between -3 and 3
print(W)
TOTAL_ERROR = c() # set up empty vector to fill the error
SaveOutput = c() # empty vector to save output


while(i > 0 && NUMTIMES < 25) {                 # task will be solved when i = 0  
  
  
  for(Example in 1:NUM_EXAMPLES)  {          # four examples of pairwise input 0 or 1
    
    SumIP = 0                               # sums the product from the three weights * signals
    for(Weight in 1:(NUM_INPUTS + 1)) {          # Weight is counter for weights (2 inputs + bias)
      
      if(Weight == NUM_INPUTS + 1) {          # if weight is bias
        Signal = 1                      # get bias signal
      } else {
        Signal = INPUT[Example, Weight] # get signal from signal vector
      }
      
      IP = Signal*W[Weight]         # inner product
      SumIP = SumIP + IP                 # sum for all weights
    }
    
    Output = StepFunction(SumIP)           #check if incoming signal produces output in transfer function   
    Error = Output - CORRECT_OUTPUT[Example] # difference between expected and actual output (=supervision!)
    
    TOTAL_ERROR[Example] = abs(Error)
    
    if(Output > CORRECT_OUTPUT[Example]) {          # check if weights should be decreased
      W[1] = W[1] - abs(Error*INPUT[Example,1]*eta)      # adjust weight 1
      W[2] = W[2] - abs(Error*INPUT[Example,2]*eta)      # adjust weight 2
      W[3] = W[3] - abs(Error*eta)                      # adjust weight 3 (=bias)
    } 
    
    if(Output < CORRECT_OUTPUT[Example] ) {      # check if weights should be increased
      
      W[1] = W[1] + abs(Error*INPUT[Example,1]*eta)   # adjust weight 1
      W[2] = W[2] + abs(Error*INPUT[Example,2]*eta)   # adjust weight 2
      W[3] = W[3] + abs(Error*eta)                      # adjust weight 3 (=bias) 
      
    }
    
    if(Example == NUM_EXAMPLES && sum(TOTAL_ERROR) == 0) i = 0 # if we are at example 4 with no error -> task is done, exit 
    
  }
  
  NUMTIMES = NUMTIMES + 1
  Count = Count + 1
  SaveOutput[Count] = sum(TOTAL_ERROR)/NUM_EXAMPLES
}

slope = W[1]/(-W[2])
intercept = W[3]/(-W[2])

par(mfrow = c(1,2)) 
plot(1:NUMTIMES,SaveOutput,type="l",ylab="Error",xlab="iterations",
     main="error after n interations",ylim=c(0,1)) # plot error rate at the end
plot(INPUT, col = as.factor(CORRECT_OUTPUT),pch=19,xlim=c(-0.5,1.5),
     main="decision boundary") # plot decision boundary
abline(intercept,slope,lwd=2)

cat(NUMTIMES)