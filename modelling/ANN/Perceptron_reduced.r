
# perceptron reduced code

# set.seed(3)

x = c(0,1,0,1) # input on x axis
y = c(0,0,1,1) # input on y axis

W = runif(3,-3,3) # 3 randomly chosen weights between 3,-1
correct_output = c(1,0,0,0) # OR
# correct_output = c(0,0,0,1) # AND
# correct_output = c(0,1,1,0) # XOR

eta = 0.8 #self test

SaveOutput = c()
for(j in 1:25) {

  Total_Error = c()
  
  for(i in 1:4) { # go through all the x,y values
  
  Activation = W[1]*x[i] + W[2]*y[i] + W[3] # W[3] is the bias; sum all the inputs together to "Activate" the neuron
  
  Output = ifelse(Activation <= 0,0,1) # if SumIP is less than 0 (negative) classify as 0, else classify as 1; Threshold at zero
  Error = Output - correct_output[i] 
  Total_Error[i] = abs(Error)
  
  
  if(Output > correct_output[i]) {          # check if weights should be decreased
    W[1] = W[1] - abs(Error*x[i]*eta)      
    W[2] = W[2] - abs(Error*y[i]*eta)      
    W[3] = W[3] - abs(Error*eta)  
  } 
      
  if(Output < correct_output[i]) {      # check if weights should be increased
    W[1] = W[1] + abs(Error*x[i]*eta)   
    W[2] = W[2] + abs(Error*y[i]*eta)   
    W[3] = W[3] + abs(Error*eta)  
  }
  
  }
  
  SaveOutput[j] = sum(Total_Error)/4 # get the mean total error
  if(sum(Total_Error) == 0) break # end for loop if no mistakes were made

}    

print(sum(Total_Error))
print(W)

# calculate things for plotting
slope = W[1]/(-W[2])
intercept = W[3]/(-W[2])

# jpeg("C:/Users/Owner/Desktop/plot.jpg",height=10,width=20,units="in",res=300)
par(mfrow = c(1,2)) 
plot(1:j,SaveOutput,type="l",ylab="Error",xlab="iterations",main="error after n interations",ylim=c(0,1)) # plot error rate at the end
plot(x,y, col = as.factor(correct_output),pch=19,xlim=c(-0.5,1.5),main="decision boundary") # plot decision boundary
abline(intercept,slope,lwd=2)
# dev.off()

