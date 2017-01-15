print("Enter a number")

x <- scan(nmax=1) #scans the input

cat("You entered:", x, fill=TRUE)

if (x >= 0) {
  cat("It is a positive number\n")
  cat("Its double value is", 2*x, "\n")
  
} else {
  cat("It is a negative number", fill=TRUE)
  cat("Its double value is", 2*x, "\n")
}