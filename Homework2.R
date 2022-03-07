# Price vs Age of used car
library(matrixcalc)

# Price of car - y 
y <- matrix(c(6,9,8,10,11,12,11,13),8,1)
y

# Age of car - x
x <- matrix(0,8,2)
x[,1] <- 1
x[,2] <- c(6,5,4,3,2,2,1,1)
x

# Hat Matrix for the data
hat <- x %*% solve(t(x) %*% x) %*% t(x)
hat

# Symmetric hat matrix 
hat_trans <- t(hat)
hat_trans
all.equal(hat_trans,hat)


# Idempotent hat matrix
hat_2 <- hat %*% hat
hat_2
all.equal(hat_2,hat)


# Limits of diagonal elements
for (i in 1:8) {
  if(hat[i,i]<=1 | hat[i,i]>=0){
    print("TRUE")
  }else{
    print("FALSE")
  }
}

# Trace of hat matrix
trace <- 0
for (i in 1:8) {
  trace <- trace + hat[i,i]
}
trace

#---------------------------------------------------------------------------

# Trace of product of 2 matrices
for(i in 1:3){
  A<-matrix(sample(1:5,16, replace=TRUE),4,4);
  B<-matrix(sample(1:5,16, replace=TRUE),4,4);
  a <- sum(diag(A %*% B))
  b <- sum(diag(B %*% A))
  print(paste("Trace of AB is ", a))
  print(paste("Trace of BA is ", b))
}

