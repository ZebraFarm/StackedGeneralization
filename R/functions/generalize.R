
get_residuals <- function(X,Y,k){
  
  n = length(Y)
  R = matrix(0,ncol = k, nrow = k)
  
  for(i in 1:k){ 
    for(j in 1:k){ 
      for(l in 1:n){
        R[i,j] = R[i,j] + (Y[l] - X[l,i]) * (Y[l] - X[l,j])
      }
    }  
  }
  
  return(R)
}

SSE <- function(exp, truth){
  
  ans = sum((exp - truth)^2)
  if(ans == 0) ans = 0.00001
  return(ans)
}

# Generalize 
generalize <-  function(X,Y, k){
      
  model <- lm(Y ~ 0 + X[,1] + X[,2] + X[,3] + X[,4])
  return(model$coefficients)
}




set.seed(10)
k = 4
Y = rbinom(100,1,p = 0.8)
X = data.frame(X1 = rbinom(100,1,p = 0.2),X2 = rbinom(100,1,p = 0.2),X3 = rbinom(100,1,p = 0.6),X4 = rbinom(100,1,p = 0.6))

# Find Appropriate Starting values    
SumsSquares <- numeric(k)
for(i in 1:k) SumsSquares[i] = SSE(X[,i],Y)

SumsSquaresTotal <- sum(SumsSquares)
InvSumsSquares <- SumsSquaresTotal/SumsSquares
InvTotalPerc <- sum(InvSumsSquares)
a <- InvSumsSquares / InvTotalPerc 
a
# Breimann Optimized weights

# Figure out constrained optimzation of     a'Ra , a >= 0 , sum(a) = 1
R = get_residuals(X,Y,k)

t(a) %*% R %*% a

obj <- function(x) t(x) %*% R %*% x
res <- nlminb(start = a, obj)
w <-  res$par
w/sum(w)

#library(CVXR)
#obj <- function(x) t(a) %*% R %*% a
#constr <- list(a >= 0, sum(a) == 1)
#pro <- Problem(Minimize(obj), constr)
#solve(pro)



