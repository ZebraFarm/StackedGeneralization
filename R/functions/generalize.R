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
  
  # Breimann Optimized weights
  R = get_residuals(X,Y,k)
  #write.csv(R,'Res.csv')
  
  start <- rep(1/k,k)
  obj <- function(a) t(a/sum(a)) %*% R %*% (a/sum(a))
  weights <- nlminb(start, obj, lower = rep(0,k) )$par
  
  #cat("\n",weights/sum(weights))
  return(weights/sum(weights))
}


# Generalize 
#generalize <-  function(X,Y, k){
#  model <- lm(Y ~ 0 + . , data = X[,1:k])
  
#  coeff = model$coefficients
#  coeff[is.na(coeff)] = 0
  
#  return(coeff)
#}

#k = 30
#R = read.csv('Res.csv')
#R = as.matrix(R[2:31])
#R
#start <- rep(1/k,k)

#obj <- function(x) {
#  x = x / sum(x)
#  t(x) %*% R %*% (x)
#}

#ans = nlminb(start, obj, lower = rep(0,k) )$par
#optim(par = start, fn = obj, lower = rep(0,k) )$par
#ans/sum(ans)