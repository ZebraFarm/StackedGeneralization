# Train & Pred Level
train.predict <- function(data, i){
  
  #library(np) According to wikipedia?

  pred.i <- matrix(0,ncol = k, nrow = p) # k x p
  model <- rep(list(),3*k)
  degree = 1


  for(i in 1:k){
    
    model[[i]]   <- poly_reg(data[-i,],degree + i, 0) # No regularization
    predict(model[[i]],newdata = data[i,])

    model[[i+1]] <- poly_reg(data[-i,],degree + i, 0.5) # Some regularization
    predict(model[[i]],newdata = data[i,])

    model[[i+2]] <- poly_reg(data[-i,],degree + i, 1) # Lots regularization
    predict(model[[i]],newdata = data[i,])

  }  
  
  return(pred.i)
}