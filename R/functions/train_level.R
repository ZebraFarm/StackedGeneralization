# Train Level
train_level <- function(data, k){
  
  model <- rep(list(),3*k)
  
  degree = 1

  for(i in 1:k){
    
    model[[i]]   <- poly_reg(data,degree + i, 0) # No regularization
    model[[i+1]] <- poly_reg(data,degree + i, 0.5) # Some regularization
    model[[i+2]] <- poly_reg(data,degree + i, 1) # Lots regularization
    
  }  
  
  return(model)
}