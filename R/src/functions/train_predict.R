# Train & Pred # Single step in p-out
train_predict <- function(data, d, p, k, row){

  tr.data <- data[-row,]
  te.data <- data[row,]

  pred.row <- data.frame(matrix(ncol = k, nrow = p))
  FUN  <- legendre.polynomials(k+2)
  
  for(i in 1:k){
    
    train <- matrix(0, nrow = nrow(tr.data), ncol = i)
    test  <- matrix(0, nrow = nrow(te.data), ncol = i)
    
    for(j in 1:i){
      train[,j] <- predict(FUN[[j+2]], tr.data$X)
      test[,j]  <- predict(FUN[[j+2]], te.data$X) 
    }
    
    coeff  <- lm(tr.data$Ye ~ ., data = data.frame(train))$coefficients
    coeff[is.na(coeff)] = 0
    
    
    pred.row[,i]   = coeff[1]
    for(j in 1:i)
      pred.row[,i] = pred.row[,i] + coeff[i+1] * test[,i]
    
    #coeff <- lm(tr.data$Ye ~ I(tr.data[,1:d]^(i+2)))$coefficients
    #pred.row[,i]   = coeff[1] + coeff[2] * te.data[,1:d]^(i+2)
  }

  pred.row = data.frame(pred.row, Ye = te.data$Ye)
  
  return(pred.row)
}


# Train & Pred  All Data
train_predict_all <- function(tr.data, generate = TRUE, d, p, k, sigma){
  
  if(generate) 
    te.data = data_gen(1000,d,p,true_model, sigma)
  else 
    te.data = tr.data
  
  pred <- data.frame(matrix(ncol = k, nrow = nrow(te.data) ) )
  FUN  <- legendre.polynomials(k+2)
  
  for(i in 1:k){
    
    train <- matrix(0, nrow = nrow(tr.data), ncol = i)
    test  <- matrix(0, nrow = nrow(te.data), ncol = i)
    
    for(j in 1:i){
      train[,j] <- predict(FUN[[j+2]], tr.data$X)
      test[,j]  <- predict(FUN[[j+2]], te.data$X) 
    }
    
    coeff  <- lm(tr.data$Ye ~ ., data = data.frame(train))$coefficients
    coeff[is.na(coeff)] = 0
    
    
    pred[,i]   = coeff[1]
    for(j in 1:i)
      pred[,i] = pred[,i] + coeff[i+1] * test[,i]
    
    #coeff <- lm(tr.data$Ye ~ I(tr.data[,1:d]^(i+2)))$coefficients
    #pred[,i]   = coeff[1] + coeff[2] * te.data[,1:d]^(i+2)
  }
  
  pred = data.frame(pred, Ye = te.data$Ye) # I do no think this works p > 1

  return(pred)
}
