# Train & Pred # Single step in p-out
train_predict <- function(data, d, p, k, row){

  tr.data <- data[-row,]
  te.data <- data[row,]

  pred.row <- data.frame(matrix(ncol = k, nrow = p))

  # Building models so that as i increases the model increases.
  for(i in 1:k){
    input <- matrix(tr.data$X, nrow = nrow(tr.data) ,ncol = i)
    for(j in 1:i) 
      input[,j] = input[,j]^(j+2)
    
    coeff  <- lm(tr.data$Ye ~ ., data = data.frame(input))$coefficients
    coeff[is.na(coeff)] = 0
  
    
    pred.row[,i]   = coeff[1]
    for(j in 1:i)
      pred.row[,i] = pred.row[,i] + coeff[i+1] * te.data[,1:d]^(j+2)
    
    #coeff <- lm(tr.data$Ye ~ I(tr.data[,1:d]^(i+2)))
    #pred.row[,i]   = coeff[1] + coeff[2] * te.data[,1:d]^(i+2)
  }


  pred.row = data.frame(pred.row, Ye = te.data$Ye) # I do not think this works p > 1

  #Data frame with row size = 1
  return(pred.row)
}


# Train & Pred  All Data
train_predict_all <- function(tr.data, generate = TRUE, d, p, k, sigma){
  
  if(generate) 
    te.data = data_gen(1000,d,p,true_model, sigma)
  else 
    te.data = tr.data
  
  pred <- data.frame(matrix(ncol = k, nrow = nrow(te.data) ) )
  
  for(i in 1:k){
    input <- matrix(tr.data$X, nrow = nrow(tr.data) ,ncol = i)
    for(j in 1:i) 
      input[,j] = input[,j]^(j+2)
    
    coeff  <- lm(tr.data$Ye ~ ., data = data.frame(input))$coefficients
    coeff[is.na(coeff)] = 0
    
    pred[,i]   = coeff[1]
    for(j in 1:i)
      pred[,i] = pred[,i] + coeff[i+1] * (te.data[,1:d]^(j+2))
  }
  
  pred = data.frame(pred, Ye = te.data$Ye) # I do no think this works p > 1

  return(pred)
}
