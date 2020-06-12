# Train & Pred # Single step in p-out
train_predict <- function(data, d, p, row){

  k = 4
  
  tr.data <- data[-row,]
  te.data <- data[row,]

  model <- rep(list(),k)
  pred.row <- data.frame(matrix(ncol = k, nrow = p))

  model[[1]] <- lm(tr.data$Ye ~ I(tr.data[,1:d]^6)) # No regularization
  model[[2]] <- lm(tr.data$Ye ~ I(tr.data[,1:d]^5)) # No regularization
  model[[3]] <- lm(tr.data$Ye ~ I(tr.data[,1:d]^3)) # No regularization
  model[[4]] <- lm(tr.data$Ye ~ I(tr.data[,1:d]^4)) # No regularization

  # try lm.ridge(formula, lambda = c(a,b,c))
  # also try glmnet

  
  pred.row[,1]   = model[[1]]$coefficients[1] + model[[1]]$coefficients[2] * te.data[,1:d]^6   #predict(model[[1]], data.frame(x = te.data[,1:d]) )
  pred.row[,2]   = model[[2]]$coefficients[1] + model[[2]]$coefficients[2] * (te.data[,1:d]^5)
  pred.row[,3]   = model[[3]]$coefficients[1] + model[[3]]$coefficients[2] * (te.data[,1:d]^3)
  pred.row[,4]   = model[[4]]$coefficients[1] + model[[4]]$coefficients[2] * (te.data[,1:d]^4)

  
  #pred.row <- data.frame(matrix(ncol = 3*k, nrow = p))
  
  # Train and predict
  # model <- rep(list(),3*k)
  # degree = 1    # degree of the polynomial predictor
  # #for(i in 1:k){
    
  #   model[[i]]    <- lm(tr.data[,d + 1:p] ~ poly(tr.data,degree + i)) # No regularization
  #   pred.row[,i]   = predict(model[[i]],newdata = te.data)

  #   model[[i+1]]  <- poly_reg(tr.data,degree + i, 0.5) # Some regularization
  #   pred.row[,i+1] = predict(model[[i+1]],newdata = te.data)

  #   model[[i+2]]  <- poly_reg(tr.data,degree + i, 1) # Lots regularization
  #   pred.row[,i+2] = predict(model[[i+2]],newdata = te.data)

  # #Spline Models
  
  # #GAM's

  # # Nonpolynomial (or non-parametric) Kernel regression
  # library(np)
  # npreg(bws, txdat = tr.data[,1:d], tydat = tr.data[,d + 1:p])

  # }
  
  pred.row = data.frame(pred.row, Yt = te.data$Yt) # I do not think this works p > 1

  #Data frame with row size = 1
  return(pred.row)
}


# Train & Pred  All Data
train_predict_all <- function(tr.data, generate = TRUE, d, p, sigma){

  model <- rep(list(),4)
  
  model[[1]] <- lm(tr.data[,d + 1:p] ~ I(tr.data[,1:d]^6)) # No regularization
  model[[2]] <- lm(tr.data[,d + 1:p] ~ I(tr.data[,1:d]^5)) # No regularization
  model[[3]] <- lm(tr.data[,d + 1:p] ~ I(tr.data[,1:d]^3)) # No regularization
  model[[4]] <- lm(tr.data[,d + 1:p] ~ I(tr.data[,1:d]^4)) # No regularization
  #model[[4]] <- npreg(tydat = tr.data[,d + 1:p], txdat = tr.data[,1:d])
  
  if(generate) 
    te.data = data_gen(1000,d,p,true_model, sigma)
  else 
    te.data = tr.data
    
  pred <- data.frame(matrix(ncol = 4, nrow = nrow(te.data) ) )
  
  pred[,1]   = model[[1]]$coefficients[1] + model[[1]]$coefficients[2] * te.data[,1:d]^6 #predict(model[[1]], data.frame(x = te.data[,1:d]) )
  pred[,2]   = model[[2]]$coefficients[1] + model[[2]]$coefficients[2] * te.data[,1:d]^5
  pred[,3]   = model[[3]]$coefficients[1] + model[[3]]$coefficients[2] * te.data[,1:d]^3 
  pred[,4]   = model[[4]]$coefficients[1] + model[[4]]$coefficients[2] * te.data[,1:d]^4 
  
  #pred.row <- data.frame(matrix(ncol = 3*k, nrow = p))
  
  # Train and predict
  # model <- rep(list(),3*k)
  # degree = 1    # degree of the polynomial predictor
  # #for(i in 1:k){
  
  #   model[[i]]    <- lm(tr.data[,d + 1:p] ~ poly(tr.data,degree + i)) # No regularization
  #   pred.row[,i]   = predict(model[[i]],newdata = te.data)
  
  #   model[[i+1]]  <- poly_reg(tr.data,degree + i, 0.5) # Some regularization
  #   pred.row[,i+1] = predict(model[[i+1]],newdata = te.data)
  
  #   model[[i+2]]  <- poly_reg(tr.data,degree + i, 1) # Lots regularization
  #   pred.row[,i+2] = predict(model[[i+2]],newdata = te.data)
  
  # #Spline Models
  
  # #GAM's
  
  # # Nonpolynomial Kernel regression
  # library(np)
  # npreg(bws, txdat = tr.data[,1:d], tydat = tr.data[,d + 1:p])
  
  # }
  
  pred = data.frame(pred, Yt = te.data$Yt) # I do no think this works p > 1

  return(pred)
}
