# Train & Pred Level
train_predict <- function(data, d, p, row){

  tr.data <- data[-row,]
  te.data <- data[row,]

  model <- rep(list(),4)
  pred.row <- data.frame(matrix(ncol = 4, nrow = p))

  model[[1]] <- lm(tr.data[,d + 1:p] ~ I(tr.data[,1:d]^1)) # No regularization
  model[[2]] <- lm(tr.data[,d + 1:p] ~ I(tr.data[,1:d]^2)) # No regularization
  model[[3]] <- lm(tr.data[,d + 1:p] ~ I(tr.data[,1:d]^3)) # No regularization
  model[[4]] <- npreg(tydat = tr.data[,d + 1:p], txdat = tr.data[,1:d])
  
  pred.row[,1]   = predict(model[[1]],newdata = te.data[,1:d])
  pred.row[,2]   = predict(model[[2]],newdata = te.data[,1:d])
  pred.row[,3]   = predict(model[[3]],newdata = te.data[,1:d])
  pred.row[,4]   = predict(model[[4]],newdata = te.data[,1:d])

  
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
  
  pred.row = data.frame(pred.row, te.data$Yt) # I do no think this works p > 1

  #Data frame with row size = 1
  return(pred.row)
}