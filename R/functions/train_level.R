# Train Level
train_level <- function(data, d, p, k){

  # Train and predict
  model <- rep(list(),3*k)
  degree = 1    # degree of the polynomial predictor
  for(i in 1:k){
    
  	#Regression Models
    model[[i]]    <- lm(data[,d + 1:p] ~ poly(data,degree + i)) # No regularization
    model[[i+1]]  <- poly_reg(data,degree + i, d, p, 0.5)       # Some regularization
    model[[i+2]]  <- poly_reg(data,degree + i, d, p, 1)         # Lots regularization

	#Spline Models
	#GAM's

	# Nonpolynomial
  }  

  #Data frame with row size = 1
  return(model)
}