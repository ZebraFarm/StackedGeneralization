# Stacked Generalization

#Date: May 20, 2020
#Author: Cole Sibbald

# Prep
source('./data_gen.R')
source('./split.R')
source('./poly_reg.R')
source('./predict_level.R')
source('./train_level.R')
source('./train_predict.R')
source('./generalize.R')

library(caret)   # ML algorithms
library(mlbench) # datasets
library(ggplot2) # graphics
library(np)      # Nonparametric Kernel


#true_model <- function(input, p = 1){
#  scale = c(1,0) # (aX^2, bX)
#  shift = 0      # c
  
#  tmp = input %*% t(scale)
#  ans = tmp[,1]^2 + tmp[,2] + shift
  
#  return(sum(ans))
#}

SG <- function(n, d, p, sigma, true.model){

  #sigma for error generation ~ N(0, sigma)
n = 100; d = 1; p = 1

# make data
tr.data <- data_gen(n, d, p, true_model, sigma)
te.data <- data_gen(n, d, p, true_model, sigma)

#ggplot(tr.data,aes(X)) +
#  geom_line(aes(y=Yt),colour="red") +
#  geom_point(aes(y=Ye),colour="black")


#l1.input <- t (sapply(1:n,function(x) train_predict(tr.data[,],d,p,x)))

l1.input <- data.frame(matrix(ncol = 4,nrow = n), Yt = 0)
for(i in 1:n){
  l1.input[i,] <- train_predict(tr.data[,],d,p,i)
}
#l1.input$Yt

# Train Generalizer
# Weighted Least Squares, sum(w_i) = 1, w_i > 0.
SG.model <- generalize(l1.input[,1:4],l1.input[,5])


# Test
true.label = te.data$Yt
test.pred <- train_predict_all(tr.data,te.data, d,p)
#SG.pred <- predict(SG.model, newdata = test.pred)      # n x p vector
 
SG.pred <- numeric(nrow(test.pred))
coeff <- SG.model$coefficients
for(i in 1:nrow(test.pred)){
  SG.pred[i] <- coeff[1] * test.pred[i,1] + coeff[2] * test.pred[i,2] + coeff[3] * test.pred[i,3] + coeff[4] * test.pred[i,4]
}

return(list(test.pred[,1:4], SG.pred, true.label))
}