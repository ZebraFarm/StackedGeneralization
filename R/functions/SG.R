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


true_model <- function(input, p = 1){
  scale = c(1,0) # (aX^2, bX)
  shift = 0      # c
  
  tmp = input %*% t(scale)
  ans = tmp[,1]^2 + tmp[,2] + shift
  
  return(sum(ans))
}

# make data
#SG <- function(n, d, p, true.model){

sigma = 0.05  #for N(0, sigma)
n = 10; d = 1; p = 1

set.seed(10)
tr.data <- data_gen(n, d, p, true_model, sigma)
te.data <- data_gen(n, d, p, true_model, sigma)

ggplot(tr.data,aes(X)) +
  geom_line(aes(y=Yt),colour="red") +
  geom_point(aes(y=Ye),colour="black")


l1.input <- sapply(1:n,function(x) train_predict(tr.data[,],d,p,x))

# Train Generalizer
# Weighted Least Squares, sum(w_i) = 1, w_i > 0.
SG.model <- generalize(l1.input)


# Test
true.label = te.data$Yt

#models <- train_level(tr.data)
#test.pred <- predict_level(models, te.data)            # n x kp vector

test.pred <- train_predict()

SG.pred <- predict(SG.model, newdata = test.pred)      # n x p vector


# Test All data

true.label = iris[,5]

test.pred <- predict_level(out.models, iris)          # n x kp vector
SG.pred   <- predict(SG.model, newdata = test.pred)   # n x p vector

#}