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
library(MASS)    # Models

#For Testing
#n = 100; d = 1; p = 1
#tr.data <- data_gen(n, d, p, true_model, sigma)
#te.data <- data_gen(n, d, p, true_model, sigma)

SG <- function(n, d, p, sigma, true.model, tr.data, te.data){

k = 4

#l1.input <- t (sapply(1:n,function(x) train_predict(tr.data[,],d,p,x)))
l1.input <- data.frame(matrix(ncol = k ,nrow = n), Yt = 0)
for(i in 1:n){
  l1.input[i,] <- train_predict(tr.data[,],d,p,i)
}

# Train Generalizer
# Weighted Least Squares, sum(w_i) = 1, w_i > 0.
SG.weights <- generalize(l1.input[,1:4],l1.input[,5], k)


# Test Training Data
true.label = tr.data$Yt
tr.pred <- train_predict_all(tr.data,tr.data, d,p)

tr.SG.pred <- numeric(nrow(tr.pred))
for(i in 1:nrow(tr.pred)){
  for(j in 1:k){
    tr.SG.pred[i] = tr.SG.pred[i] + SG.weights[j] * tr.pred[i,j]
  }
}

# Test Testing Data
true.label = te.data$Yt
te.pred <- train_predict_all(tr.data,te.data, d,p)

# Based on my cheap version of WLS estimates
te.SG.pred <- numeric(nrow(te.pred))
for(i in 1:nrow(te.pred)){
  for(j in 1:k){
    te.SG.pred[i] = te.SG.pred[i] + SG.weights[j] * te.pred[i,j]
  }
}

return(list(list(tr.pred[,1:4], tr.SG.pred, tr.data$Yt), 
            list(te.pred[,1:4], te.SG.pred, te.data$Yt)))
}