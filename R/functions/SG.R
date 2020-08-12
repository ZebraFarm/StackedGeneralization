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

#library(caret)   # ML algorithms
#library(mlbench) # datasets
#library(ggplot2) # graphics
#library(np)      # Nonparametric Kernel
#library(MASS)    # Models

#library(tidyverse) # Ridge Regression
#library(broom)     # Ridge Regression
#library(glmnet)    # Ridge Regression


SG <- function(n, d, p, k, sigma, true.model, tr.data){
  

########################
# Train
########################

l1.input <- data.frame(matrix(ncol = k ,nrow = n), Ye = 0)
for(row in 1:n){
  l1.input[row,] <- train_predict(tr.data,d,p, k, row)
}

# Train Generalizer
SG.weights <- generalize(l1.input[,1:k],tr.data$Ye, k)

########################
# Predict
########################

# Test Training Data
tr.pred <- train_predict_all(tr.data, generate = FALSE, d,p,k, sigma)

tr.SG.pred <- numeric(nrow(tr.pred))
for(i in 1:nrow(tr.pred)){
  for(j in 1:k){
    tr.SG.pred[i] = tr.SG.pred[i] + SG.weights[j] * tr.pred[i,j]
  }
}

# Test Testing Data
te.pred <- train_predict_all(tr.data,generate = TRUE, d,p,k, sigma)
te.SG.pred <- numeric(nrow(te.pred))
for(i in 1:nrow(te.pred)){
  for(j in 1:k){
    te.SG.pred[i] = te.SG.pred[i] + SG.weights[j] * te.pred[i,j]
  }
}

return(list(list(tr.pred[,1:4], tr.SG.pred, tr.pred$Ye), 
            list(te.pred[,1:4], te.SG.pred, te.pred$Ye),SG.weights))
}