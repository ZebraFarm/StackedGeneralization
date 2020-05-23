# Stacked Generalization

#Date: May 20, 2020
#Author: Cole Sibbald

# Prep
source('./functions/data_gen.R')
source('./functions/split.R')
source('./functions/poly_reg.R')
source('./functions/predict_level.R')
source('./functions/train_level.R')
source('./functions/train_predict.R')

library(caret)   # ML algorithms
library(mlbench) # datasets
library(ggplot2) # graphics
library(parallel)
no_cores <- detectCores()-1


# aX^2 + bX + c
true_model <- function(input, p = 1)
              {
                scale = c(1,0) # (aX^2, bX)
                shift = 0      # c

                tmp = input %*% t(scale)
                ans = tmp[,1]^2 + tmp[,2] + shift

                return(sum(ans))
              }

# make data
set.seed(100)
n = 100; d = 1; p = 1
sigma = 0.05  #for N(0, sigma)

tr.data <- data_gen(n, d, p, true_model, sigma)
te.data <- data_gen(n, d, p, true_model, sigma)

ggplot(dat,aes(X)) +
  geom_line(aes(y=Yt),colour="red") +
  geom_point(aes(y=Ye),colour="black")



l1.input <- data.frame()
models <- lapply(1:n,function(x) train.pred(data[,],x))

# Train Generalizer
# Weighted Least Squares, sum(w_i) = 1, w_i > 0.
SG.model <- genralize(l1.input)


# Test
true.label = te.data[,5]

models <- train.k(tr.data)
test.pred <- predict_level(models, te.data)            # n x kp vector
SG.pred <- predict(SG.model, newdata = test.pred)      # n x p vector



# Test All data

true.label = iris[,5]

test.pred <- predict_level(out.models, iris)          # n x kp vector
SG.pred   <- predict(SG.model, newdata = test.pred)   # n x p vector




# Statistics
#run statistics
model.stat <- function(res.k, res.SG, res.True){

#Majority <- mode(test.pred)                     # Classifiers
Averaged <- rowSums(test.pred) / k



cl  <- makeCluster(no_cores) # library(parallel)
SSE <- sum(parSapply(cl, 1:nrow(pred) , function(x) (pred[x,1:p] - pred[x,(p+1):(2*p)])^2 ) )
stopCluster(cl)

# Results

#confusionMatrix(Majority, true.label)$table
#confusionMatrix(Averaged, true.label)$table
confusionMatrix(SG.pred, true.label)$table


# Compare with single classifier
mod <- train(x = iris[rand[1:100],1:4],y = iris[rand[1:100],5], method = 'rf')
pr <- predict(mod, newdate = iris[,1:4])
confusionMatrix(pr,iris[,5])$table
  
} 