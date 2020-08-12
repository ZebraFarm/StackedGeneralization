### Testing Stacked Generalization with NO leave-one-out or k-fold

remove(list = ls()) # Clear Environment
#setwd('./functions')
source('./SG.R')                # Seems to work, once setwd(./functions)
library(ggplot2)

# aX^2 + bX + c
true_model <- function(input, p = 1){
    
    coff = c(3, 1, 5) # (aX^2, bX, c)
    ans = coff[1] * (input^2) + coff[2] * input + coff[3]
    return(ans)
}

########################################
# Program Runs From Here!!
########################################

set.seed(9)

param.min = 5
param.max = 90
replicate = 100
iter = 5
d = 1
p = 1
k = 4
sigma = 1

res <- rep(list(), 3) 

tr.param_MSE <- data.frame("SampleSize" = param.min:param.max , "SG Model" = 0)
te.param_MSE <- data.frame("SampleSize" = param.min:param.max , "SG Model" = 0)
true.Model.MSE <- data.frame("SampleSize" = param.min:param.max , "True Model" = 0)
dg.MSE       <- data.frame("SampleSize" = param.min:param.max , "True Model" = 0)

for(param in seq(1,(param.max - param.min + 1),iter) ) {

  tr.ans <- rep(0,replicate)
  te.ans <- rep(0,replicate)
  dg.ans <- numeric(replicate)
  true.model.ans <- numeric(replicate)
  
  n = param.min + param - 1
  
  for(r in 1: replicate){
    # make data
    tr.data <- data_gen(n, d, p, true_model, sigma)
    te.data <- data_gen(1000, d, p, true_model, sigma)
    
    ## Train k - models +  Stacked Generalizer
    pred <- train_predict_all(tr.data,generate = FALSE, d,p, sigma)
    SG.weights <- generalize(pred[,1:k], pred$Ye, k)
    
    # Predct
    tr.SG.pred <- numeric( nrow( tr.data))
    te.SG.pred <- numeric( nrow( te.data))

    for(i in 1:nrow(tr.data)){
      for(j in 1:k){
        tr.SG.pred[i] = tr.SG.pred[i] + SG.weights[j] * pred[i,j]
      }
    }
    
    model <- rep(list(),4)
    
    model[[1]] <- lm(tr.data$Ye ~ I(tr.data[,1:d]^3)) # No regularization
    model[[2]] <- lm(tr.data$Ye ~ I(tr.data[,1:d]^4)) # No regularization
    model[[3]] <- lm(tr.data$Ye ~ I(tr.data[,1:d]^5)) # No regularization
    model[[4]] <- lm(tr.data$Ye ~ I(tr.data[,1:d]^6)) # No regularization
    
    pred <- matrix(ncol = 4, nrow = nrow(te.data) )
    pred[,1]   = model[[1]]$coefficients[1] + model[[1]]$coefficients[2] * te.data[,1:d]^3
    pred[,2]   = model[[2]]$coefficients[1] + model[[2]]$coefficients[2] * te.data[,1:d]^4
    pred[,3]   = model[[3]]$coefficients[1] + model[[3]]$coefficients[2] * te.data[,1:d]^5 
    pred[,4]   = model[[4]]$coefficients[1] + model[[4]]$coefficients[2] * te.data[,1:d]^6 
    pred = data.frame(pred, Ye = te.data$Yt)
    
    for(i in 1:nrow(te.data)){
      for(j in 1:k){
        te.SG.pred[i] = te.SG.pred[i] + SG.weights[j] * pred[i,j]
      }
    }
    
    tr.ans[r] <- sum(( (tr.SG.pred - tr.data$Ye)^2) /length(tr.data$Ye))
    te.ans[r] <- sum(( (te.SG.pred - te.data$Ye)^2) /length(te.data$Ye))
    
    dg.ans[r] <- 1/n * sum((tr.data$Ye - tr.data$Yt)^2) # MSE of train data
    
    ## Actual Best Model
    coff <- lm(tr.data$Ye ~ tr.data$X + I((tr.data$X)^2))$coefficients
    true.pred <- coff[1] + coff[2] * (tr.data$X) + coff[3] * (tr.data$X)^2
    true.model.ans[r] <- 1/n * sum((true.pred - tr.data$Ye)^2) # MSE of train data
    
  }
  tr.param_MSE[param,2] <- mean(tr.ans)
  te.param_MSE[param,2] <- mean(te.ans)
  dg.MSE[param,2]       <- mean(dg.ans)
  true.Model.MSE[param,2] <- mean(true.model.ans)
  
    
  cat(" ",param.max + 1 - param)
}

# Remove skipped elements (eg when Sample Size = 5,10,15,20...)
include = seq(1,(param.max - param.min + 1),iter)
tr.param_MSE = tr.param_MSE[include,]
te.param_MSE = te.param_MSE[include,]
dg.MSE     = dg.MSE[include,]
true.Model.MSE = true.Model.MSE[include,]

MSEs = data.frame(SampleSize = dg.MSE$SampleSize, 
                  tr = tr.param_MSE, 
                  te = te.param_MSE, 
                  True.Model = true.Model.MSE$True.Model,
                  dg.Model = dg.MSE$True.Model)
MSEs

ggplot(MSEs,aes(SampleSize)) + 
  geom_line(aes(y=tr.SG.Model,colour="Training SG")) +
  geom_line(aes(y=te.SG.Model,colour="Testing SG")) +
  geom_line(aes(y=True.Model,colour="True Model")) +
  geom_line(aes(y=dg.Model,colour="Gen Function")) +
  labs(y = "MSE",x="Sample Size",title = "MSE") + theme(legend.position = "bottom") + 
  scale_color_manual("", values = c("Testing SG" = "blue", "Training SG" = "black", "Gen Function" = "red","True Model" = "green"))

ggplot(MSEs,aes(SampleSize)) + 
  geom_line(aes(y=tr.SG.Model,colour="Training SG")) +
  geom_line(aes(y=te.SG.Model,colour="Testing SG")) +
  geom_line(aes(y=True.Model,colour="True Model")) +
  geom_line(aes(y=dg.Model,colour="Gen Function")) +
  labs(y = "MSE",x="Sample Size", title = "MSE : ZOOMED") + ylim(-0.001,3) +
  theme(legend.position = "bottom") + 
  scale_color_manual("", values = c("Testing SG" = "blue", "Training SG" = "black", "Gen Function" = "red","True Model" = "green"))

ggplot(MSEs,aes(SampleSize)) + 
  geom_line(aes(y=tr.SG.Model,colour="Training SG")) +
  geom_line(aes(y=te.SG.Model,colour="Testing SG")) +
  geom_line(aes(y=True.Model,colour="True Model")) +
  geom_line(aes(y=dg.Model,colour="Gen Function")) +
  labs(y = "MSE",x="Sample Size", title = "MSE : ZOOMED") + ylim(-0.001,1.3) +
  theme(legend.position = "bottom") + 
  scale_color_manual("", values = c("Testing SG" = "blue", "Training SG" = "black", "Gen Function" = "red","True Model" = "green"))


Diff = data.frame(Difference = MSEs$te.SG.Model - MSEs$tr.SG.Model, SampleSize = MSEs$SampleSize)
ggplot(Diff,aes(SampleSize)) + 
  geom_line(aes(y=Difference,colour="Difference")) +
  labs(y = "MSE",x="Sample Size",title = "MSE Difference Between Training and Testing") + theme(legend.position = "bottom") + 
  scale_color_manual("", values = c("Difference" = "black"))

ggplot(Diff,aes(SampleSize)) + 
  geom_line(aes(y=Difference),colour="black") +
  labs(y = "MSE",x="Sample Size",title = "MSE Difference Between Training and Testing: ZOOMED") + 
  theme(legend.position = "bottom") + ylim(-0.1,0.5)



#######################################
#Plot of 1 data instance
#######################################

n = 80
sigma = 1
tr.data <- data_gen(n, d, p, true_model, sigma)
te.data <- data_gen(1000, d, p, true_model, sigma)
res <- SG(n, d = 1, p = 1, sigma = 10, true.model, tr.data)
res[[3]] = tr.data$X

ggplot(tr.data,aes(X)) +
  geom_line(aes(y=Yt,colour="True Model")) +
  geom_point(aes(y=Ye,colour="Data")) + 
  geom_line(aes(y=res[[1]][[2]],colour="SG Fit")) + 
  labs(x = "X", y = "Y", title = "Train data")+ theme(legend.position = "bottom") + 
  scale_color_manual("", values = c("SG Fit" = "green", "Data" = "black", "True Model" = "red"))