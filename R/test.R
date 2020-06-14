remove(list = ls()) # Clear Environment

#source("./functions/SG.R") # Not sure why this isn't working

#setwd('./functions')
source('./SG.R')                # Seems to work, once setwd(./functions)

#library(parallel)
#no_cores <- detectCores()-1
#cl <- makeCluster(no_cores) # library(parallel)
# FUNCTION GOES HERE
#stopCluster(cl)

# aX^2 + bX + c
true_model <- function(input, p = 1){
    
    coff = c(3, 1, 5) # (aX^2, bX, c)
    ans = coff[1] * (input^2) + coff[2] * input + coff[3]
    return(ans)
}

# Statistics
my.stat <- function(k.pred, gen.pred, true.label){
  k = length(k.pred)
  #Majority <- mode(k.pred)                     # Classifiers
  
  # Best k model
  SSE.k <- numeric(k)
  for(j in 1:k){
    SSE.k[j] <- sum((k.pred[,j] - true.label)^2)
  }
  MSE_best <- min(SSE.k)/length(true.label)
  
  # Averaged Stats from k models
  Avg.pred <- rowMeans(k.pred)
  SSE <- sum((Avg.pred - true.label)^2)
  MSE_avg <- SSE/length(true.label)
  
  # SG stats
  SSE <- sum((gen.pred - true.label)^2)
  MSE_SG <- SSE/length(true.label)
  
  return(c(MSE_best, MSE_avg, MSE_SG))
}


########################################
# Program Runs From Here!!
########################################



set.seed(9)

param.min = 59
param.max = 60
replicate = 1
iter = 1
d = 1
p = 1
sigma = 1

res <- rep(list(), 3) 

tmp <- data.frame(matrix(0, ncol = 3, nrow = (param.max - param.min + 1)))
tr.param_MSE <- data.frame("SampleSize" = param.min:param.max , "Best k Model" = tmp[,1], "Avg Model" = tmp[,2], "SG Model" = tmp[,3])
te.param_MSE <- data.frame("SampleSize" = param.min:param.max , "Best k Model" = tmp[,1], "Avg Model" = tmp[,2], "SG Model" = tmp[,3])
true.Model.MSE <- data.frame("SampleSize" = param.min:param.max , "True Model" = tmp[,1])
dg.MSE       <- data.frame("SampleSize" = param.min:param.max , "True Model" = tmp[,1])

for(param in seq(1,(param.max - param.min + 1),iter) ) {

  tr.ans <- matrix(0, nrow=replicate, ncol = 3)
  te.ans <- matrix(0, nrow=replicate, ncol = 3)
  dg.ans <- numeric(replicate)
  true.model.ans <- numeric(replicate)
  
  n = param.min + param - 1
  
  for(j in 1: replicate){
    # make data
    tr.data <- data_gen(n, d, p, true_model, sigma)
    tr.data
    res <- SG(n, d = 1, p = 1, sigma = 10, true.model, tr.data)
  
    tr.ans[j,]        <- my.stat(res[[1]][[1]], res[[1]][[2]], res[[1]][[3]])
    te.ans[j,]        <- my.stat(res[[2]][[1]], res[[2]][[2]], res[[2]][[3]])
    dg.ans[j]         <- 1/n * sum((tr.data$Ye - tr.data$Yt)^2) # MSE of train data
    
    coff <- lm(tr.data$Ye ~ tr.data$X + I((tr.data$X)^2))$coefficients
    tr.pred <- coff[1] + coff[2] * (tr.data$X) + coff[3] * (tr.data$X)^2
    true.model.ans[j] <- 1/n * sum((tr.pred - tr.data$Yt)^2) # MSE of train data
    
  }
  tr.param_MSE[param,2:4] <- colMeans(tr.ans)
  te.param_MSE[param,2:4] <- colMeans(te.ans)
  dg.MSE[param,2]       <- mean(dg.ans)
  true.Model.MSE[param,2] <- mean(true.model.ans)
  
    
  cat(" ",param.max + 1 - param, "\n")
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

ggplot(te.data,aes(X)) +
  geom_line(aes(y=Yt,colour="True Model")) +
  geom_point(aes(y=Ye,colour="Data")) + 
  geom_line(aes(y=res[[2]][[2]],colour="SG Fit")) + 
  labs(x = "X", y = "Y", title = "Test data")+ theme(legend.position = "bottom") + 
  scale_color_manual("", values = c("SG Fit" = "green", "Data" = "black", "True Model" = "red"))


#tr.data <- data_gen(10000, d, p, true_model, sigma)
#plot(tr.data$X,tr.data$Ye)
#model <- lm(tr.data$Ye ~ tr.data$X + I((tr.data$X)^2))
#coff <- model$coefficients
#tr.pred <- coff[1] + coff[2] * (tr.data$X) + coff[3] * (tr.data$X)^2
#t <- 1/n * sum((tr.pred - tr.data$Yt)^2) # MSE of train data
#summary(model)
