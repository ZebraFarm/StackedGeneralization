
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
    scale = c(1,0) # (aX^2, bX)
    shift = 0      # c

    tmp = input %*% t(scale)
    ans = tmp[,1]^2 + tmp[,2] + shift

    return(sum(ans))
}

# Statistics
my.stat <- function(k.pred, gen.pred, true.label){
  k = length(k.pred)
  #Majority <- mode(k.pred)                     # Classifiers
  
  # Best k model
  SSE.k <- numeric(k)
  for(j in 1:k){
    SSE.k[j] <- sum(sapply(1:length(k.pred) , function(x) (k.pred[x,j] - true.label[x])^2 ) )}
  MSE_best <- min(SSE.k)/length(true.label)
  
  # Averaged Stats from k models
  Avg.pred <- rowMeans(k.pred)
  SSE <- sum(sapply(1:length(true.label) , function(x) (Avg.pred[x] - true.label[x])^2 ) )
  MSE_avg <- SSE/length(true.label)
  
  # SG stats
  SSE <- sum(sapply(1:length(true.label) , function(x) (gen.pred[x] - true.label[x])^2 ) )
  MSE_SG <- SSE/length(true.label)
  
  return(c(MSE_best, MSE_avg, MSE_SG))
}


########################################
# Program Runs From Here!!
########################################



set.seed(9)

param.min = 10
param.max = 100
replicate = 1
res <- rep(list(), 3) 

tmp <- data.frame(matrix(0, ncol = 3, nrow = (param.max - param.min + 1)))
tr.param_MSE <- data.frame("SampleSize" = param.min:param.max , "Best k Model" = tmp[,1], "Avg Model" = tmp[,2], "SG Model" = tmp[,3])
te.param_MSE <- data.frame("SampleSize" = param.min:param.max , "Best k Model" = tmp[,1], "Avg Model" = tmp[,2], "SG Model" = tmp[,3])

for(param in seq(1,(param.max - param.min + 1),10) ) {

  tr.ans <- matrix(0, nrow=replicate, ncol = 3)
  te.ans <- matrix(0, nrow=replicate, ncol = 3)

  for(j in 1: replicate){
    # make data
    tr.data <- data_gen(n, d, p, true_model, sigma)
    te.data <- data_gen(n, d, p, true_model, sigma)
    res <- SG(n = param.min + param - 1 , d = 1, p = 1, sigma = 10, true.model, tr.data, te.data)
  
    tr.k.pred      <- res[[1]][[1]]
    tr.SG.pred    <- res[[1]][[2]]
    tr.true.label  <- res[[1]][[3]]
    tr.ans[j,] <- my.stat(tr.k.pred, tr.SG.pred, tr.true.label)
    
    te.k.pred      <- res[[2]][[1]]
    te.SG.pred    <- res[[2]][[2]]
    te.true.label  <- res[[2]][[3]]
    te.ans[j,] <- my.stat(te.k.pred, te.SG.pred, te.true.label)
  }
  tr.param_MSE[param,2:4] <- colMeans(tr.ans)
  te.param_MSE[param,2:4] <- colMeans(te.ans)
  cat(param, " ")
}

tr.param_MSE = tr.param_MSE[seq(1,(param.max - param.min + 1),5),]
te.param_MSE = te.param_MSE[seq(1,(param.max - param.min + 1),5),]

ggplot(tr.param_MSE,aes(SampleSize)) + 
  geom_line(aes(y=Best.k.Model,colour="Best K Model")) +
  geom_line(aes(y=Avg.Model,colour="Average Model")) +
  geom_line(aes(y=SG.Model,colour="Stacked Model")) +
  labs(y = "MSE",x="Sample Size") + theme(legend.position = "bottom") + 
  scale_color_manual("", values = c("Best K Model" = "blue", "Average Model" = "red", "Stacked Model" = "black"))

ggplot(tr.param_MSE,aes(SampleSize)) + 
  geom_line(aes(y=Best.k.Model,colour="Best K Model")) +
#  geom_line(aes(y=Avg.Model,colour="Average Model")) +
  geom_line(aes(y=SG.Model,colour="Stacked Model")) +
  labs(y = "MSE",x="Sample Size") + theme(legend.position = "bottom") + 
  scale_color_manual("", values = c("Best K Model" = "blue", "Average Model" = "red", "Stacked Model" = "black"))


# Example Data
#sigma = 10  #for N(0, sigma)
#n = 100; d = 1; p = 1
#data <- data_gen(n, d, p, true_model, sigma)
#ggplot(data,aes(X)) +
#  geom_line(aes(y=Yt),colour="red") +
#  geom_point(aes(y=Ye),colour="black")
