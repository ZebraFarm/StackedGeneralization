
source('./functions/SG.R')


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

set.seed(100)
SG(n = 50:100, d = 1, p = 1, true.model)

# Statistics
#run statistics
model.stat <- function(res.k, res.SG, res.True){

#Majority <- mode(test.pred)                     # Classifiers
Averaged <- rowSums(test.pred) / k


SSE <- sum(sapply(cl, 1:nrow(pred) , function(x) (pred[x,1:p] - pred[x,(p+1):(2*p)])^2 ) )
MSE <- SSE/nrow(pred)
RMSE <- sqrt(MSE)


# Results

#confusionMatrix(Majority, true.label)$table
#confusionMatrix(Averaged, true.label)$table
confusionMatrix(SG.pred, true.label)$table


# Compare with single classifier
mod <- train(x = iris[rand[1:100],1:4],y = iris[rand[1:100],5], method = 'rf')
pr <- predict(mod, newdate = iris[,1:4])
confusionMatrix(pr,iris[,5])$table
  
} 