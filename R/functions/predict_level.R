# Predict Level
predict_level <- function(models, data){
  
  k = length(models)
  pred <- data.frame(matrix(nrow = length(data), ncol = k))
  
  for(r in 1:nrow(data)){
    for(i in 1:k){
        pred[r,i] <- predict(models[[i]], newdata = data[r,1:4])
    }
  }
  
  return(pred)
}