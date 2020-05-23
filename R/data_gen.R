# Data Generation
data_gen <-  function(n, d, p, true.model){
  
  data <- data.frame(matrix(ncol = (d + 2*p)) )  # x's + y_pred + y_true
  
  xi = seq(-3,3,0.1)
  
  vapply(1:nrow(data), function(x) true.model(data[x,]))
  
  rand  <- sample(nrow(data))
  error <- rnorm(n, mean = 0, sd = 1) # assuming p = 1
  
  ret.data = ret.data[, d+1] + error
  
  return(ret.data)
}