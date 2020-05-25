# Data Generation
data_gen <-  function(n, d, p, true_model,sigma = 0.05, a = -10, b = 10){
    
  if(a > b){
    tmp = a
    b = a
    a = tmp
  }
  
  x = seq(a,b,0.01)

  input  = matrix(sample(x, size = n*d, replace = TRUE),ncol = d)
  output = sapply(1:n, function(x) true_model(input[x,]))
  error  = matrix(rnorm(n*p, mean = 0, sd = sigma), ncol = p)

  data = data.frame(X = input, Ye = output + error, Yt = output)

  return(data)
}

#data_gen(10,2,1,true_model, 0.05)
