# Generalize 
generalize <-  function(X,Y, k){

    # Measure all SSE of each algorithm
    # Assign weights depending on SSE_i / (SSE_total)

    SumsSquares <- numeric(k)
    for(i in 1:k) SumsSquares[i] = SSE(X[,i],Y)
    
    SumsSquares
    SumsSquaresTotal <- sum(SumsSquares)
    InvSumsSquares <- SumsSquaresTotal/SumsSquares
    InvTotalPerc <- sum(InvSumsSquares)
    
    weights <- InvSumsSquares / InvTotalPerc # This is just an estimate that I have found to be seemingly appropriate
    
    #model <- lm(Y ~ 0 + X[,1] + X[,2] + X[,3] + X[,4])
    return(weights)
}

SSE <- function(exp, truth){
  
  truth <- as.data.frame(truth)
  exp <- as.data.frame(exp)
  
  ans = sum ((exp - truth)^2)

  if(ans == 0) ans = 0.00001
  return(ans)
}
