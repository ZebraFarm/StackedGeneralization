# Generalize 
generalize <-  function(X,Y){

    # Probably need numerical methods
    model <- lm(Y ~ 0 + X[,1] + X[,2] + X[,3] + X[,4])

    return(model)
}