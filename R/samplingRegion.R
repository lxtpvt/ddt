


rangeContinuous <- function(X){
  #sapply(X, typeof) -> X.types

  sapply(X, class) -> X.class

  names(which(X.class=="numeric")) -> namesX.continuous
  X.continuous = X[,namesX.continuous]

  return(sapply(X.continuous, range))
}



