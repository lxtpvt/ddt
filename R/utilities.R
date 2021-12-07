
setRpartPara <- function(method,control,predict_type){
  return(list(method = method, control = control, predict_type = predict_type))
}

# C is a vector
runifCategorical <- function(n, C){

  r_v = runif(n,min = 0,max = length(C))
  res = character(n)
  for (j in 1:n) {
    res[j] = C[ceiling(r_v[j])]
  }
  return(res)
}


criterionMse <- function(y, pred_y, type){



}

criterionMisclass <- function(y, pred_y, type){



}
