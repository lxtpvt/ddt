

# predict_type = c("vector", "class")
setRpartPara <- function(method,control,predict_type){
  return(list(method = method, control = control, predict_type = predict_type))
}
