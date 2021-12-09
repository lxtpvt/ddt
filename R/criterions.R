


criterionMse <- function(y, pred_y, type){

  if(type!="vector"){
    return("y should be the prediction of a regression")
  }

  # assume y and pred_y are vectors
  n = length(y)
  return(sum((y-pred_y)^2)/n)

}

criterionMisclass <- function(y, pred_y, type){

  if(type!="class"){
    return("y should be the prediction of a classification")
  }

  # assume y and pred_y are vectors
  n = length(y)
  return(sum(y==pred_y)/n)
}



