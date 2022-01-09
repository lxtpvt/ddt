
parseCondition <- function(condition){

  signs1 = c(">=","<=","!=")
  signs2 = c("<","=",">")

  for (j_1 in 1:length(signs1)) {
    if(grepl(signs1[j_1],condition,fixed = T)){
      strsplit(condition,signs1[j_1])[[1]] -> temp_v
      split=trimws(temp_v[2])
      return(split)
    }
  }
  for (j_2 in 1:length(signs2)) {
    if(grepl(signs2[j_2],condition,fixed = T)){
      strsplit(condition,signs2[j_2])[[1]] -> temp_v
      split=trimws(temp_v[2])
      return(split)
    }
  }
}

isNumeric <- function(strName, dR){

  if(strName %in% dR$names){
    if(strName %in% names(dR$factor)){
      return(F)
    }else{
      return(T)
    }

  }else{
    return(NULL)
  }

}




#===============================================================================
# sample() function  can instead of this function.
# C is a vector
runifCategorical <- function(n, C){

  r_v = runif(n,min = 0,max = length(C))
  res = character(n)
  for (j in 1:n) {
    res[j] = C[ceiling(r_v[j])]
  }
  return(res)
}

