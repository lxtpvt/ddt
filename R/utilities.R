
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

