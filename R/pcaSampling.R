# PCA sampling

# samplingRegion: a matrix for the hypercube support of continuous covariates
pcaSampling <- function(X, samplingDim, n, samplingRegion) {

  p = dim(X)[2]
  res.pca <- prcomp(X,scale=TRUE)
  X.pca = res.pca$x
  pcaRegion = (dataRange(as.data.frame(X.pca)))$numeric

  size_v = rep(1,p)
  (res.pca$sdev^2)[1:samplingDim]/(res.pca$sdev^2)[samplingDim]->tm
  size_v[1:samplingDim] = round(tm*((n/prod(tm))^(1/samplingDim)),0)

  #prod(size_v)

  temp_s = list()
  for (i in 1:samplingDim) {
    temp_s[[i]] <- runif(size_v[i],min = pcaRegion[1,i], pcaRegion[2,i])
  }
  for (i in (samplingDim+1):p) {
    temp_s[[i]] <- median(X.pca[,i],rm=TRUE)
  }

  pcaSample = matrix(nrow = prod(size_v),ncol = 0)
  for (i in 1:p) {
    if(i == 1){
      pcaSample = cbind(pcaSample,rep(temp_s[[i]],each = 1,
                                      times = prod(size_v[(i+1):p])))
    }else if(i>1 && i<p){
      pcaSample = cbind(pcaSample,rep(temp_s[[i]],each = prod(size_v[1:i-1]),
                                      times = prod(size_v[(i+1):p])))
    }else{
      pcaSample = cbind(pcaSample,rep(temp_s[[i]],each = prod(size_v[1:i-1]),
                                      times = 1))
    }
  }

  X_temp = pcaSample%*%t(res.pca$rotation)
  X_sampled = t(t(X_temp) * res.pca$scale + res.pca$center)

  good_flag = rep(TRUE,prod(size_v))
  for (j in 1:p) {
    good_flag = good_flag & (X_sampled[, j] >= samplingRegion[1,j] & X_sampled[, j] <= samplingRegion[2,j])
  }
  X_sampled = X_sampled[good_flag,]
  return(X_sampled)
}


