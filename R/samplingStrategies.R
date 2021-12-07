
setSamplingStrategy <- function(method, samplingRegion, para){

  return(list(method = method, samplingRegion = samplingRegion, para = para))

}

# PCA sampling
# samplingRegionNum: a matrix for the hypercube support of continuous covariates
pcaSamplingContinuous <- function(X, samplingDim, n, samplingRegionNum) {

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
    good_flag = good_flag & (X_sampled[, j] >= samplingRegionNum[1,j] & X_sampled[, j] <= samplingRegionNum[2,j])
  }
  X_sampled = X_sampled[good_flag,]
  return(X_sampled)
}


marginalRandomSampling <- function(samplingRegion, para, n){

  X = para$X
  samplingDim = para$samplingDim
  dr = dataRange(as.data.frame(X))
  n.factor = length(dr$factor)
  n.numeric = dim(dr$numeric)[2]

  if(is.null(dim(dr$numeric)[2])){

    X.continuous = X[,colnames(dr$numeric)]
    numeric_sampled = pcaSamplingContinuous(X.continuous, samplingDim, n, samplingRegionNum)

  }else{

  }

}

randomSampling <- function(samplingRegion, para=NULL, n){

  X_sampled <- data.frame(matrix(NA,    # Create empty data frame
                            nrow = n,
                            ncol = length(samplingRegion$names)))
  colnames(X_sampled) <- samplingRegion$names


  n.factor = length(samplingRegion$factor)
  n.numeric = dim(samplingRegion$numeric)[2]

  if(n.factor>0){
    for (i in 1:n.factor) {
      factor(runifCategorical(n,samplingRegion$factor[[i]])) ->
        X_sampled[,which(samplingRegion$names == names(samplingRegion$factor)[i])]
    }
  }
  if(!is.null(n.numeric)){
    for (i in 1:n.numeric) {
      runif(n,min = samplingRegion$numeric[1,i],max = samplingRegion$numeric[2,i]) ->
        X_sampled[,which(samplingRegion$names == colnames(samplingRegion$numeric)[i])]
    }
  }
  return(X_sampled)
}


