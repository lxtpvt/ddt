
# if X=NULL set the follows
setSamplingParameters <- function(null=TRUE, X, percentageVariance, nRandomSampling=dim(X)[1]){
  if(null){
    return(NULL)
  }else{
    return(list(X = X, nRandomSampling = nRandomSampling, percentageVariance = percentageVariance))
  }
}

setSamplingStrategy <- function(samplingMethod, samplingRegion, samplingParameters){
  return(list(samplingMethod = samplingMethod, samplingRegion = samplingRegion,
              samplingParameters = samplingParameters))
}

#===============================================================================
# uniform random sampling
randomSampling <- function(samplingRegion, samplingParameters=NULL, n){

  #set.seed(round(runif(n=1, min =1, max = 200000)))

  X_sampled <- data.frame(matrix(NA,    # Create empty data frame
                                 nrow = n,
                                 ncol = length(samplingRegion$data_range$names)))
  colnames(X_sampled) <- samplingRegion$data_range$names


  n.factor = length(samplingRegion$id_range$factor)
  n.numeric = dim(samplingRegion$id_range$numeric)[2]

  if(n.factor>0){
    for (i in 1:n.factor) {
      sample(samplingRegion$id_range$factor[[i]], n, replace = TRUE) ->
        X_sampled[,which(colnames(X_sampled) == names(samplingRegion$id_range$factor)[i])]
    }
  }
  if(!is.null(n.numeric)){
    for (i in 1:n.numeric) {
      runif(n,min = samplingRegion$id_range$numeric[1,i],max = samplingRegion$id_range$numeric[2,i]) ->
        X_sampled[,which(colnames(X_sampled) == colnames(samplingRegion$id_range$numeric)[i])]
    }
  }
  # factor()
  for (i in 1:length(samplingRegion$data_range$names)) {
    if(colnames(X_sampled)[i] %in% names(samplingRegion$data_range$factor)){
      X_sampled[,i] = factor(X_sampled[,i],levels = samplingRegion$data_range$factor[[colnames(X_sampled)[i]]])
    }
  }
  return(X_sampled)
}

#===============================================================================
# PCA sampling

# samplingRegionNumeric: a matrix for the hypercube support of continuous covariates
pcaSamplingContinuous <- function(X, percentageVariance , samplingRegionNumeric) {

  #set.seed(round(runif(n=1, min =1, max = 100000)))

  p = dim(X)[2]
  res.pca <- prcomp(X,scale=TRUE)
  X.pca = res.pca$x
  pcaRegion = (dataRange(as.data.frame(X.pca)))$numeric

  # ratio of sample size
  for (i in 1:p) {
    if((percentageVariance-sum((res.pca$sdev^2/sum(res.pca$sdev^2))[1:i]))<=0){
      cfInd = i
      break
    }
  }
  ceiling(res.pca$sdev^2/(res.pca$sdev^2)[cfInd]) -> size_v
  if(0 %in% size_v){
    return("In X, observations' number n is less than covariates number p.")
  }

  # sampling in the pca region
  temp_s = list()
  for (i in 1:p) {
    temp_s[[i]] <- runif(size_v[i],min = pcaRegion[1,i], pcaRegion[2,i])
  }
  #
  if(cfInd>2){
    for (i in 2:(cfInd-1)) {
      temp_s[[i]] <- sample(temp_s[[i]],size_v[1],replace = TRUE)
    }
  }
  for (i in cfInd:p) {
    temp_s[[i]] <- rep(temp_s[[i]],size_v[1])
  }

  # construct the matrix
  pcaSample = matrix(nrow = size_v[1],ncol = 0)
  for (i in 1:p) {
    pcaSample = cbind(pcaSample,temp_s[[i]])
  }

  # pca transform
  X_temp = pcaSample%*%t(res.pca$rotation)
  X_sampled = t(t(X_temp) * res.pca$scale + res.pca$center)

  good_flag = rep(TRUE,size_v[1])
  for (j in 1:p) {
    good_flag = good_flag & (X_sampled[, j] >= samplingRegionNumeric[1,j] &
                               X_sampled[, j] <= samplingRegionNumeric[2,j])
  }
  X_sampled = X_sampled[good_flag,]

  if(sum(good_flag)==1){
    X_sampled = matrix(X_sampled,nrow = 1, ncol = length(X_sampled))
  }
  return(as.data.frame(X_sampled))
}


# pca sampling include both categorical and numerical covariates
pcaSamplingSizeOne <- function(X, samplingRegion, percentageVariance){

  # if numerical covariates do exist
  # first, do pca sampling
  X.continuous = X[,colnames(samplingRegion$data_range$numeric)]
  # do pca sampling
  pca_sampled = pcaSamplingContinuous(X.continuous,
                                      percentageVariance, samplingRegion$id_range$numeric)
  n_good_numeric_sampled = dim(pca_sampled)[1]
  # if pcaSamplingContinuous' return is not null
  if(n_good_numeric_sampled>0){
    n.factor = length(samplingRegion$data_range$factor)
    # if there are some categorical covariates, draw them and combined them with numeric covariates
    if(n.factor>0){
      for (i in 1:n.factor) {
        pca_sampled <- cbind(pca_sampled,
                             rep(sample(samplingRegion$id_range$factor[[i]], 1, replace = TRUE),
                                 n_good_numeric_sampled))
      }
    }
    colnames(pca_sampled)<-c(colnames(samplingRegion$data_range$numeric),
                             names(samplingRegion$data_range$factor))
    return(pca_sampled)
  }else{
    # if pcaSamplingContinuous' return is null
    return(NULL)
  }
}

# marginal random sampling based on pca random sampling
marginalRandomSampling <- function(samplingRegion, samplingParameters, n){

  if(is.null(samplingParameters)){
    return("Please set sampling parameters")
  }
  # first random sampling
  if(is.null(samplingParameters$X)){
    X = randomSampling(samplingRegion, samplingParameters=NULL, samplingParameters$nRandomSampling)
  }else{
    X = samplingParameters$X
  }

  n.numeric = dim(samplingRegion$data_range$numeric)[2]
  # if numerical covariates don't exist
  if(is.null(n.numeric)){
    # if there are only categorical covariates, do random sampling only in categorical ones.
    return(randomSampling(samplingRegion, samplingParameters=NULL, n))
  }else{
    pca_sampled = pcaSamplingSizeOne(X, samplingRegion, samplingParameters$percentageVariance)
    if(is.null(pca_sampled)){
      sz = 0
    }else{
      sz = dim(pca_sampled)[1]
    }
    while (sz<n) {
      pca_sampled = rbind(pca_sampled, pcaSamplingSizeOne(X, samplingRegion,
                                                          samplingParameters$percentageVariance))
      if(is.null(pca_sampled)){
        sz = 0
      }else{
        sz = dim(pca_sampled)[1]
      }
    }
    #print(head(pca_sampled))
    # factor()
    for (i in 1:length(samplingRegion$data_range$names)) {
      #print(names(samplingRegion$data_range$factor))
      print(i)
      print(samplingRegion$data_range$names))
      print(colnames(pca_sampled))
      if(colnames(pca_sampled)[i] %in% names(samplingRegion$data_range$factor)){
        pca_sampled[,i] = factor(pca_sampled[,i],
                                 levels = samplingRegion$data_range$factor[[colnames(pca_sampled)[i]]])
      }
    }
    return(pca_sampled)
  }
}


#===============================================================================
# This is a old version, never using.
# samplingRegionNum: a matrix for the hypercube support of continuous covariates
pcaSamplingContinuousOld <- function(X, samplingDim, n, samplingRegionNum) {

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
    good_flag = good_flag & (X_sampled[, j] >= samplingRegionNum[1,j] &
                               X_sampled[, j] <= samplingRegionNum[2,j])
  }
  X_sampled = X_sampled[good_flag,]
  return(X_sampled)
}


