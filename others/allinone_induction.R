library(randomForest)
library(rpart)
library(datasets)
library(foreach)
library(doParallel)


#=========================================================================================================
# tree function
#=========================================================================================================

getRids <- function(id){
  rids=id
  temp = id
  while (temp!=1) {
    rids=c(rids,floor(temp/2))
    temp=floor(temp/2)
  }
  rids
}

getAllNidByLevel <- function(level){
  return(1:(2^(level+1)-1))
}

treeInfo <- function(tree, digits = 5, minlength = 0L){
  frame=tree$frame
  frame$nid = as.numeric(row.names(tree$frame))
  frame$conditions<-labels(tree, digits = digits, minlength = minlength)
  frame
}

# create node information
pseudoTreeInfo <- function(nid, parentTreeInfo, secondClassStabilityStump, dataRange){

  # if it is the root
  if(nid==1){
    newPseudoTreeInfo = data.frame(list(var = "<leaf>",
                                        nid = 1,conditions = "root"), stringsAsFactors=FALSE)
    parentTreeInfo = list(nid = 1, pseudoTreeInfo = newPseudoTreeInfo)
  }
  # if nid is in the pseudoTreeInfo
  if(nid %in% parentTreeInfo$pseudoTreeInfo$nid){
    # if children already exist
    if((nid*2) %in% parentTreeInfo$pseudoTreeInfo$nid | (nid*2+1) %in% parentTreeInfo$pseudoTreeInfo$nid){
      return("Child node is already in the tree.")
    }else{
      # create a new PseudoTreeInfo
      newPseudoTreeInfo = parentTreeInfo$pseudoTreeInfo

      # (1) modify the var column
      newPseudoTreeInfo$var[which(newPseudoTreeInfo$nid==nid)] = secondClassStabilityStump$name
      # (2) add two children
      if(secondClassStabilityStump$isNumeric){
        # if split on a numerical covariate
        strCondition1 = paste0(secondClassStabilityStump$name, ">=",
                               as.character(secondClassStabilityStump$max))
        strCondition2 = paste0(secondClassStabilityStump$name, "<",
                               as.character(secondClassStabilityStump$max))

      }else{
        # if split on a factor covariate
        unlist(strsplit(secondClassStabilityStump$max, ","))->tp_max
        dataRange$factor[[secondClassStabilityStump$name]]->all_level
        setdiff(all_level,tp_max) -> tp_n_max
        tp = tp_n_max[1]
        if(length(tp_n_max)>1){
          for (i in 2:length(tp_n_max)) {
            tp = paste0(tp,",",tp_n_max[i])
          }
        }
        strCondition1 = paste0(secondClassStabilityStump$name, "=",
                               secondClassStabilityStump$max)

        strCondition2 = paste0(secondClassStabilityStump$name, "=", tp)
      }
      # add the children to newPseudoTreeInfo
      newPseudoTreeInfo = rbind(newPseudoTreeInfo,
                                list(var = "<leaf>", nid = nid*2,
                                     conditions = strCondition1))
      newPseudoTreeInfo = rbind(newPseudoTreeInfo,
                                list(var = "<leaf>", nid = nid*2+1,
                                     conditions = strCondition2))
      return(list(nid = nid, pseudoTreeInfo = newPseudoTreeInfo))
    }
  }else{
    return("This nid is not in the pseudoTreeInfo.")
  }
}

modifyPseudoTreeInfo <- function(nid, dataRange, stumpsRes, parentTreeInfo=NULL){
  if(length(stumpsRes$stump_list)==1){
    simResMatStump = stumpsToMat(stumpsRes$stump_list[[1]])
  }else if(length(stumpsRes$stump_list)>1){
    simResMatStump = stumpsToMat(stumpsRes$stump_list)
  }
  # first order stability
  pmf_mat = firstClassStabilityStump(dataRange$data_range$names, simResMatStump)
  # best covariate
  bestCovName = colnames(pmf_mat)[which.max(pmf_mat)]
  # second order stability
  secStb = secondClassStabilityStump(nameCov=bestCovName,
                                     isNumeric=isNumeric(bestCovName, dataRange$data_range), simResMatStump)
  # create node info
  tInfo = pseudoTreeInfo(nid=nid, parentTreeInfo=parentTreeInfo, secStb, dataRange$data_range)
  return(tInfo)
}
#=========================================================================================================
# sampling region
#=========================================================================================================

# predict_type = c("vector", "class")
setRpartPara <- function(method,control,predict_type){
  return(list(method = method, control = control, predict_type = predict_type))
}

#=========================================================================================================
# sampling region
#=========================================================================================================

setSamplingRegion <- function(X_range,treeInfo=NULL,id=NA){
  if(is.na(id)){
    return(list(data_range = X_range, id_range = X_range))
  }else{
    return(samplingRegion(id,X_range,treeInfo))
  }
}

# Sampling Range
dataRange <- function(X){

  if(!is.data.frame(X)){
    return("Please input a data frame!")
  }

  namesX.continuous=vector()
  namesX.categorical=vector()
  sapply(X, class) -> X.class
  for (i in 1:length(X.class)) {
    if("numeric" %in% X.class[[i]] || "integer" %in% X.class[[i]]){
      namesX.continuous=c(namesX.continuous,names(X.class)[i])
    }else if ("factor" %in% X.class[[i]]){
      namesX.categorical = c(namesX.categorical,names(X.class)[i])
    }else{
      return("Wrong format in the dataframe. numeric, integer or factor are needed.")
    }
  }
  numeric=NULL
  factor=NULL
  #int_levels <- function(x){as.numeric(levels(x))}
  if(length(namesX.continuous)!=0){
    X.continuous = data.frame(X[,namesX.continuous])
    colnames(X.continuous)<-namesX.continuous
    numeric = sapply(X.continuous, range)
  }
  if(length(namesX.categorical)!=0){
    X.categorical = data.frame(X[,namesX.categorical])
    colnames(X.categorical)<-namesX.categorical
    if(length(namesX.categorical)==1){
      factor = list(levels(X.categorical[,1]))
      names(factor)<-namesX.categorical
    }else{
      factor = sapply(X.categorical, levels)
    }
  }
  return(list(names=colnames(X),numeric=numeric,factor=factor))
}

rangeConditions <- function(id,treeInfo){
  df_numeric <- data.frame(nid=integer(), var=character(), sign=character(), split=double(),
                           stringsAsFactors=FALSE)
  df_factor <- data.frame(nid=integer(), var=character(), sign=character(), split=character(),
                          stringsAsFactors=FALSE)
  treeInfo[treeInfo$nid %in% getRids(id),c("nid","conditions")] -> conditions
  signs1 = c(">=","<=","!=")
  signs2 = c("<","=",">")
  for (i in 1:dim(conditions)[1]) {
    flag_signs1 = FALSE
    for (j_1 in 1:length(signs1)) {
      if(grepl(signs1[j_1],conditions[i,"conditions"],fixed = T)){
        strsplit(conditions[i,"conditions"],signs1[j_1])[[1]] -> temp_v
        if(signs1[j_1]!="!="){
          df_numeric = rbind(df_numeric,
                             data.frame(nid=as.integer(conditions[i,"nid"]),
                                        var=trimws(temp_v[1]),sign=signs1[j_1],
                                        split=as.numeric(trimws(temp_v[2]))))
        }else{
          df_factor = rbind(df_factor,
                            data.frame(nid=as.integer(conditions[i,"nid"]),
                                       var=trimws(temp_v[1]),sign=signs1[j_1],
                                       split=trimws(temp_v[2])))
        }
        flag_signs1=TRUE
        break
      }
    }
    if(!flag_signs1){
      for (j_2 in 1:length(signs2)) {
        if(grepl(signs2[j_2],conditions[i,"conditions"],fixed = T)){
          strsplit(conditions[i,"conditions"],signs2[j_2])[[1]] -> temp_v
          if(signs2[j_2]!="="){
            df_numeric = rbind(df_numeric,
                               data.frame(nid=as.integer(conditions[i,"nid"]),
                                          var=trimws(temp_v[1]),sign=signs2[j_2],
                                          split=as.numeric(trimws(temp_v[2]))))
          }else{
            df_factor = rbind(df_factor,
                              data.frame(nid=as.integer(conditions[i,"nid"]),
                                         var=trimws(temp_v[1]),sign=signs2[j_2],
                                         split=trimws(temp_v[2])))
          }
          break
        }
      }
    }
  }
  return(list(numeric = df_numeric, factor = df_factor))
}

samplingRegion <- function(id,X_range,treeInfo){

  id_X_range = X_range
  df_conditions = rangeConditions(id,treeInfo)
  df_numeric = df_conditions$numeric
  df_factor = df_conditions$factor
  # categorical variables
  if(dim(df_factor)[1]>0 && length(X_range$factor)>0){
    for (i in 1:dim(df_factor)[1]) {
      for (j in 1:length(X_range$factor)) {
        if(df_factor$var[i]==names(X_range$factor)[j]){
          unlist(strsplit(df_factor$split[i], ","))->tp_i
          id_X_range$factor[[j]] = intersect(id_X_range$factor[[j]], tp_i)
        }
      }
    }
  }

  # continuous variables
  if(dim(df_numeric)[1]>0 && dim(X_range$numeric)[1]>0){
    for (i in 1:dim(df_numeric)[1]) {
      for (j in 1:dim(X_range$numeric)[2]) {
        if(df_numeric$var[i]==colnames(X_range$numeric)[j]){
          if(df_numeric$sign[i] %in% c(">=",">")){
            if(X_range$numeric[1,j]<df_numeric$split[i]){
              id_X_range$numeric[1,j]=df_numeric$split[i]
            }
          }else if(df_numeric$sign[i] %in% c("<=","<")){
            if(X_range$numeric[2,j]>df_numeric$split[i]){
              id_X_range$numeric[2,j]=df_numeric$split[i]
            }
          }
        }
      }
    }
  }

  return(list(data_range = X_range, id_range = id_X_range))
}


#=========================================================================================================
# sampling strategies
#=========================================================================================================


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
      #print(i)
      #print(colnames(pca_sampled)[i])
      if(colnames(pca_sampled)[i] %in% names(samplingRegion$data_range$factor)){
        pca_sampled[,i] = factor(pca_sampled[,i],
                                 levels = samplingRegion$data_range$factor[[colnames(pca_sampled)[i]]])
      }
    }
    return(pca_sampled)
  }
}


#=========================================================================================================
# utilities function
#=========================================================================================================

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

#=========================================================================================================
# ddt functions
#=========================================================================================================


# Fit a large number of stumps
stumps <- function(nSim, fitedModel, samplingStrategies, sampleSize, rpartParas){

  # check the maxdepth parameter in rpart.control to keep maxdepth = 1
  if(rpartParas$control$maxdepth!=1){
    return("The maxdepth parameter should be set to 1!")
  }
  nStrategies = length(samplingStrategies)
  length(samplingStrategies[[1]]$samplingRegion$data_range$names) -> p
  samplingStrategies[[1]]$samplingRegion$data_range$names -> namesCov

  spT = list()


  for (j in 1:nStrategies) {
    for (i in 1:nSim) {
      X = do.call(samplingStrategies[[j]]$samplingMethod,
                  list(samplingStrategies[[j]]$samplingRegion,
                       samplingStrategies[[j]]$samplingParameters, sampleSize[[j]]))
      #print(head(X))
      y = do.call("predict",list(fitedModel,X))
      df = data.frame(X,y)
      stump = rpart(y~., data = df, method = rpartParas$method, control = rpartParas$control)
      spT = append(spT, list(stump))
      print(paste0(i, " out of ", nSim))
    }
  }
  return(stumpsList = list(stump_list = spT, samplingStrategies=samplingStrategies,
                           sampleSize=sampleSize, namesCov=namesCov))
}

# parallel version
stumpsParallel <- function(nSim, fitedModel, samplingStrategies, sampleSize, rpartParas, rm_seed=1){

  func_list = c("dataRange", "rangeConditions", "samplingRegion",
                "randomSampling", "pcaSamplingContinuous", "pcaSamplingSizeOne", "marginalRandomSampling")
  registerDoParallel(makeForkCluster(10))
  # check the maxdepth parameter in rpart.control to keep maxdepth = 1
  if(rpartParas$control$maxdepth!=1){
    return("The maxdepth parameter should be set to 1!")
  }
  nStrategies = length(samplingStrategies)
  length(samplingStrategies[[1]]$samplingRegion$data_range$names) -> p
  samplingStrategies[[1]]$samplingRegion$data_range$names -> namesCov
  spT = list()
  for (j in 1:nStrategies) {
    temp_spT =  foreach(i = 1:nSim, .export= func_list,
                        .packages = c("randomForest", "rpart")) %dopar% {
                          set.seed(rm_seed*i*j)
                          X = do.call(samplingStrategies[[j]]$samplingMethod,
                                      list(samplingStrategies[[j]]$samplingRegion,
                                           samplingStrategies[[j]]$samplingParameters, sampleSize[[j]]))
                          y = do.call("predict",list(fitedModel,X))
                          df = data.frame(X,y)
                          stump = rpart(y~., data = df, method = rpartParas$method, control = rpartParas$control)
                        }
    spT = append(spT, list(temp_spT))
  }
  return(stumpsList = list(stump_list = spT, samplingStrategies=samplingStrategies,
                           sampleSize=sampleSize, namesCov=namesCov))
}


# birth two children
birth <- function(nid, nSim, dataRange, sampleSize, pTreeInfo, stumpFun,
                  samplingParameters, samplingMethod, fitedModel, rpartParas){

  samplingRegion(nid,dataRange$data_range,pTreeInfo$pseudoTreeInfo)->sR
  #print(sR$id_range)
  # set root sampling strategies
  stg = setSamplingStrategy(samplingMethod=samplingMethod,
                            samplingRegion=sR,samplingParameters=samplingParameters)
  samplingStrategies = list(stg)
  # run simulation
  stumpsRes=do.call(stumpFun, list(nSim, fitedModel, samplingStrategies, sampleSize, rpartParas))
  # analysis simulation results and store it.
  #stumps_mat = stumpsToMat(stumpsRes$stump_list[[1]])
  return(list(nid = nid, stumpsRes = stumpsRes))

}

inductionByLevel <- function(level, dataX, samplingMethod, samplingParameters, stumpFun,
                             nSim, fitedModel, sampleSize, rpartParas){
  nids = getAllNidByLevel(level)
  sR = setSamplingRegion(X_range = dataRange(X))
  simResMatList = list()
  for (nid in nids) {
    if(nid==1){
      # if it's the root
      # set root sampling strategies
      stg = setSamplingStrategy(samplingMethod=samplingMethod,
                                samplingRegion=sR,samplingParameters=samplingParameters)
      samplingStrategies = list(stg)
      stumpsRes = do.call(stumpFun, list(nSim, fitedModel, samplingStrategies, sampleSize, rpartParas))
      treeInfo = modifyPseudoTreeInfo(nid = nid, dataRange = sR, stumpsRes = stumpsRes)
      simResMatList = append(simResMatList, list(stumpsToMat(stumpsRes$stump_list)))
    }else{
      resBirth = birth(nid=nid, nSim=nSim, dataRange=sR, sampleSize=sampleSize,
                       pTreeInfo=treeInfo, stumpFun=stumpFun, samplingParameters=samplingParameters,
                       samplingMethod=samplingMethod, fitedModel=fitedModel, rpartParas=rpartParas)
      treeInfo = modifyPseudoTreeInfo(nid = nid, dataRange = sR,
                                      stumpsRes = resBirth$stumpsRes, parentTreeInfo = treeInfo)
      simResMatList = append(simResMatList, list(stumpsToMat(resBirth$stumpsRes$stump_list)))
    }
  }
  return(list(treeInfo = treeInfo, simResMatList = simResMatList))
}



#===============================================================================
# Single stump stability for tree induction
#===============================================================================

# analysis split (stump) stability
# extract simulation results to a matrix
stumpsToMat <- function(stumps_list){
  simRes = list()
  for (stump in stumps_list) {
    #print(stump)
    treeInfo(stump) -> stumpInfo
    var = as.character(stumpInfo$var[1])
    condition = stumpInfo$conditions[2]
    split = parseCondition(condition)
    simRes = append(simRes, list(c(var = var, split = split)))
  }
  simResMatStump = do.call(rbind, simRes)
  return(simResMatStump)
}

# create the pmf of covariates
firstClassStabilityStump <- function(namesCovariates, simResMatStump){

  length(namesCovariates) -> p
  pmf_mat = matrix(0,nrow = 1,ncol = p)
  colnames(pmf_mat)<-namesCovariates
  n = dim(simResMatStump)[1]
  if(is.null(simResMatStump) | n==0){
    return("simResMatStump can't be empty!")
  }
  for (i in 1:p) {

    id_flag = (simResMatStump[,"var"]==namesCovariates[i])
    pmf_mat[1,namesCovariates[i]] = sum(id_flag)/n

  }
  return(pmf_mat)

}

# analyze simulation results based on covariate name
secondClassStabilityStump <- function(nameCov, isNumeric, simResMatStump){
  if(is.null(simResMatStump)){
    return("simResMat can't be empty!")
  }
  n = dim(simResMatStump)[1]
  ids = (simResMatStump[,"var"]==nameCov)
  if(isNumeric){
    if(n>1){
      x = as.numeric(simResMatStump[ids,"split"])
      dx <- density(x,bw = "SJ")
      max = dx$x[which.max(dx$y)]
    }else{
      # if n=1
      dx = NULL
      max=simResMatStump[1, "split"]
    }
    return(list(name = nameCov, isNumeric = isNumeric, density = dx, max=max))
  }else{
    if(n>1){
      prop.table(table(simResMatStump[ids,"split"])) -> a
    }else{
      # if n=1
      a = NULL
      max=simResMatStump[1, "var"]
    }
    return(list(name = nameCov, isNumeric = isNumeric, prop.table = a, max = names(which.max(a))))
  }
}

#=========================================================================================================
# run simulation
#=========================================================================================================
#=========================================================================================================
# run simulation
#=========================================================================================================
# The dataset
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- factor(cyl)
  gear <- factor(gear)
  carb <- factor(carb)
})
# covariates data matrix
X=mtcars2[,-1]
# Data range
sR = setSamplingRegion(X_range = dataRange(X))
covNames = sR$data_range$names

#===========================================
# number of simulations
nSim=50

#===========================================
# set rpart parameters for fitting stump
ctl <- rpart.control(xval=10, minbucket=5, minsplit=10,  cp=0, maxdepth = 1)
rpartParas = setRpartPara(method = "anova", control=ctl, predict_type="vector")

#===========================================
# Black-box model, random forest
mtcars_rf <- randomForest(mpg ~ ., data = mtcars2, importance = TRUE)

#===========================================
# set sampling parameters

# root
#pcaPara_root = setSamplingParameters(null = F, X=X, percentageVariance = 0.7)
# other nodes
pcaPara = setSamplingParameters(null = F, X=NULL, percentageVariance = 0.7, nRandomSampling = 5000)

n_X = dim(X)[1]
sampleSize = list()
n = 1
for (i in 1:n) {
  sampleSize = append(sampleSize, list(i*300*n_X))
}

#===============================================================================

mtcars_ddt=inductionByLevel(level=3, data=X, samplingMethod=marginalRandomSampling, samplingParameters=pcaPara,
                   stumpFun=stumps, nSim, fitedModel=mtcars_rf, sampleSize, rpartParas)
#save(mtcars_ddt, file = "mtcars_ddt.RData")
