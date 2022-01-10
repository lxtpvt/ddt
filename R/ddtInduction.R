
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
  registerDoParallel(makeForkCluster(2))
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
























