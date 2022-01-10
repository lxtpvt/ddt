
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








