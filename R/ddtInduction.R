
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


# analysis split (stump) stability
# extract simulation results to a matrix
stumpsToMat <- function(stumps){

  simRes = list()
  for (stump in stumps$stump_list) {
    treeInfo(stump) -> stumpInfo
    var = stumpInfo$var[1]
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
  ids = (simResMatStump[,"var"]==nameCov)
  if(isNumeric){
    x = as.numeric(simResMatStump[ids,"split"])
    dx <- density(x,bw = "SJ")
    max = dx$x[which.max(dx$y)]
    return(list(name = nameCov, isNumeric = isNumeric, density = dx, max=max))
  }else{
    prop.table(table(simResMatStump[ids,"split"])) -> a
    return(list(name = nameCov, isNumeric = isNumeric, prop.table = a, max = names(which.max(a))))
  }
}

createNodeInfo <- function(nid, parentNodeInfo, secondClassStabilityStump, dataRange){

  # if it is the root
  if(nid==1){
    newPseudoTreeInfo = data.frame(list(var = "<leaf>",
                                              nid = 1,conditions = "root"), stringsAsFactors=FALSE)
    parentNodeInfo = list(nid = 1, pseudoTreeInfo = newPseudoTreeInfo)
  }

  if(nid %in% parentNodeInfo$pseudoTreeInfo$nid){
    # if children already exist
    if((nid*2) %in% parentNodeInfo$pseudoTreeInfo$nid | (nid*2+1) %in% parentNodeInfo$pseudoTreeInfo$nid){
      return("Child node is already in the tree.")
    }else{
      # create a new PseudoTreeInfo
      newPseudoTreeInfo = parentNodeInfo$pseudoTreeInfo
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

        unlist(strsplit(secondClassStabilityStump$max, ","))->tp_max
        dataRange$factor[[secondClassStabilityStump$name]]->all_level
        setdiff(all_level,tp_max) -> tp_n_max
        tp = tp_n_max[1]
        for (i in 2:length(tp_n_max)) {
          tp = paste0(tp,",",tp_n_max[i])
        }
        # if split on a factor covariate
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





