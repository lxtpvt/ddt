

stableSampleSize <- function(fitedModel, samplingStrategy, rpartParas, sampleSize_v, criterion){

  m = length(sampleSize_v)
  distance = numeric(m)

  for (i in 1:m) {
    # training data
    X_tr = do.call(samplingStrategy$samplingMethod,
                   list(samplingStrategy$samplingRegion,
                        samplingStrategy$samplingParameters, sampleSize_v[i]))
    y_tr = do.call("predict",list(fitedModel,X_tr))
    df_sm = data.frame(X_tr,y_tr)
    # fit tree
    tree_m = rpart(y_tr~., data = df_sm, method = rpartParas$method,
                   control = rpartParas$control)
    # testing data
    X_te = do.call(samplingStrategy$samplingMethod,
                   list(samplingStrategy$samplingRegion,
                        samplingStrategy$samplingParameters, floor(sampleSize_v[i]*0.3)))
    y_te = do.call("predict",list(fitedModel,X_te))
    # predit
    pred = predict(object = tree_m, newdata = X_te, type = rpartParas$predict_type)
    # calculate distance
    distance[i] = do.call(criterion, list(y_te, pred, rpartParas$predict_type))
  }
  return(data.frame(sampleSize = sampleSize_v, distance = distance))
}


# do simulations: fit distillation tree
simStability <- function(nSim, fitedModel, samplingStrategy, sampleSize, rpartParas){

  length(samplingStrategy$samplingRegion$data_range$names) -> p
  samplingStrategy$samplingRegion$data_range$names -> namesCov
  matrix(0,nrow = 1,ncol = p)
  spT = list()

  for (i in 1:nSim) {
    X = do.call(samplingStrategy$samplingMethod,
                list(samplingStrategy$samplingRegion,
                     samplingStrategy$samplingParameters, sampleSize))
    y = do.call("predict",list(fitedModel,X))
    df = data.frame(X,y)
    tree = rpart(y~., data = df, method = rpartParas$method, control = rpartParas$control)
    spT = append(spT, list(splitTable(treeInfo(tree))))
  }

  return(simRes = list(splitTables = spT, namesCov=namesCov))

}


# analysis tree stability
stabilityAnalyze <- function(simRes){

  splitTables = simRes$splitTables
  length(simRes$namesCov) -> p
  pmf_mat = matrix(0,nrow = 1,ncol = p)
  colnames(pmf_mat)<-simRes$namesCov
  nid_v = c()

  for (sp in splitTables) {
    nid_v = union(nid_v,sp$nid)
  }
  nodes_list = split(nid_v,nid_v)

  for (i in 1:length(nid_v)) {
    nodes_list[[i]] = list(pmf = pmf_mat)
  }

  for (sp in splitTables) {
    for (j in 1:nrow(sp)) {
      ind = which(colnames(nodes_list[[as.character(sp$nid[j])]]$pmf)==sp$var[j])
      nodes_list[[as.character(sp$nid[j])]]$pmf[1,ind] =
        nodes_list[[as.character(sp$nid[j])]]$pmf[1,ind] + 1

      if(sp$var[j] %in% names(nodes_list[[as.character(sp$nid[j])]])[-1]){
        temp_id = which(names(nodes_list[[as.character(sp$nid[j])]])==sp$var[j])
        nodes_list[[as.character(sp$nid[j])]][[temp_id]] = append(nodes_list[[as.character(sp$nid[j])]][[temp_id]],sp$split[j])
      }else{
        nodes_list[[as.character(sp$nid[j])]] = append(nodes_list[[as.character(sp$nid[j])]],list(l = list(sp$split[j])))
        names(nodes_list[[as.character(sp$nid[j])]])[length(names(nodes_list[[as.character(sp$nid[j])]]))]=sp$var[j]
      }
    }
  }

  return(nodes_list)

}

#rpart:::tree.depth(as.numeric(rownames(boston_tree$frame)))->treeDp


