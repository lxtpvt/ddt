#=========================================================================================================
# plot functions
#=========================================================================================================


plotSplitStability <- function(node_list, nid, nTopCovs=0, numericVarsName = NULL){
  n = sum(node_list[[1]]$pmf)
  n.c = length(node_list[[1]]$pmf)
  node_list[[as.character(nid)]]->node
  if(is.null(node)){
    return("nid is not valid")
  }
  
  if(nTopCovs<0 | nTopCovs>n.c){
    return("nTopCovs is out of bounds.")
  }else if(nTopCovs>0 && !is.null(numericVarsName)){
    
    cov_names = names(node)[-1]
    top_names = (colnames(node$pmf)[order(node$pmf,decreasing = T)])[1:nTopCovs]
    name_set = top_names[match(intersect(top_names,cov_names),top_names)]
    par(mfrow=c(ceiling((length(name_set)+1)/2),2))
    barplot(node$pmf/n, ylim=c(0,1), main=paste("Node id: ",as.character(nid)))
    for (nm in name_set) {
      temp_mat = do.call(rbind, node[[nm]])
      if(nm %in% numericVarsName){
        x=as.numeric(temp_mat[,2])
        if(length(x)>1){
          dx <- density(x,bw = "SJ")
          plot(dx, lwd = 2, col = "red", main = nm)
          abline(v = dx$x[which.max(dx$y)])
          legend('topright',legend=parse(text=sprintf('Mode == %s',
                                                      round(dx$x[which.max(dx$y)],2))),bty='n')
        }
      }else{
        barplot(prop.table(table(temp_mat[,2])), main = nm, ylim = c(0,1))
      }
    }
  }else{
    par(mfrow=c(1,1))
    barplot(node$pmf/n, ylim=c(0,1), main=paste("Node id: ",as.character(nid)))
  }
  par(mfrow=c(1,1))
}

# Find the stable sample size
plotStableSize <- function(distance_list, name_vector, distance_type, title){
  par(mfrow = c(1,1))
  n = length(distance_list)
  distance = distance_list[[1]]
  plot(distance$sampleSize, distance$distance, xlab = "Sample Size",
       ylab = paste("Distance: ", distance_type),
       xlim =c(0,max(distance$sampleSize)*1.1),
       ylim =c(0,max(distance$distance)*1.1),
       main = title,
       type = "o")
  if(n>1){
    for (i in 2:n) {
      lines(distance_list[[i]]$sampleSize, distance_list[[i]]$distance, col = i)
      points(distance_list[[i]]$sampleSize, distance_list[[i]]$distance, col = i)
    }
  }
  legend("topright", name_vector,
         lty = rep(1,n),
         col = c(1:n))
}


# Plot the first-class stability of a split (stump)

plotFCstability <- function(pmf_mat, nid){
  #par(mfrow = c(1,1))
  barplot(pmf_mat, ylim=c(0,1), main=paste("Node id: ",as.character(nid)))
}

# Plot the second-class stability of a split (stump)
plotSCstability <- function(nameCov, isNumeric, simResMatStump, simSize, sampleSize){
  
  
  if(is.null(simResMatStump)){
    return("simResMat can't be empty!")
  }
  size_list = list()
  for (i in 1:length(simSize)) {
    
    if(i == 1){
      size_list = append(size_list, list(c(i:sum(simSize[c(1:i)]))))
    }else{
      size_list = append(size_list,
                         list(c((1+sum(simSize[c(1:(i-1))])):sum(simSize[c(1:i)]))))
    }
  }
  
  #par(mfrow=c(ceiling((length(simSize)+1)/2),2))
  
  for (i in 1:length(simSize)) {
    strMain = paste(nameCov, " - Sample Size: ")
    scRes = secondClassStabilityStump(nameCov,
                                      isNumeric, simResMatStump[size_list[[i]],])
    if(isNumeric){
      plot(scRes$density, lwd = 2, col = "red",
           main=paste(strMain,as.character(sampleSize[[i]])))
      abline(v = scRes$max)
      legend('topright',
             legend=parse(text=sprintf('Mode == %s',
                                       round(scRes$max,2))),bty='n')
    }else{
      barplot(scRes$prop.table, main = nameCov,
              ylim = c(0,1),main=paste(strMain,as.character(sampleSize[[i]])))
    }
  }
  #par(mfrow = c(1,1))
}

#=========================================================================================================
# ddt functions
#=========================================================================================================

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
                                        nid = 1,conditions = "root"))
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

#=========================================================================================================
#=========================================================================================================


load("C:\\Users\\xlu7\\OneDrive - mdanderson.org\\Documents\\ddt\\others\\stumps_mat1.RData")

pmf_mat = firstClassStabilityStump(sR$data_range$names, stumps_mat1)
plotFCstability(pmf_mat, 1)
nSim=100
simSize = rep(nSim,1)
sampleSize = list(100)
plotSCstability("disp", T, stumps_mat1, simSize, sampleSize)

secondClassStabilityStump1 = secondClassStabilityStump(nameCov="disp", isNumeric=T, stumps_mat1)

info_1 = createNodeInfo(nid=1, parentNodeInfo=NULL, secondClassStabilityStump1, dataRange=sR)
samplingRegion(2,dataRange(X),info_1$pseudoTreeInfo)->sR_2
samplingRegion(3,dataRange(X),info_1$pseudoTreeInfo)->sR_3



