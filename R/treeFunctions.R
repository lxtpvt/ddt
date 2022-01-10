


getRids <- function(id){
  rids=id
  temp = id
  while (temp!=1) {
    rids=c(rids,floor(temp/2))
    temp=floor(temp/2)
  }
  rids
}


treeInfo <- function(tree, digits = 5, minlength = 0L){
  frame=tree$frame
  frame$nid = as.numeric(row.names(tree$frame))
  frame$conditions<-labels(tree, digits = digits, minlength = minlength)
  frame
}


splitTable <- function(treeInfo){
  treeInfo[treeInfo$var!="<leaf>",c("nid","var")] -> table
  table$split = NA
  dim(table)[1]->n
  for (i in 1:n) {
    tempId = 2*table$nid[i]
    rangeConditions(tempId,treeInfo) -> conditions
    a = conditions$numeric[which(conditions$numeric$nid==tempId),"split"]
    b = conditions$factor[which(conditions$factor$nid==tempId),"split"]
    if(length(a>0)){
      table$split[i]=a
    }else if(length(b>0)){
      table$split[i]=b
    }
  }
  return(table[order(table$nid),])
}

snipNodes <- function(stableNodes){
  n_list = length(stableNodes)
  stableNodes_v = as.integer(unlist(stableNodes))
  for (i in n_list:1) {
    id_v = getRids(as.integer(stableNodes[[i]]))
    for (id in id_v) {
      if(!(id %in% stableNodes_v)){
        stableNodes_v[i]=NA
      }
    }
  }
  as.vector(na.omit(stableNodes_v))->nodes_keep
  l_children = nodes_keep*2
  r_children = nodes_keep*2 + 1
  return(setdiff(union(l_children, r_children),nodes_keep))

}

# create node information
createNodeInfo <- function(nid, parentNodeInfo, secondClassStabilityStump, dataRange){

  # if it is the root
  if(nid==1){
    newPseudoTreeInfo = data.frame(list(var = "<leaf>",
                                        nid = 1,conditions = "root"), stringsAsFactors=FALSE)
    parentNodeInfo = list(nid = 1, pseudoTreeInfo = newPseudoTreeInfo)
  }
  # if nid is in the pseudoTreeInfo
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
        # if split on a factor covariate
        unlist(strsplit(secondClassStabilityStump$max, ","))->tp_max
        dataRange$factor[[secondClassStabilityStump$name]]->all_level
        setdiff(all_level,tp_max) -> tp_n_max
        tp = tp_n_max[1]
        for (i in 2:length(tp_n_max)) {
          tp = paste0(tp,",",tp_n_max[i])
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







