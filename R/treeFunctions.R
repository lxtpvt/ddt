


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

