


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

  for (i in 1:dim(table)[1]) {
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
