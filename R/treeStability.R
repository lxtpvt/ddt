
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
  return(table)
}




