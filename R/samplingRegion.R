
# Sampling Range


getRids <- function(id){
  rids=id
  temp = id
  while (temp!=1) {
    rids=c(rids,floor(temp/2))
    temp=floor(temp/2)
  }
  rids
}

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
  return(list(numeric=numeric,factor=factor))
}

treeInfo <- function(tree, digits = 5, minlength = 0L){
  frame=tree$frame
  frame$nid = as.numeric(row.names(tree$frame))
  frame$conditions<-labels(tree, digits = digits, minlength = minlength)
  frame
}

rangeConditions <- function(id,treeInfo){
  df_numeric <- data.frame(var=character(), sign=character(), split=double(),
                           stringsAsFactors=FALSE)
  df_factor <- data.frame(var=character(), sign=character(), split=character(),
                          stringsAsFactors=FALSE)
  treeInfo[treeInfo$nid %in% getRids(id),"conditions"] -> strConditions
  signs1 = c(">=","<=","!=")
  signs2 = c("<","=",">")
  for (i in 1:length(strConditions)) {
    flag_signs1 = FALSE
    for (j_1 in 1:length(signs1)) {
      if(grepl(signs1[j_1],strConditions[i],fixed = T)){
        strsplit(strConditions[i],signs1[j_1])[[1]] -> temp_v
        if(signs1[j_1]!="!="){
          df_numeric = rbind(df_numeric,
                             data.frame(var=trimws(temp_v[1]),sign=signs1[j_1],
                                        split=as.numeric(trimws(temp_v[2]))))
        }else{
          df_factor = rbind(df_factor,
                            data.frame(var=trimws(temp_v[1]),sign=signs1[j_1],
                                       split=trimws(temp_v[2])))
        }
        flag_signs1=TRUE
        break
      }
    }
    if(!flag_signs1){
      for (j_2 in 1:length(signs2)) {
        if(grepl(signs2[j_2],strConditions[i],fixed = T)){
          strsplit(strConditions[i],signs2[j_2])[[1]] -> temp_v
          if(signs2[j_2]!="="){
            df_numeric = rbind(df_numeric,
                               data.frame(var=trimws(temp_v[1]),sign=signs2[j_2],
                                          split=as.numeric(trimws(temp_v[2]))))
          }else{
            df_factor = rbind(df_factor,
                              data.frame(var=trimws(temp_v[1]),sign=signs2[j_2],
                                         split=trimws(temp_v[2])))
          }
          break
        }
      }
    }
  }
  return(list(numeric = df_numeric, factor = df_factor))
}

samplingRegion <- function(id,tree,X,digits = 5, minlength = 0L){

  X_range = dataRange(X)
  tree_info <- treeInfo(tree, digits = digits, minlength = minlength)
  df_conditions = rangeConditions(id,tree_info)
  df_numeric = df_conditions$numeric
  df_factor = df_conditions$factor
  # categorical variables
  if(dim(df_factor)[1]>0 && length(X_range$factor)>0){
    for (i in 1:dim(df_factor)[1]) {
      for (j in 1:length(X_range$factor)) {
        if(df_factor$var[i]==names(X_range$factor)[j]){
          unlist(strsplit(df_factor$split[i], ","))->tp_i
          X_range$factor[[j]] = intersect(X_range$factor[[j]], tp_i)
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
              X_range$numeric[1,j]=df_numeric$split[i]
            }
          }else if(df_numeric$sign[i] %in% c("<=","<")){
            if(X_range$numeric[2,j]>df_numeric$split[i]){
              X_range$numeric[2,j]=df_numeric$split[i]
            }
          }
        }
      }
    }
  }

  return(X_range)
}


