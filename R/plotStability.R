

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










