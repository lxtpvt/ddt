

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
    barplot(node$pmf/n, ylim=c(0,1))
    for (nm in name_set) {
      if(nm %in% numericVarsName){
        x=as.numeric(unlist(node[[nm]]))
        if(length(x)>1){
          dx <- density(x,bw = "SJ")
          plot(dx, lwd = 2, col = "red", main = nm)
          abline(v = dx$x[which.max(dx$y)])
          legend('topright',legend=parse(text=sprintf('Mode == %s',round(dx$x[which.max(dx$y)],2))),bty='n')
        }
      }else{
        barplot(prop.table(table(unlist(node[[nm]]))), main = nm, ylim = c(0,1))
      }
    }
  }else{
    par(mfrow=c(1,1))
    barplot(node$pmf/n, ylim=c(0,1))
  }
}

plotStableSize <- function(distance){
  par(mfrow = c(1,1))
  plot(distance$sampleSize, distance$distance, xlab = "Sample Size",
       ylab = "Distance", xlim =c(0,max(distance$sampleSize)*1.1), ylim =c(0,max(distance$distance)*1.1),
       type = "o")
}



