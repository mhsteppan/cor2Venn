varfit<-function(cor2Vennobj,Rsquared,type="overlap"){


  exclude<-cor2Vennobj$exclude


    overlap<-overlap(cor2Vennobj)
    euclid<-euclidean(cor2Vennobj)





  ovlong<-melt(overlap)
  elong<-melt(euclid)


    c<-as.matrix(cor2Vennobj$cormat)




  if (Rsquared==T){c<-c^2*(c/abs(c))}

    oldcolnames<-colnames(c)
  colnames(c)<-rownames(c)
  u<-colnames(c)
  clong<-melt(c)




  result<-data.frame(matrix(NA,nrow=length(u),ncol=2))
  rownames(result)<-oldcolnames

  for (i in 1:length(u)){
    sel<-which(clong[,1]==u[i])
    if (length(sel)>0){
    result[i,1]<-cor(clong[sel,3],ovlong[sel,3],use="pairwise.complete.obs")
    result[i,2]<-cor(clong[sel,3],elong[sel,3],use="pairwise.complete.obs")*-1
    }}

  if (Rsquared==T){
    colnames(result) <- c("cor(overlap, R2)", "cor(dist, R2)")

  }

  if (Rsquared==F){
    colnames(result) <- c("cor(overlap, r)", "cor(dist, r)")

  }



return(result)

}
