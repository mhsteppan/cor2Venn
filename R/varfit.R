varfit<-function(cor2Vennobj,Rsquared,type="overlap",recode=F){


  exclude<-cor2Vennobj$exclude


    overlap<-overlap(cor2Vennobj)
    euclid<-euclidean(cor2Vennobj)



  ovlong<-melt(overlap)
  elong<-melt(euclid)


    c<-as.matrix(cor2Vennobj$cormat)


  if (recode==TRUE){
    c<-autorecode(c)
  }

  if (Rsquared==T){c<-c^2*(c/abs(c))}

  clong<-melt(c)


  u<-colnames(c)

  result<-data.frame(matrix(NA,nrow=length(u),ncol=2))
  rownames(result)<-u

  for (i in 1:length(u)){
    sel<-which(clong[,1]==u[i])
    if (length(sel)>0){
    result[i,1]<-cor(clong[sel,3],ovlong[sel,3],use="pairwise.complete.obs")
    result[i,2]<-cor(clong[sel,3],elong[sel,3],use="pairwise.complete.obs")*-1
  }}


return(result)

}
