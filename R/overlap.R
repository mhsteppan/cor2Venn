overlap<-function(cor2Vennobj){


  ex<-cor2Vennobj$exclude


    x<-cor2Vennobj$xna
    y<-cor2Vennobj$yna
    cormat<-as.matrix(cor2Vennobj$cormat)








  overlap<-matrix(NA,nrow=nrow(cormat),ncol=ncol(cormat))
  di<-matrix(NA,nrow=nrow(cormat),ncol=ncol(cormat))

  n<-length(x)




  for (i in 1:length(x)){
    for (j in 1:length(y)){

      if ((is.na(x[i])==F && is.na(x[j])==F)==TRUE){
      overlap[i,j]<-inters(x[i],y[i],1,x[j],y[j],1)
      di[i,j]<-dis(x[i],y[i],x[j],y[j])
      }
    }
  }


  diag(overlap)<-NA
  diag(cormat)<-NA

  overlap<-overlap/pi
 return(overlap)



}
