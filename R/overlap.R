overlap<-function(cor2Vennobj,Rsquared){



  x<-cor2Vennobj$x
  y<-cor2Vennobj$y

  cormat<-as.matrix(cor2Vennobj$cormat)


  overlap<-matrix(NA,nrow=nrow(cormat),ncol=ncol(cormat))
  di<-matrix(NA,nrow=nrow(cormat),ncol=ncol(cormat))

  n<-length(x)




  for (i in 1:length(x)){
    for (j in 1:length(y)){

      overlap[i,j]<-inters(x[i],y[i],1,x[j],y[j],1)
      di[i,j]<-dis(x[i],y[i],x[j],y[j])
    }
  }


  diag(overlap)<-NA
  diag(cormat)<-NA

  overlap<-overlap/pi
 return(overlap)



}
