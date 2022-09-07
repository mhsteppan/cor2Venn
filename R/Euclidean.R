euclidean<-function(cor2Vennobj){



  x<-cor2Vennobj$xna
  y<-cor2Vennobj$yna

  cormat<-as.matrix(cor2Vennobj$cormat)


  di<-matrix(NA,nrow=nrow(cormat),ncol=ncol(cormat))

  n<-length(x)




  for (i in 1:length(x)){
    for (j in 1:length(y)){

      di[i,j]<-dis(x[i],y[i],x[j],y[j])
    }
  }



 return(di)



}
