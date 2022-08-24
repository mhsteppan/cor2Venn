fit<-function(x2,cormat,Rsquared=TRUE,cor2dist="0"){





  cormat<-as.matrix(cormat)


  overlap<-matrix(NA,nrow=nrow(cormat),ncol=ncol(cormat))
  di<-matrix(NA,nrow=nrow(cormat),ncol=ncol(cormat))

  n<-length(x2)
  x=x2[1:(n/2)]
  y=x2[(n/2+1):n]



  for (i in 1:length(x)){
    for (j in 1:length(y)){

      overlap[i,j]<-inters(x[i],y[i],1,x[j],y[j],1)
      di[i,j]<-dis(x[i],y[i],x[j],y[j])
    }
  }


  diag(overlap)<-NA
  diag(cormat)<-NA

  overlap<-overlap/pi
  olong<-melt(overlap)
  clong<-melt(as.matrix(cormat))

  dilong<-melt(di)

  wr<-cormat
  wrlong<-melt(as.matrix(wr))


  wr2<-cormat^2*(cormat/abs(cormat))
  wr2long<-melt(as.matrix(wr2))





  if (Rsquared==TRUE){
    target<-wr2
    ccc<-cor(cbind(olong[,3],wr2long[,3]),use="pairwise.complete.obs")[1,2]
    }
  if (Rsquared==FALSE){
    target<-wr
    ccc<-cor(cbind(olong[,3],wrlong[,3]),use="pairwise.complete.obs")[1,2]
    }





  #cor(cbind(olong[,3],clong[,3]^2))[1,2]

  if (cor2dist==FALSE){
    cc<-1
  }

  if (cor2dist==TRUE){

    if (Rsquared==TRUE){
    cc<-cor(cbind(dilong[,3],wr2long[,3]*-1),use="pairwise.complete.obs")[1,2]
}
    }

  if (Rsquared==FALSE){
    cc<-cor(cbind(dilong[,3],wrlong[,3]*-1),use="pairwise.complete.obs")[1,2]
  }





  dev<-sd(colMeans((overlap-cormat^2)^2,na.rm=T))
  dev2<-sd(colMeans((di-cormat)^2,na.rm=T))

  rr1<-1-sum((overlap-cormat^2)^2,na.rm=T)/length(x)
  rr1<- 1-sum(overlap-cormat^2,na.rm=T)/(length(x)*length(x)-length(x))

  rr2<-1-sum((overlap-abs(cormat))^2,na.rm=T)/length(x)
  rr2<- 1-sum(overlap-cormat,na.rm=T)/(length(x)*length(x)-length(x))

  if (rr1>1){rr1<-"negative"}
  if (rr2>1){rr2<-"negative"}


  if (Rsquared==T){
    print(paste("Accuracy: ", rr1,"%"," r=",cc," ",ccc, sep=""))
  }

  if (Rsquared==F){
    print(paste("Accuracy: ", rr2," r=",cc," ",ccc,sep=""))
  }

  print(paste("overlap:",sum(overlap,na.rm=T),sep=""))
  print(paste("target:",sum(target,na.rm=T),sep=""))

  #sum(((overlap-wr2)^2)/length(x),na.rm=T)
  #sum(((overlap-wr2)^2)/length(x)*1/cc*dev*dev2,na.rm=T)


  old<-sum((overlap-target)^2,na.rm=T)/length(x)

  1/cc*1/ccc*old






}
