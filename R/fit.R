fit<-function(x2,cormat,Rsq,cor2d){




print(length(Rsq))


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





  if (Rsq==TRUE){
    target<-wr2
    ccc<-cor(cbind(olong[,3],wr2long[,3]),use="pairwise.complete.obs")[1,2]

    rr1<-mean((overlap-cormat^2)^2,na.rm=T)^0.5


    }
  if (Rsq==FALSE){
    target<-wr
    ccc<-cor(cbind(olong[,3],wrlong[,3]),use="pairwise.complete.obs")[1,2]

    rr1<-mean((overlap-abs(cormat))^2,na.rm=T)^0.5

    }







  if (cor2d==FALSE){
    cc <- 1
  }

  if (cor2d==TRUE){

    if (Rsq==TRUE){
    cc<-cor(cbind(dilong[,3],wr2long[,3]*-1),use="pairwise.complete.obs")[1,2]
}
    }

  if (Rsq==FALSE){
    cc<-cor(cbind(dilong[,3],wrlong[,3]*-1),use="pairwise.complete.obs")[1,2]
  }





  dev<-sd(colMeans((overlap-cormat^2)^2,na.rm=T))
  dev2<-sd(colMeans((di-cormat)^2,na.rm=T))




  phi.SRMR <- 1/(1+exp(8-100*rr1))


  old<-sum((overlap-target)^2,na.rm=T)/length(x)


  out<-paste(cc, " ", ccc,sep="")
  print(out)

  1/cc*1/ccc*rr1





}
