ov<-function(cor2Vennobj,Rsquared=TRUE){



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
  olong<-melt(overlap)
  clong<-melt(as.matrix(cormat))

  dilong<-melt(di)

  wr<-cormat
  wrlong<-melt(as.matrix(wr))


  wr2<-cormat^2*(cormat/abs(cormat))
  wr2long<-melt(as.matrix(wr2))


  if (Rsquared==T){


    rr1<- 1-sum(overlap-cormat^2,na.rm=T)/(length(x)*length(x)-length(x))
    cc1<-cor(cbind(dilong[,3],wr2long[,3]*-1),use="pairwise.complete.obs")[1,2]

  }

  if (Rsquared==F){

    rr1<- 1-sum(overlap-cormat,na.rm=T)/(length(x)*length(x)-length(x))

    cc1<-cor(cbind(dilong[,3],wrlong[,3]*-1),use="pairwise.complete.obs")[1,2]

  }





    result <-c(rr1,cc1)

    if (Rsquared == T){
      names(result)<-c("Percent of Rsquared or r accurately depicted by overlap","r(Euclidean distance-Rsquared)")
    }

    if (Rsquared == F){
      names(result)<-c("Percent of abs(r) accurately depicted by overlap","r(Euclidean distance-r)")
    }
      return(result)


}
