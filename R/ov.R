ov<-function(cor2Vennobj,Rsquared=TRUE){


  exclude<-cor2Vennobj$exclude

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
      di[i,j]<-dis(x[i],y[i],x[j],y[j])}
    }
  }


  #diag(overlap)<-NA
  #diag(cormat)<-NA

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
    rr1<-cor(cbind(olong[,3],wr2long[,3]),use="pairwise.complete.obs")[1,2]
    cc1<-cor(cbind(dilong[,3],wr2long[,3]*-1),use="pairwise.complete.obs")[1,2]

    obs <- cormat^2
    imp <- overlap

    lobs <-  obs[!lower.tri(obs)]
    limp <-  imp[!lower.tri(imp)]


    srmr1 <- mean((overlap-cormat^2)^2,na.rm=T)^0.5

  }

  if (Rsquared==F){

    rr1<- 1-sum(overlap-cormat,na.rm=T)/(length(x)*length(x)-length(x))
    rr1<-cor(cbind(olong[,3],wrlong[,3]),use="pairwise.complete.obs")[1,2]
    cc1<-cor(cbind(dilong[,3],wrlong[,3]*-1),use="pairwise.complete.obs")[1,2]

    obs <- abs(cormat)
    imp <- overlap

    lobs <-  obs[!lower.tri(obs)]
    limp <-  imp[!lower.tri(imp)]

    srmr1 <- mean((overlap-cormat)^2,na.rm=T)^0.5


  }





    result <-c(rr1,srmr1,cc1)

    if (Rsquared == T){
      names(result)<-c("r (R2,surface overlap)","SRMR (R2,surface overlap)","r (Euclidean distance-Rsquared)")
    }

    if (Rsquared == F){
      names(result)<-c("r (r,surface overlap)","SRMR (r,surface overlap)","r (Euclidean distance-r)")
    }
      return(result)


}
