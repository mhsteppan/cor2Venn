ovcor<-function(par,cormat){
  overlap<-matrix(NA,nrow=nrow(cormat),ncol=ncol(cormat))
  n<-length(par)
  x=par[1:(n/2)]
  y=par[(n/2+1):n]


  for (i in 1:length(x)){
    for (j in 1:length(y)){

      overlap[i,j]<-inters(x[i],y[i],1,x[j],y[j],1)

    }
  }

  overlap<-overlap/pi
  r<-matrix()

  for (i in 1:length(x)){

    cc<-cor.test(overlap[,i],cormat[,i]^2)
    r[i]<-cc$estimate

  }
  r
}



