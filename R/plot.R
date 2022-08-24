library(ggrepel)
library(mclust)
library(MASS)

cor2Vennplot <- function(cor2Vennobject, fillmode="Eigen", PCs=0, annotate=TRUE, showcenter=FALSE,manualcolors = NA, manualfill = NA, manualnodelabels = NA,manualalphafill=0.5,labelalpha=NA,labelfill=NA, avoidoverlap = TRUE,density=FALSE)
{



  x<-cor2Vennobject$x
  y<-cor2Vennobject$y
  s<-cor2Vennobject$radius
  c<-cor2Vennobject$cormat

  xx<-logical(0)
  yy<-logical(0)



  if (length(manualfill)>1){
    fillmode="manual"
  }

  dens<-data.frame(matrix(NA,nrow=length(x)*1000,ncol=2))

  for (i in 1:length(x)){
    xx<-c(xx,rnorm(1000,x[i],1))
    yy<-c(yy,rnorm(1000,y[i],1))
  }

  dens[,1]<-xx
  dens[,2]<-yy





  e<-eigen(c)


  if (is.na(manualfill[1])==T){

    if (fillmode == "Eigen"){

      if (sum(e$vectors[,1])<0){manualfill<-e$vectors[,1]*-1}
      if (sum(e$vectors[,1])>0){manualfill<-e$vectors[,1]}

    }

    if (fillmode == "mclust"){

      mc<-Mclust(c)

      manualfill<-paste("Cluster ", mc$classification,sep="")
    }

  }



  if (is.na(manualnodelabels)==TRUE){
    lbls<-colnames(c)
  }

  if (is.na(manualnodelabels)==FALSE){
    lbls<-manualnodelabels
  }





  dd<-data.frame(x,y,lbls,s,manualcolors,manualfill,manualalphafill,labelfill)
  colnames(dd)<-c("x","y","lbl","fill","manualalphafill","labelfill")







  p<-ggplot(data=dd)


  p<-p+geom_circle(aes(x0=as.numeric(x),y0=as.numeric(y),r=as.numeric(s),fill=manualfill),alpha=manualalphafill,col="transparent")

  if (density ==TRUE){
    p<-p+geom_density2d(data=dens,aes(x=as.numeric(dens[,1]),y=as.numeric(dens[,2])),col="gray")

  }

  #p<-p+geom_density2d_filled(data=dens,aes(x=as.numeric(dens[,1]),y=as.numeric(dens[,2])))
  #p<-p+geom_bin2d(data=dens,aes(x=as.numeric(dens[,1]),y=as.numeric(dens[,2])),bins=70,alpha=0.1)



  xmi<-min(dens[,1])
  xma<-max(dens[,1])
  ymi<-min(dens[,2])
  yma<-max(dens[,2])

  dens2<-data.frame(matrix(NA,nrow=100,ncol=100))


  if (showcenter==TRUE){

    p<-p+geom_point(aes(x=mean(x),y=mean(y),shape="Center"))
    p<- p +labs(shape="")
  }






  #p<-p+geom_circle(aes(x0=as.numeric(x),y0=as.numeric(y),r=as.numeric(s),fill=as.factor(clust)),col="transparent",alpha=0.5)
  #p<-p+geom_circle(aes(x0=o$par[1:length(startx)],y0=o$par[(length(startx)+1):(length(startx)*2)],r=s),alpha=0.2,fill="gray",col="transparent")


  if (avoidoverlap==FALSE){p<-p+geom_label(aes(x=x,y=y,label=lbl, alpha=as.numeric(labelalpha),fill=labelfill))}
  if (avoidoverlap==TRUE){p<-p+geom_label_repel(aes(x=x,y=y,label=lbl,alpha=as.numeric(labelalpha),fill=labelfill))}









  p<-p+theme_void()

  p<-p+coord_equal()




  Rsquared<-cor2Vennobject$optimization[1]
  cor2dist<-cor2Vennobject$optimization[2]

  modelfit<-ov(cor2Vennobject,Rsquared=Rsquared)


annotation<-"Visualization based on R package {cor2Venn}."

if (Rsquared=="TRUE"){
  annotation<-paste(annotation,"\n","Shared variance (R^2) is equivalent to shared surface. Model fit: ",round(as.numeric(modelfit[1]),digits=4),sep="")
}

if (Rsquared=="FALSE"){
  annotation<-paste(annotation,"\n","Correlation is equivalent to shared surface.","Accuracy (Pearson correlation): ",round(as.numeric(modelfit[1]),digits=4),sep="")
}

if (cor2dist==TRUE){
  if (Rsquared==FALSE){
  annotation<-paste(annotation,"\n","Euclidean distance between circles is equivalent to correlation. Accuracy (Pearson correlation): ",round(as.numeric(modelfit[2]),digits=4),sep="")
  }

  if (Rsquared==TRUE){
    annotation<-paste(annotation,"\n","Euclidean distance between circles is equivalent to signed R squared. Accuracy (Pearson correlation): ",round(as.numeric(modelfit[2]),digits=4),sep="")

  }

  }



if (annotate==TRUE){

  p<-p+labs(caption = annotation)


  }
  p<-p+ggtitle("Correlation to Venn Plot")


  if (fillmode=="Eigen"){p<-p+labs(fill="First Eigenvector")}
  if (fillmode=="mclust"){p<-p+labs(fill="Cluster")}

  if (fillmode=="manual"){p<-p+labs(fill="Manual fill")}



p



}

