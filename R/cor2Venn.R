#' @title Correlation matrix to Venn diagram
#'
#' This function transforms correlation matrices into venn diagrams. The shared surface area of circles corresponds to the shared variance (R squared) or to another metric (e.g. Pearson / spearman correlation). The algorithm is an approximation based on a quasi-Newton algorithm.
#' @param cormat A (square) correlation matrix (or other n times n numeric matrix)
#' @param squared Shared surface areas between circles correspond to the squared correlation matrix (defaults to TRUE)
#' @param cor2dist Euclidean distances between data points are proportional to correlations (positive correlation = close, negative correlation = distant). Three options are available: cor2dist="1": distances are proportional to correlations; cor2dist="2": distances are equivalent to signed R squared, cor2dist="0": no optimization of distances between nodes (except for surface overlap)
#' @param fillmode Coloring of circles. There are three options: "Eigen" = according to first eigenvector; "mclust" = according to cluster ananlysis of coordinates; "manual" = manual filling of nodes (use manualfill to provide a vector)
#' @param Recode Automatically recodes the correlation matrix so that the highest correlation of each variable is always positive (defaults to TRUE)
#' @keywords Correlation plot, psychometrics
#' @return p A ggplot2 object showing the graphical approximation
#' @return xyr A three-column matrix providing x and y coordinates for all variables in the plot and the radius as depicted in the ggplot2 object
#' @return t CPU time required to find the optimized solution
#' @usage
#' cor2venn(cormat, Rsquared = TRUE, cor2dist=c("0","1","2"), Recode = TRUE,
#' Coloring = c("PC1", "Mclust", "manual"))
#'
#' @references
#' Martin Steppan (2022). corr2venn: Correlation to Venn diagramm. R package version 0.1.0.
#' @import ggplot2, pracma, ggforce, mclust, optimr, reshape2, MASS
#'
#' @examples
#' cormat <- cor(mtcars[,2:13])
#'
#' solve <- cor2venn(cormat)
#' plot(solve$p)



#' @export

cor2Venn <- function(cormat, Rsquared = TRUE, Recode = FALSE,maxit=100,threshold=-1,cor2dist=FALSE,startingvalues=list())
{


  start_time <- Sys.time()




  c<-completeCormat(cormat)

  if (Recode == TRUE){

    c<-autorecode(c)

  }

  e<-eigen(c)

  mc <- Mclust(c)




 n <-ncol(c)



  if (length(startingvalues)==0){
    startx<-e$vectors[,1]
    starty<-e$vectors[,2]
  }


  if (length(startingvalues)>0){


    startx<-startingvalues$x
    starty<-startingvalues$y
  }



  #startx<-rnorm(length(e$vectors[,1]),0,0.5)
  #starty<-rnorm(length(e$vectors[,1]),0,0.5)

  l = rep(-1*length(startx)^0.5,length(startx)*2)
  u = rep(1*length(startx)^0.5,length(startx)*2)

  sizel<-rep(0.2,length(startx))
  sizeu<-rep(2,length(startx))

  o <- NULL


  exclude<-logical(0)

  belows<-NA

  while (length(belows)>0){

  if (length(exclude)>0){
  startx<-startx[-exclude]
  starty<-starty[-exclude]
  o<-optimr(par=c(startx,starty),method="L-BFGS-B",lower=l[-exclude],upper=u[-exclude],fn=fit,cormat=c[-exclude,-exclude],Rsquared=Rsquared,cor2dist=cor2dist,control = list(fnscale=1,maxit = maxit, trace = 60,
                                                                       REPORT = 50))
  }

  if (length(exclude)==0){

    o<-optimr(par=c(startx,starty),method="L-BFGS-B",lower=l,upper=u,fn=fit,cormat=c,Rsquared=Rsquared,cor2dist=cor2dist,control = list(fnscale=1,maxit = maxit, trace = 60,
                                                                                                                                                                               REPORT = 50))
  }



  x<-o$par[1:length(startx)]
  y<-o$par[(length(startx)+1):(length(startx)*2)]
  s<-o$par[(length(startx)*2+1):(length(startx)*3)]
  s<-rep(1,length(startx))


  se<-seq(1,n,1)
  fill<-setdiff(se,exclude)
  xna<- rep(NA,n)
  yna<- rep(NA,n)

  xna[fill]<-x
  yna[fill]<-y

  end_time <- Sys.time()
  totaltime <- as.numeric(end_time - start_time)




  if (is.na(paste(startingvalues,collapse=""))==FALSE){totaltime<-totaltime + startingvalues$cputime}



  optimization<-matrix(c(Rsquared,cor2dist),ncol=2)
  colnames(optimization)<-c("Rsquared","cor2dist")

  result<-list(x,y,s,NA,as.numeric(totaltime),c,exclude,xna,yna)
  names(result)<-c("x","y","radius","modelfit","cputime","cormat","exclude","xna","yna")

  goodness<-ov(result,Rsquared=Rsquared)


  result<-list(optimization, x,y,s,goodness,as.numeric(totaltime),c,exclude,xna,yna)
  names(result)<-c("optimization","x","y","radius","modelfit","cputime","cormat","exclude","xna","yna")



  vf<-varfit(result,Rsquared=Rsquared)
  print(exclude)
  print(vf)

  belows<-which(vf[,1]<threshold)
  mi<-belows

  exclude<-c(exclude,mi)


  startx<-result$x
  starty<-result$y

  }
  result<-list(optimization, x,y,s,goodness,as.numeric(totaltime),c,exclude,xna,yna,vf)
  names(result)<-c("optimization","x","y","radius","modelfit","cputime","cormat","exclude","xna","yna","varfit")



  return(result)
}
