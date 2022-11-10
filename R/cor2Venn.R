#' @title Correlation matrix to Venn diagram
#' @description This function transforms correlation matrices into venn diagrams. The shared surface area of circles corresponds to the shared variance (R squared) or to another metric (e.g. Pearson / spearman correlation). The algorithm is an approximation based on a quasi-Newton algorithm.
#' @param cormat A (square) correlation matrix (or other n times n numeric matrix)
#' @param squared Shared surface areas between circles correspond to the squared correlation matrix (defaults to TRUE)
#' @param cor2dist Euclidean distances between data points are proportional to correlations / signed R squared (high correlation / signed R squared = close, negative correlation / low R squared = distant).
#' @param fillmode Coloring of circles. There are three options: "Eigen" = according to first eigenvector; "mclust" = according to cluster ananlysis of coordinates; "manual" = manual filling of nodes (use manualfill to provide a vector)
#' @param threshold Variables with an accuracy (Pearson correlation) of < threshold will be dropped from the model, and a new model is calculated using the rest of the variables (defaults to -1, i.e. no variables are dropped)
#' @param autorecode Automatically recodes the correlation matrix so that the highest correlation of each variable is always positive (defaults to FALSE)
#' @param recode Vector containing the variables within the correlation matrix that will be inverted (*-1)
#' @param maxit The maximum number of iterations used by the optimization algorithm (optimr) defaults to 100
#' @keywords Correlation plot, psychometrics
#' @return p A ggplot2 object showing the graphical approximation
#' @return xyr A three-column matrix providing x and y coordinates for all variables in the plot and the radius as depicted in the ggplot2 object
#' @return t CPU time required to find the optimized solution
#' @usage
#' cor2Venn(cormat, Rsquared = TRUE, cor2dist=FALSE, Recode = TRUE)
#'
#' @references
#' Martin Steppan (2022). corr2venn: Correlation to Venn diagramm. R package version 0.1.0.
#'
#' @examples
#' cormat <- ids2cormat
#'
#' fit <- cor2Venn(cormat)
#' cor2Vennplot(fit)



#' @export

cor2Venn <- function(cormat, Rsquared=TRUE, cor2dist=TRUE, autorecode = FALSE,maxit=100,threshold=-1,startingvalues=list())
{


  start_time <- Sys.time()




  c<-completeCormat(cormat)
  cold<-c




  if (autorecode == TRUE){

    c<-autorecode(c)

  }

  e<-eigen(c)

  mc <- Mclust(c)

  print("Optimizing... Abort pressing ESC")


 n <-ncol(c)



  if (length(startingvalues)==0){


    startx<-e$vectors[,1]
    starty<-e$vectors[,2]

    #startx<-rnorm(n,0,1)
    #starty<-rnorm(n,0,1)
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


### THIS IS THE FUNCTION THAT DOES THE MINIMIZATION: CALL FOR COLLABORATION TO INCREASE THE SPEED OF THIS MINIMIZATION

   o<-optimr(par=c(startx,starty),method="L-BFGS-B",lower=l[-exclude],upper=u[-exclude],fn=fit,cormat=c[-exclude,-exclude],Rsq=Rsquared,cor2d=cor2dist,control = list(fnscale=1,maxit = maxit, trace = 60, REPORT = 50))
  }

  if (length(exclude)==0){

    o<-optimr(par=c(startx,starty),method="L-BFGS-B",lower=l,upper=u,fn=fit,cormat=c,Rsq=Rsquared,cor2d=cor2dist,control = list(fnscale=1,maxit = maxit, trace = 60, REPORT = 50))
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
  result<-list(optimization, x,y,s,goodness,as.numeric(totaltime),c,cold,exclude,xna,yna,vf)
  names(result)<-c("optimization","x","y","radius","modelfit","cputime","cormat","cormat_not_recoded","exclude","xna","yna","varfit")



  return(result)
}
