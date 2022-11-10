#' @title Automatically recodes a correlation matrix so that the strongest correlation of each variable is always positive
#'
#' This function transforms correlation matrices into venn diagrams. The shared surface area of circles corresponds to the shared variance (R squared) or to another metric (e.g. Pearson / spearman correlation). The algorithm is an approximation based on a quasi-Newton algorithm.
#' @param cormat A (square) correlation matrix (or other n times n numeric matrix)
#' @keywords Correlation plot, psychometrics, recoding
#' @return c A correlation matrix with recoded values
#' @usage
#' autorecode(cormat)
#'
#' @references
#' Martin Steppan (2022). corr2venn: Correlation to Venn diagramm. R package version 0.1.0.
#'
#' @examples
#' cormat <- cor(big5, use="pairwise.complete.obs)
#' cormat2 <- autorecode(cormat)



autorecode <- function(c){


  c<-completeCormat(c)

  n<-ncol(c)

  usum<-sum(c[upper.tri(c)],na.rm=T)
  lsum<-sum(c[lower.tri(c)],na.rm=T)

  if (usum == 0){c[upper.tri(c)]<-t(c[lower.tri(c)])}


  mc<-Mclust(abs(c))

  result<-c



  clusters<-unique(mc$classification)
  anchor<-matrix()

  for (i in clusters){

    sel<-as.numeric(which(mc$classification==i))

    anchor[i]<-sel[as.numeric(which.max(colMeans(c[sel,sel])))]
  }


  for (i in 1:n){

    if (c[i,anchor[mc$classification[i]]]<0){

      result[,i]<-result[,i]*-1
      result[i,]<-result[i,]*-1

      colnames(result)[i]<-paste(colnames(result)[i],"*",sep="")

    }


  }

  result



}


