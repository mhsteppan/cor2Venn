#' @title Completes a correlation matrix, if only upper or lower triangle are present and / or diagonal values are not 1
#'
#' @param cormat A (square) correlation matrix (or other n times n numeric matrix)
#' @keywords data handling
#' @return c A complete correlation matrix
#' @usage
#' completeCormat(cormat)
#'

#' completeCormat(c)
#'
#'
#' @references
#' Martin Steppan (2022). corr2venn: Correlation to Venn diagramm. R package version 0.1.0.
#'
#' @examples
#' c<-cor(mtcars)
#' c[upper.tri(c)]<-NA
#' c





completeCormat <- function(c)
  {

  result<-c
  n<-ncol(c)

  diagsum<-sum(c[diag(c)],na.rm=T)


  if (diagsum == 0){diag(result)<-1}

  if (isSymmetric(c)==FALSE){

    for (i in 1:n){
      for (j in 1:n){

        if (is.na(result[i,j]==FALSE))
        {
          result[i,j]<-as.numeric(result[j,i])
          }

      }
    }

  }


return(result)



}

