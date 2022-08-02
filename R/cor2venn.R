#' Correlation matrix to Venn diagram
#'
#' This function transforms correlation matrices into venn diagrams. The shared surface area of circles corresponds to the shared variance (R squared) or to another metric (e.g. Pearson / spearman correlation). The algorithm is an approximation based on a quasi-Newton algorithm.
#' @param cormat A (square) correlation matrix (or other n times n numeric matrix)
#' @param Rsquared Shared surface areas correspond to R^2 (defaults to TRUE)
#' @param DistProp Distances between data points are proportional to correlations (positive correlation = close, negative correlation = distant; defaults to TRUE)
#' @param Coloring Coloring of circles. There are three options: "PC1"
#' @param Recode Automatically recodes the correlation matrix so that the highest correlation of each variable is always positive (defaults to TRUE)
#' @keywords Correlation plot, psychometrics
#' @return p A ggplot2 object showing the graphical approximation
#' @return xyr A three-column matrix providing x and y coordinates for all variables in the plot and the radius as depicted in the ggplot2 object
#' @return t CPU time required to find the optimized solution
#' @usage
#' cor2venn(cormat, Rsquared = TRUE, DistProp = TRUE, Recode = TRUE,
#' Coloring = c("PC1", "Mclust", "manual"))
#'
#' @references
#' Martin Steppan (2022). corr2venn: Correlation to Venn diagramm. R package version 0.7.6. https://CRAN.R-project.org/package=dplyr
#' @import ggplot2
#'
#' @examples
#' cormat <- cor(mtcars[,2:13])
#'
#' solve <- cor2venn(cormat)
#' plot(solve$p)

cor2venn <- function(cormat, Rsquared = TRUE, Recode = TRUE)
{



  if(ncol(ffmprofiles)==5){

    if(nrow(ffmprofiles)>0){

      ExtraversionZ<-ffmprofiles[,1]
      AgreeablenessZ<-ffmprofiles[,2]
      ConscientiousnessZ<-ffmprofiles[,3]
      NeuroticismZ<-ffmprofiles[,4]
      OpennessZ<-ffmprofiles[,5]

      result<-data.frame(matrix(NA,nrow=nrow(ffmprofiles),ncol=11))

      a<-system.file("extdata","adjectives.csv",package="ffm2pd")
      adjectives<-read.csv(a,sep=";")

      p<-system.file("extdata","pdmeanvectordist.csv",package="ffm2pd")
      pdmeanvectordist<-read.csv(p,sep=",")


      me<-system.file("extdata","metaanalyse.csv",package="ffm2pd")
      metaanalyse<-read.csv(me,sep=";")

      d<-adjectives
      d<-data.frame(d)

      m<-pdmeanvectordist

      meta<-metaanalyse


      for (i in 1:nrow(ffmprofiles)){





        dis <- as.matrix(as.numeric(((d[,2]-AgreeablenessZ[i])^2 + (d[,3]-ExtraversionZ[i])^2 +(d[,4]-ConscientiousnessZ[i])^2 + (d[,5]-NeuroticismZ[i])^2 + (d[,6]-OpennessZ[i])^2)))

        pd<-  as.matrix(as.numeric(((meta[,2]*8-ExtraversionZ[i])^2 + (meta[,3]*8-OpennessZ[i])^2 +(meta[,4]*8-NeuroticismZ[i])^2 + (meta[,5]*8-ConscientiousnessZ[i])^2 + (meta[,6]*8-AgreeablenessZ[i])^2)))

        pdnorm<- (1-pnorm(as.matrix(as.numeric(((pd-m[,3])/m[,4])))))*100



        result[i,1:11]<-pdnorm

      }
      names(result)<-m[,2]
      return(result)




    }
    else {
      print("You need to provide at least one FFM profile")
    }
  }
  else {
    print("Your data needs exactly 5 columns of the order E-A-C-N-O (Extraversion, Agreeabeleness, Conscientiousness, Neuroticism, Openness")
  }

}
