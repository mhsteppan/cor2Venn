#' Correlation matrix to Venn diagram
#'
#' This function transforms correlation matrices into venn diagrams. The shared surface area of circles corresponds to the shared variance (R squared) or to another metric (e.g. Pearson / spearman correlation). The algorithm is an approximation based on a quasi-Newton algorithm.
#' @param cormat A (square) correlation matrix (or other n times n numeric matrix)
#' @param squared Shared surface areas between circles correspond to the squared correlation matrix (defaults to TRUE)
#' @param cor2dist Distances between data points are proportional to correlations (positive correlation = close, negative correlation = distant; defaults to TRUE)
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

library(ggplot2)
data(mtcars)


cor2venn <- function(cormat, Rsquared = TRUE, Recode = TRUE)
{


  x<-c("a","b","c")

  plot <- ggplot(data=mtcars, aes(x=wt, y=mpg)) + geom_point()


  result<-list(plot,x)

  names(result)<-c("plot","x")

  return(result)
}
